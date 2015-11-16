%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines the data structures we use to record
% what entities are available from which modules and with what permissions.
%

:- module parse_tree.module_qual.id_set.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.module_qual.qual_errors.

:- import_module list.

%---------------------------------------------------------------------------%

    % We keep track of these kinds of entities.
:- type id_type
    --->    type_id
    ;       mode_id
    ;       inst_id
    ;       class_id.

    % This identifies an entity among other entities of the same kind.
:- type mq_id
    --->    mq_id(sym_name, int).

%---------------------------------------------------------------------------%
%
% We record two kinds of permissions for each entity:
%
% - whether it may be used in the interface, and
% - whether it may be used without module qualification.
%
% An entity may be used in the interface if it is defined in the current
% module or one of its ancestors, or if it is defined in a module that is
% imported (or used) in the interface.
%
% An entity may be used without module qualification if it is defined
% in the current module, in one of its ancestors, or in a module that is
% the subject of an explicit import_module declaration in the current module.
% It may not be used without qualification if it is made available by
% a use_module declaration, or if it is defined in a `.opt' and `.trans_opt'
% file.
%

:- type module_permissions
    --->    module_permissions(
                use_in_interface,
                need_qualifier
            ).

:- type use_in_interface
    --->    may_not_use_in_interface
    ;       may_use_in_interface.

%---------------------------------------------------------------------------%
%
% An id_set represents the set of entities of a particular kind
% whose definitions we have seen so far, and which are therefore available
% to resolve any ambiguities in unqualified references.
%
% Modules don't have an arity, but for simplicity we use the same
% data structure for modules as for types etc, assigning arity zero
% to all module names.
%

:- type id_set.

:- type type_id_set == id_set.
:- type inst_id_set == id_set.
:- type mode_id_set == id_set.
:- type class_id_set == id_set.
:- type module_id_set == id_set.

%---------------------------------------------------------------------------%
%
% The operations on id_sets.
%

:- pred id_set_init(id_set::out) is det.

    % Insert an mq_id into an id_set, aborting with an error if the
    % mq_id is not module qualified.
    %
:- pred id_set_insert(module_permissions::in, mq_id::in,
    id_set::in, id_set::out) is det.

    % Find the unique match in the current name space for a given mq_id
    % from a list of ids. If none exists, either because no match was found
    % or multiple matches were found, report an error.
    %
    % This predicate assumes that type_ids, inst_ids, mode_ids and
    % class_ids have the same representation.
    %
:- pred find_unique_match(mq_in_interface::in, mq_error_context::in,
    id_set::in, id_type::in, mq_id::in, sym_name::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

    % Check whether the parent module was imported, given the name of a
    % child (or grandchild, etc.) module occurring in that parent module.
    %
:- pred parent_module_is_imported(module_name::in, module_name::in,
    module_id_set::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module require.

% This want efficient retrieval of all the modules which define an id
% with a certain name and arity. We therefore implement an id_set as a
% staged map from the base name of an entity, to its arity, to the
% permissions map which records which modules define an entity of that
% name and arity and with what permissions.

:- type id_set == map(string, map(arity, permissions_map)).
:- type permissions_map == map(module_name, module_permissions).

%---------------------------------------------------------------------------%

id_set_init(IdSet) :-
    map.init(IdSet).

%---------------------------------------------------------------------------%

id_set_insert(Permissions, MQId, !IdSet) :-
    MQId = mq_id(SymName, Arity),
    (
        SymName = unqualified(_),
        unexpected($module, $pred, "unqualified id")
    ;
        SymName = qualified(ModuleName, BaseName),
        % Most of the time, BaseName does not occur in !.IdSet.
        % We therefore try the insertion first, and only if it fails
        % do we update the existing entry that caused that failure.
        FreshPermissionsMap = map.singleton(ModuleName, Permissions),
        FreshSubMap = map.singleton(Arity, FreshPermissionsMap),
        ( if map.insert(BaseName, FreshSubMap, !IdSet) then
            true
        else
            map.lookup(!.IdSet, BaseName, SubMap0),
            ( if map.search(SubMap0, Arity, PermissionsMap0) then
                insert_into_permissions_map(Permissions, ModuleName,
                    PermissionsMap0, PermissionsMap),
                map.det_update(Arity, PermissionsMap, SubMap0, SubMap),
                map.det_update(BaseName, SubMap, !IdSet)
            else
                map.det_insert(Arity, FreshPermissionsMap, SubMap0, SubMap),
                map.det_update(BaseName, SubMap, !IdSet)
            )
        )
    ).

:- pred insert_into_permissions_map(module_permissions::in, module_name::in,
    permissions_map::in, permissions_map::out) is det.

insert_into_permissions_map(NewPermissions, ModuleName, !PermissionsMap) :-
    ( if map.search(!.PermissionsMap, ModuleName, OldPermissions) then
        OldPermissions = module_permissions(OldMayUseInInterface, OldNeedQual),
        NewPermissions = module_permissions(NewMayUseInInterface, NewNeedQual),
        % Grant the permissions granted by either OldPermissions or
        % NewPermissions.
        (
            OldMayUseInInterface = may_use_in_interface,
            MayUseInInterface = OldMayUseInInterface
        ;
            OldMayUseInInterface = may_not_use_in_interface,
            MayUseInInterface = NewMayUseInInterface
        ),
        (
            OldNeedQual = may_be_unqualified,
            NeedQual = OldNeedQual
        ;
            OldNeedQual = must_be_qualified,
            NeedQual = NewNeedQual
        ),
        % Update the entry only if it changed.
        ( if
            MayUseInInterface = OldMayUseInInterface,
            NeedQual = OldNeedQual
        then
            true
        else
            Permissions = module_permissions(MayUseInInterface, NeedQual),
            map.det_update(ModuleName, Permissions, !PermissionsMap)
        )
    else
        map.det_insert(ModuleName, NewPermissions, !PermissionsMap)
    ).

%---------------------------------------------------------------------------%

find_unique_match(InInt, ErrorContext, IdSet, IdType, Id0, SymName,
        !Info, !Specs) :-
    % Find all IDs which match the current id.
    Id0 = mq_id(SymName0, Arity),
    mq_info_get_modules(!.Info, Modules),
    id_set_search_sym_arity(IdSet, Modules, SymName0, Arity,
        ModuleNamesPermissions),
    (
        ModuleNamesPermissions = [],
        % No matches for this id.
        MaybeUniqModuleName = no,
        mq_info_get_should_report_errors(!.Info, ReportErrors),
        (
            ReportErrors = should_report_errors,
            id_set_search_sym(IdSet, Modules, SymName0, PossibleArities),
            report_undefined_mq_id(!.Info, ErrorContext, Id0, IdType,
                [], PossibleArities, !Specs),
            mq_info_record_undef_mq_id(IdType, !Info)
        ;
            ReportErrors = should_not_report_errors
        )
    ;
        ModuleNamesPermissions = [ModuleName - ModulePermissions],
        % A unique match for this ID.
        ModulePermissions = module_permissions(MayUseInInterface, _),
        ( if
            InInt = mq_used_in_interface,
            MayUseInInterface = may_not_use_in_interface
        then
            MaybeUniqModuleName = no,
            mq_info_get_should_report_errors(!.Info, ReportErrors),
            (
                ReportErrors = should_report_errors,
                BadBaseName = unqualify_name(SymName0),
                BadSymName = qualified(ModuleName, BadBaseName),
                mq_info_get_this_module(!.Info, ThisModuleName),
                ( if ModuleName = ThisModuleName then
                    report_may_not_use_in_interface(ErrorContext, IdType,
                        BadSymName, Arity, !Specs)
                else
                    id_set_search_sym(IdSet, Modules, SymName0,
                        PossibleArities),
                    report_undefined_mq_id(!.Info, ErrorContext, Id0, IdType,
                        [ModuleName], PossibleArities, !Specs)
                ),
                mq_info_record_undef_mq_id(IdType, !Info)
            ;
                ReportErrors = should_not_report_errors
            )
        else
            MaybeUniqModuleName = yes(ModuleName)
        )
    ;
        ModuleNamesPermissions = [_, _ | _],
        (
            InInt = mq_used_in_interface,
            which_modules_may_be_used_in_interface(ModuleNamesPermissions,
                UsableModuleNamesPermissions, NonUsableModuleNames)
        ;
            InInt = mq_not_used_in_interface,
            UsableModuleNamesPermissions = ModuleNamesPermissions,
            NonUsableModuleNames = []
        ),
        (
            UsableModuleNamesPermissions = [],
            % There are several matches, but none is usable from the interface.
            MaybeUniqModuleName = no,
            mq_info_get_should_report_errors(!.Info, ReportErrors),
            (
                ReportErrors = should_report_errors,
                id_set_search_sym(IdSet, Modules, SymName0, PossibleArities),
                report_undefined_mq_id(!.Info, ErrorContext, Id0, IdType,
                    NonUsableModuleNames, PossibleArities, !Specs),
                mq_info_record_undef_mq_id(IdType, !Info)
            ;
                ReportErrors = should_not_report_errors
            )
        ;
            UsableModuleNamesPermissions = [ModuleName - _Permissions],
            MaybeUniqModuleName = yes(ModuleName)
        ;
            UsableModuleNamesPermissions = [_, _ | _],
            MaybeUniqModuleName = no,
            mq_info_get_should_report_errors(!.Info, ReportErrors),
            (
                ReportErrors = should_report_errors,
                assoc_list.keys(UsableModuleNamesPermissions,
                    UsableModuleNames),
                report_ambiguous_match(ErrorContext, Id0, IdType,
                    UsableModuleNames, NonUsableModuleNames, !Specs),
                mq_info_record_undef_mq_id(IdType, !Info)
            ;
                ReportErrors = should_not_report_errors
            )
        )
    ),
    (
        MaybeUniqModuleName = no,
        % Returning any SymName is fine, since it won't be used.
        Id0 = mq_id(SymName, _)
    ;
        MaybeUniqModuleName = yes(UniqModuleName),
        BaseName = unqualify_name(SymName0),
        SymName = qualified(UniqModuleName, BaseName),
        mq_info_set_module_used(InInt, UniqModuleName, !Info),
        ItemType = convert_simple_item_type(IdType),
        ItemName0 = item_name(SymName0, Arity),
        ItemName = item_name(SymName, Arity),
        update_recompilation_info(
            recompilation.record_used_item(ItemType, ItemName0, ItemName),
            !Info)
    ).

:- pred which_modules_may_be_used_in_interface(
    assoc_list(module_name, module_permissions)::in,
    assoc_list(module_name, module_permissions)::out,
    list(module_name)::out) is det.

which_modules_may_be_used_in_interface([], [], []).
which_modules_may_be_used_in_interface([Pair | Pairs],
        MayUsePairs, MayNotUseNames) :-
    which_modules_may_be_used_in_interface(Pairs,
        MayUsePairsTail, MayNotUseNamesTail),
    Pair = ModuleName - module_permissions(MayUseInInterface, _),
    (
        MayUseInInterface = may_use_in_interface,
        MayUsePairs = [Pair | MayUsePairsTail],
        MayNotUseNames = MayNotUseNamesTail
    ;
        MayUseInInterface = may_not_use_in_interface,
        MayUsePairs = MayUsePairsTail,
        MayNotUseNames = [ModuleName | MayNotUseNamesTail]
    ).

:- func convert_simple_item_type(id_type) = item_type.

convert_simple_item_type(type_id) = type_abstract_item.
convert_simple_item_type(mode_id) = mode_item.
convert_simple_item_type(inst_id) = inst_item.
convert_simple_item_type(class_id) = typeclass_item.

%---------------------------------------------------------------------------%

:- pred id_set_search_sym_arity(id_set::in, module_id_set::in,
    sym_name::in, int::in, assoc_list(module_name, module_permissions)::out)
    is det.

id_set_search_sym_arity(IdSet, ModuleIdSet, SymName, Arity,
        MatchingModuleNamesPermissions) :-
    UnqualName = unqualify_name(SymName),
    ( if
        map.search(IdSet, UnqualName, SubMap),
        map.search(SubMap, Arity, PermissionsMap)
    then
        find_matches_in_permissions_map(SymName, ModuleIdSet, PermissionsMap,
            MatchingModuleNamesPermissions)
    else
        MatchingModuleNamesPermissions = []
    ).

:- pred find_matches_in_permissions_map(sym_name::in, module_id_set::in,
    permissions_map::in,
    assoc_list(module_name, module_permissions)::out) is det.

find_matches_in_permissions_map(SymName, ModuleIdSet, PermissionsMap,
        MatchingModuleNamesPermissions) :-
    (
        SymName = unqualified(_),
        map.foldr(add_may_be_unqualified_modules, PermissionsMap,
            [], MatchingModuleNamesPermissions)
    ;
        SymName = qualified(ModuleName, _),

        % Compute the set of modules that this module specifier
        % could possibly refer to.
        %
        % Do a recursive search to find nested modules that match
        % the specified module name.
        ModuleArity = 0,
        id_set_search_sym_arity(ModuleIdSet, ModuleIdSet,
            ModuleName, ModuleArity, MatchingParentModuleNamesPermissions),
        ModuleBaseName = unqualify_name(ModuleName),
        AppendModuleBaseName = (pred(X::in, Y::out) is det :-
            Y = qualified(X, ModuleBaseName)
        ),
        assoc_list.keys(MatchingParentModuleNamesPermissions,
            MatchingParentModuleNames),
        list.map(AppendModuleBaseName,
            MatchingParentModuleNames, MatchingNestedModuleNames),

        % Add the specified module name itself, in case it refers to
        % a top-level (unnested) module name, since top-level modules
        % don't get inserted into the module_id_set.
        set.list_to_set([ModuleName | MatchingNestedModuleNames],
            MatchingModuleNamesSet),

        map.foldr(add_modules_in_set(MatchingModuleNamesSet), PermissionsMap,
            [], MatchingModuleNamesPermissions)
    ).

:- pred add_may_be_unqualified_modules(module_name::in, module_permissions::in,
    assoc_list(module_name, module_permissions)::in,
    assoc_list(module_name, module_permissions)::out) is det.

add_may_be_unqualified_modules(ModuleName, Permissions, !NamesPermissions) :-
    Permissions = module_permissions(_, NeedQual),
    (
        NeedQual = may_be_unqualified,
        !:NamesPermissions = [ModuleName - Permissions | !.NamesPermissions]
    ;
        NeedQual = must_be_qualified
    ).

:- pred add_modules_in_set(set(module_name)::in,
    module_name::in, module_permissions::in,
    assoc_list(module_name, module_permissions)::in,
    assoc_list(module_name, module_permissions)::out) is det.

add_modules_in_set(MatchingModuleNames, ModuleName, Permissions,
        !NamesPermissions) :-
    ( if set.member(ModuleName, MatchingModuleNames) then
        !:NamesPermissions = [ModuleName - Permissions | !.NamesPermissions]
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred id_set_search_sym(id_set::in, module_id_set::in, sym_name::in,
    set(int)::out) is det.

id_set_search_sym(IdSet, ModuleIdSet, SymName, PossibleArities) :-
    UnqualName = unqualify_name(SymName),
    ( if
        map.search(IdSet, UnqualName, SubMap)
    then
        map.to_assoc_list(SubMap, SubMapPairs),
        find_matching_arities(SymName, ModuleIdSet, SubMapPairs,
            set.init, PossibleArities)
    else
        set.init(PossibleArities)
    ).

:- pred find_matching_arities(sym_name::in, module_id_set::in,
    assoc_list(int, permissions_map)::in, set(int)::in, set(int)::out) is det.

find_matching_arities(_SymName, _ModuleIdSet, [], !PossibleArities).
find_matching_arities(SymName, ModuleIdSet, [Pair | Pairs],
        !PossibleArities) :-
    Pair = Arity - PermissionsMap,
    find_matches_in_permissions_map(SymName, ModuleIdSet, PermissionsMap,
        MatchingModuleNamesPermissions),
    (
        MatchingModuleNamesPermissions = []
    ;
        MatchingModuleNamesPermissions = [_ | _],
        set.insert(Arity, !PossibleArities)
    ),
    find_matching_arities(SymName, ModuleIdSet, Pairs, !PossibleArities).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

parent_module_is_imported(ParentModule, ChildModule, ModuleIdSet) :-
    % Find the module name at the start of the ChildModule;
    % this submodule will be a direct sub-module of ParentModule.
    DirectSubModuleName = get_first_module_name(ChildModule),

    % Check that the ParentModule was imported.
    % We do this by looking up the definitions for the direct sub-module
    % and checking that the one in ParentModule came from an
    % imported module.
    Arity = 0,
    map.search(ModuleIdSet, DirectSubModuleName, SubMap),
    map.search(SubMap, Arity, PermissionsMap),
    map.search(PermissionsMap, ParentModule, ParentModulePermissions),
    ParentModulePermissions = module_permissions(_, may_be_unqualified).

    % Given a module name, possibly module-qualified, return the name
    % of the first module in the qualifier list. For example, given
    % `foo.bar.baz', this returns `foo', and given just `baz',
    % it returns `baz'.
    %
:- func get_first_module_name(module_name) = string.

get_first_module_name(unqualified(ModuleName)) = ModuleName.
get_first_module_name(qualified(Parent, _)) = get_first_module_name(Parent).

%---------------------------------------------------------------------------%
:- end_module parse_tree.module_qual.id_set.
%---------------------------------------------------------------------------%

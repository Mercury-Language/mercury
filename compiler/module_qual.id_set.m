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
    ;       inst_id
    ;       mode_id
    ;       class_id.

    % This identifies an entity among other entities of the same kind.
:- type mq_id
    --->    mq_id(sym_name, int).

%---------------------------------------------------------------------------%
%
% We record two kinds of permissions for each entity:
%
% - whether it may be used in the interface, and
% - whether it may be used without full module qualification.
%
% An entity may be used in the interface if it is defined in the current
% module or one of its ancestors, or if it is defined in a module that is
% imported (or used) in the interface.
%
% An entity may be used without full module qualification if it is defined
% in the current module, in one of its ancestors, or in a module that is
% the subject of an explicit `import_module' declaration in the current module.
% It may not be used without full qualification if it is made available by
% a `use_module' declaration, or if it is defined in a `.opt' and `.trans_opt'
% file.
%
% The two are not independent: an entity may be usable in the interface
% only if fully qualified (if it is defined in a module that has a
% `use_module' declaration for it in the interface), while it may be
% usable in the implementation even if not fully qualified (if that defining
% module has an `import_module' declaration in the implementation.)
%

:- type module_permissions
    --->    module_permissions(
                mp_in_int               :: perm_in_int,
                mp_in_imp               :: perm_in_imp
            ).

:- type perm_in_int
    --->    may_not_use_in_int
    ;       may_use_in_int(need_qualifier).

:- type perm_in_imp
    --->    may_use_in_imp(need_qualifier).

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
:- pred parent_module_is_imported(mq_in_interface::in,
    module_name::in, module_name::in, module_id_set::in) is semidet.

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
        unexpected($pred, "unqualified id")
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
        % Grant the permissions granted by either OldPermissions or
        % NewPermissions.
        OldPermissions = module_permissions(OldPermInt, OldPermImp),
        NewPermissions = module_permissions(NewPermInt, NewPermImp),
        (
            OldPermInt = may_not_use_in_int,
            PermInt = NewPermInt
        ;
            OldPermInt = may_use_in_int(OldIntNeedQual),
            (
                NewPermInt = may_not_use_in_int,
                PermInt = OldPermInt
            ;
                NewPermInt = may_use_in_int(NewIntNeedQual),
                need_qual_only_if_both(OldIntNeedQual, NewIntNeedQual,
                    IntNeedQual),
                PermInt = may_use_in_int(IntNeedQual)
            )
        ),
        OldPermImp = may_use_in_imp(OldImpNeedQual),
        NewPermImp = may_use_in_imp(NewImpNeedQual),
        need_qual_only_if_both(OldImpNeedQual, NewImpNeedQual, ImpNeedQual),
        PermImp = may_use_in_imp(ImpNeedQual),

        % Update the entry only if it changed.
        ( if
            PermInt = OldPermInt,
            PermImp = OldPermImp
        then
            true
        else
            Permissions = module_permissions(PermInt, PermImp),
            map.det_update(ModuleName, Permissions, !PermissionsMap)
        )
    else
        map.det_insert(ModuleName, NewPermissions, !PermissionsMap)
    ).

:- pred need_qual_only_if_both(need_qualifier::in, need_qualifier::in,
    need_qualifier::out) is det.

need_qual_only_if_both(NeedQualA, NeedQualB, NeedQual) :-
    ( if
        NeedQualA = must_be_qualified,
        NeedQualB = must_be_qualified
    then
        NeedQual = must_be_qualified
    else
        NeedQual = may_be_unqualified
    ).

%---------------------------------------------------------------------------%

find_unique_match(InInt, ErrorContext, IdSet, IdType, Id0, SymName,
        !Info, !Specs) :-
    % Find all IDs which match the current id.
    Id0 = mq_id(SymName0, Arity),
    BaseName = unqualify_name(SymName0),
    id_set_search_sym_arity(InInt, IdSet, SymName0, BaseName, Arity,
        Matches, IntMismatches, QualMismatches),
    (
        Matches = [],
        % No matches for this id.
        MaybeUniqModuleName = no,
        mq_info_get_should_report_errors(!.Info, ReportErrors),
        (
            ReportErrors = should_report_errors,
            mq_info_record_undef_mq_id(IdType, !Info),

            mq_info_get_this_module(!.Info, ThisModuleName),
            id_set_search_sym(IdSet, SymName0, PossibleArities),
            report_undefined_mq_id(!.Info, ErrorContext, Id0, IdType,
                ThisModuleName, IntMismatches, QualMismatches,
                PossibleArities, !Specs),

            % If a module defines an entity that this module refers to,
            % even without the required module qualification, then reporting
            % that module as being unused would be wrong.
            %
            % This is so even if the correct definition of Id0 could have
            % come from any one of several modules.
            list.foldl(mq_info_set_module_used(InInt), QualMismatches, !Info)
        ;
            ReportErrors = should_not_report_errors
        )
    ;
        Matches = [ModuleName],
        % A unique match for this ID.
        MaybeUniqModuleName = yes(ModuleName)
    ;
        Matches = [_, _ | _],
        MaybeUniqModuleName = no,
        mq_info_get_should_report_errors(!.Info, ReportErrors),
        (
            ReportErrors = should_report_errors,
            mq_info_record_undef_mq_id(IdType, !Info),
            NonUsableModuleNames = IntMismatches ++ QualMismatches,
            report_ambiguous_match(ErrorContext, Id0, IdType,
                Matches, NonUsableModuleNames, !Specs)
        ;
            ReportErrors = should_not_report_errors
        )
    ),
    (
        MaybeUniqModuleName = no,
        % Returning any SymName is fine, since it won't be used.
        Id0 = mq_id(SymName, _)
    ;
        MaybeUniqModuleName = yes(UniqModuleName),
        SymName = qualified(UniqModuleName, BaseName),
        mq_info_set_module_used(InInt, UniqModuleName, !Info),
        ItemType = convert_simple_item_type(IdType),
        ItemName0 = item_name(SymName0, Arity),
        ItemName = item_name(SymName, Arity),
        update_recompilation_info(
            recompilation.record_used_item(ItemType, ItemName0, ItemName),
            !Info)
    ).

:- func convert_simple_item_type(id_type) = item_type.

convert_simple_item_type(type_id) = type_abstract_item.
convert_simple_item_type(inst_id) = inst_item.
convert_simple_item_type(mode_id) = mode_item.
convert_simple_item_type(class_id) = typeclass_item.

%---------------------------------------------------------------------------%

:- pred id_set_search_sym_arity(mq_in_interface::in, id_set::in,
    sym_name::in, string::in, int::in, list(module_name)::out,
    list(module_name)::out, list(module_name)::out) is det.

id_set_search_sym_arity(InInt, IdSet, SymName, UnqualName, Arity,
        Matches, IntMismatches, QualMismatches) :-
    ( if
        map.search(IdSet, UnqualName, SubMap),
        map.search(SubMap, Arity, PermissionsMap)
    then
        find_matches_in_permissions_map(InInt, SymName, PermissionsMap,
            Matches, IntMismatches, QualMismatches)
    else
        Matches = [],
        IntMismatches = [],
        QualMismatches = []
    ).

:- pred find_matches_in_permissions_map(mq_in_interface::in, sym_name::in,
    permissions_map::in, list(module_name)::out,
    list(module_name)::out, list(module_name)::out) is det.

find_matches_in_permissions_map(InInt, SymName, PermissionsMap,
        Matches, IntMismatches, QualMismatches) :-
    map.foldr3(add_matching_and_nearmiss_modules(InInt, SymName),
        PermissionsMap, [], Matches, [], IntMismatches, [], QualMismatches).

:- pred add_matching_and_nearmiss_modules(mq_in_interface::in, sym_name::in,
    module_name::in, module_permissions::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out) is det.

add_matching_and_nearmiss_modules(InInt, SymName, ModuleName, Permissions,
        !Matches, !IntMismatches, !QualMismatches) :-
    (
        SymName = unqualified(_),
        FullyModuleQualified = no,
        add_matching_and_nearmiss_modules_int(InInt, FullyModuleQualified,
            ModuleName, Permissions,
            !Matches, !IntMismatches, !QualMismatches)
    ;
        SymName = qualified(QualModuleName, _),
        ( if
            partial_sym_name_matches_full(QualModuleName, ModuleName)
        then
            ( if QualModuleName = ModuleName then
                FullyModuleQualified = yes
            else
                FullyModuleQualified = no
            ),
            add_matching_and_nearmiss_modules_int(InInt, FullyModuleQualified,
                ModuleName, Permissions,
                !Matches, !IntMismatches, !QualMismatches)
        else if
            ModuleNameComponents = sym_name_to_list(ModuleName),
            QualModuleNameComponents = sym_name_to_list(QualModuleName),
            list.sublist(QualModuleNameComponents, ModuleNameComponents)
        then
            % The missing module name components are not all at the start.
            % XXX *Should* this be a problem?
            !:QualMismatches = [ModuleName | !.QualMismatches]
        else
            true
        )
    ).

:- pred add_matching_and_nearmiss_modules_int(mq_in_interface::in, bool::in,
    module_name::in, module_permissions::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out) is det.

add_matching_and_nearmiss_modules_int(InInt, FullyModuleQualified,
        ModuleName, Permissions, !Matches, !IntMismatches, !QualMismatches) :-
    Permissions = module_permissions(PermInInt, PermInImp),
    (
        InInt = mq_used_in_interface,
        (
            PermInInt = may_not_use_in_int,
            !:IntMismatches = [ModuleName | !.IntMismatches]
        ;
            PermInInt = may_use_in_int(NeedQual),
            add_matching_and_nearmiss_modules_qual(FullyModuleQualified,
                NeedQual, ModuleName, !Matches, !QualMismatches)
        )
    ;
        InInt = mq_not_used_in_interface,
        PermInImp = may_use_in_imp(NeedQual),
        add_matching_and_nearmiss_modules_qual(FullyModuleQualified,
            NeedQual, ModuleName, !Matches, !QualMismatches)
    ).

:- pred add_matching_and_nearmiss_modules_qual(bool::in, need_qualifier::in,
    module_name::in,
    list(module_name)::in, list(module_name)::out,
    list(module_name)::in, list(module_name)::out) is det.

add_matching_and_nearmiss_modules_qual(FullyModuleQualified, NeedQual,
        ModuleName, !Matches, !QualMismatches) :-
    ( if
        ( FullyModuleQualified = yes
        ; NeedQual = may_be_unqualified
        )
    then
        !:Matches = [ModuleName | !.Matches]
    else
        !:QualMismatches = [ModuleName | !.QualMismatches]
    ).

%---------------------------------------------------------------------------%

:- pred id_set_search_sym(id_set::in, sym_name::in, set(int)::out) is det.

id_set_search_sym(IdSet, SymName, PossibleArities) :-
    UnqualName = unqualify_name(SymName),
    ( if
        map.search(IdSet, UnqualName, SubMap)
    then
        map.to_assoc_list(SubMap, SubMapPairs),
        find_matching_arities(SymName, SubMapPairs, set.init, PossibleArities)
    else
        set.init(PossibleArities)
    ).

:- pred find_matching_arities(sym_name::in,
    assoc_list(int, permissions_map)::in, set(int)::in, set(int)::out) is det.

find_matching_arities(_SymName, [], !PossibleArities).
find_matching_arities(SymName, [Pair | Pairs], !PossibleArities) :-
    Pair = Arity - PermissionsMap,
    find_matches_in_permissions_map(mq_not_used_in_interface, SymName,
        PermissionsMap, Matches, IntMismatches, QualMismatches),
    ( if
        ( Matches = [_ | _]
        ; IntMismatches = [_ | _]
        ; QualMismatches = [_ | _]
        )
    then
        set.insert(Arity, !PossibleArities)
    else
        true
    ),
    find_matching_arities(SymName, Pairs, !PossibleArities).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

parent_module_is_imported(InInt, ParentModule, ChildModule, ModuleIdSet) :-
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
    ParentModulePermissions = module_permissions(PermInInt, PermInImp),
    (
        InInt = mq_used_in_interface,
        PermInInt = may_use_in_int(may_be_unqualified)
    ;
        InInt = mq_not_used_in_interface,
        PermInImp = may_use_in_imp(may_be_unqualified)
    ).

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

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2007, 2011 The University of Melbourne.
% Copyright (C) 2014-2015, 2017, 2019-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: recompilation.m.
% Main author: stayl.
%
% Type declarations for smart recompilation.
% Predicates to record program items used by a compilation.
%
% A module must be recompiled if
% - The file itself has changed.
% - An imported item used in compiling the module has changed or been removed.
% - An item has been added to an imported module which could cause an
%   ambiguity with an item used in compiling the module.
%
% Currently smart recompilation does not work properly with
% inter-module optimization. If a `.opt' file changes, all modules
% importing it need to be recompiled.
%
%-----------------------------------------------------------------------------%

:- module recompilation.record_uses.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module recompilation.item_types.

:- import_module map.
:- import_module maybe.
:- import_module set.

%-----------------------------------------------------------------------------%

:- type recompilation_info
    --->    recompilation_info(
                % Name of the current module.
                recomp_module_name      :: module_name,

                % Used items imported from other modules.
                recomp_used_items       :: used_items,

                % For now we only record dependencies of imported items
                % on equivalence types. The rest of the dependencies can be
                % found by examining the pred_infos, type_defns etc of the
                % items recorded in the used_items field above.
                recomp_dependencies     :: map(recomp_item_id,
                                            set(recomp_item_id)),

                recomp_version_numbers  :: module_item_version_numbers_map
            ).

:- func init_recompilation_info(module_name) = recompilation_info.

%-----------------------------------------------------------------------------%

:- type used_item_type
    --->    used_type_name
            % Just the name of the type, not its body. It is common
            % for a value of a type to be passed through a predicate without
            % inspecting the value -- such predicates do not need to be
            % recompiled if the body of the type changes (except for
            % equivalence types).
    ;       used_type_defn
    ;       used_inst
    ;       used_mode
    ;       used_typeclass
    ;       used_functor        % The RHS of a var-functor unification.
    ;       used_predicate
    ;       used_function.

    % A simple_item_set records the single possible match for an item.
    %
    % XXX RECOMP RENAME The type name should reflect that fact.
    %
:- type simple_item_set == map(name_arity, map(module_qualifier, module_name)).

    % Items which are used by local items.
    %
    % XXX That "documentation" is not exactly complete ...
    %
:- type used_items
    --->    used_items(
                used_type_names     :: simple_item_set,
                used_type_defns     :: simple_item_set,
                used_insts          :: simple_item_set,
                used_modes          :: simple_item_set,
                used_typeclasses    :: simple_item_set,
                used_functors       :: simple_item_set,
                used_predicates     :: simple_item_set,
                used_functions      :: simple_item_set
            ).

%-----------------------------------------------------------------------------%

    % unqualified("") if the symbol was unqualified.
:- type module_qualifier == module_name.

:- func find_module_qualifier(sym_name) = module_qualifier.

:- func module_qualify_name(module_qualifier, string) = sym_name.

%-----------------------------------------------------------------------------%

    % record_used_item(ItemType, UnqualifiedId, QualifiedId, !Info).
    %
    % Record a reference to UnqualifiedId, for which QualifiedId
    % is the only match. If a new declaration is added so that
    % QualifiedId is not the only match, we need to recompile.
    %
:- pred record_used_item(used_item_type::in,
    recomp_item_name::in, recomp_item_name::in,
    recompilation_info::in, recompilation_info::out) is det.

    % For each imported item we need to record which equivalence types
    % are used because equiv_type.m removes all references to the
    % equivalence types, and at that point we don't know which imported
    % items are going to be used by the compilation.
    %
    % For predicates declared using `with_type` annotations,
    % the version number in the interface file and the
    % version_numbers map will refer to the arity before expansion
    % of the `with_type` annotation, so that needs to be recorded
    % here as well.
    %
:- pred record_expanded_items(recomp_item_id::in, set(recomp_item_id)::in,
    recompilation_info::in, recompilation_info::out) is det.

%-----------------------------------------------------------------------------%

    % XXX RECOMP RENAME It is the eqv_expanded_item_set type that plays
    % the role that X_info types (for X = polymorphism, simplify etc)
    % play in the rest of the compiler, in that it contains a data structure
    % that is threaded through a set of predicates (a) to give them the
    % info they need to do their jobs, and (b) to collect their observations.
    %
:- type eqv_expand_info
    --->    no_eqv_expand_info
    ;       eqv_expand_info(module_name, set(recomp_item_id)).
            % The module_name field contains the name of the module
            % currently being compiled.
            %
            % XXX Document the meaning of the second field.

    % For smart recompilation we need to record which items were expanded
    % in each declaration. Any items which depend on that declaration also
    % depend on the expanded items.
    %
:- pred maybe_start_recording_expanded_items(module_name::in, sym_name::in,
    maybe(recompilation_info)::in, eqv_expand_info::out) is det.

:- pred record_expanded_item(recomp_item_id::in,
    eqv_expand_info::in, eqv_expand_info::out) is det.

    % Record all the expanded items in the recompilation_info.
    %
:- pred finish_recording_expanded_items(recomp_item_id::in,
    eqv_expand_info::in,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%-----------------------------------------------------------------------------%

init_recompilation_info(ModuleName) =
    recompilation_info(ModuleName, init_used_items, map.init, map.init).

:- func init_used_items = used_items.

init_used_items =
    used_items(map.init, map.init, map.init, map.init, map.init, map.init,
        map.init, map.init).

:- func get_used_item_ids(used_items, used_item_type) = simple_item_set.

get_used_item_ids(Used, used_type_name) = Used ^ used_type_names.
get_used_item_ids(Used, used_type_defn) = Used ^ used_type_defns.
get_used_item_ids(Used, used_inst) = Used ^ used_insts.
get_used_item_ids(Used, used_mode) = Used ^ used_modes.
get_used_item_ids(Used, used_typeclass) = Used ^ used_typeclasses.
get_used_item_ids(Used, used_functor) = Used ^ used_functors.
get_used_item_ids(Used, used_predicate) = Used ^ used_predicates.
get_used_item_ids(Used, used_function) = Used ^ used_functions.

:- pred set_used_item_ids(used_item_type::in, simple_item_set::in,
    used_items::in, used_items::out) is det.

set_used_item_ids(used_type_name, IdMap, !Used) :-
    !Used ^ used_type_names := IdMap.
set_used_item_ids(used_type_defn, IdMap, !Used) :-
    !Used ^ used_type_defns := IdMap.
set_used_item_ids(used_inst, IdMap, !Used) :-
    !Used ^ used_insts := IdMap.
set_used_item_ids(used_mode, IdMap, !Used) :-
    !Used ^ used_modes := IdMap.
set_used_item_ids(used_typeclass, IdMap, !Used) :-
    !Used ^ used_typeclasses := IdMap.
set_used_item_ids(used_functor, IdMap, !Used) :-
    !Used ^ used_functors := IdMap.
set_used_item_ids(used_predicate, IdMap, !Used) :-
    !Used ^ used_predicates := IdMap.
set_used_item_ids(used_function, IdMap, !Used) :-
    !Used ^ used_functions := IdMap.

%-----------------------------------------------------------------------------%

find_module_qualifier(unqualified(_)) = unqualified("").
find_module_qualifier(qualified(ModuleName, _)) = ModuleName.

module_qualify_name(Qualifier, Name) =
    ( if Qualifier = unqualified("") then
        unqualified(Name)
    else
        qualified(Qualifier, Name)
    ).

%-----------------------------------------------------------------------------%

record_used_item(UsedItemType, Id, QualifiedId, !Info) :-
    QualifiedId = recomp_item_name(QualifiedName, Arity),
    ( if
        % Don't record builtin items (QualifiedId may be unqualified
        % for predicates, functions and functors because they aren't
        % qualified until after typechecking).
        QualifiedName = unqualified(_),
        ignore_unqual_item_for_item_type(UsedItemType) = ignore
    then
        true
    else
        Used0 = !.Info ^ recomp_used_items,
        IdSet0 = get_used_item_ids(Used0, UsedItemType),
        UnqualifiedName = unqualify_name(QualifiedName),
        ModuleName = find_module_qualifier(QualifiedName),
        UnqualifiedId = name_arity(UnqualifiedName, Arity),
        Id = recomp_item_name(SymName, _),
        ModuleQualifier = find_module_qualifier(SymName),
        ( if map.search(IdSet0, UnqualifiedId, MatchingNames0) then
            ( if map.contains(MatchingNames0, ModuleQualifier) then
                true
            else
                map.det_insert(ModuleQualifier, ModuleName,
                    MatchingNames0, MatchingNames),
                map.det_update(UnqualifiedId, MatchingNames, IdSet0, IdSet),
                set_used_item_ids(UsedItemType, IdSet, Used0, Used),
                !Info ^ recomp_used_items := Used
            )
        else
            MatchingNames = map.singleton(ModuleQualifier, ModuleName),
            map.det_insert(UnqualifiedId, MatchingNames, IdSet0, IdSet),
            set_used_item_ids(UsedItemType, IdSet, Used0, Used),
            !Info ^ recomp_used_items := Used
        )
    ).

:- type maybe_ignore
    --->    do_not_ignore
    ;       ignore.

:- func ignore_unqual_item_for_item_type(used_item_type) = maybe_ignore.

ignore_unqual_item_for_item_type(UsedItemType) = Ignore :-
    (
        ( UsedItemType = used_type_name
        ; UsedItemType = used_type_defn
        ; UsedItemType = used_inst
        ; UsedItemType = used_mode
        ; UsedItemType = used_typeclass
        ),
        Ignore = ignore
    ;
        ( UsedItemType = used_functor
        ; UsedItemType = used_predicate
        ; UsedItemType = used_function
        ),
        Ignore = do_not_ignore
    ).

%-----------------------------------------------------------------------------%

record_expanded_items(Item, ExpandedItems, !Info) :-
    ( if set.is_empty(ExpandedItems) then
        true
    else
        DepsMap0 = !.Info ^ recomp_dependencies,
        ( if map.search(DepsMap0, Item, Deps0) then
            set.union(ExpandedItems, Deps0, Deps),
            map.det_update(Item, Deps, DepsMap0, DepsMap)
        else
            map.det_insert(Item, ExpandedItems, DepsMap0, DepsMap)
        ),
        !Info ^ recomp_dependencies := DepsMap
    ).

maybe_start_recording_expanded_items(ModuleName, SymName, MaybeRecompInfo,
        ExpandInfo) :-
    (
        MaybeRecompInfo = no,
        ExpandInfo = no_eqv_expand_info
    ;
        MaybeRecompInfo = yes(_),
        ( if SymName = qualified(ModuleName, _) then
            ExpandInfo = no_eqv_expand_info
        else
            ExpandInfo = eqv_expand_info(ModuleName, set.init)
        )
    ).

record_expanded_item(ItemId, !ExpandInfo) :-
    (
        !.ExpandInfo = no_eqv_expand_info
    ;
        !.ExpandInfo = eqv_expand_info(ModuleName, ExpandedItemIds0),
        ItemId = recomp_item_id(_, ItemName),
        ( if ItemName = recomp_item_name(qualified(ModuleName, _), _) then
            % We don't need to record local items.
            true
        else
            set.insert(ItemId, ExpandedItemIds0, ExpandedItemIds),
            !:ExpandInfo = eqv_expand_info(ModuleName, ExpandedItemIds)
        )
    ).

finish_recording_expanded_items(ItemId, ExpandInfo,
        MaybeRecomp0, MaybeRecomp) :-
    (
        ExpandInfo = no_eqv_expand_info,
        MaybeRecomp = MaybeRecomp0
    ;
        ExpandInfo = eqv_expand_info(_, ExpandedItemIds),
        (
            MaybeRecomp0 = no,
            unexpected($pred, "items but no info")
        ;
            MaybeRecomp0 = yes(Recomp0),
            record_expanded_items(ItemId, ExpandedItemIds, Recomp0, Recomp),
            MaybeRecomp = yes(Recomp)
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module recompilation.record_uses.
%-----------------------------------------------------------------------------%

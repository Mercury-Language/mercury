%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2007, 2011 The University of Melbourne.
% Copyright (C) 2014-2015, 2017, 2019-2024, 2026 The Mercury team.
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

    % record_gathered_item_deps(ItemId, GatheredItemDeps, !Info):
    %
    % This predicate does the final part of the job of
    % finish_gathering_item_recomp_deps, i.e. the incorporation
    % of the gathered information in the recompilation_info.
    %
    % It is a separate predicate because for decl_pragma_type_spec_info
    % and decl_pragma_type_spec_constr_info items, even though equiv_type.m
    % gathers the ids of the items expanded out within them, it does NOT
    % record the result of this gathering in the recompilation_info.
    % (In fact, the code there does this gathering even in the *absence*
    % of any recompilation_info.) Instead, it records the gathered item_ids
    % in the updated decl_pragma_type_spec{,_constr}_info, and leaves it
    % to add add_pragma_type_spec.m to invoke this predicate.
    %
    % I (zs) have no idea what the point of this delay is.
    %
:- pred record_gathered_item_deps(recomp_item_id::in, set(recomp_item_id)::in,
    recompilation_info::in, recompilation_info::out) is det.

%-----------------------------------------------------------------------------%

    % For smart recompilation we need to record which items were expanded
    % in each declaration.
    %
    % For each imported item we need to record which equivalence types
    % are used in it, because equiv_type.m removes all references to the
    % equivalence types, and at that point we don't know which imported items
    % are going to be used by the compilation.
    %
    % For predicates declared using `with_type` annotations,
    % the version number in the interface file and the
    % version_numbers map will refer to the arity before expansion
:- type item_recomp_deps
    --->    no_item_recomp_deps
    ;       item_recomp_deps(
                % The name of the module currently being compiled.
                module_name,

                % We create one of these structures when we start recording
                % the set of items expanded out within a declaration item
                % (such as a item_type_defn_info, item_pred_decl_info,
                % item_mode_decl_info, item_typeclass_info etc).
                % This field contains the ids of those expanded-out items.
                set(recomp_item_id)
            ).

    % maybe_start_gathering_item_recomp_deps(ModuleName, ItemId,
    %   MaybeRecompInfo, MaybeGatheredItemDeps):
    %
    % If smart recompilation is enabled (as shown by the presence of a
    % recompilation_info in the third argument), then return the initial
    % version of the data structure in we will gather (by means of calls
    % to gather_item_recomp_dep) the set of items that were expanded out
    % while processing the given item, which will be a declaration.
    %
    % We do this because entities that depend on that declaration
    % also depend on the expanded-out items within that declaration.
    %
    % maybe_start_gathering_item_recomp_deps_sym_name does the same job,
    % but it callable from a context in which a recomp_item_id cannot (yet)
    % be constructed. The contexts are pred declarations and mode declarations
    % that, due to the presence of with_type and/or with_inst annotations,
    % don't know their final arity yet, and may possibly also lack
    % a pred_or_func indication. (recomp_item_ids for those kinds of items
    % need both the arity and the pred_or_func indication.)
    %
:- pred maybe_start_gathering_item_recomp_deps(module_name::in,
    recomp_item_id::in, maybe(recompilation_info)::in,
    item_recomp_deps::out) is det.
:- pred maybe_start_gathering_item_recomp_deps_sym_name(module_name::in,
    sym_name::in, maybe(recompilation_info)::in,
    item_recomp_deps::out) is det.

    % gather_item_recomp_dep(DepItemId, !MaybeGatheredItemDeps):
    %
    % Record DepItemId as being expanded during the processing
    % of the declaration item for which !.MaybeGatheredItemDeps was created.
    %
:- pred gather_item_recomp_dep(recomp_item_id::in,
    item_recomp_deps::in, item_recomp_deps::out) is det.

    % finish_gathering_item_recomp_deps(ItemId, MaybeGatheredItemDeps,
    %   MaybeRecomp0, MaybeRecomp):
    %
    % This predicate should be called when the compiler has finished
    % expanding out equivalences in the various components of the
    % original declaration item. This predicate then records
    % the fact that
    % Record all the expanded items in the recompilation_info.
    %
:- pred finish_gathering_item_recomp_deps(recomp_item_id::in,
    item_recomp_deps::in,
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

record_gathered_item_deps(ItemId, GatheredItemDeps, !Info) :-
    ( if set.is_empty(GatheredItemDeps) then
        true
    else
        DepsMap0 = !.Info ^ recomp_dependencies,
        ( if map.search(DepsMap0, ItemId, Deps0) then
            set.union(GatheredItemDeps, Deps0, Deps),
            map.det_update(ItemId, Deps, DepsMap0, DepsMap)
        else
            map.det_insert(ItemId, GatheredItemDeps, DepsMap0, DepsMap)
        ),
        !Info ^ recomp_dependencies := DepsMap
    ).

maybe_start_gathering_item_recomp_deps(ModuleName, ItemId, MaybeRecompInfo,
        MaybeGatheredItemDeps) :-
    (
        MaybeRecompInfo = no,
        MaybeGatheredItemDeps = no_item_recomp_deps
    ;
        MaybeRecompInfo = yes(_),
        ItemId = recomp_item_id(_, ItemName),
        ItemName = recomp_item_name(ItemSymName, _ItemArity),
        ( if ItemSymName = qualified(ModuleName, _) then
            MaybeGatheredItemDeps = no_item_recomp_deps
        else
            MaybeGatheredItemDeps = item_recomp_deps(ModuleName, set.init)
        )
    ).

maybe_start_gathering_item_recomp_deps_sym_name(ModuleName, ItemSymName,
        MaybeRecompInfo, MaybeGatheredItemDeps) :-
    (
        MaybeRecompInfo = no,
        MaybeGatheredItemDeps = no_item_recomp_deps
    ;
        MaybeRecompInfo = yes(_),
        ( if ItemSymName = qualified(ModuleName, _) then
            MaybeGatheredItemDeps = no_item_recomp_deps
        else
            MaybeGatheredItemDeps = item_recomp_deps(ModuleName, set.init)
        )
    ).

gather_item_recomp_dep(DepItemId, !MaybeGatheredItemDeps) :-
    (
        !.MaybeGatheredItemDeps = no_item_recomp_deps
    ;
        !.MaybeGatheredItemDeps = item_recomp_deps(ModuleName, GatheredDeps0),
        DepItemId = recomp_item_id(_, DepItemName),
        ( if DepItemName = recomp_item_name(qualified(ModuleName, _), _) then
            % We don't need to record local items.
            true
        else
            set.insert(DepItemId, GatheredDeps0, GatheredDeps),
            !:MaybeGatheredItemDeps = item_recomp_deps(ModuleName, GatheredDeps)
        )
    ).

finish_gathering_item_recomp_deps(ItemId, MaybeGatheredItemDeps,
        MaybeRecomp0, MaybeRecomp) :-
    (
        MaybeGatheredItemDeps = no_item_recomp_deps,
        MaybeRecomp = MaybeRecomp0
    ;
        MaybeGatheredItemDeps = item_recomp_deps(_, GatheredDeps),
        (
            MaybeRecomp0 = no,
            unexpected($pred, "gathered deps but no recomp info")
        ;
            MaybeRecomp0 = yes(Recomp0),
            record_gathered_item_deps(ItemId, GatheredDeps, Recomp0, Recomp),
            MaybeRecomp = yes(Recomp)
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module recompilation.record_uses.
%-----------------------------------------------------------------------------%

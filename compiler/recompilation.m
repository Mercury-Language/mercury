%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2007, 2011 The University of Melbourne.
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

:- module recompilation.
:- interface.

:- import_module check_hlds.
:- import_module hlds.
:- import_module libs.
:- import_module mdbcomp.
:- import_module parse_tree.

:- import_module libs.timestamp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module map.
:- import_module pair.
:- import_module set.
:- import_module term.

:- include_module recompilation.check.
:- include_module recompilation.usage.
:- include_module recompilation.version.

    % Identify a particular version of a program item.
    % This could be done using a timestamp or a hash value.
:- type version_number == timestamp.

:- pred write_version_number(version_number::in, io::di, io::uo) is det.

:- func term_to_version_number(term(T)) = version_number is semidet.

:- func term_to_timestamp(term(T)) = timestamp is semidet.

%-----------------------------------------------------------------------------%

:- type item_id
    --->    item_id(item_type, item_name).

:- type item_name
    --->    item_name(sym_name, arity).

:- type item_type
    --->    type_abstract_item  % Just the name of the type, not its body.
                                % It is common for a value of a type to
                                % be passed through a predicate without
                                % inspecting the value -- such predicates
                                % do not need to be recompiled if the
                                % body of the type changes (except for
                                % equivalence types).
    ;       type_body_item
    ;       mode_item
    ;       inst_item
    ;       typeclass_item
    ;       functor_item        % The RHS of a var-functor unification.
    ;       predicate_item
    ;       function_item
    ;       mutable_item
    ;       foreign_proc_item.

:- inst simple_item
    --->    type_abstract_item
    ;       type_body_item
    ;       mode_item
    ;       inst_item
    ;       typeclass_item.

:- inst pred_or_func
    --->    predicate_item
    ;       function_item.

:- pred is_simple_item_type(item_type::(ground >> simple_item)) is semidet.

:- pred is_pred_or_func_item_type(item_type::(ground >> pred_or_func))
    is semidet.

:- pred string_to_item_type(string, item_type).
:- mode string_to_item_type(in, out) is semidet.
:- mode string_to_item_type(out, in) is det.

:- func pred_or_func_to_item_type(pred_or_func::in)
    = (item_type::out(pred_or_func)) is det.

:- func type_ctor_to_item_name(type_ctor) = item_name.
:- func inst_id_to_item_name(inst_id) = item_name.
:- func mode_id_to_item_name(mode_id) = item_name.

:- func item_name_to_type_ctor(item_name) = type_ctor.
:- func item_name_to_inst_id(item_name) = inst_id.
:- func item_name_to_mode_id(item_name) = mode_id.

%-----------------------------------------------------------------------------%

:- type recompilation_info
    --->    recompilation_info(
                % Name of the current module.
                module_name         :: module_name,

                % Used items imported from other modules.
                used_items          :: used_items,

                % For now we only record dependencies of imported items
                % on equivalence types. The rest of the dependencies can be
                % found by examining the pred_infos, type_defns etc of the
                % items recorded in the used_items field above.
                dependencies        :: map(item_id, set(item_id)),

                version_numbers     :: map(module_name, version_numbers)
            ).

:- func init_recompilation_info(module_name) = recompilation_info.

    % recompilation.add_used_item(ItemType, UnqualifiedId, QualifiedId,
    %   !Info).
    %
    % Record a reference to UnqualifiedId, for which QualifiedId
    % is the only match. If a new declaration is added so that
    % QualifiedId is not the only match, we need to recompile.
    %
:- pred record_used_item(item_type::in, item_name::in, item_name::in,
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
:- pred record_expanded_items(item_id::in, set(item_id)::in,
    recompilation_info::in, recompilation_info::out) is det.

%-----------------------------------------------------------------------------%

:- type item_id_set(Map, Set, Cons)
    --->    item_id_set(
                types           :: Map,
                type_bodies     :: Map,
                modes           :: Map,
                insts           :: Map,
                typeclasses     :: Map,
                functors        :: Cons,
                predicates      :: Set,
                functions       :: Set,
                mutables        :: Set,
                foreign_procs   :: Set
            ).

:- type item_id_set(T) == item_id_set(T, T, T).

:- func init_item_id_set(T) = item_id_set(T).

:- func init_item_id_set(Simple, PorF, Cons) = item_id_set(Simple, PorF, Cons).

%-----------------------------------------------------------------------------%

    % A simple_item_set records the single possible match for an item.
    %
:- type simple_item_set ==
    map(pair(string, arity), map(module_qualifier, module_name)).

    % For constructors, predicates and functions we can't work out
    % which item is actually used until we've run typechecking.
    %
:- type pred_or_func_set == simple_item_set.

:- type functor_set == simple_item_set.

    % Items which are used by local items.
:- type used_items ==
    item_id_set(
        simple_item_set,
        pred_or_func_set,
        functor_set
    ).

:- func init_used_items = used_items.

%-----------------------------------------------------------------------------%
%
% Access functions for item_id_sets.
%

:- func extract_simple_item_set(item_id_set(Simple, PorF, Cons)::in,
    item_type::in(simple_item)) = (Simple::out) is det.

:- func update_simple_item_set(item_id_set(Simple, PorF, Cons)::in,
    item_type::in(simple_item), Simple::in)
    = (item_id_set(Simple, PorF, Cons)::out) is det.

:- func extract_pred_or_func_set(item_id_set(Simple, PorF, Cons)::in,
    item_type::in(pred_or_func)) = (PorF::out) is det.

:- func update_pred_or_func_set(item_id_set(Simple, PorF, Cons)::in,
    item_type::in(pred_or_func), PorF::in)
    = (item_id_set(Simple, PorF, Cons)::out) is det.

:- func extract_ids(item_id_set(T), item_type) = T.

:- func update_ids(item_id_set(T), item_type, T) = item_id_set(T).

:- func map_ids((func(item_type, T) = U), item_id_set(T), U) = item_id_set(U).

%-----------------------------------------------------------------------------%

    % Version numbers for items in a single module.
:- type version_numbers
    --->    version_numbers(
                item_version_numbers,
                instance_version_numbers
            ).

    % The constructors set should always be empty -
    % constructors are never imported separately.
:- type item_version_numbers == item_id_set(version_number_map).

:- type version_number_map == map(pair(string, arity), version_number).

    % For each interface file, we keep a version number for each class.
:- type instance_version_numbers == map(item_name, version_number).

%-----------------------------------------------------------------------------%

    % unqualified("") if the symbol was unqualified.
:- type module_qualifier == module_name.

:- func find_module_qualifier(sym_name) = module_qualifier.

:- func module_qualify_name(module_qualifier, string) = sym_name.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.passes_aux.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module time.

%-----------------------------------------------------------------------------%

term_to_version_number(Term) = term_to_timestamp(Term).

term_to_timestamp(term.functor(term.string(TimestampString), [], _)) =
    string_to_timestamp(TimestampString).

write_version_number(VersionNumber, !IO) :-
    io.write_string("""", !IO),
    io.write_string(timestamp_to_string(VersionNumber), !IO),
    io.write_string("""", !IO).

%-----------------------------------------------------------------------------%

pred_or_func_to_item_type(pf_predicate) = predicate_item.
pred_or_func_to_item_type(pf_function) = function_item.

is_simple_item_type(type_abstract_item).
is_simple_item_type(type_body_item).
is_simple_item_type(inst_item).
is_simple_item_type(mode_item).
is_simple_item_type(typeclass_item).

is_pred_or_func_item_type(predicate_item).
is_pred_or_func_item_type(function_item).

string_to_item_type("type", type_abstract_item).
string_to_item_type("type_body", type_body_item).
string_to_item_type("inst", inst_item).
string_to_item_type("mode", mode_item).
string_to_item_type("typeclass", typeclass_item).
string_to_item_type("predicate", predicate_item).
string_to_item_type("function", function_item).
string_to_item_type("functor", functor_item).
string_to_item_type("mutable", mutable_item).
string_to_item_type("foreign_proc", foreign_proc_item).

type_ctor_to_item_name(type_ctor(SymName, Arity)) = item_name(SymName, Arity).
inst_id_to_item_name(inst_id(SymName, Arity)) = item_name(SymName, Arity).
mode_id_to_item_name(mode_id(SymName, Arity)) = item_name(SymName, Arity).

item_name_to_type_ctor(item_name(SymName, Arity)) = type_ctor(SymName, Arity).
item_name_to_inst_id(item_name(SymName, Arity)) = inst_id(SymName, Arity).
item_name_to_mode_id(item_name(SymName, Arity)) = mode_id(SymName, Arity).

%-----------------------------------------------------------------------------%

init_item_id_set(Init) =
    item_id_set(Init, Init, Init, Init, Init, Init, Init, Init, Init, Init).

init_item_id_set(Simple, PorF, Cons) =
    item_id_set(Simple, Simple, Simple, Simple, Simple, Cons, PorF, PorF,
        PorF, PorF).

init_used_items = item_id_set(map.init, map.init, map.init, map.init,
    map.init, map.init, map.init, map.init, map.init, map.init).

extract_simple_item_set(Items, type_abstract_item) = Items ^ types.
extract_simple_item_set(Items, type_body_item) = Items ^ type_bodies.
extract_simple_item_set(Items, mode_item) = Items ^ modes.
extract_simple_item_set(Items, inst_item) = Items ^ insts.
extract_simple_item_set(Items, typeclass_item) = Items ^ typeclasses.

update_simple_item_set(Items, type_abstract_item, IdMap) =
    Items ^ types := IdMap.
update_simple_item_set(Items, type_body_item, IdMap) =
    Items ^ type_bodies := IdMap.
update_simple_item_set(Items, mode_item, IdMap) = Items ^ modes := IdMap.
update_simple_item_set(Items, inst_item, IdMap) = Items ^ insts := IdMap.
update_simple_item_set(Items, typeclass_item, IdMap) =
    Items ^ typeclasses := IdMap.

extract_pred_or_func_set(Items, predicate_item) = Items ^ predicates.
extract_pred_or_func_set(Items, function_item) = Items ^ functions.

update_pred_or_func_set(Items, predicate_item, Set) =
    Items ^ predicates := Set.
update_pred_or_func_set(Items, function_item, Set) =
    Items ^ functions := Set.

extract_ids(Items, type_abstract_item) = Items ^ types.
extract_ids(Items, type_body_item) = Items ^ type_bodies.
extract_ids(Items, mode_item) = Items ^ modes.
extract_ids(Items, inst_item) = Items ^ insts.
extract_ids(Items, typeclass_item) = Items ^ typeclasses.
extract_ids(Items, functor_item) = Items ^ functors.
extract_ids(Items, predicate_item) = Items ^ predicates.
extract_ids(Items, function_item) = Items ^ functions.
extract_ids(Items, mutable_item) = Items ^ mutables.
extract_ids(Items, foreign_proc_item) = Items ^ foreign_procs.

update_ids(Items, type_abstract_item, IdMap) = Items ^ types := IdMap.
update_ids(Items, type_body_item, IdMap) = Items ^ type_bodies := IdMap.
update_ids(Items, mode_item, IdMap) = Items ^ modes := IdMap.
update_ids(Items, inst_item, IdMap) = Items ^ insts := IdMap.
update_ids(Items, typeclass_item, IdMap) = Items ^ typeclasses := IdMap.
update_ids(Items, predicate_item, IdMap) = Items ^ predicates := IdMap.
update_ids(Items, function_item, IdMap) = Items ^ functions := IdMap.
update_ids(Items, functor_item, IdMap) = Items ^ functors := IdMap.
update_ids(Items, mutable_item, IdMap) = Items ^ mutables := IdMap.
update_ids(Items, foreign_proc_item, IdMap) = Items ^ foreign_procs := IdMap.

map_ids(Func, Items0, Init) = Items :-
    Items1 = init_item_id_set(Init),
    Items = list.foldl(
        (func(ItemType, NewItems0) =
            update_ids(NewItems0, ItemType,
                Func(ItemType, extract_ids(Items0, ItemType)))
        ),
        [type_abstract_item, type_body_item, mode_item, inst_item,
            typeclass_item, functor_item, predicate_item, function_item],
        Items1).

%-----------------------------------------------------------------------------%

find_module_qualifier(unqualified(_)) = unqualified("").
find_module_qualifier(qualified(ModuleName, _)) = ModuleName.

module_qualify_name(Qualifier, Name) =
    ( Qualifier = unqualified("") ->
        unqualified(Name)
    ;
        qualified(Qualifier, Name)
    ).

%-----------------------------------------------------------------------------%

init_recompilation_info(ModuleName) =
    recompilation_info(
        ModuleName,
        init_used_items,
        map.init,
        map.init
    ).

record_used_item(ItemType, Id, QualifiedId, !Info) :-
    QualifiedId = item_name(QualifiedName, Arity),
    (
        % Don't record builtin items (QualifiedId may be unqualified
        % for predicates, functions and functors because they aren't
        % qualified until after typechecking).
        ItemType \= predicate_item,
        ItemType \= function_item,
        ItemType \= functor_item,
        QualifiedName = unqualified(_)
    ->
        true
    ;
        ItemSet0 = !.Info ^ used_items,
        IdSet0 = extract_ids(ItemSet0, ItemType),
        UnqualifiedName = unqualify_name(QualifiedName),
        ModuleName = find_module_qualifier(QualifiedName),
        UnqualifiedId = UnqualifiedName - Arity,
        Id = item_name(SymName, _),
        ModuleQualifier = find_module_qualifier(SymName),
        ( map.search(IdSet0, UnqualifiedId, MatchingNames0) ->
            MatchingNames1 = MatchingNames0
        ;
            map.init(MatchingNames1)
        ),
        ( map.contains(MatchingNames1, ModuleQualifier) ->
            true
        ;
            map.det_insert(ModuleQualifier, ModuleName,
                MatchingNames1, MatchingNames),
            map.set(UnqualifiedId, MatchingNames, IdSet0, IdSet),
            ItemSet = update_ids(ItemSet0, ItemType, IdSet),
            !Info ^ used_items := ItemSet
        )
    ).

record_expanded_items(Item, ExpandedItems, !Info) :-
    ( set.is_empty(ExpandedItems) ->
        true
    ;
        DepsMap0 = !.Info ^ dependencies,
        ( map.search(DepsMap0, Item, Deps0) ->
            Deps1 = Deps0
        ;
            set.init(Deps1)
        ),
        set.union(Deps1, ExpandedItems, Deps),
        map.set(Item, Deps, DepsMap0, DepsMap),
        !Info ^ dependencies := DepsMap
    ).

%-----------------------------------------------------------------------------%
:- end_module recompilation.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: recompilation.m
% Main author: stayl
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

:- import_module parse_tree.
:- import_module hlds.
:- import_module libs.

:- include_module recompilation__check.
:- include_module recompilation__usage.
:- include_module recompilation__version.

:- import_module parse_tree__prog_data, libs__timestamp.
:- import_module io, map, set, std_util, term.

	% Identify a particular version of a program item.
	% This could be done using a timestamp or a hash value. 
:- type version_number == timestamp.

:- pred write_version_number(version_number::in,
		io__state::di, io__state::uo) is det.

:- func term_to_version_number(term(T)) = version_number is semidet.

:- func term_to_timestamp(term(T)) = timestamp is semidet.

%-----------------------------------------------------------------------------%

:- type item_id
	--->	item_id(item_type, item_name).

:- type item_name == pair(sym_name, arity).

:- type item_type
	--->	(type)		% Just the name of the type, not its body.
				% It is common for a value of a type to
				% be passed through a predicate without
				% inspecting the value -- such predicates
				% do not need to be recompiled if the
				% body of the type changes (except for
				% equivalence types).
	;	type_body
	;	(mode)
	; 	(inst)
	;	(typeclass)
	;	functor		% The RHS of a var-functor unification.
	;	predicate
	;	function
	.

:- inst simple_item
	--->	(type)
	;	type_body
	;	(mode)
	;	(inst)
	;	(typeclass)
	.

:- inst pred_or_func
	--->	predicate
	;	function
	.

:- pred is_simple_item_type(
		item_type::(ground->simple_item)) is semidet.

:- pred is_pred_or_func_item_type(
		item_type::(ground->pred_or_func)) is semidet.

:- pred string_to_item_type(string, item_type).
:- mode string_to_item_type(in, out) is semidet.
:- mode string_to_item_type(out, in) is det.

:- func pred_or_func_to_item_type(pred_or_func::in)
		= (item_type::out(pred_or_func)) is det.

%-----------------------------------------------------------------------------%

:- type recompilation_info
	---> recompilation_info(
			% name of the current module
		module_name :: module_name,

			% used items imported from other modules
		used_items :: used_items,	

			% For now we only record dependencies of imported
			% items on equivalence types. The rest of the
			% dependencies can be found be examining the
			% pred_infos, type_defns etc. of the items
			% recorded in the used_items field above.
		dependencies :: map(item_id, set(item_id)),

		version_numbers :: map(module_name, version_numbers)
	).

:- func init_recompilation_info(module_name) = recompilation_info.

	% recompilation__add_used_item(ItemType, UnqualifiedId, QualifiedId,
	%	Info0, Info).
	%
	% Record a reference to UnqualifiedId, for which QualifiedId
	% is the only match. If a new declaration is added so that
	% QualifiedId is not the only match, we need to recompile.
:- pred recompilation__record_used_item(item_type::in, item_name::in,
	item_name::in, recompilation_info::in, recompilation_info::out) is det.

	% For each imported item we need to record which equivalence types
	% are used because equiv_type.m removes all references to the
	% equivalence types, and at that point we don't know which imported
	% items are going to be used by the compilation.
	%
	% For predicates declared using `with_type` annotations,
	% the version number in the interface file and the
	% version_numbers map will refer tothe arity before expansion
	% of the `with_type` annotation, so that needs to be recorded
	% here as well.
:- pred recompilation__record_expanded_items(item_id::in, set(item_id)::in,
	recompilation_info::in, recompilation_info::out) is det.

%-----------------------------------------------------------------------------%

:- type item_id_set(Map, Set, Cons) 
	---> item_id_set(
		types :: Map,
		type_bodies :: Map,
		modes :: Map,
		insts :: Map,
		typeclasses :: Map,
		functors :: Cons,
		predicates :: Set,
		functions :: Set
	).

:- type item_id_set(T) == item_id_set(T, T, T).

:- func init_item_id_set(T) = item_id_set(T).

:- func init_item_id_set(Simple, PorF, Cons) = item_id_set(Simple, PorF, Cons).

%-----------------------------------------------------------------------------%

	% An simple_item_set records the single possible match for an item.
:- type simple_item_set == map(pair(string, arity),
				map(module_qualifier, module_name)).

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

:- func map_ids((func(item_type, T) = U),
		item_id_set(T), U) = item_id_set(U).

%-----------------------------------------------------------------------------%

	% Version numbers for items in a single module.
:- type version_numbers
	---> version_numbers(
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

:- import_module parse_tree__prog_util.
:- import_module libs__globals, libs__options, hlds__passes_aux.
:- import_module parse_tree__modules.
:- import_module int, time, bool, list, require, string.

%-----------------------------------------------------------------------------%

term_to_version_number(Term) = term_to_timestamp(Term).

term_to_timestamp(term__functor(term__string(TimestampString), [], _)) = 
		string_to_timestamp(TimestampString).

write_version_number(VersionNumber) -->
	io__write_string(""""),
	io__write_string(timestamp_to_string(VersionNumber)),
	io__write_string("""").

%-----------------------------------------------------------------------------%

pred_or_func_to_item_type(predicate) = predicate.
pred_or_func_to_item_type(function) = function.

is_simple_item_type((type)).
is_simple_item_type(type_body).
is_simple_item_type((inst)).
is_simple_item_type((mode)).
is_simple_item_type((typeclass)).

is_pred_or_func_item_type(predicate).
is_pred_or_func_item_type(function).

string_to_item_type("type", (type)).
string_to_item_type("type_body", type_body).
string_to_item_type("inst", (inst)).
string_to_item_type("mode", (mode)).
string_to_item_type("typeclass", (typeclass)).
string_to_item_type("predicate", predicate).
string_to_item_type("function", function).
string_to_item_type("functor", functor).

%-----------------------------------------------------------------------------%

init_item_id_set(Init) =
		item_id_set(Init, Init, Init, Init, Init, Init, Init, Init).

init_item_id_set(Simple, PorF, Cons) =
		item_id_set(Simple, Simple, Simple, Simple, Simple,
			Cons, PorF, PorF).

init_used_items = item_id_set(map__init, map__init, map__init, map__init,
			map__init, map__init, map__init, map__init).

extract_simple_item_set(Items, type) = Items ^ types.
extract_simple_item_set(Items, type_body) = Items ^ type_bodies.
extract_simple_item_set(Items, mode) = Items ^ modes.
extract_simple_item_set(Items, inst) = Items ^ insts.
extract_simple_item_set(Items, typeclass) = Items ^ typeclasses.

update_simple_item_set(Items, type, IdMap) = Items ^ types := IdMap.
update_simple_item_set(Items, type_body, IdMap) = Items ^ type_bodies := IdMap.
update_simple_item_set(Items, mode, IdMap) = Items ^ modes := IdMap.
update_simple_item_set(Items, inst, IdMap) = Items ^ insts := IdMap.
update_simple_item_set(Items, typeclass, IdMap) = Items ^ typeclasses := IdMap.

extract_pred_or_func_set(Items, predicate) = Items ^ predicates.
extract_pred_or_func_set(Items, function) = Items ^ functions.

update_pred_or_func_set(Items, predicate, Set) = Items ^ predicates := Set.
update_pred_or_func_set(Items, function, Set) = Items ^ functions := Set.

extract_ids(Items, type) = Items ^ types.
extract_ids(Items, type_body) = Items ^ type_bodies.
extract_ids(Items, mode) = Items ^ modes.
extract_ids(Items, inst) = Items ^ insts.
extract_ids(Items, typeclass) = Items ^ typeclasses.
extract_ids(Items, functor) = Items ^ functors.
extract_ids(Items, predicate) = Items ^ predicates.
extract_ids(Items, function) = Items ^ functions.

update_ids(Items, type, IdMap) = Items ^ types := IdMap.
update_ids(Items, type_body, IdMap) = Items ^ type_bodies := IdMap.
update_ids(Items, mode, IdMap) = Items ^ modes := IdMap.
update_ids(Items, inst, IdMap) = Items ^ insts := IdMap.
update_ids(Items, typeclass, IdMap) = Items ^ typeclasses := IdMap.
update_ids(Items, predicate, IdMap) = Items ^ predicates := IdMap.
update_ids(Items, function, IdMap) = Items ^ functions := IdMap.
update_ids(Items, functor, IdMap) = Items ^ functors := IdMap.

map_ids(Func, Items0, Init) = Items :-
	Items1 = init_item_id_set(Init),
	Items = list__foldl(
		(func(ItemType, NewItems0) = 
			update_ids(NewItems0, ItemType,
				Func(ItemType, extract_ids(Items0, ItemType)))
		),
		[(type), type_body, (mode), (inst), (typeclass),
			functor, predicate, function],
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
		map__init,
		map__init
	).

recompilation__record_used_item(ItemType, Id, QualifiedId) -->
	ItemSet0 =^ used_items,
	{ IdSet0 = extract_ids(ItemSet0, ItemType) },
	{ QualifiedId = QualifiedName - Arity },
	{ unqualify_name(QualifiedName, UnqualifiedName) },
	{ ModuleName = find_module_qualifier(QualifiedName) },
	{ UnqualifiedId = UnqualifiedName - Arity },
	{ Id = SymName - _ },
	{ ModuleQualifier = find_module_qualifier(SymName) },
	( { map__search(IdSet0, UnqualifiedId, MatchingNames0) } ->
		{ MatchingNames1 = MatchingNames0 }
	;
		{ map__init(MatchingNames1) }
	),
	( { map__contains(MatchingNames1, ModuleQualifier) } ->
		[]	
	;
		{ map__det_insert(MatchingNames1, ModuleQualifier,
			ModuleName, MatchingNames) },
		{ map__set(IdSet0, UnqualifiedId,
			MatchingNames, IdSet) },
		{ ItemSet = update_ids(ItemSet0, ItemType, IdSet) },
		^ used_items := ItemSet
	).

recompilation__record_expanded_items(Item, ExpandedItems, Info0, Info) :-
	( set__empty(ExpandedItems) ->
		Info = Info0
	;
		DepsMap0 = Info0 ^ dependencies,
		( map__search(DepsMap0, Item, Deps0) ->
			Deps1 = Deps0
		;
			set__init(Deps1)
		),
		set__union(Deps1, ExpandedItems, Deps),
		map__set(DepsMap0, Item, Deps, DepsMap),
		Info = Info0 ^ dependencies := DepsMap
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

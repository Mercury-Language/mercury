%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: recompilation_version.m
% Main author: stayl
%
% Compute version numbers for program items in interface files.
%-----------------------------------------------------------------------------%
:- module recompilation_version.

:- interface.

:- import_module recompilation, prog_data, prog_io_util, timestamp.
:- import_module io, std_util, term.

	% recompilation_version__compute_version_numbers(SourceFileModTime,
	%	NewItems, MaybeOldItems, VersionNumbers).
:- pred recompilation_version__compute_version_numbers(timestamp::in,
	item_list::in, maybe(item_list)::in, version_numbers::out) is det.

:- pred recompilation_version__write_version_numbers(version_numbers::in,
	io__state::di, io__state::uo) is det.

:- pred recompilation_version__parse_version_numbers(term::in,
	maybe1(version_numbers)::out) is det.

	% The version number for the format of the version numbers
	% written to the interface files.
:- func version_numbers_version_number = int.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module mercury_to_mercury, prog_io, prog_util.
:- import_module assoc_list, bool, list, map, require, string.

recompilation_version__compute_version_numbers(SourceFileTime, Items,
		MaybeOldItems, ItemVersionNumbers - InstanceVersionNumbers) :-
	recompilation_version__gather_items(Items,
		GatheredItems, InstanceItems),
	(
		MaybeOldItems = yes(OldItems0),
		OldItems0 = [VersionNumberItem | OldItems],
		VersionNumberItem = module_defn(_,
			version_numbers(_, OldVersionNumbers)) - _
	->
		OldVersionNumbers = OldItemVersionNumbers
					- OldInstanceVersionNumbers,
		recompilation_version__gather_items(OldItems, GatheredOldItems,
			OldInstanceItems)
	;
		% There were no old version numbers, so every item
		% gets the same timestamp as the source module.
		OldItemVersionNumbers = init_item_id_set(map__init),
		GatheredOldItems = init_item_id_set(map__init),
		map__init(OldInstanceItems),
		map__init(OldInstanceVersionNumbers)
	),
	
	recompilation_version__compute_item_version_numbers(SourceFileTime,
		GatheredItems, GatheredOldItems, OldItemVersionNumbers,
		ItemVersionNumbers),
		
	recompilation_version__compute_instance_version_numbers(SourceFileTime,
		InstanceItems, OldInstanceItems, OldInstanceVersionNumbers,
		InstanceVersionNumbers).

:- pred recompilation_version__compute_item_version_numbers(timestamp::in,
	gathered_items::in, gathered_items::in,
	item_version_numbers::in, item_version_numbers::out) is det.

recompilation_version__compute_item_version_numbers(SourceFileTime,
		GatheredItems, GatheredOldItems,
		OldVersionNumbers, VersionNumbers) :-
	VersionNumbers = map_ids(
	    (func(ItemType, Items0) =
		map__map_values(
		    (func(NameArity, Items) = VersionNumber :-
			OldIds = extract_ids(GatheredOldItems, ItemType),
			(
			    map__search(OldIds, NameArity, OldItems),
			    items_are_unchanged(OldItems, Items),
		    	    map__search(
			    	extract_ids(OldVersionNumbers, ItemType),
			    	NameArity, OldVersionNumber)
			->
			    VersionNumber = OldVersionNumber
			;
			    VersionNumber = SourceFileTime
			)
		    ),
		    Items0
		)
	    ),
	    GatheredItems,
	    map__init
	).

:- pred recompilation_version__compute_instance_version_numbers(timestamp::in,
	instance_item_map::in, instance_item_map::in, 
	instance_version_numbers::in, instance_version_numbers::out) is det.

recompilation_version__compute_instance_version_numbers(SourceFileTime,
		InstanceItems, OldInstanceItems,
		OldInstanceVersionNumbers, InstanceVersionNumbers) :-
	InstanceVersionNumbers =
	    map__map_values(
		(func(ClassId, Items) = VersionNumber :-
		    (
		    	map__search(OldInstanceItems, ClassId, OldItems),
			items_are_unchanged(OldItems, Items),
		    	map__search(OldInstanceVersionNumbers, ClassId,
				OldVersionNumber)
		    ->
			VersionNumber = OldVersionNumber
		    ;
			VersionNumber = SourceFileTime
		    )
		),
		InstanceItems
	    ).

:- pred recompilation_version__gather_items(item_list::in,
		gathered_items::out, instance_item_map::out) is det.

recompilation_version__gather_items(Items, GatheredItems, Instances) :-
	list__reverse(Items, RevItems),
	Info0 = gathered_item_info(init_item_id_set(map__init),
			[], [], map__init),
	list__foldl(recompilation_version__gather_items_2, RevItems,
			Info0, Info1),

	%
	% Items which could appear in _OtherItems (those which aren't
	% gathered into the list for another type of item) can't appear
	% in the interface section. Those other items (e.g. assertions)
	% will need to be handled here when smart recompilation is made to
	% work with `--intermodule-optimization'.
	%
	Info1 = gathered_item_info(GatheredItems1, PragmaItems,
			_OtherItems, Instances),
	list__reverse(PragmaItems, RevPragmaItems),
	list__foldl(distribute_pragma_items, RevPragmaItems,
		GatheredItems1, GatheredItems).

:- pred distribute_pragma_items(
		pair(maybe_pred_or_func_id, item_and_context)::in,
		gathered_items::in, gathered_items::out) is det.

distribute_pragma_items(ItemId - ItemAndContext,
		GatheredItems0, GatheredItems) :-
	ItemId = MaybePredOrFunc - SymName / Arity,
	ItemAndContext = Item - ItemContext,
	AddIfNotExisting = no,
	(
		MaybePredOrFunc = yes(PredOrFunc),

		ItemType = pred_or_func_to_item_type(PredOrFunc),
		recompilation_version__add_gathered_item(Item,
			item_id(ItemType, SymName - Arity),
			ItemContext, AddIfNotExisting,
			GatheredItems0, GatheredItems)
	;
		MaybePredOrFunc = no,

		recompilation_version__add_gathered_item(Item,
			item_id(predicate, SymName - Arity),
			ItemContext, AddIfNotExisting,
			GatheredItems0, GatheredItems1),

		adjust_func_arity(function, Arity, FuncArity),
		recompilation_version__add_gathered_item(Item,
			item_id(function, SymName - FuncArity),
			ItemContext, AddIfNotExisting,
			GatheredItems1, GatheredItems)
	).


:- type gathered_item_info
	--->	gathered_item_info(
			gathered_items :: gathered_items,
			pragma_items :: assoc_list(maybe_pred_or_func_id, 
						item_and_context),
			other_items :: item_list,
			instances :: instance_item_map
		).

:- type instance_item_map == map(item_name, item_list).

	% The constructors set should always be empty.
:- type gathered_items == item_id_set(map(pair(string, arity), item_list)).

:- pred recompilation_version__gather_items_2(item_and_context::in,
		gathered_item_info::in, gathered_item_info::out) is det.

recompilation_version__gather_items_2(ItemAndContext) -->
	{ ItemAndContext = Item - ItemContext },
	(
		{ Item = type_defn(VarSet, Name, Args, Body, Cond) }
	->
		(
			{ Body = abstract_type },
			{ NameItem = Item },
			% The body of an abstract type can be recorded
			% as used when generating a call to the automatically
			% generated unification procedure.
			{ BodyItem = Item }
		;
			{ Body = du_type(_, _) },
			{ NameItem = type_defn(VarSet, Name, Args,
				abstract_type, Cond) },
			{ BodyItem = Item }	
		;
			{ Body = eqv_type(_) },
			% When we use an equivalence type we
			% always use the body.
			{ NameItem = Item },
			{ BodyItem = Item }
		;
			{ Body = uu_type(_) },
			{ error(
			"recompilation_version__gather_items_2: uu_type") }
		),
		{ TypeId = Name - list__length(Args) },
		GatheredItems0 =^ gathered_items,
		{ recompilation_version__add_gathered_item(NameItem,
			item_id((type), TypeId), ItemContext,
			yes, GatheredItems0, GatheredItems1) },
		{ recompilation_version__add_gathered_item(BodyItem,
			item_id(type_body, TypeId), ItemContext,
			yes, GatheredItems1, GatheredItems) },
		^ gathered_items := GatheredItems
	;
		{ Item = instance(_, ClassName, ClassArgs, _, _, _) }
	->
		Instances0 =^ instances,
		{ ClassArity = list__length(ClassArgs) },
		(
			{ map__search(Instances0, ClassName - ClassArity,
				InstanceItems0) }
		->
			{ InstanceItems = InstanceItems0 }
		;
			{ InstanceItems = [] }
		),
		{ map__set(Instances0, ClassName - ClassArity,
			[Item - ItemContext | InstanceItems], Instances) },
		^ instances := Instances
	;
		{ item_to_item_id(Item, ItemId) }
	->
		GatheredItems0 =^ gathered_items,
		{ recompilation_version__add_gathered_item(Item, ItemId,
			ItemContext, yes, GatheredItems0, GatheredItems) },
		^ gathered_items := GatheredItems
	;
		{ Item = pragma(PragmaType) },
		{ is_pred_pragma(PragmaType, yes(PredOrFuncId)) }
	->
		PragmaItems =^ pragma_items,
		^ pragma_items := [PredOrFuncId - ItemAndContext | PragmaItems]
	;
		OtherItems =^ other_items,
		^ other_items := [ItemAndContext | OtherItems]
	).

:- pred recompilation_version__add_gathered_item(item::in, item_id::in,
		prog_context::in, bool::in, gathered_items::in,
		gathered_items::out) is det.

recompilation_version__add_gathered_item(Item, ItemId, ItemContext,
		AddIfNotExisting, GatheredItems0, GatheredItems) :-
	ItemId = item_id(ItemType, Id),
	Id = SymName - Arity,
	unqualify_name(SymName, Name),
	IdMap0 = extract_ids(GatheredItems0, ItemType),
	NameArity = Name - Arity,
	( map__search(IdMap0, NameArity, MatchingItems0) ->
		MatchingItems = MatchingItems0
	;
		MatchingItems = []
	),
	( MatchingItems = [], AddIfNotExisting = no ->
		GatheredItems = GatheredItems0
	;
		recompilation_version__add_gathered_item_2(Item, ItemType,
			NameArity, ItemContext, MatchingItems,
			GatheredItems0, GatheredItems)
	).

:- pred recompilation_version__add_gathered_item_2(item::in, item_type::in,
		pair(string, arity)::in, prog_context::in, item_list::in,
		gathered_items::in, gathered_items::out) is det.

recompilation_version__add_gathered_item_2(Item, ItemType, NameArity,
		ItemContext, MatchingItems0, GatheredItems0, GatheredItems) :-

	% mercury_to_mercury.m splits combined pred and mode
	% declarations. That needs to be done here as well
	% the item list read from the interface file will match
	% the item list generated here.
	(
		Item = pred_or_func(TVarSet, InstVarSet, ExistQVars,
			PredOrFunc, PredName, TypesAndModes, Det,
			Cond, Purity, ClassContext),
		split_types_and_modes(TypesAndModes,
			Types, MaybeModes),
		MaybeModes = yes(Modes)
	->
		TypesWithoutModes = list__map(
			(func(Type) = type_only(Type)), Types),
		PredOrFuncItem = pred_or_func(TVarSet, InstVarSet,
			ExistQVars, PredOrFunc, PredName,
			TypesWithoutModes, no, Cond, Purity,
			ClassContext),
		PredOrFuncModeItem = pred_or_func_mode(InstVarSet,
			PredOrFunc, PredName, Modes, Det, Cond),
		MatchingItems =
			[PredOrFuncItem - ItemContext,
			PredOrFuncModeItem - ItemContext
			| MatchingItems0]
	;
		Item = typeclass(Constraints, ClassName, ClassArgs,
			ClassInterface0, ClassTVarSet),
		ClassInterface0 = concrete(Methods0)
	->
		MethodsList = list__map(
			split_class_method_types_and_modes, Methods0),
		list__condense(MethodsList, Methods),
		TypeclassItem = typeclass(Constraints, ClassName, ClassArgs,
			concrete(Methods), ClassTVarSet),
		MatchingItems = [TypeclassItem - ItemContext | MatchingItems0]
	;
		MatchingItems = [Item - ItemContext| MatchingItems0]
	),

	IdMap0 = extract_ids(GatheredItems0, ItemType),
	map__set(IdMap0, NameArity, MatchingItems, IdMap),
	GatheredItems = update_ids(GatheredItems0, ItemType, IdMap).

:- func split_class_method_types_and_modes(class_method) = list(class_method).

split_class_method_types_and_modes(Method0) = Items :-
	% Always strip the context from the item -- this is needed
	% so the items can be easily tested for equality.
	Method0 = pred_or_func(TVarSet, InstVarSet, ExistQVars,
		PredOrFunc, SymName, TypesAndModes, MaybeDet,
		Cond, Purity, ClassContext, _),
	(
		split_types_and_modes(TypesAndModes, Types, MaybeModes),
		MaybeModes = yes(Modes)
	->
		TypesWithoutModes = list__map(
			(func(Type) = type_only(Type)), Types),
		PredOrFuncModeItem = pred_or_func_mode(InstVarSet, PredOrFunc,
			SymName, Modes, MaybeDet, Cond, term__context_init),
		PredOrFuncModeItems = [PredOrFuncModeItem]
	;
		TypesWithoutModes = TypesAndModes,
		PredOrFuncModeItems = []
	),
	PredOrFuncItem = pred_or_func(TVarSet, InstVarSet,
		ExistQVars, PredOrFunc, SymName,
		TypesWithoutModes, no, Cond, Purity,
		ClassContext, term__context_init),
	Items = [PredOrFuncItem | PredOrFuncModeItems].
split_class_method_types_and_modes(Method0) = [Method] :-
	% Always strip the context from the item -- this is needed
	% so the items can be easily tested for equality.
	Method0 = pred_or_func_mode(A, B, C, D, E, F, _),
	Method = pred_or_func_mode(A, B, C, D, E, F, term__context_init).

:- pred item_to_item_id(item::in, item_id::out) is semidet.

item_to_item_id(Item, ItemId) :-
	item_to_item_id_2(Item, yes(ItemId)).

:- pred item_to_item_id_2(item::in, maybe(item_id)::out) is det.

item_to_item_id_2(clause(_, _, _, _, _), no).
item_to_item_id_2(type_defn(_, Name, Params, _, _),
		yes(item_id((type), Name - Arity))) :-
	list__length(Params, Arity).		
item_to_item_id_2(inst_defn(_, Name, Params, _, _),
		yes(item_id((inst), Name - Arity))) :-	
	list__length(Params, Arity).
item_to_item_id_2(mode_defn(_, Name, Params, _, _),
		yes(item_id((mode), Name - Arity))) :-	
	list__length(Params, Arity).
item_to_item_id_2(module_defn(_, _), no).
item_to_item_id_2(Item, yes(item_id(ItemType, SymName - Arity))) :-
	Item = pred_or_func(_, _, _, PredOrFunc, SymName,
			TypesAndModes, _, _, _, _),
	adjust_func_arity(PredOrFunc, Arity, list__length(TypesAndModes)),
	ItemType = pred_or_func_to_item_type(PredOrFunc).
item_to_item_id_2(Item, yes(item_id(ItemType, SymName - Arity))) :-
	Item = pred_or_func_mode(_, PredOrFunc, SymName, Modes, _, _),
	adjust_func_arity(PredOrFunc, Arity, list__length(Modes)),
	ItemType = pred_or_func_to_item_type(PredOrFunc).

	% We need to handle these separately because some pragmas
	% may affect a predicate and a function.
item_to_item_id_2(pragma(_), no).
item_to_item_id_2(assertion(_, _), no).
item_to_item_id_2(Item, yes(item_id((typeclass), ClassName - ClassArity))) :-
	Item = typeclass(_, ClassName, ClassVars, _, _),
	list__length(ClassVars, ClassArity).	
item_to_item_id_2(Item, yes(item_id((typeclass), ClassName - ClassArity))) :-
	Item = instance(_, ClassName, ClassArgs, _, _, _),
	list__length(ClassArgs, ClassArity).	
item_to_item_id_2(nothing(_), no).

:- type maybe_pred_or_func_id ==
		pair(maybe(pred_or_func), sym_name_and_arity).

:- pred is_pred_pragma(pragma_type::in,
		maybe(maybe_pred_or_func_id)::out) is det.

is_pred_pragma(foreign_decl(_, _), no).
is_pred_pragma(foreign_code(_, _), no).
is_pred_pragma(foreign_proc(_, Name, PredOrFunc, Args, _, _),
		yes(yes(PredOrFunc) - Name / Arity)) :-
	adjust_func_arity(PredOrFunc, Arity, list__length(Args)).
is_pred_pragma(type_spec(Name, _, Arity, MaybePredOrFunc, _, _, _, _),
		yes(MaybePredOrFunc - Name / Arity)).
is_pred_pragma(inline(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(no_inline(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(obsolete(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(export(Name, PredOrFunc, Modes, _), 
		yes(yes(PredOrFunc) - Name / Arity)) :-
	adjust_func_arity(PredOrFunc, Arity, list__length(Modes)).	
	% Pragma import declarations are never used
	% directly by Mercury code.
is_pred_pragma(import(_, _, _, _, _), no).
is_pred_pragma(source_file(_), no).
is_pred_pragma(unused_args(PredOrFunc, Name, Arity, _, _),
		yes(yes(PredOrFunc) - Name / Arity)).
is_pred_pragma(fact_table(Name, Arity, _), yes(no - Name / Arity)).
is_pred_pragma(aditi(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(base_relation(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(aditi_index(Name, Arity, _), yes(no - Name / Arity)).
is_pred_pragma(naive(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(psn(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(aditi_memo(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(aditi_no_memo(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(supp_magic(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(context(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(owner(Name, Arity, _), yes(no - Name / Arity)).
is_pred_pragma(tabled(_, Name, Arity, MaybePredOrFunc, _),
		yes(MaybePredOrFunc - Name / Arity)).
is_pred_pragma(promise_pure(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(promise_semipure(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(termination_info(PredOrFunc, Name, Modes, _, _),
		yes(yes(PredOrFunc) - Name / Arity)) :-
	adjust_func_arity(PredOrFunc, Arity, list__length(Modes)).	
is_pred_pragma(terminates(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(does_not_terminate(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(check_termination(Name, Arity), yes(no - Name / Arity)).

	% XXX This is a bit brittle (need to be careful with term__contexts).
	% For example, it won't work for clauses.
	% It will never succeed when it shouldn't, so it will never
	% cause a necessary recompilation to be missed.
:- pred items_are_unchanged(item_list::in, item_list::in) is semidet.

items_are_unchanged([], []).
items_are_unchanged([Item - _ | Items1], [Item - _ | Items2]) :-
	items_are_unchanged(Items1, Items2).

%-----------------------------------------------------------------------------%

recompilation_version__write_version_numbers(
		VersionNumbers - InstanceVersionNumbers) -->
	{ VersionNumbersList = list__filter_map(
		(func(ItemType) = (ItemType - ItemVersions) is semidet :-
			ItemVersions = extract_ids(VersionNumbers, ItemType),
			\+ map__is_empty(ItemVersions)
		),
		[(type), type_body, (mode), (inst),
			predicate, function, (typeclass)]) },
	io__write_string("{\n\t"),
	io__write_list(VersionNumbersList, ",\n\t",
	    (pred((ItemType - ItemVersions)::in, di, uo) is det -->
		{ string_to_item_type(ItemTypeStr, ItemType) },
		io__write_string(ItemTypeStr),
		io__write_string("(\n\t\t"),
		{ map__to_assoc_list(ItemVersions, ItemVersionsList) },
		io__write_list(ItemVersionsList, ",\n\t\t",
		    (pred((NameArity - VersionNumber)::in, di, uo) is det -->
			{ NameArity = Name - Arity },
			mercury_output_bracketed_sym_name(unqualified(Name),
				next_to_graphic_token),
			io__write_string("/"),
			io__write_int(Arity),
			io__write_string(" - "),
			write_version_number(VersionNumber)
		    )),
	    	io__write_string("\n\t)")
	    )),
	( { map__is_empty(InstanceVersionNumbers) } ->
		[]
	;
		( { VersionNumbersList = [] } ->
			[]
		;
			io__write_string(",\n\t")
		),
		io__write_string("instance("),
		{ map__to_assoc_list(InstanceVersionNumbers, InstanceAL) },
		io__write_list(InstanceAL, ",\n\n\t",
		    (pred((ClassNameArity - ClassVersionNumber)::in,
		    		di, uo) is det -->
			{ ClassNameArity = ClassName - ClassArity },
			mercury_output_bracketed_sym_name(ClassName,
				next_to_graphic_token),
			io__write_string("/"),
			io__write_int(ClassArity),
			io__write_string(" - "),
			write_version_number(ClassVersionNumber)
		    )),
		io__write_string(")\n\t")
	),
	io__write_string("\n}").

%-----------------------------------------------------------------------------%

version_numbers_version_number = 1.

%-----------------------------------------------------------------------------%

parse_version_numbers(VersionNumbersTerm, Result) :-
	(
		VersionNumbersTerm = term__functor(term__atom("{}"),
			VersionNumbersTermList0, _)
	->
		VersionNumbersTermList = VersionNumbersTermList0
	;
		VersionNumbersTermList = [VersionNumbersTerm]
	),	
	map_parser(parse_item_type_version_numbers,
		VersionNumbersTermList, Result0),
	(
		Result0 = ok(List),
		VersionNumbers0 = init_item_id_set(map__init) - map__init,
		VersionNumbers = list__foldl(
		    (func(VNResult, VNs0 - Instances0) = VNs - Instances :-
			( 
				VNResult = items(ItemType, ItemVNs),
				VNs = update_ids(VNs0, ItemType, ItemVNs),
				Instances = Instances0
			;
				VNResult = instances(Instances),
				VNs = VNs0
			)
		    ), List, VersionNumbers0),
		Result = ok(VersionNumbers)
	;
		Result0 = error(A, B),
		Result = error(A, B)
	).

:- type item_version_numbers_result
	--->	items(item_type, version_number_map)
	;	instances(instance_version_numbers)
	.

:- pred parse_item_type_version_numbers(term::in,
		maybe1(item_version_numbers_result)::out) is det.

parse_item_type_version_numbers(Term, Result) :-
	(
		Term = term__functor(term__atom(ItemTypeStr),
				ItemsVNsTerms, _),
		string_to_item_type(ItemTypeStr, ItemType)
	->
		ParseName =
		    (pred(NameTerm::in, Name::out) is semidet :-
			NameTerm = term__functor(term__atom(Name), [], _)
		    ),
		map_parser(parse_item_version_number(ParseName),
			ItemsVNsTerms, Result0),
		(
			Result0 = ok(VNsAL),
			map__from_assoc_list(VNsAL, VNsMap),
			Result = ok(items(ItemType, VNsMap))
		;
			Result0 = error(A, B),
			Result = error(A, B)
		)
	;
		Term = term__functor(term__atom("instance"),
				InstanceVNsTerms, _)
	->
		ParseName =
		    (pred(NameTerm::in, Name::out) is semidet :-
			sym_name_and_args(NameTerm, Name, [])
		    ),
		map_parser(parse_item_version_number(ParseName),
			InstanceVNsTerms, Result1),
		(
			Result1 = ok(VNsAL),
			map__from_assoc_list(VNsAL, VNsMap),
			Result = ok(instances(VNsMap))	
		;
			Result1 = error(A, B),
			Result = error(A, B)
		)	
	;
		Result = error("invalid item type version numbers",
				Term)
	).

:- pred parse_item_version_number(pred(term, T)::(pred(in, out) is semidet),
	term::in, maybe1(pair(pair(T, arity), version_number))::out) is det.

parse_item_version_number(ParseName, Term, Result) :-
	(
		Term = term__functor(term__atom("-"),
			[ItemNameArityTerm, VersionNumberTerm], _),
		ItemNameArityTerm = term__functor(term__atom("/"),
			[NameTerm, ArityTerm], _),
		ParseName(NameTerm, Name),
		ArityTerm = term__functor(term__integer(Arity), _, _),
		VersionNumber = term_to_version_number(VersionNumberTerm)
	->
		Result = ok((Name - Arity) - VersionNumber)
	;
		Result = error("error in item version number", Term)
	).

%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module contains a parse-tree to parse-tree transformation
% that expands equivalence types.

% main author: fjh

:- module equiv_type.
:- interface.
:- import_module prog_data, list.

%-----------------------------------------------------------------------------%

	% The following predicate equiv_type__expand_eqv_types traverses
	% through the list of items.  Each time it finds an eqv_type
	% definition, it replaces all occurrences of the type (both
	% before and after it in the list of items) with type that it
	% is equivalent to.  This has the effect of eliminating all the
	% equivalence types from the source code.  Circular equivalence
	% types in the input will cause references to undefined types
	% in the output.

:- pred equiv_type__expand_eqv_types(list(item_and_context),
					list(item_and_context), eqv_map).
:- mode equiv_type__expand_eqv_types(in, out, out) is det.

	% Replace equivalence types in a given type.
:- pred equiv_type__replace_in_type(type, tvarset, eqv_map, type, tvarset).
:- mode equiv_type__replace_in_type(in, in, in, out, out) is det.

:- type eqv_map.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bool, std_util, map, term, varset.
:- import_module type_util, prog_util.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% First we build up a mapping which records the equivalence type
	% definitions.  Then we go through the item list and replace
	% them.

equiv_type__expand_eqv_types(Items0, Items, EqvMap) :-
	map__init(EqvMap0),
	equiv_type__build_eqv_map(Items0, EqvMap0, EqvMap),
	equiv_type__replace_in_item_list(Items0, EqvMap, Items).

:- type eqv_type_body ---> eqv_type_body(tvarset, list(type_param), type).
:- type eqv_map == map(type_id, eqv_type_body).

:- pred equiv_type__build_eqv_map(list(item_and_context), eqv_map, eqv_map).
:- mode equiv_type__build_eqv_map(in, in, out) is det.

equiv_type__build_eqv_map([], EqvMap, EqvMap).
equiv_type__build_eqv_map([Item - _Context | Items], EqvMap0, EqvMap) :-
	( Item = type_defn(VarSet, eqv_type(Name, Args, Body), _Cond) ->
		list__length(Args, Arity),
		map__set(EqvMap0, Name - Arity,
			eqv_type_body(VarSet, Args, Body), EqvMap1)
	;
		EqvMap1 = EqvMap0
	),
	equiv_type__build_eqv_map(Items, EqvMap1, EqvMap).

	% The following predicate equiv_type__replace_in_item_list
	% performs substititution of equivalence types on a list
	% of items.  Similarly the replace_in_<foo> predicates that
	% follow perform substitution of equivalence types on <foo>s.

:- pred equiv_type__replace_in_item_list(list(item_and_context), eqv_map,
			list(item_and_context)).
:- mode equiv_type__replace_in_item_list(in, in, out) is det.

equiv_type__replace_in_item_list([], _, []).
equiv_type__replace_in_item_list([Item0 - Context | Items0], EqvMap,
				[Item - Context | Items]) :-
	( equiv_type__replace_in_item(Item0, EqvMap, Item1) ->
		Item = Item1
	;
		Item = Item0
	),
	equiv_type__replace_in_item_list(Items0, EqvMap, Items).

:- pred equiv_type__replace_in_item(item, eqv_map, item).
:- mode equiv_type__replace_in_item(in, in, out) is semidet.

equiv_type__replace_in_item(type_defn(VarSet0, TypeDefn0, Cond),
		EqvMap, type_defn(VarSet, TypeDefn, Cond)) :-
	equiv_type__replace_in_type_defn(TypeDefn0, VarSet0, EqvMap,
				TypeDefn, VarSet).

equiv_type__replace_in_item(pred(VarSet0, PredName, TypesAndModes0, Det, Cond),
		EqvMap, pred(VarSet, PredName, TypesAndModes, Det, Cond)) :-
	equiv_type__replace_in_tms(TypesAndModes0, VarSet0, EqvMap, 
					TypesAndModes, VarSet).

equiv_type__replace_in_item(
			func(VarSet0, PredName, TypesAndModes0, 
				RetTypeAndMode0, Det, Cond),
			EqvMap,
			func(VarSet, PredName, TypesAndModes, RetTypeAndMode,
				Det, Cond)) :-
	equiv_type__replace_in_tms(TypesAndModes0, VarSet0, EqvMap,
				TypesAndModes, VarSet1),
	equiv_type__replace_in_tm(RetTypeAndMode0, VarSet1, EqvMap,
				RetTypeAndMode, VarSet).

:- pred equiv_type__replace_in_type_defn(type_defn, tvarset, eqv_map,
					type_defn, tvarset).
:- mode equiv_type__replace_in_type_defn(in, in, in, out, out) is semidet.

equiv_type__replace_in_type_defn(eqv_type(TName, TArgs, TBody0), VarSet0,
			EqvMap, eqv_type(TName, TArgs, TBody), VarSet) :-
	equiv_type__replace_in_type(TBody0, VarSet0, EqvMap, TBody, VarSet).

equiv_type__replace_in_type_defn(uu_type(TName, TArgs, TBody0), VarSet0,
			EqvMap, uu_type(TName, TArgs, TBody), VarSet) :-
	equiv_type__replace_in_uu(TBody0, VarSet0, EqvMap, TBody, VarSet).

equiv_type__replace_in_type_defn(du_type(TName, TArgs, TBody0), VarSet0,
			EqvMap, du_type(TName, TArgs, TBody), VarSet) :-
	equiv_type__replace_in_du(TBody0, VarSet0, EqvMap, TBody, VarSet).

:- pred equiv_type__replace_in_uu(list(type), tvarset, eqv_map,
					list(type), tvarset).
:- mode equiv_type__replace_in_uu(in, in, in, out, out) is det.

equiv_type__replace_in_uu(Ts0, VarSet0, EqvMap,
				Ts, VarSet) :-
	equiv_type__replace_in_type_list(Ts0, VarSet0, EqvMap,
					Ts, VarSet).

:- pred equiv_type__replace_in_du(list(constructor), tvarset, eqv_map,
				list(constructor), tvarset).
:- mode equiv_type__replace_in_du(in, in, in, out, out) is det.

equiv_type__replace_in_du([], VarSet, _EqvMap, [], VarSet).
equiv_type__replace_in_du([T0|Ts0], VarSet0, EqvMap, [T|Ts], VarSet) :-
	equiv_type__replace_in_ctor(T0, VarSet0, EqvMap, T, VarSet1),
	equiv_type__replace_in_du(Ts0, VarSet1, EqvMap, Ts, VarSet).

:- pred equiv_type__replace_in_ctor(constructor, tvarset, eqv_map,
				constructor, tvarset).
:- mode equiv_type__replace_in_ctor(in, in, in, out, out) is det.

equiv_type__replace_in_ctor(TName - Targs0, VarSet0, EqvMap,
		TName - Targs, VarSet) :-
	equiv_type__replace_in_type_list(Targs0, VarSet0, EqvMap,
		Targs, VarSet).

:- pred equiv_type__replace_in_type_list(list(type), tvarset, eqv_map,
					list(type), tvarset).
:- mode equiv_type__replace_in_type_list(in, in, in, out, out) is det.

equiv_type__replace_in_type_list(Ts0, VarSet0, EqvMap,
				Ts, VarSet) :-
	equiv_type__replace_in_type_list_2(Ts0, VarSet0, EqvMap, [],
					Ts, VarSet).

:- pred equiv_type__replace_in_type_list_2(list(type), tvarset, eqv_map,
			list(type_id), list(type), tvarset).
:- mode equiv_type__replace_in_type_list_2(in, in, in, in, out, out) is det.

equiv_type__replace_in_type_list_2([], VarSet, _EqvMap, _Seen,
					[], VarSet).
equiv_type__replace_in_type_list_2([T0|Ts0], VarSet0, EqvMap, Seen,
				[T|Ts], VarSet) :-
	equiv_type__replace_in_type_2(T0, VarSet0, EqvMap, Seen,
					T, VarSet1),
	equiv_type__replace_in_type_list_2(Ts0, VarSet1, EqvMap, Seen,
					Ts, VarSet).

equiv_type__replace_in_type(Type0, VarSet0, EqvMap, Type, VarSet) :-
	equiv_type__replace_in_type_2(Type0, VarSet0, EqvMap,
			[], Type, VarSet).

:- pred equiv_type__replace_in_type_2(type, tvarset, eqv_map,
					list(type_id),
					type, tvarset).
:- mode equiv_type__replace_in_type_2(in, in, in, in, out, out) is det.

equiv_type__replace_in_type_2(term__variable(V), VarSet, _EqvMap,
		_Seen, term__variable(V), VarSet).
equiv_type__replace_in_type_2(Type0, VarSet0, EqvMap,
		TypeIdsAlreadyExpanded, Type, VarSet) :- 

	Type0 = term__functor(_, _, Context),
	(
		type_to_type_id(Type0, EqvTypeId, TArgs0)
	->
		equiv_type__replace_in_type_list_2(TArgs0, VarSet0, EqvMap,
				TypeIdsAlreadyExpanded, TArgs1, VarSet1),

		(	
			map__search(EqvMap, EqvTypeId,
				eqv_type_body(EqvVarSet, Args0, Body0)),
			varset__merge(VarSet1, EqvVarSet, [Body0 | Args0],
					VarSet2, [Body | Args]),
			\+ list__member(EqvTypeId, TypeIdsAlreadyExpanded)
		->
			term__term_list_to_var_list(Args, ArgVars),
			term__substitute_corresponding(ArgVars, TArgs1,
							Body, Type1),
			equiv_type__replace_in_type_2(Type1, VarSet2,
				EqvMap, [EqvTypeId | TypeIdsAlreadyExpanded],
				Type, VarSet)
		;
			VarSet = VarSet1,
			EqvTypeId = SymName - _,
			construct_qualified_term(SymName, TArgs1,
							Context, Type)
			
		)
	;
		VarSet = VarSet0,
		Type = Type0
	).

:- pred equiv_type__replace_in_tms(list(type_and_mode), tvarset, eqv_map,
			list(type_and_mode), tvarset).
:- mode equiv_type__replace_in_tms(in, in, in, out, out) is det.

equiv_type__replace_in_tms([], VarSet, _EqvMap, [], VarSet).
equiv_type__replace_in_tms([TM0|TMs0], VarSet0, EqvMap, [TM|TMs], VarSet) :-
	equiv_type__replace_in_tm(TM0, VarSet0, EqvMap, TM, VarSet1),
	equiv_type__replace_in_tms(TMs0, VarSet1, EqvMap, TMs, VarSet).

:- pred equiv_type__replace_in_tm(type_and_mode, tvarset, eqv_map,
				type_and_mode, tvarset).
:- mode equiv_type__replace_in_tm(in, in, in, out, out) is det.

equiv_type__replace_in_tm(type_only(Type0), VarSet0, EqvMap,
				type_only(Type), VarSet) :-
	equiv_type__replace_in_type(Type0, VarSet0, EqvMap, Type, VarSet).

equiv_type__replace_in_tm(type_and_mode(Type0, Mode), VarSet0, EqvMap,
			type_and_mode(Type, Mode), VarSet) :-
	equiv_type__replace_in_type(Type0, VarSet0, EqvMap, Type, VarSet).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

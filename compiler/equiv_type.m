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
					list(item_and_context)).
:- mode equiv_type__expand_eqv_types(in, out) is det.

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

equiv_type__expand_eqv_types(Items0, Items) :-
	map__init(EqvMap0),
	equiv_type__build_eqv_map(Items0, EqvMap0, EqvMap),
	equiv_type__replace_in_item_list(Items0, EqvMap, [], Items1),
	list__reverse(Items1, Items).

:- type eqv_type_id == pair(sym_name, arity).
:- type eqv_type_body ---> eqv_type_body(tvarset, list(type_param), type).
:- type eqv_map == map(eqv_type_id, eqv_type_body).

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
			list(item_and_context), list(item_and_context)).
:- mode equiv_type__replace_in_item_list(in, in, in, out) is det.

equiv_type__replace_in_item_list([], _, Items, Items).
equiv_type__replace_in_item_list([Item0 - Context | Items0], EqvMap,
				Items1, Items) :-
	( equiv_type__replace_in_item(Item0, EqvMap, Item) ->
		Items2 = [Item - Context | Items1]
	;
		Items2 = [Item0 - Context | Items1]
	),
	equiv_type__replace_in_item_list(Items0, EqvMap, Items2, Items).

:- pred equiv_type__replace_in_item(item, eqv_map, item).
:- mode equiv_type__replace_in_item(in, in, out) is semidet.

equiv_type__replace_in_item(type_defn(VarSet0, TypeDefn0, Cond),
		EqvMap, type_defn(VarSet, TypeDefn, Cond)) :-
	equiv_type__replace_in_type_defn(TypeDefn0, VarSet0, EqvMap,
				TypeDefn, VarSet).

equiv_type__replace_in_item(pred(VarSet0, PredName, TypesAndModes0, Det, Cond),
		EqvMap, pred(VarSet, PredName, TypesAndModes, Det, Cond)) :-
	equiv_type__replace_in_tms(TypesAndModes0, VarSet0, EqvMap, 
					no, TypesAndModes, VarSet, yes).

equiv_type__replace_in_item(
			func(VarSet0, PredName, TypesAndModes0, 
				RetTypeAndMode0, Det, Cond),
			EqvMap,
			func(VarSet, PredName, TypesAndModes, RetTypeAndMode,
				Det, Cond)) :-
	equiv_type__replace_in_tms(TypesAndModes0, VarSet0, EqvMap,
				no, TypesAndModes, VarSet1, Found),
	equiv_type__replace_in_tm(RetTypeAndMode0, VarSet1, EqvMap,
				Found, RetTypeAndMode, VarSet, yes).

:- pred equiv_type__replace_in_type_defn(type_defn, tvarset, eqv_map,
					type_defn, tvarset).
:- mode equiv_type__replace_in_type_defn(in, in, in, out, out) is semidet.

equiv_type__replace_in_type_defn(eqv_type(TName, TArgs, TBody0), VarSet0,
			EqvMap, eqv_type(TName, TArgs, TBody), VarSet) :-
	equiv_type__replace_in_type(TBody0, VarSet0, EqvMap, no,
				TBody, VarSet, yes).

equiv_type__replace_in_type_defn(uu_type(TName, TArgs, TBody0), VarSet0,
			EqvMap, uu_type(TName, TArgs, TBody), VarSet) :-
	equiv_type__replace_in_uu(TBody0, VarSet0, EqvMap, no,
				TBody, VarSet, yes).

equiv_type__replace_in_type_defn(du_type(TName, TArgs, TBody0), VarSet0,
			EqvMap, du_type(TName, TArgs, TBody), VarSet) :-
	equiv_type__replace_in_du(TBody0, VarSet0, EqvMap, no,
				TBody, VarSet, yes).

:- pred equiv_type__replace_in_uu(list(type), tvarset, eqv_map,
					bool, list(type), tvarset, bool).
:- mode equiv_type__replace_in_uu(in, in, in, in, out, out, out) is det.

equiv_type__replace_in_uu(Ts0, VarSet0, EqvMap, Found0,
				Ts, VarSet, Found) :-
	equiv_type__replace_in_type_list(Ts0, VarSet0, EqvMap, Found0,
					Ts, VarSet, Found).

:- pred equiv_type__replace_in_du(list(constructor), tvarset, eqv_map,
				bool, list(constructor), tvarset, bool).
:- mode equiv_type__replace_in_du(in, in, in, in, out, out, out) is det.

equiv_type__replace_in_du([], VarSet, _EqvMap, Found, [], VarSet, Found).
equiv_type__replace_in_du([T0|Ts0], VarSet0, EqvMap, Found0,
				[T|Ts], VarSet, Found) :-
	equiv_type__replace_in_ctor(T0, VarSet0, EqvMap, Found0,
					T, VarSet1, Found1),
	equiv_type__replace_in_du(Ts0, VarSet1, EqvMap, Found1,
					Ts, VarSet, Found).

:- pred equiv_type__replace_in_ctor(constructor, tvarset, eqv_map,
				bool, constructor, tvarset, bool).
:- mode equiv_type__replace_in_ctor(in, in, in, in, out, out, out) is det.

equiv_type__replace_in_ctor(TName - Targs0, VarSet0, EqvMap, Found0,
		TName - Targs, VarSet, Found) :-
	equiv_type__replace_in_type_list(Targs0, VarSet0, EqvMap, Found0,
		Targs, VarSet, Found).

:- pred equiv_type__replace_in_type_list(list(type), tvarset, eqv_map,
					bool, list(type), tvarset, bool).
:- mode equiv_type__replace_in_type_list(in, in, in, in, out, out, out) is det.

equiv_type__replace_in_type_list(Ts0, VarSet0, EqvMap, Found0,
				Ts, VarSet, Found) :-
	equiv_type__replace_in_type_list_2(Ts0, VarSet0, EqvMap, Found0, [],
					Ts, VarSet, Found).

:- pred equiv_type__replace_in_type_list_2(list(type), tvarset, eqv_map,
			bool, list(type_id), list(type), tvarset, bool).
:- mode equiv_type__replace_in_type_list_2(in, in, in, in, in, out, out, out)
	is det.

equiv_type__replace_in_type_list_2([], VarSet, _EqvMap, Found, _Seen,
					[], VarSet, Found).
equiv_type__replace_in_type_list_2([T0|Ts0], VarSet0, EqvMap, Found0, Seen,
				[T|Ts], VarSet, Found) :-
	equiv_type__replace_in_type_2(T0, VarSet0, EqvMap, Found0, Seen,
					T, VarSet1, Found1),
	equiv_type__replace_in_type_list_2(Ts0, VarSet1, EqvMap, Found1, Seen,
					Ts, VarSet, Found).

:- pred equiv_type__replace_in_type(type, tvarset, eqv_map, bool,
					type, tvarset, bool).
:- mode equiv_type__replace_in_type(in, in, in, in, out, out, out) is det.

equiv_type__replace_in_type(Type0, VarSet0, EqvMap, Found0,
		Type, VarSet, Found) :-
	equiv_type__replace_in_type_2(Type0, VarSet0, EqvMap, Found0,
			[], Type, VarSet, Found).

:- pred equiv_type__replace_in_type_2(type, tvarset, eqv_map, bool,
					list(eqv_type_id),
					type, tvarset, bool).
:- mode equiv_type__replace_in_type_2(in, in, in, in, in, out, out, out)
		is det.

equiv_type__replace_in_type_2(term__variable(V), VarSet, _EqvMap, Found,
		_Seen, term__variable(V), VarSet, Found).
equiv_type__replace_in_type_2(Type0, VarSet0, EqvMap, Found0,
		TypeIdsAlreadyExpanded, Type, VarSet, Found) :- 

	Type0 = term__functor(_, _, Context),
	(
		type_to_type_id(Type0, EqvTypeId, TArgs0)
	->
		equiv_type__replace_in_type_list_2(TArgs0, VarSet0, EqvMap,
				Found0, TypeIdsAlreadyExpanded,
				TArgs1, VarSet1, Found1),

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
				EqvMap, yes, 
				[EqvTypeId | TypeIdsAlreadyExpanded],
				Type, VarSet, Found)
		;
			VarSet = VarSet1,
			Found = Found1,
			EqvTypeId = SymName - _,
			construct_qualified_term(SymName, TArgs1,
							Context, Type)
			
		)
	;
		VarSet = VarSet0,
		Found = Found0,
		Type = Type0
	).

:- pred equiv_type__replace_in_tms(list(type_and_mode), tvarset, eqv_map,
			bool, list(type_and_mode), tvarset, bool).
:- mode equiv_type__replace_in_tms(in, in, in, in, out, out, out) is det.

equiv_type__replace_in_tms([], VarSet, _EqvMap, Found, [], VarSet, Found).
equiv_type__replace_in_tms([TM0|TMs0], VarSet0, EqvMap, Found0,
				[TM|TMs], VarSet, Found) :-
	equiv_type__replace_in_tm(TM0, VarSet0, EqvMap, Found0,
				TM, VarSet1, Found1),
	equiv_type__replace_in_tms(TMs0, VarSet1, EqvMap, Found1,
					TMs, VarSet, Found).

:- pred equiv_type__replace_in_tm(type_and_mode, tvarset, eqv_map,
				bool, type_and_mode, tvarset, bool).
:- mode equiv_type__replace_in_tm(in, in, in, in, out, out, out) is det.

equiv_type__replace_in_tm(type_only(Type0), VarSet0, EqvMap, Found0,
				type_only(Type), VarSet, Found) :-
	equiv_type__replace_in_type(Type0, VarSet0, EqvMap, Found0, Type,
		VarSet, Found).

equiv_type__replace_in_tm(type_and_mode(Type0, Mode), VarSet0, EqvMap,
			Found0, type_and_mode(Type, Mode), VarSet, Found) :-
	equiv_type__replace_in_type(Type0, VarSet0, EqvMap, Found0,
			Type, VarSet, Found).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module prog_util.
:- interface.
:- import_module string, varset, prog_data, list.

%-----------------------------------------------------------------------------%

	% The following predicate prog_util__expand_eqv_types traverses
	% through the list of items.  Each time it finds an eqv_type
	% definition, it replaces all occurrences of the type (both
	% before and after it in the list of items) with type that it
	% is equivalent to.  This has the effect of eliminating all the
	% equivalence types from the source code.  Circular equivalence
	% types in the input will cause references to undefined types
	% in the output.

:- pred prog_util__expand_eqv_types(list(item_and_context),
					list(item_and_context)).
:- mode prog_util__expand_eqv_types(in, out) is det.

%-----------------------------------------------------------------------------%

	% Convert a sym_name into a string.

:- pred unqualify_name(sym_name, string).
:- mode unqualify_name(in, out) is det.

:- pred sym_name_get_module_name(sym_name, module_name, module_name).
:- mode sym_name_get_module_name(in, in, out) is det.

        % Given a possible module qualified sym_name and a list of
	% argument types and a context, construct a term. This is
	% used to construct types. 

:- pred construct_qualified_term(sym_name, list(term), term).
:- mode construct_qualified_term(in, in, out) is det.

:- pred construct_qualified_term(sym_name, list(term), term__context, term).
:- mode construct_qualified_term(in, in, in, out) is det.

%-----------------------------------------------------------------------------%

	% A pred declaration may contains just types, as in
	%	:- pred list__append(list(T), list(T), list(T)).
	% or it may contain both types and modes, as in
	%	:- pred list__append(list(T)::in, list(T)::in,
	%			list(T)::output).
	%
	% This predicate takes the argument list of a pred declaration,
	% splits it into two separate lists for the types and (if present)
	% the modes.

:- type maybe_modes == maybe(list(mode)).

:- pred split_types_and_modes(list(type_and_mode), list(type), maybe_modes).
:- mode split_types_and_modes(in, out, out) is det.

:- pred split_type_and_mode(type_and_mode, type, maybe(mode)).
:- mode split_type_and_mode(in, out, out) is det.

%-----------------------------------------------------------------------------%

	% Perform a substitution on a goal.

:- pred prog_util__rename_in_goal(goal, var, var, goal).
:- mode prog_util__rename_in_goal(in, in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bool, std_util, map, term, type_util.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% First we build up a mapping which records the equivalence type
	% definitions.  Then we go through the item list and replace
	% them.

prog_util__expand_eqv_types(Items0, Items) :-
	map__init(EqvMap0),
	prog_util__build_eqv_map(Items0, EqvMap0, EqvMap),
	prog_util__replace_eqv_type_item_list(Items0, EqvMap, [], Items1),
	list__reverse(Items1, Items).

:- type eqv_type_id == pair(sym_name, arity).
:- type eqv_type_body ---> eqv_type_body(tvarset, list(type_param), type).
:- type eqv_map == map(eqv_type_id, eqv_type_body).

:- pred prog_util__build_eqv_map(list(item_and_context), eqv_map, eqv_map).
:- mode prog_util__build_eqv_map(in, in, out) is det.

prog_util__build_eqv_map([], EqvMap, EqvMap).
prog_util__build_eqv_map([Item - _Context | Items], EqvMap0, EqvMap) :-
	( Item = type_defn(VarSet, eqv_type(Name, Args, Body), _Cond) ->
		list__length(Args, Arity),
		map__set(EqvMap0, Name - Arity,
			eqv_type_body(VarSet, Args, Body), EqvMap1)
	;
		EqvMap1 = EqvMap0
	),
	prog_util__build_eqv_map(Items, EqvMap1, EqvMap).

	% The following predicate prog_util__replace_eqv_type_item_list
	% performs substititution of a single type on a list
	% of items.

:- pred prog_util__replace_eqv_type_item_list(list(item_and_context), eqv_map,
			list(item_and_context), list(item_and_context)).
:- mode prog_util__replace_eqv_type_item_list(in, in, in, out) is det.

prog_util__replace_eqv_type_item_list([], _, Items, Items).
prog_util__replace_eqv_type_item_list([Item0 - Context | Items0], EqvMap,
				Items1, Items) :-
	( prog_util__replace_eqv_type(Item0, EqvMap, Item) ->
		Items2 = [Item - Context | Items1]
	;
		Items2 = [Item0 - Context | Items1]
	),
	prog_util__replace_eqv_type_item_list(Items0, EqvMap, Items2, Items).

:- pred prog_util__replace_eqv_type(item, eqv_map, item).
:- mode prog_util__replace_eqv_type(in, in, out) is semidet.

prog_util__replace_eqv_type(type_defn(VarSet0, TypeDefn0, Cond),
		EqvMap, type_defn(VarSet, TypeDefn, Cond)) :-
	prog_util__replace_eqv_type_defn(TypeDefn0, VarSet0, EqvMap,
				TypeDefn, VarSet).

prog_util__replace_eqv_type(pred(VarSet0, PredName, TypesAndModes0, Det, Cond),
		EqvMap, pred(VarSet, PredName, TypesAndModes, Det, Cond)) :-
	prog_util__replace_eqv_type_tms(TypesAndModes0, VarSet0, EqvMap, 
					no, TypesAndModes, VarSet, yes).

prog_util__replace_eqv_type(
			func(VarSet0, PredName, TypesAndModes0, 
				RetTypeAndMode0, Det, Cond),
			EqvMap,
			func(VarSet, PredName, TypesAndModes, RetTypeAndMode,
				Det, Cond)) :-
	prog_util__replace_eqv_type_tms(TypesAndModes0, VarSet0, EqvMap,
				no, TypesAndModes, VarSet1, Found),
	prog_util__replace_eqv_type_tm(RetTypeAndMode0, VarSet1, EqvMap,
				Found, RetTypeAndMode, VarSet, yes).

:- pred prog_util__replace_eqv_type_defn(type_defn, tvarset, eqv_map,
					type_defn, tvarset).
:- mode prog_util__replace_eqv_type_defn(in, in, in, out, out) is semidet.

prog_util__replace_eqv_type_defn(eqv_type(TName, TArgs, TBody0), VarSet0,
			EqvMap, eqv_type(TName, TArgs, TBody), VarSet) :-
	prog_util__replace_eqv_type_type(TBody0, VarSet0, EqvMap, no,
				TBody, VarSet, yes).

prog_util__replace_eqv_type_defn(uu_type(TName, TArgs, TBody0), VarSet0,
			EqvMap, uu_type(TName, TArgs, TBody), VarSet) :-
	prog_util__replace_eqv_type_uu(TBody0, VarSet0, EqvMap, no,
				TBody, VarSet, yes).

prog_util__replace_eqv_type_defn(du_type(TName, TArgs, TBody0), VarSet0,
			EqvMap, du_type(TName, TArgs, TBody), VarSet) :-
	prog_util__replace_eqv_type_du(TBody0, VarSet0, EqvMap, no,
				TBody, VarSet, yes).

:- pred prog_util__replace_eqv_type_uu(list(type), tvarset, eqv_map,
					bool, list(type), tvarset, bool).
:- mode prog_util__replace_eqv_type_uu(in, in, in, in, out, out, out) is det.

prog_util__replace_eqv_type_uu(Ts0, VarSet0, EqvMap, Found0,
				Ts, VarSet, Found) :-
	prog_util__replace_eqv_type_list(Ts0, VarSet0, EqvMap, Found0,
					Ts, VarSet, Found).

:- pred prog_util__replace_eqv_type_du(list(constructor), tvarset, eqv_map,
				bool, list(constructor), tvarset, bool).
:- mode prog_util__replace_eqv_type_du(in, in, in, in, out, out, out) is det.

prog_util__replace_eqv_type_du([], VarSet, _EqvMap, Found, [], VarSet, Found).
prog_util__replace_eqv_type_du([T0|Ts0], VarSet0, EqvMap, Found0,
				[T|Ts], VarSet, Found) :-
	prog_util__replace_eqv_type_ctor(T0, VarSet0, EqvMap, Found0,
					T, VarSet1, Found1),
	prog_util__replace_eqv_type_du(Ts0, VarSet1, EqvMap, Found1,
					Ts, VarSet, Found).

:- pred prog_util__replace_eqv_type_ctor(constructor, tvarset, eqv_map,
				bool, constructor, tvarset, bool).
:- mode prog_util__replace_eqv_type_ctor(in, in, in, in, out, out, out) is det.

prog_util__replace_eqv_type_ctor(TName - Targs0, VarSet0, EqvMap, Found0,
		TName - Targs, VarSet, Found) :-
	prog_util__replace_eqv_type_list(Targs0, VarSet0, EqvMap, Found0,
		Targs, VarSet, Found).

:- pred prog_util__replace_eqv_type_list(list(type), tvarset, eqv_map,
					bool, list(type), tvarset, bool).
:- mode prog_util__replace_eqv_type_list(in, in, in, in, out, out, out) is det.

prog_util__replace_eqv_type_list(Ts0, VarSet0, EqvMap, Found0,
				Ts, VarSet, Found) :-
	prog_util__replace_eqv_type_list_2(Ts0, VarSet0, EqvMap, Found0, [],
					Ts, VarSet, Found).

:- pred prog_util__replace_eqv_type_list_2(list(type), tvarset, eqv_map,
			bool, list(type_id), list(type), tvarset, bool).
:- mode prog_util__replace_eqv_type_list_2(in, in, in, in, in, out, out, out)
	is det.

prog_util__replace_eqv_type_list_2([], VarSet, _EqvMap, Found, _Seen,
					[], VarSet, Found).
prog_util__replace_eqv_type_list_2([T0|Ts0], VarSet0, EqvMap, Found0, Seen,
				[T|Ts], VarSet, Found) :-
	prog_util__replace_eqv_type_type_2(T0, VarSet0, EqvMap, Found0, Seen,
					T, VarSet1, Found1),
	prog_util__replace_eqv_type_list_2(Ts0, VarSet1, EqvMap, Found1, Seen,
					Ts, VarSet, Found).

:- pred prog_util__replace_eqv_type_type(type, tvarset, eqv_map, bool,
					type, tvarset, bool).
:- mode prog_util__replace_eqv_type_type(in, in, in, in, out, out, out) is det.

prog_util__replace_eqv_type_type(Type0, VarSet0, EqvMap, Found0,
		Type, VarSet, Found) :-
	prog_util__replace_eqv_type_type_2(Type0, VarSet0, EqvMap, Found0,
			[], Type, VarSet, Found).

:- pred prog_util__replace_eqv_type_type_2(type, tvarset, eqv_map, bool,
					list(eqv_type_id),
					type, tvarset, bool).
:- mode prog_util__replace_eqv_type_type_2(in, in, in, in, in, out, out, out)
		is det.

prog_util__replace_eqv_type_type_2(term__variable(V), VarSet, _EqvMap, Found,
		_Seen, term__variable(V), VarSet, Found).
prog_util__replace_eqv_type_type_2(Type0, VarSet0, EqvMap, Found0,
		TypeIdsAlreadyExpanded, Type, VarSet, Found) :- 

	Type0 = term__functor(_, _, Context),
	(
		type_to_type_id(Type0, EqvTypeId, TArgs0)
	->
		TypeIdsAlreadyExpanded1 = [EqvTypeId | TypeIdsAlreadyExpanded],

		prog_util__replace_eqv_type_list_2(TArgs0, VarSet0, EqvMap,
				Found0, TypeIdsAlreadyExpanded1,
				TArgs1, VarSet1, Found1),

		(	
			\+ list__member(EqvTypeId, TypeIdsAlreadyExpanded),
			map__search(EqvMap, EqvTypeId,
				eqv_type_body(EqvVarSet, Args0, Body0)),
			varset__merge(VarSet1, EqvVarSet, [Body0 | Args0],
					VarSet2, [Body | Args])
		->
			term__term_list_to_var_list(Args, Args2),
			term__substitute_corresponding(Args2, TArgs1,
							Body, Type1),
			prog_util__replace_eqv_type_type_2(Type1, VarSet2,
				EqvMap, yes, TypeIdsAlreadyExpanded1,
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

:- pred prog_util__replace_eqv_type_tms(list(type_and_mode), tvarset, eqv_map,
			bool, list(type_and_mode), tvarset, bool).
:- mode prog_util__replace_eqv_type_tms(in, in, in, in, out, out, out) is det.

prog_util__replace_eqv_type_tms([], VarSet, _EqvMap, Found, [], VarSet, Found).
prog_util__replace_eqv_type_tms([TM0|TMs0], VarSet0, EqvMap, Found0,
				[TM|TMs], VarSet, Found) :-
	prog_util__replace_eqv_type_tm(TM0, VarSet0, EqvMap, Found0,
				TM, VarSet1, Found1),
	prog_util__replace_eqv_type_tms(TMs0, VarSet1, EqvMap, Found1,
					TMs, VarSet, Found).

:- pred prog_util__replace_eqv_type_tm(type_and_mode, tvarset, eqv_map,
				bool, type_and_mode, tvarset, bool).
:- mode prog_util__replace_eqv_type_tm(in, in, in, in, out, out, out) is det.

prog_util__replace_eqv_type_tm(type_only(Type0), VarSet0, EqvMap, Found0,
				type_only(Type), VarSet, Found) :-
	prog_util__replace_eqv_type_type(Type0, VarSet0, EqvMap, Found0, Type,
		VarSet, Found).

prog_util__replace_eqv_type_tm(type_and_mode(Type0, Mode), VarSet0, EqvMap,
			Found0, type_and_mode(Type, Mode), VarSet, Found) :-
	prog_util__replace_eqv_type_type(Type0, VarSet0, EqvMap, Found0,
			Type, VarSet, Found).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

unqualify_name(unqualified(PredName), PredName).
unqualify_name(qualified(_ModuleName, PredName), PredName).

sym_name_get_module_name(unqualified(_), ModuleName, ModuleName).
sym_name_get_module_name(qualified(ModuleName, _PredName), _, ModuleName).

construct_qualified_term(qualified(Module, Name), Args, Context, Term) :-
	ModuleTerm = term__functor(term__atom(Module), [], Context),
	UnqualifiedTerm = term__functor(term__atom(Name), Args, Context),
	Term = term__functor(term__atom(":"), [ModuleTerm, UnqualifiedTerm],
							Context).
construct_qualified_term(unqualified(Name), Args, Context, Term) :-
	Term = term__functor(term__atom(Name), Args, Context).


construct_qualified_term(SymName, Args, Term) :-
	term__context_init(Context),
	construct_qualified_term(SymName, Args, Context, Term).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

split_types_and_modes(TypesAndModes, Types, MaybeModes) :-
	split_types_and_modes_2(TypesAndModes, yes, Types, Modes, Result),
	(
		Result = yes
	->
		MaybeModes = yes(Modes)
	;
		MaybeModes = no
	).

:- pred split_types_and_modes_2(list(type_and_mode), bool,
				list(type), list(mode), bool).
:- mode split_types_and_modes_2(in, in, out, out, out) is det.

	% T = type, M = mode, TM = combined type and mode
split_types_and_modes_2([], Result, [], [], Result).
split_types_and_modes_2([TM|TMs], Result0, [T|Ts], [M|Ms], Result) :-
	split_type_and_mode(TM, Result0, T, M, Result1),
	split_types_and_modes_2(TMs, Result1, Ts, Ms, Result).

	% if a pred declaration specifies modes for some but
	% not all of the arguments, then the modes are ignored
	% - should this be an error instead?

:- pred split_type_and_mode(type_and_mode, bool, type, mode, bool).
:- mode split_type_and_mode(in, in, out, out, out) is det.

split_type_and_mode(type_only(T), _, T, (free -> free), no).
split_type_and_mode(type_and_mode(T,M), R, T, M, R).

split_type_and_mode(type_only(T), T, no).
split_type_and_mode(type_and_mode(T,M), T, yes(M)).

%-----------------------------------------------------------------------------%

prog_util__rename_in_goal(Goal0 - Context, OldVar, NewVar, Goal - Context) :-
	prog_util__rename_in_goal_expr(Goal0, OldVar, NewVar, Goal).

:- pred prog_util__rename_in_goal_expr(goal_expr, var, var, goal_expr).
:- mode prog_util__rename_in_goal_expr(in, in, in, out) is det.

prog_util__rename_in_goal_expr((GoalA0, GoalB0), OldVar, NewVar,
		(GoalA, GoalB)) :-
	prog_util__rename_in_goal(GoalA0, OldVar, NewVar, GoalA),
	prog_util__rename_in_goal(GoalB0, OldVar, NewVar, GoalB).
prog_util__rename_in_goal_expr(true, _Var, _NewVar, true).
prog_util__rename_in_goal_expr((GoalA0; GoalB0), OldVar, NewVar,
		(GoalA; GoalB)) :-
	prog_util__rename_in_goal(GoalA0, OldVar, NewVar, GoalA),
	prog_util__rename_in_goal(GoalB0, OldVar, NewVar, GoalB).
prog_util__rename_in_goal_expr(fail, _Var, _NewVar, fail).
prog_util__rename_in_goal_expr(not(Goal0), OldVar, NewVar, not(Goal)) :-
	prog_util__rename_in_goal(Goal0, OldVar, NewVar, Goal).
prog_util__rename_in_goal_expr(some(Vars0, Goal0), OldVar, NewVar,
		some(Vars, Goal)) :-
	prog_util__rename_in_vars(Vars0, OldVar, NewVar, Vars),
	prog_util__rename_in_goal(Goal0, OldVar, NewVar, Goal).
prog_util__rename_in_goal_expr(all(Vars0, Goal0), OldVar, NewVar,
		all(Vars, Goal)) :-
	prog_util__rename_in_vars(Vars0, OldVar, NewVar, Vars),
	prog_util__rename_in_goal(Goal0, OldVar, NewVar, Goal).
prog_util__rename_in_goal_expr(implies(GoalA0, GoalB0), OldVar, NewVar,
		implies(GoalA, GoalB)) :-
	prog_util__rename_in_goal(GoalA0, OldVar, NewVar, GoalA),
	prog_util__rename_in_goal(GoalB0, OldVar, NewVar, GoalB).
prog_util__rename_in_goal_expr(equivalent(GoalA0, GoalB0), OldVar, NewVar,
		equivalent(GoalA, GoalB)) :-
	prog_util__rename_in_goal(GoalA0, OldVar, NewVar, GoalA),
	prog_util__rename_in_goal(GoalB0, OldVar, NewVar, GoalB).
prog_util__rename_in_goal_expr(if_then(Vars0, Cond0, Then0), OldVar, NewVar,
		if_then(Vars, Cond, Then)) :-
	prog_util__rename_in_vars(Vars0, OldVar, NewVar, Vars),
	prog_util__rename_in_goal(Cond0, OldVar, NewVar, Cond),
	prog_util__rename_in_goal(Then0, OldVar, NewVar, Then).
prog_util__rename_in_goal_expr(if_then_else(Vars0, Cond0, Then0, Else0),
		OldVar, NewVar, if_then_else(Vars, Cond, Then, Else)) :-
	prog_util__rename_in_vars(Vars0, OldVar, NewVar, Vars),
	prog_util__rename_in_goal(Cond0, OldVar, NewVar, Cond),
	prog_util__rename_in_goal(Then0, OldVar, NewVar, Then),
	prog_util__rename_in_goal(Else0, OldVar, NewVar, Else).
prog_util__rename_in_goal_expr(call(SymName, Terms0), OldVar, NewVar,
		call(SymName, Terms)) :-
	term__substitute_list(Terms0, OldVar, term__variable(NewVar), Terms).
prog_util__rename_in_goal_expr(unify(TermA0, TermB0), OldVar, NewVar,
		unify(TermA, TermB)) :-
	term__substitute(TermA0, OldVar, term__variable(NewVar), TermA),
	term__substitute(TermB0, OldVar, term__variable(NewVar), TermB).

:- pred prog_util__rename_in_vars(list(var), var, var, list(var)).
:- mode prog_util__rename_in_vars(in, in, in, out) is det.

prog_util__rename_in_vars([], _, _, []).
prog_util__rename_in_vars([Var0 | Vars0], OldVar, NewVar, [Var | Vars]) :-
	( Var0 = OldVar ->
		Var = NewVar
	;
		Var = Var0
	),
	prog_util__rename_in_vars(Vars0, OldVar, NewVar, Vars).

%-----------------------------------------------------------------------------%

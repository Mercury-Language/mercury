%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: conway.
%
% This module provides some functionality for renaming variables in goals.
% The predicates rename_var* take a structure and a mapping from var -> var
% and apply that translation. If a var in the input structure does not
% occur as a key in the mapping, then the variable is left unsubstituted.

% goal_util__create_variables takes a list of variables, a varset an
% initial translation mapping and an initial mapping from variable to
% type, and creates new instances of each of the source variables in the
% translation mapping, adding the new variable to the type mapping and
% updating the varset. The type for each new variable is found by looking
% in the type map given in the 5th argument - the last input.
% (This interface will not easily admit uniqueness in the type map for this
% reason - such is the sacrifice for generality.)

:- module goal_util.

%-----------------------------------------------------------------------------%

:- interface.

:- import_module hlds_goal, hlds_pred.
:- import_module bool, list, map.

	% goal_util__rename_vars_in_goals(GoalList, MustRename, Substitution,
	%	NewGoalList).
:- pred goal_util__rename_vars_in_goals(list(hlds_goal), bool, map(var, var),
	list(hlds_goal)).
:- mode goal_util__rename_vars_in_goals(in, in, in, out) is det.

:- pred goal_util__rename_vars_in_goal(hlds_goal, map(var, var), hlds_goal).
:- mode goal_util__rename_vars_in_goal(in, in, out) is det.

:- pred goal_util__must_rename_vars_in_goal(hlds_goal,
					map(var, var), hlds_goal).
:- mode goal_util__must_rename_vars_in_goal(in, in, out) is det.

:- pred goal_util__rename_var_list(list(var), bool, map(var, var), list(var)).
:- mode goal_util__rename_var_list(in, in, in, out) is det.

	% goal_util__create_variables(OldVariables, OldVarset, InitialVarTypes,
	%	InitialSubstitution, OldVarTypes, OldVarNames,  NewVarset,
	%	NewVarTypes, NewSubstitution)
:- pred goal_util__create_variables(list(var),
			varset, map(var, type), map(var, var),
			map(var, type),
			varset, varset, map(var, type), map(var, var)).
:- mode goal_util__create_variables(in, in, in, in, in, in, out, out, out)
		is det.

	% Return all the variables in the goal.
	% Unlike quantification:goal_vars, this predicate returns
	% even the explicitly quantified variables.
:- pred goal_util__goal_vars(hlds_goal, set(var)).
:- mode goal_util__goal_vars(in, out) is det.

	% See whether the goal is a branched structure.
:- pred goal_util__goal_is_branched(hlds_goal_expr).
:- mode goal_util__goal_is_branched(in) is semidet.

	% Return an indication of the size of the goal.
:- pred goal_size(hlds_goal, int).
:- mode goal_size(in, out) is det.

	% Test whether the goal calls the given procedure.
:- pred goal_calls(hlds_goal, pred_proc_id).
:- mode goal_calls(in, in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_data, mode_util, code_aux, prog_data, instmap.
:- import_module int, set, std_util, assoc_list, term, require, varset.

%-----------------------------------------------------------------------------%

goal_util__create_variables([], Varset, VarTypes, Subn, _OldVarTypes,
		_OldVarNames, Varset, VarTypes, Subn).
goal_util__create_variables([V | Vs], Varset0, VarTypes0, Subn0, OldVarTypes,
					OldVarNames, Varset, VarTypes, Subn) :-
	(
		map__contains(Subn0, V)
	->
		Varset2 = Varset0,
		Subn1 = Subn0,
		VarTypes1 = VarTypes0
	;
		varset__new_var(Varset0, NV, Varset1),
		(
			varset__search_name(OldVarNames, V, Name)
		->
			varset__name_var(Varset1, NV, Name, Varset2)
		;
			Varset2 = Varset1
		),
		map__det_insert(Subn0, V, NV, Subn1),
		(
			map__search(OldVarTypes, V, VT)
		->
			map__set(VarTypes0, NV, VT, VarTypes1)
		;
			VarTypes1 = VarTypes0
		)
	),
	goal_util__create_variables(Vs, Varset2, VarTypes1, Subn1, OldVarTypes,
		OldVarNames, Varset, VarTypes, Subn).

%-----------------------------------------------------------------------------%

:- pred goal_util__init_subn(assoc_list(var, var),
	map(var, var), map(var, var)).
:- mode goal_util__init_subn(in, in, out) is det.

goal_util__init_subn([], Subn, Subn).
goal_util__init_subn([A - H | Vs], Subn0, Subn) :-
	map__set(Subn0, H, A, Subn1),
	goal_util__init_subn(Vs, Subn1, Subn).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_var_pair_list(assoc_list(var, T), bool,
	map(var, var), list(pair(var, T))).
:- mode goal_util__rename_var_pair_list(in, in, in, out) is det.

goal_util__rename_var_pair_list([], _Must, _Subn, []).
goal_util__rename_var_pair_list([V - D | VDs], Must, Subn, [N - D | NDs]) :-
	goal_util__rename_var(V, Must, Subn, N),
	goal_util__rename_var_pair_list(VDs, Must, Subn, NDs).

goal_util__rename_var_list([], _Must, _Subn, []).
goal_util__rename_var_list([V | Vs], Must, Subn, [N | Ns]) :-
	goal_util__rename_var(V, Must, Subn, N),
	goal_util__rename_var_list(Vs, Must, Subn, Ns).

:- pred goal_util__rename_var(var, bool, map(var, var), var).
:- mode goal_util__rename_var(in, in, in, out) is det.

goal_util__rename_var(V, Must, Subn, N) :-
	(
		map__search(Subn, V, N0)
	->
		N = N0
	;
		(
			Must = no,
			N = V
		;
			Must = yes,
			error("goal_util__rename_var: no substitute")
		)
	).

%-----------------------------------------------------------------------------%

goal_util__rename_vars_in_goal(Goal0, Subn, Goal) :-
	goal_util__rename_vars_in_goal(Goal0, no, Subn, Goal).

goal_util__must_rename_vars_in_goal(Goal0, Subn, Goal) :-
	goal_util__rename_vars_in_goal(Goal0, yes, Subn, Goal).

%-----------------------------------------------------------------------------%

goal_util__rename_vars_in_goals([], _, _, []).
goal_util__rename_vars_in_goals([Goal0 | Goals0], Must, Subn, [Goal | Goals]) :-
	goal_util__rename_vars_in_goal(Goal0, Must, Subn, Goal),
	goal_util__rename_vars_in_goals(Goals0, Must, Subn, Goals).

:- pred goal_util__rename_vars_in_goal(hlds_goal, bool, map(var, var),
		hlds_goal).
:- mode goal_util__rename_vars_in_goal(in, in, in, out) is det.

goal_util__rename_vars_in_goal(Goal0 - GoalInfo0, Must, Subn, Goal - GoalInfo) :-
	goal_util__name_apart_2(Goal0, Must, Subn, Goal),
	goal_util__name_apart_goalinfo(GoalInfo0, Must, Subn, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred goal_util__name_apart_2(hlds_goal_expr, bool, map(var, var),
		hlds_goal_expr).
:- mode goal_util__name_apart_2(in, in, in, out) is det.

goal_util__name_apart_2(conj(Goals0), Must, Subn, conj(Goals)) :-
	goal_util__name_apart_list(Goals0, Must, Subn, Goals).

goal_util__name_apart_2(disj(Goals0, SM0), Must, Subn, disj(Goals, SM)) :-
	goal_util__name_apart_list(Goals0, Must, Subn, Goals),
	goal_util__rename_var_maps(SM0, Must, Subn, SM).

goal_util__name_apart_2(switch(Var0, Det, Cases0, SM0), Must, Subn,
		switch(Var, Det, Cases, SM)) :-
	goal_util__rename_var(Var0, Must, Subn, Var),
	goal_util__name_apart_cases(Cases0, Must, Subn, Cases),
	goal_util__rename_var_maps(SM0, Must, Subn, SM).

goal_util__name_apart_2(if_then_else(Vars0, Cond0, Then0, Else0, SM0),
		Must, Subn, if_then_else(Vars, Cond, Then, Else, SM)) :-
	goal_util__rename_var_list(Vars0, Must, Subn, Vars),
	goal_util__rename_vars_in_goal(Cond0, Must, Subn, Cond),
	goal_util__rename_vars_in_goal(Then0, Must, Subn, Then),
	goal_util__rename_vars_in_goal(Else0, Must, Subn, Else),
	goal_util__rename_var_maps(SM0, Must, Subn, SM).

goal_util__name_apart_2(not(Goal0), Must, Subn, not(Goal)) :-
	goal_util__rename_vars_in_goal(Goal0, Must, Subn, Goal).

goal_util__name_apart_2(some(Vars0, Goal0), Must, Subn, some(Vars, Goal)) :-
	goal_util__rename_var_list(Vars0, Must, Subn, Vars),
	goal_util__rename_vars_in_goal(Goal0, Must, Subn, Goal).

goal_util__name_apart_2(
		higher_order_call(PredVar0, Args0, Types, Modes, Det),
		Must, Subn,
		higher_order_call(PredVar, Args, Types, Modes, Det)) :-
	goal_util__rename_var(PredVar0, Must, Subn, PredVar),
	goal_util__rename_var_list(Args0, Must, Subn, Args).

goal_util__name_apart_2(
		call(PredId, ProcId, Args0, Builtin, Context, Sym),
		Must, Subn,
		call(PredId, ProcId, Args, Builtin, Context, Sym)) :-
	goal_util__rename_var_list(Args0, Must, Subn, Args).

goal_util__name_apart_2(unify(TermL0,TermR0,Mode,Unify0,Context), Must, Subn,
		unify(TermL,TermR,Mode,Unify,Context)) :-
	goal_util__rename_var(TermL0, Must, Subn, TermL),
	goal_util__rename_unify_rhs(TermR0, Must, Subn, TermR),
	goal_util__rename_unify(Unify0, Must, Subn, Unify).

goal_util__name_apart_2(pragma_c_code(A,B,C,D,Vars0,F,G,Extra0), Must, Subn,
		pragma_c_code(A,B,C,D,Vars,F,G,Extra)) :-
	goal_util__rename_var_list(Vars0, Must, Subn, Vars),
	(
		Extra0 = none,
		Extra = none
	;
		Extra0 = extra_pragma_info(SavedVars0, LabelNames),
		goal_util__rename_var_pair_list(SavedVars0, Must, Subn,
			SavedVars),
		Extra = extra_pragma_info(SavedVars, LabelNames)
	).

%-----------------------------------------------------------------------------%

:- pred goal_util__name_apart_list(list(hlds_goal), bool, map(var, var),
							list(hlds_goal)).
:- mode goal_util__name_apart_list(in, in, in, out) is det.

goal_util__name_apart_list([], _Must, _Subn, []).
goal_util__name_apart_list([G0 | Gs0], Must, Subn, [G | Gs]) :-
	goal_util__rename_vars_in_goal(G0, Must, Subn, G),
	goal_util__name_apart_list(Gs0, Must, Subn, Gs).

%-----------------------------------------------------------------------------%

:- pred goal_util__name_apart_cases(list(case), bool, map(var, var),
		list(case)).
:- mode goal_util__name_apart_cases(in, in, in, out) is det.

goal_util__name_apart_cases([], _Must, _Subn, []).
goal_util__name_apart_cases([case(Cons, G0) | Gs0], Must, Subn,
		[case(Cons, G) | Gs]) :-
	goal_util__rename_vars_in_goal(G0, Must, Subn, G),
	goal_util__name_apart_cases(Gs0, Must, Subn, Gs).

%-----------------------------------------------------------------------------%

	% These predicates probably belong in term.m.

:- pred goal_util__rename_args(list(term), bool, map(var, var), list(term)).
:- mode goal_util__rename_args(in, in, in, out) is det.

goal_util__rename_args([], _Must, _Subn, []).
goal_util__rename_args([T0 | Ts0], Must, Subn, [T | Ts]) :-
	goal_util__rename_term(T0, Must, Subn, T),
	goal_util__rename_args(Ts0, Must, Subn, Ts).

:- pred goal_util__rename_term(term, bool, map(var, var), term).
:- mode goal_util__rename_term(in, in, in, out) is det.

goal_util__rename_term(term__variable(V), Must, Subn, term__variable(N)) :-
	goal_util__rename_var(V, Must, Subn, N).
goal_util__rename_term(term__functor(Cons, Terms0, Cont), Must, Subn,
				term__functor(Cons, Terms, Cont)) :-
	goal_util__rename_args(Terms0, Must, Subn, Terms).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_unify_rhs(unify_rhs, bool, map(var, var), unify_rhs).
:- mode goal_util__rename_unify_rhs(in, in, in, out) is det.

goal_util__rename_unify_rhs(var(Var0), Must, Subn, var(Var)) :-
	goal_util__rename_var(Var0, Must, Subn, Var).
goal_util__rename_unify_rhs(functor(Functor, ArgVars0), Must, Subn,
			functor(Functor, ArgVars)) :-
	goal_util__rename_var_list(ArgVars0, Must, Subn, ArgVars).
goal_util__rename_unify_rhs(lambda_goal(PredOrFunc, Vars0, Modes, Det, Goal0),
		Must, Subn, lambda_goal(PredOrFunc, Vars, Modes, Det, Goal)) :-
	goal_util__rename_var_list(Vars0, Must, Subn, Vars),
	goal_util__rename_vars_in_goal(Goal0, Must, Subn, Goal).

:- pred goal_util__rename_unify(unification, bool, map(var, var), unification).
:- mode goal_util__rename_unify(in, in, in, out) is det.

goal_util__rename_unify(construct(Var0, ConsId, Vars0, Modes), Must, Subn,
			construct(Var, ConsId, Vars, Modes)) :-
	goal_util__rename_var(Var0, Must, Subn, Var),
	goal_util__rename_var_list(Vars0, Must, Subn, Vars).
goal_util__rename_unify(deconstruct(Var0, ConsId, Vars0, Modes, Cat),
		Must, Subn, deconstruct(Var, ConsId, Vars, Modes, Cat)) :-
	goal_util__rename_var(Var0, Must, Subn, Var),
	goal_util__rename_var_list(Vars0, Must, Subn, Vars).
goal_util__rename_unify(assign(L0, R0), Must, Subn, assign(L, R)) :-
	goal_util__rename_var(L0, Must, Subn, L),
	goal_util__rename_var(R0, Must, Subn, R).
goal_util__rename_unify(simple_test(L0, R0), Must, Subn, simple_test(L, R)) :-
	goal_util__rename_var(L0, Must, Subn, L),
	goal_util__rename_var(R0, Must, Subn, R).
goal_util__rename_unify(complicated_unify(Modes, Cat), _Must, _Subn,
			complicated_unify(Modes, Cat)).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_var_maps(map(var, T), bool,
				map(var, var), map(var, T)).
:- mode goal_util__rename_var_maps(in, in, in, out) is det.

goal_util__rename_var_maps(Map0, Must, Subn, Map) :-
	map__to_assoc_list(Map0, AssocList0),
	goal_util__rename_var_maps_2(AssocList0, Must, Subn, AssocList),
	map__from_assoc_list(AssocList, Map).

:- pred goal_util__rename_var_maps_2(assoc_list(var, T),
				bool, map(var, var), assoc_list(var, T)).
:- mode goal_util__rename_var_maps_2(in, in, in, out) is det.

goal_util__rename_var_maps_2([], _Must, _Subn, []).
goal_util__rename_var_maps_2([V - L | Vs], Must, Subn, [N - L | Ns]) :-
	goal_util__rename_var(V, Must, Subn, N),
	goal_util__rename_var_maps_2(Vs, Must, Subn, Ns).

%-----------------------------------------------------------------------------%

:- pred goal_util__name_apart_goalinfo(hlds_goal_info,
					bool, map(var, var), hlds_goal_info).
:- mode goal_util__name_apart_goalinfo(in, in, in, out) is det.

goal_util__name_apart_goalinfo(GoalInfo0, Must, Subn, GoalInfo) :-
	goal_info_get_pre_births(GoalInfo0, PreBirths0),
	goal_util__name_apart_set(PreBirths0, Must, Subn, PreBirths),
	goal_info_set_pre_births(GoalInfo0, PreBirths, GoalInfo1),

	goal_info_get_pre_deaths(GoalInfo1, PreDeaths0),
	goal_util__name_apart_set(PreDeaths0, Must, Subn, PreDeaths),
	goal_info_set_pre_deaths(GoalInfo1, PreDeaths, GoalInfo2),

	goal_info_get_post_births(GoalInfo2, PostBirths0),
	goal_util__name_apart_set(PostBirths0, Must, Subn, PostBirths),
	goal_info_set_post_births(GoalInfo2, PostBirths, GoalInfo3),

	goal_info_get_post_deaths(GoalInfo3, PostDeaths0),
	goal_util__name_apart_set(PostDeaths0, Must, Subn, PostDeaths),
	goal_info_set_post_deaths(GoalInfo3, PostDeaths, GoalInfo4),

	goal_info_get_nonlocals(GoalInfo4, NonLocals0),
	goal_util__name_apart_set(NonLocals0, Must, Subn, NonLocals),
	goal_info_set_nonlocals(GoalInfo4, NonLocals, GoalInfo5),

	goal_info_get_instmap_delta(GoalInfo5, InstMap0),
	instmap_delta_apply_sub(InstMap0, Must, Subn, InstMap),
	goal_info_set_instmap_delta(GoalInfo5, InstMap, GoalInfo6),

	goal_info_get_follow_vars(GoalInfo6, MaybeFollowVars0),
	(
		MaybeFollowVars0 = no,
		MaybeFollowVars = no
	;
		MaybeFollowVars0 = yes(FollowVars0),
		goal_util__rename_var_maps(FollowVars0, Must, Subn, FollowVars),
		MaybeFollowVars = yes(FollowVars)
	),
	goal_info_set_follow_vars(GoalInfo6, MaybeFollowVars, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred goal_util__name_apart_set(set(var), bool, map(var, var), set(var)).
:- mode goal_util__name_apart_set(in, in, in, out) is det.

goal_util__name_apart_set(Vars0, Must, Subn, Vars) :-
	set__to_sorted_list(Vars0, VarsList0),
	goal_util__rename_var_list(VarsList0, Must, Subn, VarsList),
	set__list_to_set(VarsList, Vars).

%-----------------------------------------------------------------------------%

goal_util__goal_vars(Goal - _GoalInfo, Set) :-
	set__init(Set0),
	goal_util__goal_vars_2(Goal, Set0, Set).

:- pred goal_util__goal_vars_2(hlds_goal_expr, set(var), set(var)).
:- mode goal_util__goal_vars_2(in, in, out) is det.

goal_util__goal_vars_2(unify(Var, RHS, _, _, _), Set0, Set) :-
	set__insert(Set0, Var, Set1),
	goal_util__rhs_goal_vars(RHS, Set1, Set).

goal_util__goal_vars_2(higher_order_call(PredVar, ArgVars, _, _, _),
		Set0, Set) :-
	set__insert_list(Set0, [PredVar | ArgVars], Set).

goal_util__goal_vars_2(call(_, _, ArgVars, _, _, _), Set0, Set) :-
	set__insert_list(Set0, ArgVars, Set).

goal_util__goal_vars_2(conj(Goals), Set0, Set) :-
	goal_util__goals_goal_vars(Goals, Set0, Set).

goal_util__goal_vars_2(disj(Goals, _), Set0, Set) :-
	goal_util__goals_goal_vars(Goals, Set0, Set).

goal_util__goal_vars_2(switch(Var, _Det, Cases, _), Set0, Set) :-
	set__insert(Set0, Var, Set1),
	goal_util__cases_goal_vars(Cases, Set1, Set).

goal_util__goal_vars_2(some(Vars, Goal - _), Set0, Set) :-
	set__insert_list(Set0, Vars, Set1),
	goal_util__goal_vars_2(Goal, Set1, Set).

goal_util__goal_vars_2(not(Goal - _GoalInfo), Set0, Set) :-
	goal_util__goal_vars_2(Goal, Set0, Set).

goal_util__goal_vars_2(if_then_else(Vars, A - _, B - _, C - _, _), Set0, Set) :-
	set__insert_list(Set0, Vars, Set1),
	goal_util__goal_vars_2(A, Set1, Set2),
	goal_util__goal_vars_2(B, Set2, Set3),
	goal_util__goal_vars_2(C, Set3, Set).

goal_util__goal_vars_2(pragma_c_code(_, _, _, _, ArgVars, _, _, Extra),
		Set0, Set) :-
	set__insert_list(Set0, ArgVars, Set1),
	(
		Extra = none,
		Set = Set1
	;
		Extra = extra_pragma_info(SavedVarNames, _),
		assoc_list__keys(SavedVarNames, SavedVars),
		set__insert_list(Set1, SavedVars, Set)
	).

:- pred goal_util__goals_goal_vars(list(hlds_goal), set(var), set(var)).
:- mode goal_util__goals_goal_vars(in, in, out) is det.

goal_util__goals_goal_vars([], Set, Set).
goal_util__goals_goal_vars([Goal - _ | Goals], Set0, Set) :-
	goal_util__goal_vars_2(Goal, Set0, Set1),
	goal_util__goals_goal_vars(Goals, Set1, Set).

:- pred goal_util__cases_goal_vars(list(case), set(var), set(var)).
:- mode goal_util__cases_goal_vars(in, in, out) is det.

goal_util__cases_goal_vars([], Set, Set).
goal_util__cases_goal_vars([case(_, Goal - _) | Cases], Set0, Set) :-
	goal_util__goal_vars_2(Goal, Set0, Set1),
	goal_util__cases_goal_vars(Cases, Set1, Set).

:- pred goal_util__rhs_goal_vars(unify_rhs, set(var), set(var)).
:- mode goal_util__rhs_goal_vars(in, in, out) is det.

goal_util__rhs_goal_vars(var(X), Set0, Set) :-
	set__insert(Set0, X, Set).
goal_util__rhs_goal_vars(functor(_Functor, ArgVars), Set0, Set) :-
	set__insert_list(Set0, ArgVars, Set).
goal_util__rhs_goal_vars(lambda_goal(_PredOrFunc, LambdaVars, _Modes, _Detism,
		Goal - _), Set0, Set) :-
	set__insert_list(Set0, LambdaVars, Set1),
	goal_util__goal_vars_2(Goal, Set1, Set).

%-----------------------------------------------------------------------------%

goal_util__goal_is_branched(if_then_else(_, _, _, _, _)).
goal_util__goal_is_branched(switch(_, _, _, _)).
goal_util__goal_is_branched(disj(_, _)).

%-----------------------------------------------------------------------------%

goal_size(GoalExpr - _, Size) :-
	goal_expr_size(GoalExpr, Size).

:- pred goals_size(list(hlds_goal), int).
:- mode goals_size(in, out) is det.

goals_size([], 0).
goals_size([Goal | Goals], Size) :-
	goal_size(Goal, Size1),
	goals_size(Goals, Size2),
	Size is Size1 + Size2.

:- pred cases_size(list(case), int).
:- mode cases_size(in, out) is det.

cases_size([], 0).
cases_size([case(_, Goal) | Cases], Size) :-
	goal_size(Goal, Size1),
	cases_size(Cases, Size2),
	Size is Size1 + Size2.

:- pred goal_expr_size(hlds_goal_expr, int).
:- mode goal_expr_size(in, out) is det.

goal_expr_size(conj(Goals), Size) :-
	goals_size(Goals, Size).
goal_expr_size(disj(Goals, _), Size) :-
	goals_size(Goals, Size1),
	Size is Size1 + 1.
goal_expr_size(switch(_, _, Goals, _), Size) :-
	cases_size(Goals, Size1),
	Size is Size1 + 1.
goal_expr_size(if_then_else(_, Cond, Then, Else, _), Size) :-
	goal_size(Cond, Size1),
	goal_size(Then, Size2),
	goal_size(Else, Size3),
	Size is Size1 + Size2 + Size3 + 1.
goal_expr_size(not(Goal), Size) :-
	goal_size(Goal, Size1),
	Size is Size1 + 1.
goal_expr_size(some(_, Goal), Size) :-
	goal_size(Goal, Size1),
	Size is Size1 + 1.
goal_expr_size(call(_, _, _, _, _, _), 1).
goal_expr_size(higher_order_call(_, _, _, _, _), 1).
goal_expr_size(unify(_, _, _, _, _), 1).
goal_expr_size(pragma_c_code(_, _, _, _, _, _, _, _), 1).

%-----------------------------------------------------------------------------%

goal_calls(GoalExpr - _, PredProcId) :-
	goal_expr_calls(GoalExpr, PredProcId).

:- pred goals_calls(list(hlds_goal), pred_proc_id).
:- mode goals_calls(in, in) is semidet.

goals_calls([Goal | Goals], PredProcId) :-
	(
		goal_calls(Goal, PredProcId)
	;
		goals_calls(Goals, PredProcId)
	).

:- pred cases_calls(list(case), pred_proc_id).
:- mode cases_calls(in, in) is semidet.

cases_calls([case(_, Goal) | Cases], PredProcId) :-
	(
		goal_calls(Goal, PredProcId)
	;
		cases_calls(Cases, PredProcId)
	).

:- pred goal_expr_calls(hlds_goal_expr, pred_proc_id).
:- mode goal_expr_calls(in, in) is semidet.

goal_expr_calls(conj(Goals), PredProcId) :-
	goals_calls(Goals, PredProcId).
goal_expr_calls(disj(Goals, _), PredProcId) :-
	goals_calls(Goals, PredProcId).
goal_expr_calls(switch(_, _, Goals, _), PredProcId) :-
	cases_calls(Goals, PredProcId).
goal_expr_calls(if_then_else(_, Cond, Then, Else, _), PredProcId) :-
	(
		goal_calls(Cond, PredProcId)
	;
		goal_calls(Then, PredProcId)
	;
		goal_calls(Else, PredProcId)
	).
goal_expr_calls(not(Goal), PredProcId) :-
	goal_calls(Goal, PredProcId).
goal_expr_calls(some(_, Goal), PredProcId) :-
	goal_calls(Goal, PredProcId).
goal_expr_calls(call(PredId, ProcId, _, _, _, _), proc(PredId, ProcId)).

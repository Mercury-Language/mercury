%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module goal_util.
% Main author: conway.

%-----------------------------------------------------------------------------%

:- interface.
:- import_module hlds, llds.

:- pred goal_util__rename_vars_in_goal(hlds__goal, map(var, var), hlds__goal).
:- mode goal_util__rename_vars_in_goal(in, in, out) is det.

:- pred goal_util__create_variables(list(var), varset,
		map(var, type), map(var, var),
			varset, map(var, type), map(var, var)).
:- mode goal_util__create_variables(in, in, in, in, out, out, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, map, set, std_util.
:- import_module mode_util, term, require.
:- import_module varset, code_aux, prog_io.

%-----------------------------------------------------------------------------%

goal_util__create_variables([], Varset, VarTypes, Subn, Varset, VarTypes, Subn).
goal_util__create_variables([V|Vs], Varset0, VarTypes0, Subn0,
						Varset, VarTypes, Subn) :-
	(
		map__contains(Subn0, V)
	->
		Varset2 = Varset0,
		Subn1 = Subn0,
		VarTypes1 = VarTypes0
	;
		varset__new_var(Varset0, NV, Varset1),
		(
			varset__lookup_name(Varset1, V, Name)
		->
			varset__name_var(Varset1, NV, Name, Varset2)
		;
			Varset2 = Varset1
		),
		map__set(Subn0, V, NV, Subn1),
		(
			map__search(VarTypes0, V, VT)
		->
			map__set(VarTypes0, NV, VT, VarTypes1)
		;
			VarTypes1 = VarTypes0
		)
	),
	goal_util__create_variables(Vs, Varset2, VarTypes1, Subn1, 
		Varset, VarTypes, Subn).

%-----------------------------------------------------------------------------%

:- pred goal_util__init_subn(assoc_list(var, var), map(var, var), map(var, var)).
:- mode goal_util__init_subn(in, in, out) is det.

goal_util__init_subn([], Subn, Subn).
goal_util__init_subn([A-H|Vs], Subn0, Subn) :-
	map__set(Subn0, H, A, Subn1),
	goal_util__init_subn(Vs, Subn1, Subn).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_var_list(list(var), map(var, var), list(var)).
:- mode goal_util__rename_var_list(in, in, out) is det.

goal_util__rename_var_list([], _Subn, []).
goal_util__rename_var_list([V|Vs], Subn, [N|Ns]) :-
	goal_util__rename_var(V, Subn, N),
	goal_util__rename_var_list(Vs, Subn, Ns).

:- pred goal_util__rename_var(var, map(var, var), var).
:- mode goal_util__rename_var(in, in, out) is det.

goal_util__rename_var(V, Subn, N) :-
	(
		map__search(Subn, V, N0)
	->
		N = N0
	;
		N = V
	).

%-----------------------------------------------------------------------------%

goal_util__rename_vars_in_goal(Goal0 - GoalInfo0, Subn, Goal - GoalInfo) :-
	goal_util__name_apart_2(Goal0, Subn, Goal),
	goal_util__name_apart_goalinfo(GoalInfo0, Subn, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred goal_util__name_apart_2(hlds__goal_expr, map(var, var), hlds__goal_expr).
:- mode goal_util__name_apart_2(in, in, out) is det.

goal_util__name_apart_2(conj(Goals0), Subn, conj(Goals)) :-
	goal_util__name_apart_list(Goals0, Subn, Goals).

goal_util__name_apart_2(disj(Goals0), Subn, disj(Goals)) :-
	goal_util__name_apart_list(Goals0, Subn, Goals).

goal_util__name_apart_2(switch(Var0, Det, Cases0), Subn,
						switch(Var, Det, Cases)) :-
	(
		map__search(Subn, Var0, N0)
	->
		Var = N0
	;
		Var = Var0
	),
	goal_util__name_apart_cases(Cases0, Subn, Cases).

goal_util__name_apart_2(if_then_else(Vars0, Cond0, Then0, Else0), Subn,
				if_then_else(Vars, Cond, Then, Else)) :-
	goal_util__rename_var_list(Vars0, Subn, Vars),
	goal_util__rename_vars_in_goal(Cond0, Subn, Cond),
	goal_util__rename_vars_in_goal(Then0, Subn, Then),
	goal_util__rename_vars_in_goal(Else0, Subn, Else).

goal_util__name_apart_2(not(Goal0), Subn, not(Goal)) :-
	goal_util__rename_vars_in_goal(Goal0, Subn, Goal).

goal_util__name_apart_2(some(Vars0, Goal0), Subn, some(Vars, Goal)) :-
	goal_util__rename_var_list(Vars0, Subn, Vars),
	goal_util__rename_vars_in_goal(Goal0, Subn, Goal).

goal_util__name_apart_2(
		call(PredId, ProcId, Args0, Builtin, Context, Sym, Follow0),
		Subn,
		call(PredId, ProcId, Args, Builtin, Context, Sym, Follow)) :-
	goal_util__rename_var_list(Args0, Subn, Args),
	goal_util__rename_follow_vars(Follow0, Subn, Follow).

goal_util__name_apart_2(unify(TermL0,TermR0,Mode,Unify0,Context), Subn,
		unify(TermL,TermR,Mode,Unify,Context)) :-
	goal_util__rename_var(TermL0, Subn, TermL),
	goal_util__rename_unify_rhs(TermR0, Subn, TermR),
	goal_util__rename_unify(Unify0, Subn, Unify).

%-----------------------------------------------------------------------------%

:- pred goal_util__name_apart_list(list(hlds__goal), map(var, var),
							list(hlds__goal)).
:- mode goal_util__name_apart_list(in, in, out) is det.

goal_util__name_apart_list([], _Subn, []).
goal_util__name_apart_list([G0|Gs0], Subn, [G|Gs]) :-
	goal_util__rename_vars_in_goal(G0, Subn, G),
	goal_util__name_apart_list(Gs0, Subn, Gs).

%-----------------------------------------------------------------------------%

:- pred goal_util__name_apart_cases(list(case), map(var, var), list(case)).
:- mode goal_util__name_apart_cases(in, in, out) is det.

goal_util__name_apart_cases([], _Subn, []).
goal_util__name_apart_cases([case(Cons, G0)|Gs0], Subn, [case(Cons, G)|Gs]) :-
	goal_util__rename_vars_in_goal(G0, Subn, G),
	goal_util__name_apart_cases(Gs0, Subn, Gs).

%-----------------------------------------------------------------------------%

	% These predicates probably belong in term.m.

:- pred goal_util__rename_args(list(term), map(var, var), list(term)).
:- mode goal_util__rename_args(in, in, out) is det.

goal_util__rename_args([], _Subn, []).
goal_util__rename_args([T0|Ts0], Subn, [T|Ts]) :-
	goal_util__rename_term(T0, Subn, T),
	goal_util__rename_args(Ts0, Subn, Ts).

:- pred goal_util__rename_term(term, map(var, var), term).
:- mode goal_util__rename_term(in, in, out) is det.

goal_util__rename_term(term__variable(V), Subn, term__variable(N)) :-
	goal_util__rename_var(V, Subn, N).
goal_util__rename_term(term__functor(Cons, Terms0, Cont), Subn,
				term__functor(Cons, Terms, Cont)) :-
	goal_util__rename_args(Terms0, Subn, Terms).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_unify_rhs(unify_rhs, map(var, var), unify_rhs).
:- mode goal_util__rename_unify_rhs(in, in, out) is det.

goal_util__rename_unify_rhs(var(Var0), Subn, var(Var)) :-
	goal_util__rename_var(Var0, Subn, Var).
goal_util__rename_unify_rhs(functor(Functor, ArgVars0), Subn,
			functor(Functor, ArgVars)) :-
	goal_util__rename_var_list(ArgVars0, Subn, ArgVars).
goal_util__rename_unify_rhs(lambda_goal(Vars0, Modes, Det, Goal0), Subn,
			lambda_goal(Vars, Modes, Det, Goal)) :-
	goal_util__rename_var_list(Vars0, Subn, Vars),
	goal_util__rename_vars_in_goal(Goal0, Subn, Goal).

:- pred goal_util__rename_unify(unification, map(var, var), unification).
:- mode goal_util__rename_unify(in, in, out) is det.

goal_util__rename_unify(construct(Var0, ConsId, Vars0, Modes), Subn,
			construct(Var, ConsId, Vars, Modes)) :-
	goal_util__rename_var(Var0, Subn, Var),
	goal_util__rename_var_list(Vars0, Subn, Vars).
goal_util__rename_unify(deconstruct(Var0, ConsId, Vars0, Modes, Cat), Subn,
			deconstruct(Var, ConsId, Vars, Modes, Cat)) :-
	goal_util__rename_var(Var0, Subn, Var),
	goal_util__rename_var_list(Vars0, Subn, Vars).
goal_util__rename_unify(assign(L0, R0), Subn, assign(L, R)) :-
	goal_util__rename_var(L0, Subn, L),
	goal_util__rename_var(R0, Subn, R).
goal_util__rename_unify(simple_test(L0, R0), Subn, simple_test(L, R)) :-
	goal_util__rename_var(L0, Subn, L),
	goal_util__rename_var(R0, Subn, R).
goal_util__rename_unify(complicated_unify(Modes, Cat, Follow0), Subn,
			complicated_unify(Modes, Cat, Follow)) :-
	goal_util__rename_follow_vars(Follow0, Subn, Follow).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_follow_vars(map(var, T), map(var, var), map(var, T)).
:- mode goal_util__rename_follow_vars(in, in, out) is det.

goal_util__rename_follow_vars(Follow0, Subn, Follow) :-
	map__to_assoc_list(Follow0, FollowList0),
	goal_util__rename_follow_vars_2(FollowList0, Subn, FollowList),
	map__from_assoc_list(FollowList, Follow).

:- pred goal_util__rename_follow_vars_2(assoc_list(var, T),
				map(var, var), assoc_list(var, T)).
:- mode goal_util__rename_follow_vars_2(in, in, out) is det.

goal_util__rename_follow_vars_2([], _Subn, []).
goal_util__rename_follow_vars_2([V - L | Vs], Subn, [N - L | Ns]) :-
	goal_util__rename_var(V, Subn, N),
	goal_util__rename_follow_vars_2(Vs, Subn, Ns).

%-----------------------------------------------------------------------------%

:- pred goal_util__name_apart_goalinfo(hlds__goal_info,
					map(var, var), hlds__goal_info).
:- mode goal_util__name_apart_goalinfo(in, in, out) is det.

goal_util__name_apart_goalinfo(GoalInfo0, Subn, GoalInfo) :-
	goal_info_pre_delta_liveness(GoalInfo0, PreBirths0 - PreDeaths0),
	goal_util__name_apart_set(PreBirths0, Subn, PreBirths),
	goal_util__name_apart_set(PreDeaths0, Subn, PreDeaths),
	goal_info_set_pre_delta_liveness(GoalInfo0, PreBirths - PreDeaths,
						GoalInfo1),

	goal_info_post_delta_liveness(GoalInfo1, PostBirths0 - PostDeaths0),
	goal_util__name_apart_set(PostBirths0, Subn, PostBirths),
	goal_util__name_apart_set(PostDeaths0, Subn, PostDeaths),
	goal_info_set_pre_delta_liveness(GoalInfo1, PostBirths - PostDeaths,
						GoalInfo2),

	goal_info_get_nonlocals(GoalInfo2, NonLocals0),
	goal_util__name_apart_set(NonLocals0, Subn, NonLocals),
	goal_info_set_nonlocals(GoalInfo2, NonLocals, GoalInfo3),

	goal_info_get_instmap_delta(GoalInfo3, MaybeInstMap0),
	(
		MaybeInstMap0 = reachable(InstMap0)
	->
		goal_util__rename_follow_vars(InstMap0, Subn, InstMap),
		MaybeInstMap = reachable(InstMap)
	;
		MaybeInstMap = MaybeInstMap0
	),
	goal_info_set_instmap_delta(GoalInfo3, MaybeInstMap, GoalInfo4),

	goal_info_store_map(GoalInfo4, MaybeStoreMap0),
	(
		MaybeStoreMap0 = yes(StoreMap0)
	->
		goal_util__rename_follow_vars(StoreMap0, Subn, StoreMap),
		MaybeStoreMap = yes(StoreMap)
	;
		MaybeStoreMap = MaybeStoreMap0
	),
	goal_info_set_store_map(GoalInfo4, MaybeStoreMap, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred goal_util__name_apart_set(set(var), map(var, var), set(var)).
:- mode goal_util__name_apart_set(in, in, out) is det.

goal_util__name_apart_set(Vars0, Subn, Vars) :-
	set__to_sorted_list(Vars0, VarsList0),
	goal_util__rename_var_list(VarsList0, Subn, VarsList),
	set__list_to_set(VarsList, Vars).

%-----------------------------------------------------------------------------%


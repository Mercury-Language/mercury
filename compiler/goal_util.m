%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: conway.
%
% This module provides various utility procedures for maniupulating HLDS goals,
% e.g. some functionality for renaming variables in goals.

%-----------------------------------------------------------------------------%

:- module hlds__goal_util.
:- interface.

:- import_module parse_tree__inst, parse_tree__prog_data.
:- import_module hlds__hlds_data, hlds__hlds_goal, hlds__hlds_module.
:- import_module hlds__hlds_pred, hlds__instmap.
:- import_module assoc_list, bool, list, set, map, term, std_util.

% The predicates rename_var* take a structure and a mapping from var -> var
% and apply that translation. If a var in the input structure does not
% occur as a key in the mapping, then the variable is left unsubstituted.

	% goal_util__rename_vars_in_goals(GoalList, MustRename, Substitution,
	%	NewGoalList).
:- pred goal_util__rename_vars_in_goals(list(hlds_goal), bool,
		map(prog_var, prog_var), list(hlds_goal)).
:- mode goal_util__rename_vars_in_goals(in, in, in, out) is det.

:- pred goal_util__rename_vars_in_goal(hlds_goal, map(prog_var, prog_var),
		hlds_goal).
:- mode goal_util__rename_vars_in_goal(in, in, out) is det.

:- pred goal_util__must_rename_vars_in_goal(hlds_goal,
		map(prog_var, prog_var), hlds_goal).
:- mode goal_util__must_rename_vars_in_goal(in, in, out) is det.

:- pred goal_util__rename_vars_in_var_set(set(prog_var), bool,
	map(prog_var, prog_var), set(prog_var)).
:- mode goal_util__rename_vars_in_var_set(in, in, in, out) is det.

:- pred goal_util__rename_var_list(list(var(T)), bool,
		map(var(T), var(T)), list(var(T))).
:- mode goal_util__rename_var_list(in, in, in, out) is det.

:- pred goal_util__rename_var(var(V), bool, map(var(V), var(V)), var(V)).
:- mode goal_util__rename_var(in, in, in, out) is det.

% goal_util__create_variables takes a list of variables, a varset an
% initial translation mapping and an initial mapping from variable to
% type, and creates new instances of each of the source variables in the
% translation mapping, adding the new variable to the type mapping and
% updating the varset. The type for each new variable is found by looking
% in the type map given in the 5th argument - the last input.
% (This interface will not easily admit uniqueness in the type map for this
% reason - such is the sacrifice for generality.)

	% goal_util__create_variables(OldVariables, OldVarset, InitialVarTypes,
	%	InitialSubstitution, OldVarTypes, OldVarNames,  NewVarset,
	%	NewVarTypes, NewSubstitution)
:- pred goal_util__create_variables(list(prog_var),
			prog_varset, map(prog_var, type),
			map(prog_var, prog_var),
			map(prog_var, type),
			prog_varset, prog_varset, map(prog_var, type),
			map(prog_var, prog_var)).
:- mode goal_util__create_variables(in, in, in, in, in, in, out, out, out)
		is det.

	% Return all the variables in the goal.
	% Unlike quantification:goal_vars, this predicate returns
	% even the explicitly quantified variables.
:- pred goal_util__goal_vars(hlds_goal, set(prog_var)).
:- mode goal_util__goal_vars(in, out) is det.

	% Return all the variables in the list of goals.
	% Unlike quantification:goal_vars, this predicate returns
	% even the explicitly quantified variables.
:- pred goal_util__goals_goal_vars(list(hlds_goal), set(prog_var),
		set(prog_var)).
:- mode goal_util__goals_goal_vars(in, in, out) is det.

	% Return all the variables in a generic call.
:- pred goal_util__generic_call_vars(generic_call, list(prog_var)).
:- mode goal_util__generic_call_vars(in, out) is det.

	%
	% goal_util__extra_nonlocal_typeinfos(TypeInfoMap, TypeClassInfoMap,
	%		VarTypes, ExistQVars, NonLocals, NonLocalTypeInfos):
	% compute which type-info and type-class-info variables
	% may need to be non-local to a goal.
	%
	% A type-info variable may be non-local to a goal if any of 
	% the ordinary non-local variables for that goal are
	% polymorphically typed with a type that depends on that
	% type-info variable, or if the type-info is for an
	% existentially quantified type variable.
	%
	% In addition, a typeclass-info may be non-local to a goal if
	% any of the non-local variables for that goal are
	% polymorphically typed and are constrained by the typeclass
	% constraints for that typeclass-info variable,
	% or if the the type-class-info is for an existential constraint,
	% i.e. a constraint which contrains an existentially quantified
	% type variable.
	%
:- pred goal_util__extra_nonlocal_typeinfos(map(tvar, type_info_locn),
		map(class_constraint, prog_var), map(prog_var, type),
		existq_tvars, set(prog_var), set(prog_var)).
:- mode goal_util__extra_nonlocal_typeinfos(in, in, in, in, in, out) is det.

	% See whether the goal is a branched structure.
:- pred goal_util__goal_is_branched(hlds_goal_expr).
:- mode goal_util__goal_is_branched(in) is semidet.

	% Return an indication of the size of the goal.
:- pred goal_size(hlds_goal, int).
:- mode goal_size(in, out) is det.

	% Return an indication of the size of the list of goals.
:- pred goals_size(list(hlds_goal), int).
:- mode goals_size(in, out) is det.

	% Return an indication of the size of the list of clauses.
:- pred clause_list_size(list(clause), int).
:- mode clause_list_size(in, out) is det.

	% Test whether the goal calls the given procedure.
:- pred goal_calls(hlds_goal, pred_proc_id).
:- mode goal_calls(in, in) is semidet.
:- mode goal_calls(in, out) is nondet.

	% Test whether the goal calls the given predicate.
	% This is useful before mode analysis when the proc_ids
	% have not been determined.
:- pred goal_calls_pred_id(hlds_goal, pred_id).
:- mode goal_calls_pred_id(in, in) is semidet.
:- mode goal_calls_pred_id(in, out) is nondet.

	% Test whether the goal contains a reconstruction
	% (a construction where the `cell_to_reuse' field is `yes(_)').
:- pred goal_contains_reconstruction(hlds_goal).
:- mode goal_contains_reconstruction(in) is semidet.

	% goal_contains_goal(Goal, SubGoal) is true iff Goal contains SubGoal,
	% i.e. iff Goal = SubGoal or Goal contains SubGoal as a direct
	% or indirect sub-goal.
	%
:- pred goal_contains_goal(hlds_goal, hlds_goal).
:- mode goal_contains_goal(in, out) is multi.

	% direct_subgoal(Goal, DirectSubGoal) is true iff DirectSubGoal is
	% a direct sub-goal of Goal.
	%
:- pred direct_subgoal(hlds_goal_expr, hlds_goal).
:- mode direct_subgoal(in, out) is nondet.

	% returns all the predids that are used within a goal
:- pred predids_from_goal(hlds_goal, list(pred_id)).
:- mode predids_from_goal(in, out) is det.
	
	% returns all the predids that are used in a list of goals
:- pred predids_from_goals(list(hlds_goal), list(pred_id)).
:- mode predids_from_goals(in, out) is det.

%-----------------------------------------------------------------------------%

	% Convert a switch back into a disjunction. This is needed 
	% for the magic set transformation.
	% This aborts if any of the constructors are existentially typed.
:- pred goal_util__switch_to_disjunction(prog_var, list(case), instmap, 
		list(hlds_goal), prog_varset, prog_varset, map(prog_var, type),
		map(prog_var, type), module_info, module_info).
:- mode goal_util__switch_to_disjunction(in, in, in, out,
		in, out, in, out, in, out) is det.

	% Convert a case into a conjunction by adding a tag test 
	% (deconstruction unification) to the case goal.
	% This aborts if the constructor is existentially typed.
:- pred goal_util__case_to_disjunct(prog_var, cons_id, hlds_goal, instmap,
		hlds_goal, prog_varset, prog_varset, map(prog_var, type),
		map(prog_var, type), module_info, module_info).
:- mode goal_util__case_to_disjunct(in, in, in, in, out, in, out,
		in, out, in, out) is det.

	% Transform an if-then-else into ( Cond, Then ; \+ Cond, Else ),
	% since magic.m and rl_gen.m don't handle if-then-elses.
:- pred goal_util__if_then_else_to_disjunction(hlds_goal, hlds_goal,
		hlds_goal, hlds_goal_info, hlds_goal_expr) is det.
:- mode goal_util__if_then_else_to_disjunction(in, in, in, in, out) is det.

%-----------------------------------------------------------------------------%

	% goal_util__can_reorder_goals(ModuleInfo, FullyStrict, Goal1, Goal2).
	%
	% Goals can be reordered if
	% - the goals are independent
	% - the goals are not impure
	% - any possible change in termination behaviour is allowed
	% 	according to the semantics options.
	%
:- pred goal_util__can_reorder_goals(module_info::in, vartypes::in, bool::in,
	instmap::in, hlds_goal::in, instmap::in, hlds_goal::in) is semidet.

	% goal_util__reordering_maintains_termination(ModuleInfo,
	%		 FullyStrict, Goal1, Goal2)
	%
	% Succeeds if any possible change in termination behaviour from
	% reordering the goals is allowed according to the semantics options.
	% The information computed by termination analysis is used when
	% making this decision.
	%
:- pred goal_util__reordering_maintains_termination(module_info::in, bool::in, 
		hlds_goal::in, hlds_goal::in) is semidet.

	% generate_simple_call(ModuleName, PredName, Args,
	%		Detism, MaybeFeature, InstMapDelta,
	%		ModuleInfo, Context, CallGoal):
	% Generate a call to a builtin procedure (e.g.
	% from the private_builtin or table_builtin module).
	% This is used by HLDS->HLDS transformation passes that introduce
	% calls to builtin procedures.  This is restricted in various ways,
	% e.g. the called procedure must have exactly one mode,
	% and at most one type parameter.  So it should only be used
	% for generating calls to known builtin procedures.
	%
:- pred goal_util__generate_simple_call(module_name::in, string::in,
	list(prog_var)::in, determinism::in, maybe(goal_feature)::in,
	assoc_list(prog_var, inst)::in, module_info::in, term__context::in,
	hlds_goal::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__prog_data, parse_tree__inst.
:- import_module hlds__hlds_data, hlds__goal_form, hlds__hlds_llds.
:- import_module check_hlds__purity, check_hlds__det_analysis.
:- import_module check_hlds__inst_match, check_hlds__mode_util.
:- import_module check_hlds__type_util.

:- import_module int, string, require, varset.

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

:- pred goal_util__init_subn(assoc_list(prog_var, prog_var),
	map(prog_var, prog_var), map(prog_var, prog_var)).
:- mode goal_util__init_subn(in, in, out) is det.

goal_util__init_subn([], Subn, Subn).
goal_util__init_subn([A - H | Vs], Subn0, Subn) :-
	map__set(Subn0, H, A, Subn1),
	goal_util__init_subn(Vs, Subn1, Subn).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_var_pair_list(assoc_list(prog_var, T), bool,
	map(prog_var, prog_var), list(pair(prog_var, T))).
:- mode goal_util__rename_var_pair_list(in, in, in, out) is det.

goal_util__rename_var_pair_list([], _Must, _Subn, []).
goal_util__rename_var_pair_list([V - D | VDs], Must, Subn, [N - D | NDs]) :-
	goal_util__rename_var(V, Must, Subn, N),
	goal_util__rename_var_pair_list(VDs, Must, Subn, NDs).

goal_util__rename_var_list([], _Must, _Subn, []).
goal_util__rename_var_list([V | Vs], Must, Subn, [N | Ns]) :-
	goal_util__rename_var(V, Must, Subn, N),
	goal_util__rename_var_list(Vs, Must, Subn, Ns).

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
			term__var_to_int(V, VInt),
			string__format(
			    "goal_util__rename_var: no substitute for var %i", 
			    [i(VInt)], Msg),
			error(Msg)
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

:- pred goal_util__rename_vars_in_goal(hlds_goal, bool, map(prog_var, prog_var),
		hlds_goal).
:- mode goal_util__rename_vars_in_goal(in, in, in, out) is det.

goal_util__rename_vars_in_goal(Goal0 - GoalInfo0, Must, Subn, Goal - GoalInfo)
		:-
	goal_util__rename_vars_in_goal_expr(Goal0, Must, Subn, Goal),
	goal_util__rename_vars_in_goal_info(GoalInfo0, Must, Subn, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_vars_in_goal_expr(hlds_goal_expr, bool,
	map(prog_var, prog_var), hlds_goal_expr).
:- mode goal_util__rename_vars_in_goal_expr(in, in, in, out) is det.

goal_util__rename_vars_in_goal_expr(conj(Goals0), Must, Subn, conj(Goals)) :-
	goal_util__rename_vars_in_goals(Goals0, Must, Subn, Goals).

goal_util__rename_vars_in_goal_expr(par_conj(Goals0), Must, Subn,
		par_conj(Goals)) :-
	goal_util__rename_vars_in_goals(Goals0, Must, Subn, Goals).

goal_util__rename_vars_in_goal_expr(disj(Goals0), Must, Subn, disj(Goals)) :-
	goal_util__rename_vars_in_goals(Goals0, Must, Subn, Goals).

goal_util__rename_vars_in_goal_expr(switch(Var0, Det, Cases0), Must, Subn,
		switch(Var, Det, Cases)) :-
	goal_util__rename_var(Var0, Must, Subn, Var),
	goal_util__rename_vars_in_cases(Cases0, Must, Subn, Cases).

goal_util__rename_vars_in_goal_expr(if_then_else(Vars0, Cond0, Then0, Else0),
		Must, Subn, if_then_else(Vars, Cond, Then, Else)) :-
	goal_util__rename_var_list(Vars0, Must, Subn, Vars),
	goal_util__rename_vars_in_goal(Cond0, Must, Subn, Cond),
	goal_util__rename_vars_in_goal(Then0, Must, Subn, Then),
	goal_util__rename_vars_in_goal(Else0, Must, Subn, Else).

goal_util__rename_vars_in_goal_expr(not(Goal0), Must, Subn, not(Goal)) :-
	goal_util__rename_vars_in_goal(Goal0, Must, Subn, Goal).

goal_util__rename_vars_in_goal_expr(some(Vars0, CanRemove, Goal0), Must, Subn,
		some(Vars, CanRemove, Goal)) :-
	goal_util__rename_var_list(Vars0, Must, Subn, Vars),
	goal_util__rename_vars_in_goal(Goal0, Must, Subn, Goal).

goal_util__rename_vars_in_goal_expr(
		generic_call(GenericCall0, Args0, Modes, Det),
		Must, Subn,
		generic_call(GenericCall, Args, Modes, Det)) :-
	goal_util__rename_generic_call(GenericCall0, Must, Subn, GenericCall),
	goal_util__rename_var_list(Args0, Must, Subn, Args).

goal_util__rename_vars_in_goal_expr(
		call(PredId, ProcId, Args0, Builtin, Context, Sym),
		Must, Subn,
		call(PredId, ProcId, Args, Builtin, Context, Sym)) :-
	goal_util__rename_var_list(Args0, Must, Subn, Args).

goal_util__rename_vars_in_goal_expr(unify(TermL0,TermR0,Mode,Unify0,Context),
		Must, Subn, unify(TermL,TermR,Mode,Unify,Context)) :-
	goal_util__rename_var(TermL0, Must, Subn, TermL),
	goal_util__rename_unify_rhs(TermR0, Must, Subn, TermR),
	goal_util__rename_unify(Unify0, Must, Subn, Unify).

goal_util__rename_vars_in_goal_expr(foreign_proc(A,B,C,Vars0,E,F,G),
		Must, Subn, foreign_proc(A,B,C,Vars,E,F,G)) :-
	goal_util__rename_var_list(Vars0, Must, Subn, Vars).

goal_util__rename_vars_in_goal_expr(shorthand(ShorthandGoal0), Must, Subn,
		shorthand(ShrothandGoal)) :-
	goal_util__rename_vars_in_shorthand(ShorthandGoal0, Must, Subn,
		ShrothandGoal).

:- pred goal_util__rename_vars_in_shorthand(shorthand_goal_expr, bool,
		map(prog_var, prog_var), shorthand_goal_expr).
:- mode goal_util__rename_vars_in_shorthand(in, in, in, out) is det.

goal_util__rename_vars_in_shorthand(bi_implication(LHS0, RHS0), Must, Subn,
		bi_implication(LHS, RHS)) :-
	goal_util__rename_vars_in_goal(LHS0, Must, Subn, LHS),
	goal_util__rename_vars_in_goal(RHS0, Must, Subn, RHS).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_vars_in_cases(list(case), bool,
	map(prog_var, prog_var), list(case)).
:- mode goal_util__rename_vars_in_cases(in, in, in, out) is det.

goal_util__rename_vars_in_cases([], _Must, _Subn, []).
goal_util__rename_vars_in_cases([case(Cons, G0) | Gs0], Must, Subn,
		[case(Cons, G) | Gs]) :-
	goal_util__rename_vars_in_goal(G0, Must, Subn, G),
	goal_util__rename_vars_in_cases(Gs0, Must, Subn, Gs).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_unify_rhs(unify_rhs, bool, map(prog_var, prog_var),
		unify_rhs).
:- mode goal_util__rename_unify_rhs(in, in, in, out) is det.

goal_util__rename_unify_rhs(var(Var0), Must, Subn, var(Var)) :-
	goal_util__rename_var(Var0, Must, Subn, Var).
goal_util__rename_unify_rhs(functor(Functor, E, ArgVars0), Must, Subn,
			functor(Functor, E, ArgVars)) :-
	goal_util__rename_var_list(ArgVars0, Must, Subn, ArgVars).
goal_util__rename_unify_rhs(
	    lambda_goal(PredOrFunc, EvalMethod, FixModes, NonLocals0,
	    		Vars0, Modes, Det, Goal0),
	    Must, Subn, 
	    lambda_goal(PredOrFunc, EvalMethod, FixModes, NonLocals,
	    		Vars, Modes, Det, Goal)) :-
	goal_util__rename_var_list(NonLocals0, Must, Subn, NonLocals),
	goal_util__rename_var_list(Vars0, Must, Subn, Vars),
	goal_util__rename_vars_in_goal(Goal0, Must, Subn, Goal).

:- pred goal_util__rename_unify(unification, bool, map(prog_var, prog_var),
		unification).
:- mode goal_util__rename_unify(in, in, in, out) is det.

goal_util__rename_unify(
		construct(Var0, ConsId, Vars0, Modes, How0, Uniq, Aditi),
		Must, Subn,
		construct(Var, ConsId, Vars, Modes, How, Uniq, Aditi)) :-
	goal_util__rename_var(Var0, Must, Subn, Var),
	goal_util__rename_var_list(Vars0, Must, Subn, Vars),
	(
		How0 = reuse_cell(cell_to_reuse(ReuseVar0, B, C)),
		goal_util__rename_var(ReuseVar0, Must, Subn, ReuseVar),
		How = reuse_cell(cell_to_reuse(ReuseVar, B, C))
	;
		How0 = construct_dynamically,
		How = How0
	;
		How0 = construct_statically(_),
		How = How0
	).
goal_util__rename_unify(deconstruct(Var0, ConsId, Vars0, Modes, Cat, CanCGC),
		Must, Subn,
		deconstruct(Var, ConsId, Vars, Modes, Cat, CanCGC)) :-
	goal_util__rename_var(Var0, Must, Subn, Var),
	goal_util__rename_var_list(Vars0, Must, Subn, Vars).
goal_util__rename_unify(assign(L0, R0), Must, Subn, assign(L, R)) :-
	goal_util__rename_var(L0, Must, Subn, L),
	goal_util__rename_var(R0, Must, Subn, R).
goal_util__rename_unify(simple_test(L0, R0), Must, Subn, simple_test(L, R)) :-
	goal_util__rename_var(L0, Must, Subn, L),
	goal_util__rename_var(R0, Must, Subn, R).
goal_util__rename_unify(complicated_unify(Modes, Cat, TypeInfoVars),
			_Must, _Subn,
			complicated_unify(Modes, Cat, TypeInfoVars)).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_generic_call(generic_call, bool,
		map(prog_var, prog_var), generic_call).
:- mode goal_util__rename_generic_call(in, in, in, out) is det.

goal_util__rename_generic_call(higher_order(Var0, PredOrFunc, Arity),
		Must, Subn, higher_order(Var, PredOrFunc, Arity)) :-
	goal_util__rename_var(Var0, Must, Subn, Var).
goal_util__rename_generic_call(class_method(Var0, Method, ClassId, MethodId),
		Must, Subn, class_method(Var, Method, ClassId, MethodId)) :-
	goal_util__rename_var(Var0, Must, Subn, Var).
goal_util__rename_generic_call(aditi_builtin(Builtin, PredCallId),
		_Must, _Subn, aditi_builtin(Builtin, PredCallId)).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_var_maps(map(prog_var, T), bool,
				map(prog_var, prog_var), map(prog_var, T)).
:- mode goal_util__rename_var_maps(in, in, in, out) is det.

goal_util__rename_var_maps(Map0, Must, Subn, Map) :-
	map__to_assoc_list(Map0, AssocList0),
	goal_util__rename_var_maps_2(AssocList0, Must, Subn, AssocList),
	map__from_assoc_list(AssocList, Map).

:- pred goal_util__rename_var_maps_2(assoc_list(var(V), T),
		bool, map(var(V), var(V)), assoc_list(var(V), T)).
:- mode goal_util__rename_var_maps_2(in, in, in, out) is det.

goal_util__rename_var_maps_2([], _Must, _Subn, []).
goal_util__rename_var_maps_2([V - L | Vs], Must, Subn, [N - L | Ns]) :-
	goal_util__rename_var(V, Must, Subn, N),
	goal_util__rename_var_maps_2(Vs, Must, Subn, Ns).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_vars_in_goal_info(hlds_goal_info,
		bool, map(prog_var, prog_var), hlds_goal_info).
:- mode goal_util__rename_vars_in_goal_info(in, in, in, out) is det.

goal_util__rename_vars_in_goal_info(GoalInfo0, Must, Subn, GoalInfo) :-
	goal_info_get_nonlocals(GoalInfo0, NonLocals0),
	goal_util__rename_vars_in_var_set(NonLocals0, Must, Subn, NonLocals),
	goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),

	goal_info_get_instmap_delta(GoalInfo1, InstMap0),
	instmap_delta_apply_sub(InstMap0, Must, Subn, InstMap),
	goal_info_set_instmap_delta(GoalInfo1, InstMap, GoalInfo2),

	goal_info_get_code_gen_info(GoalInfo2, CodeGenInfo0),
	goal_util__rename_vars_in_code_gen_info(CodeGenInfo0, Must, Subn,
		CodeGenInfo),
	goal_info_set_code_gen_info(GoalInfo2, CodeGenInfo, GoalInfo).

%-----------------------------------------------------------------------------%

goal_util__rename_vars_in_var_set(Vars0, Must, Subn, Vars) :-
	set__to_sorted_list(Vars0, VarsList0),
	goal_util__rename_var_list(VarsList0, Must, Subn, VarsList),
	set__list_to_set(VarsList, Vars).

:- pred goal_util__rename_vars_in_code_gen_info(hlds_goal_code_gen_info::in,
	bool::in, map(prog_var, prog_var)::in, hlds_goal_code_gen_info::out)
	is det.

goal_util__rename_vars_in_code_gen_info(CodeGenInfo0, Must, Subn,
		CodeGenInfo) :-
	(
		CodeGenInfo0 = no_code_gen_info,
		CodeGenInfo = no_code_gen_info
	;
		CodeGenInfo0 = llds_code_gen_info(LldsInfo0),
		rename_vars_in_llds_code_gen_info(LldsInfo0, Must, Subn,
			LldsInfo),
		CodeGenInfo = llds_code_gen_info(LldsInfo)
	).

%-----------------------------------------------------------------------------%

goal_util__goal_vars(Goal - _GoalInfo, Set) :-
	set__init(Set0),
	goal_util__goal_vars_2(Goal, Set0, Set).

:- pred goal_util__goal_vars_2(hlds_goal_expr, set(prog_var), set(prog_var)).
:- mode goal_util__goal_vars_2(in, in, out) is det.

goal_util__goal_vars_2(unify(Var, RHS, _, Unif, _), Set0, Set) :-
	set__insert(Set0, Var, Set1),
	( Unif = construct(_, _, _, _, CellToReuse, _, _) ->
		( CellToReuse = reuse_cell(cell_to_reuse(Var, _, _)) ->
			set__insert(Set1, Var, Set2)
		;
			Set2 = Set1
		)
	;
		Set2 = Set1
	),	
	goal_util__rhs_goal_vars(RHS, Set2, Set).

goal_util__goal_vars_2(generic_call(GenericCall, ArgVars, _, _),
		Set0, Set) :-
	goal_util__generic_call_vars(GenericCall, Vars0),
	set__insert_list(Set0, Vars0, Set1),
	set__insert_list(Set1, ArgVars, Set).

goal_util__goal_vars_2(call(_, _, ArgVars, _, _, _), Set0, Set) :-
	set__insert_list(Set0, ArgVars, Set).

goal_util__goal_vars_2(conj(Goals), Set0, Set) :-
	goal_util__goals_goal_vars(Goals, Set0, Set).

goal_util__goal_vars_2(par_conj(Goals), Set0, Set) :-
	goal_util__goals_goal_vars(Goals, Set0, Set).

goal_util__goal_vars_2(disj(Goals), Set0, Set) :-
	goal_util__goals_goal_vars(Goals, Set0, Set).

goal_util__goal_vars_2(switch(Var, _Det, Cases), Set0, Set) :-
	set__insert(Set0, Var, Set1),
	goal_util__cases_goal_vars(Cases, Set1, Set).

goal_util__goal_vars_2(some(Vars, _, Goal - _), Set0, Set) :-
	set__insert_list(Set0, Vars, Set1),
	goal_util__goal_vars_2(Goal, Set1, Set).

goal_util__goal_vars_2(not(Goal - _GoalInfo), Set0, Set) :-
	goal_util__goal_vars_2(Goal, Set0, Set).

goal_util__goal_vars_2(if_then_else(Vars, A - _, B - _, C - _), Set0, Set) :-
	set__insert_list(Set0, Vars, Set1),
	goal_util__goal_vars_2(A, Set1, Set2),
	goal_util__goal_vars_2(B, Set2, Set3),
	goal_util__goal_vars_2(C, Set3, Set).

goal_util__goal_vars_2(foreign_proc(_, _, _, ArgVars, _, _, _),
		Set0, Set) :-
	set__insert_list(Set0, ArgVars, Set).

goal_util__goal_vars_2(shorthand(ShorthandGoal), Set0, Set) :-
	goal_util__goal_vars_2_shorthand(ShorthandGoal, Set0, Set).


:- pred goal_util__goal_vars_2_shorthand(shorthand_goal_expr, set(prog_var),
		set(prog_var)).
:- mode goal_util__goal_vars_2_shorthand(in, in, out) is det.

goal_util__goal_vars_2_shorthand(bi_implication(LHS - _, RHS - _), Set0, 
		Set) :-
	goal_util__goal_vars_2(LHS, Set0, Set1),
	goal_util__goal_vars_2(RHS, Set1, Set).



goal_util__goals_goal_vars([], Set, Set).
goal_util__goals_goal_vars([Goal - _ | Goals], Set0, Set) :-
	goal_util__goal_vars_2(Goal, Set0, Set1),
	goal_util__goals_goal_vars(Goals, Set1, Set).

:- pred goal_util__cases_goal_vars(list(case), set(prog_var), set(prog_var)).
:- mode goal_util__cases_goal_vars(in, in, out) is det.

goal_util__cases_goal_vars([], Set, Set).
goal_util__cases_goal_vars([case(_, Goal - _) | Cases], Set0, Set) :-
	goal_util__goal_vars_2(Goal, Set0, Set1),
	goal_util__cases_goal_vars(Cases, Set1, Set).

:- pred goal_util__rhs_goal_vars(unify_rhs, set(prog_var), set(prog_var)).
:- mode goal_util__rhs_goal_vars(in, in, out) is det.

goal_util__rhs_goal_vars(var(X), Set0, Set) :-
	set__insert(Set0, X, Set).
goal_util__rhs_goal_vars(functor(_Functor, _, ArgVars), Set0, Set) :-
	set__insert_list(Set0, ArgVars, Set).
goal_util__rhs_goal_vars(
		lambda_goal(_, _, _, NonLocals, LambdaVars, _M, _D, Goal - _), 
		Set0, Set) :-
	set__insert_list(Set0, NonLocals, Set1),
	set__insert_list(Set1, LambdaVars, Set2),
	goal_util__goal_vars_2(Goal, Set2, Set).

goal_util__generic_call_vars(higher_order(Var, _, _), [Var]).
goal_util__generic_call_vars(class_method(Var, _, _, _), [Var]).
goal_util__generic_call_vars(aditi_builtin(_, _), []).

%-----------------------------------------------------------------------------%

goal_util__extra_nonlocal_typeinfos(TypeVarMap, TypeClassVarMap, VarTypes,
		ExistQVars, NonLocals, NonLocalTypeInfos) :-
	set__to_sorted_list(NonLocals, NonLocalsList),
	map__apply_to_list(NonLocalsList, VarTypes, NonLocalsTypes),
	term__vars_list(NonLocalsTypes, NonLocalTypeVars),
		% Find all the type-infos and typeclass-infos that are
		% non-local
	solutions_set(lambda([Var::out] is nondet, (
			%
			% if there is some TypeVar for which either
			% (a) the type of some non-local variable
			%     depends on that type variable, or
			% (b) that type variable is existentially
			%     quantified
			%
			( list__member(TypeVar, NonLocalTypeVars)
			; list__member(TypeVar, ExistQVars)
			),
			%
			% then the type_info Var for that type,
			% and any type_class_info Vars which represent
			% constraints on types which include that type,
			% should be included in the NonLocalTypeInfos.
			%
			( map__search(TypeVarMap, TypeVar, Location),
			  type_info_locn_var(Location, Var)
			;
			  % this is probably not very efficient...
			  map__member(TypeClassVarMap, Constraint, Var),
			  Constraint = constraint(_Name, ArgTypes),
			  term__contains_var_list(ArgTypes, TypeVar)
			)
		)), NonLocalTypeInfos).

%-----------------------------------------------------------------------------%

goal_util__goal_is_branched(if_then_else(_, _, _, _)).
goal_util__goal_is_branched(switch(_, _, _)).
goal_util__goal_is_branched(disj(_)).

%-----------------------------------------------------------------------------%

goal_size(GoalExpr - _, Size) :-
	goal_expr_size(GoalExpr, Size).

goals_size([], 0).
goals_size([Goal | Goals], Size) :-
	goal_size(Goal, Size1),
	goals_size(Goals, Size2),
	Size is Size1 + Size2.

clause_list_size(Clauses, GoalSize) :-
	GetClauseSize =
		(pred(Clause::in, Size0::in, Size::out) is det :-
			Clause = clause(_, ClauseGoal, _, _),
			goal_size(ClauseGoal, ClauseSize),
			Size = Size0 + ClauseSize
		),
	list__foldl(GetClauseSize, Clauses, 0, GoalSize0),
	( Clauses = [_] ->
		GoalSize = GoalSize0
	;
		% Add one for the disjunction.
		GoalSize = GoalSize0 + 1
	).

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
goal_expr_size(par_conj(Goals), Size) :-
	goals_size(Goals, Size1),
	Size is Size1 + 1.
goal_expr_size(disj(Goals), Size) :-
	goals_size(Goals, Size1),
	Size is Size1 + 1.
goal_expr_size(switch(_, _, Goals), Size) :-
	cases_size(Goals, Size1),
	Size is Size1 + 1.
goal_expr_size(if_then_else(_, Cond, Then, Else), Size) :-
	goal_size(Cond, Size1),
	goal_size(Then, Size2),
	goal_size(Else, Size3),
	Size is Size1 + Size2 + Size3 + 1.
goal_expr_size(not(Goal), Size) :-
	goal_size(Goal, Size1),
	Size is Size1 + 1.
goal_expr_size(some(_, _, Goal), Size) :-
	goal_size(Goal, Size1),
	Size is Size1 + 1.
goal_expr_size(call(_, _, _, _, _, _), 1).
goal_expr_size(generic_call(_, _, _, _), 1).
goal_expr_size(unify(_, _, _, _, _), 1).
goal_expr_size(foreign_proc(_, _, _, _, _, _, _), 1).
goal_expr_size(shorthand(ShorthandGoal), Size) :-
	goal_expr_size_shorthand(ShorthandGoal, Size).
	
:- pred goal_expr_size_shorthand(shorthand_goal_expr, int).
:- mode goal_expr_size_shorthand(in, out) is det.

goal_expr_size_shorthand(bi_implication(LHS, RHS), Size) :-
	goal_size(LHS, Size1),
	goal_size(RHS, Size2),
	Size is Size1 + Size2 + 1.

%-----------------------------------------------------------------------------%
%
% We could implement goal_calls as
%	goal_calls(Goal, proc(PredId, ProcId)) :-
%		goal_contains_subgoal(Goal, call(PredId, ProcId, _, _, _, _)).
% but the following is more efficient in the (in, in) mode
% since it avoids creating any choice points.
%

goal_calls(GoalExpr - _, PredProcId) :-
	goal_expr_calls(GoalExpr, PredProcId).

:- pred goals_calls(list(hlds_goal), pred_proc_id).
:- mode goals_calls(in, in) is semidet.
:- mode goals_calls(in, out) is nondet.

goals_calls([Goal | Goals], PredProcId) :-
	(
		goal_calls(Goal, PredProcId)
	;
		goals_calls(Goals, PredProcId)
	).

:- pred cases_calls(list(case), pred_proc_id).
:- mode cases_calls(in, in) is semidet.
:- mode cases_calls(in, out) is nondet.

cases_calls([case(_, Goal) | Cases], PredProcId) :-
	(
		goal_calls(Goal, PredProcId)
	;
		cases_calls(Cases, PredProcId)
	).

:- pred goal_expr_calls(hlds_goal_expr, pred_proc_id).
:- mode goal_expr_calls(in, in) is semidet.
:- mode goal_expr_calls(in, out) is nondet.

goal_expr_calls(conj(Goals), PredProcId) :-
	goals_calls(Goals, PredProcId).
goal_expr_calls(par_conj(Goals), PredProcId) :-
	goals_calls(Goals, PredProcId).
goal_expr_calls(disj(Goals), PredProcId) :-
	goals_calls(Goals, PredProcId).
goal_expr_calls(switch(_, _, Goals), PredProcId) :-
	cases_calls(Goals, PredProcId).
goal_expr_calls(if_then_else(_, Cond, Then, Else), PredProcId) :-
	(
		goal_calls(Cond, PredProcId)
	;
		goal_calls(Then, PredProcId)
	;
		goal_calls(Else, PredProcId)
	).
goal_expr_calls(not(Goal), PredProcId) :-
	goal_calls(Goal, PredProcId).
goal_expr_calls(some(_, _, Goal), PredProcId) :-
	goal_calls(Goal, PredProcId).
goal_expr_calls(call(PredId, ProcId, _, _, _, _), proc(PredId, ProcId)).

%-----------------------------------------------------------------------------%
%
% We could implement goal_calls_pred_id as
%	goal_calls_pred_id(Goal, PredId) :-
%		goal_contains_subgoal(Goal, call(PredId, _, _, _, _, _)).
% but the following is more efficient in the (in, in) mode
% since it avoids creating any choice points.
%

goal_calls_pred_id(GoalExpr - _, PredId) :-
	goal_expr_calls_pred_id(GoalExpr, PredId).

:- pred goals_calls_pred_id(list(hlds_goal), pred_id).
:- mode goals_calls_pred_id(in, in) is semidet.
:- mode goals_calls_pred_id(in, out) is nondet.

goals_calls_pred_id([Goal | Goals], PredId) :-
	(
		goal_calls_pred_id(Goal, PredId)
	;
		goals_calls_pred_id(Goals, PredId)
	).

:- pred cases_calls_pred_id(list(case), pred_id).
:- mode cases_calls_pred_id(in, in) is semidet.
:- mode cases_calls_pred_id(in, out) is nondet.

cases_calls_pred_id([case(_, Goal) | Cases], PredId) :-
	(
		goal_calls_pred_id(Goal, PredId)
	;
		cases_calls_pred_id(Cases, PredId)
	).

:- pred goal_expr_calls_pred_id(hlds_goal_expr, pred_id).
:- mode goal_expr_calls_pred_id(in, in) is semidet.
:- mode goal_expr_calls_pred_id(in, out) is nondet.

goal_expr_calls_pred_id(conj(Goals), PredId) :-
	goals_calls_pred_id(Goals, PredId).
goal_expr_calls_pred_id(par_conj(Goals), PredId) :-
	goals_calls_pred_id(Goals, PredId).
goal_expr_calls_pred_id(disj(Goals), PredId) :-
	goals_calls_pred_id(Goals, PredId).
goal_expr_calls_pred_id(switch(_, _, Goals), PredId) :-
	cases_calls_pred_id(Goals, PredId).
goal_expr_calls_pred_id(if_then_else(_, Cond, Then, Else), PredId) :-
	(
		goal_calls_pred_id(Cond, PredId)
	;
		goal_calls_pred_id(Then, PredId)
	;
		goal_calls_pred_id(Else, PredId)
	).
goal_expr_calls_pred_id(not(Goal), PredId) :-
	goal_calls_pred_id(Goal, PredId).
goal_expr_calls_pred_id(some(_, _, Goal), PredId) :-
	goal_calls_pred_id(Goal, PredId).
goal_expr_calls_pred_id(call(PredId, _, _, _, _, _), PredId).

%-----------------------------------------------------------------------------%
 
goal_contains_reconstruction(_Goal - _) :-
	% This will only succeed on the alias branch with structure reuse.
	semidet_fail.
	%goal_expr_contains_reconstruction(Goal).

:- pred goal_expr_contains_reconstruction(hlds_goal_expr).
:- mode goal_expr_contains_reconstruction(in) is semidet.

goal_expr_contains_reconstruction(conj(Goals)) :-
	goals_contain_reconstruction(Goals).
goal_expr_contains_reconstruction(disj(Goals)) :-
	goals_contain_reconstruction(Goals).
goal_expr_contains_reconstruction(par_conj(Goals)) :-
	goals_contain_reconstruction(Goals).
goal_expr_contains_reconstruction(switch(_, _, Cases)) :-
	list__member(Case, Cases),
	Case = case(_, Goal),
 	goal_contains_reconstruction(Goal).
goal_expr_contains_reconstruction(if_then_else(_, Cond, Then, Else)) :-
 	goals_contain_reconstruction([Cond, Then, Else]).
goal_expr_contains_reconstruction(not(Goal)) :-
	goal_contains_reconstruction(Goal).
goal_expr_contains_reconstruction(some(_, _, Goal)) :-
	goal_contains_reconstruction(Goal).
goal_expr_contains_reconstruction(unify(_, _, _, Unify, _)) :-
	Unify = construct(_, _, _, _, HowToConstruct, _, _),
	HowToConstruct = reuse_cell(_).

:- pred goals_contain_reconstruction(list(hlds_goal)).
:- mode goals_contain_reconstruction(in) is semidet.

goals_contain_reconstruction(Goals) :-
	list__member(Goal, Goals),
	goal_contains_reconstruction(Goal).
 
%-----------------------------------------------------------------------------%

	% goal_contains_goal(Goal, SubGoal) is true iff Goal contains SubGoal,
	% i.e. iff Goal = SubGoal or Goal contains SubGoal as a direct
	% or indirect sub-goal.
	%
goal_contains_goal(Goal, Goal).
goal_contains_goal(Goal - _, SubGoal) :-
	direct_subgoal(Goal, DirectSubGoal),
	goal_contains_goal(DirectSubGoal, SubGoal).

	% direct_subgoal(Goal, SubGoal) is true iff SubGoal is
	% a direct sub-goal of Goal.
	%
direct_subgoal(some(_, _, Goal), Goal).
direct_subgoal(not(Goal), Goal).
direct_subgoal(if_then_else(_, If, Then, Else), Goal) :-
	( Goal = If
	; Goal = Then
	; Goal = Else
	).
direct_subgoal(conj(ConjList), Goal) :-
	list__member(Goal, ConjList).
direct_subgoal(par_conj(ConjList), Goal) :-
	list__member(Goal, ConjList).
direct_subgoal(disj(DisjList), Goal) :-
	list__member(Goal, DisjList).
direct_subgoal(switch(_, _, CaseList), Goal) :-
	list__member(Case, CaseList),
	Case = case(_, Goal).

%-----------------------------------------------------------------------------%

goal_util__switch_to_disjunction(_, [], _, [], VarSet, VarSet, 
		VarTypes, VarTypes, ModuleInfo, ModuleInfo).
goal_util__switch_to_disjunction(Var, [case(ConsId, Goal0) | Cases], InstMap, 
		[Goal | Goals], VarSet0, VarSet, VarTypes0, VarTypes, 
		ModuleInfo0, ModuleInfo) :-
	goal_util__case_to_disjunct(Var, ConsId, Goal0, InstMap, Goal, VarSet0,
		VarSet1, VarTypes0, VarTypes1, ModuleInfo0, ModuleInfo1),
	goal_util__switch_to_disjunction(Var, Cases, InstMap, Goals,
		VarSet1, VarSet, VarTypes1, VarTypes, ModuleInfo1, ModuleInfo).

goal_util__case_to_disjunct(Var, ConsId, CaseGoal, InstMap, Disjunct, VarSet0, 
		VarSet, VarTypes0, VarTypes, ModuleInfo0, ModuleInfo) :-
	cons_id_arity(ConsId, ConsArity),
	varset__new_vars(VarSet0, ConsArity, ArgVars, VarSet),
	map__lookup(VarTypes0, Var, VarType),
	type_util__get_cons_id_arg_types(ModuleInfo0,
		VarType, ConsId, ArgTypes),
	map__det_insert_from_corresponding_lists(VarTypes0, ArgVars,
		ArgTypes, VarTypes),
	instmap__lookup_var(InstMap, Var, Inst0),
	(
		inst_expand(ModuleInfo, Inst0, Inst1),
		get_arg_insts(Inst1, ConsId, ConsArity, ArgInsts1)
	->
		ArgInsts = ArgInsts1
	;
		error("goal_util__case_to_disjunct - get_arg_insts failed")
	),
	InstToUniMode =
		lambda([ArgInst::in, ArgUniMode::out] is det, (
			ArgUniMode = ((ArgInst - free) -> (ArgInst - ArgInst))
		)),
	list__map(InstToUniMode, ArgInsts, UniModes),
	UniMode = (Inst0 -> Inst0) - (Inst0 -> Inst0),
	UnifyContext = unify_context(explicit, []),
	Unification = deconstruct(Var, ConsId, ArgVars, UniModes,
			can_fail, no),
	ExtraGoal = unify(Var, functor(ConsId, no, ArgVars),
		UniMode, Unification, UnifyContext),
	set__singleton_set(NonLocals, Var),
	instmap_delta_init_reachable(ExtraInstMapDelta0),
	instmap_delta_bind_var_to_functor(Var, VarType, ConsId, InstMap,
		ExtraInstMapDelta0, ExtraInstMapDelta, 
		ModuleInfo0, ModuleInfo),
	goal_info_init(NonLocals, ExtraInstMapDelta, semidet, ExtraGoalInfo),

	% Conjoin the test and the rest of the case.
	goal_to_conj_list(CaseGoal, CaseGoalConj),
	GoalList = [ExtraGoal - ExtraGoalInfo | CaseGoalConj],

	% Work out the nonlocals, instmap_delta and determinism
	% of the entire conjunction.
	CaseGoal = _ - CaseGoalInfo,
	goal_info_get_nonlocals(CaseGoalInfo, CaseNonLocals0),
	set__insert(CaseNonLocals0, Var, CaseNonLocals),
	goal_info_get_instmap_delta(CaseGoalInfo, CaseInstMapDelta),
	instmap_delta_apply_instmap_delta(ExtraInstMapDelta, 
		CaseInstMapDelta, InstMapDelta),
	goal_info_get_determinism(CaseGoalInfo, CaseDetism0),
	det_conjunction_detism(semidet, CaseDetism0, Detism),
	goal_info_init(CaseNonLocals, InstMapDelta, Detism, CombinedGoalInfo),
	Disjunct = conj(GoalList) - CombinedGoalInfo.

%-----------------------------------------------------------------------------%

goal_util__if_then_else_to_disjunction(Cond0, Then, Else, GoalInfo, Goal) :-
	goal_util__compute_disjunct_goal_info(Cond0, Then, 
		GoalInfo, CondThenInfo),
	conj_list_to_goal([Cond0, Then], CondThenInfo, CondThen),

	Cond0 = _ - CondInfo0,
	goal_info_get_determinism(CondInfo0, CondDetism0),

	determinism_components(CondDetism0, CondCanFail0, CondMaxSoln0),

	% Add a commit inside the negation of the condition in the else branch
	% if the condition can succeed more than once.
	( CondMaxSoln0 = at_most_many ->
		CondMaxSoln = at_most_one,
		determinism_components(CondDetism, CondCanFail0, CondMaxSoln),
		goal_info_set_determinism(CondInfo0, CondDetism, CondInfo),
		Cond = some([], can_remove, Cond0) - CondInfo
	;
		CondDetism = CondDetism0,
		CondInfo = CondInfo0,
		Cond = Cond0
	),

	det_negation_det(CondDetism, MaybeNegCondDet),
	( MaybeNegCondDet = yes(NegCondDet1) ->
		NegCondDet = NegCondDet1
	;
		error("goal_util__if_then_else_to_disjunction: inappropriate determinism in a negation.")
	),
	determinism_components(NegCondDet, _, NegCondMaxSoln),
	( NegCondMaxSoln = at_most_zero ->
		instmap_delta_init_unreachable(NegCondDelta)
	;
		instmap_delta_init_reachable(NegCondDelta)
	),
	goal_info_get_nonlocals(CondInfo, CondNonLocals),
	goal_info_init(CondNonLocals, NegCondDelta, NegCondDet, NegCondInfo),

	goal_util__compute_disjunct_goal_info(not(Cond) - NegCondInfo, Else, 
		GoalInfo, NegCondElseInfo),
	conj_list_to_goal([not(Cond) - NegCondInfo, Else], 
		NegCondElseInfo, NegCondElse),
	Goal = disj([CondThen, NegCondElse]).

	% Compute a hlds_goal_info for a pair of conjoined goals.
:- pred goal_util__compute_disjunct_goal_info(hlds_goal::in, hlds_goal::in, 
		hlds_goal_info::in, hlds_goal_info::out) is det.

goal_util__compute_disjunct_goal_info(Goal1, Goal2, GoalInfo, CombinedInfo) :-
	Goal1 = _ - GoalInfo1,
	Goal2 = _ - GoalInfo2,

	goal_info_get_nonlocals(GoalInfo1, NonLocals1),
	goal_info_get_nonlocals(GoalInfo2, NonLocals2),
	goal_info_get_nonlocals(GoalInfo, OuterNonLocals),
	set__union(NonLocals1, NonLocals2, CombinedNonLocals0),
	set__intersect(CombinedNonLocals0, OuterNonLocals, CombinedNonLocals),

	goal_info_get_instmap_delta(GoalInfo1, Delta1),
	goal_info_get_instmap_delta(GoalInfo2, Delta2),
	instmap_delta_apply_instmap_delta(Delta1, Delta2, CombinedDelta0),
	instmap_delta_restrict(CombinedDelta0, OuterNonLocals, CombinedDelta),

	goal_info_get_determinism(GoalInfo1, Detism1),
	goal_info_get_determinism(GoalInfo2, Detism2),
	det_conjunction_detism(Detism1, Detism2, CombinedDetism),

	goal_info_init(CombinedNonLocals, CombinedDelta, 
		CombinedDetism, CombinedInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


goal_util__can_reorder_goals(ModuleInfo, VarTypes, FullyStrict,
		InstmapBeforeEarlierGoal, EarlierGoal, InstmapBeforeLaterGoal,
		LaterGoal) :-

	EarlierGoal = _ - EarlierGoalInfo,
	LaterGoal = _ - LaterGoalInfo,

		% Impure goals cannot be reordered.
	\+ goal_info_is_impure(EarlierGoalInfo),
	\+ goal_info_is_impure(LaterGoalInfo),

	goal_util__reordering_maintains_termination(ModuleInfo, FullyStrict, 
		EarlierGoal, LaterGoal),

	%
	% Don't reorder the goals if the later goal depends
	% on the outputs of the current goal.
	%
	\+ goal_depends_on_earlier_goal(LaterGoal, EarlierGoal,
			InstmapBeforeEarlierGoal, VarTypes, ModuleInfo),

	%
	% Don't reorder the goals if the later goal changes the 
	% instantiatedness of any of the non-locals of the earlier
	% goal. This is necessary if the later goal clobbers any 
	% of the non-locals of the earlier goal, and avoids rerunning
	% full mode analysis in other cases.
	%
	\+ goal_depends_on_earlier_goal(EarlierGoal, LaterGoal, 
			InstmapBeforeLaterGoal, VarTypes, ModuleInfo).


goal_util__reordering_maintains_termination(ModuleInfo, FullyStrict, 
		EarlierGoal, LaterGoal) :-
	EarlierGoal = _ - EarlierGoalInfo,
	LaterGoal = _ - LaterGoalInfo,

	goal_info_get_determinism(EarlierGoalInfo, EarlierDetism),
	determinism_components(EarlierDetism, EarlierCanFail, _),
	goal_info_get_determinism(LaterGoalInfo, LaterDetism),
	determinism_components(LaterDetism, LaterCanFail, _),

		% If --fully-strict was specified, don't convert 
		% (can_loop, can_fail) into (can_fail, can_loop). 
	( 
		FullyStrict = yes, 
		\+ goal_cannot_loop(ModuleInfo, EarlierGoal)
	->
		LaterCanFail = cannot_fail
	;
		true
	),
		% Don't convert (can_fail, can_loop) into 
		% (can_loop, can_fail), since this could worsen 
		% the termination properties of the program.
	( EarlierCanFail = can_fail ->
		goal_cannot_loop(ModuleInfo, LaterGoal)
	;
		true
	).

	%
	% If the earlier goal changes the instantiatedness of a variable
	% that is used in the later goal, then the later goal depends on
	% the earlier goal.
	%
	% This code does work on the alias branch.
	%
:- pred goal_depends_on_earlier_goal(hlds_goal::in, hlds_goal::in, instmap::in,
		vartypes::in, module_info::in) is semidet.

goal_depends_on_earlier_goal(_ - LaterGoalInfo, _ - EarlierGoalInfo,
		InstMapBeforeEarlierGoal, VarTypes, ModuleInfo) :-
	goal_info_get_instmap_delta(EarlierGoalInfo, EarlierInstMapDelta),
	instmap__apply_instmap_delta(InstMapBeforeEarlierGoal,
			EarlierInstMapDelta, InstMapAfterEarlierGoal),

	instmap_changed_vars(InstMapBeforeEarlierGoal, InstMapAfterEarlierGoal,
			VarTypes, ModuleInfo, EarlierChangedVars),

	goal_info_get_nonlocals(LaterGoalInfo, LaterGoalNonLocals),
	set__intersect(EarlierChangedVars, LaterGoalNonLocals, Intersection),
	not set__empty(Intersection).

%-----------------------------------------------------------------------------%

goal_util__generate_simple_call(ModuleName, PredName, Args, Detism,
		MaybeFeature, InstMap, Module, Context, CallGoal) :-
	list__length(Args, Arity),
	module_info_get_predicate_table(Module, PredTable),
	(
		predicate_table_search_pred_m_n_a(PredTable,
			ModuleName, PredName, Arity,
			[PredId0])
	->
		PredId = PredId0
	;
		% Some of the table builtins are polymorphic,
		% and for them we need to subtract one from the arity
		% to take into account the type_info argument.
		predicate_table_search_pred_m_n_a(PredTable,
			ModuleName, PredName, Arity - 1,
			[PredId0])
	->
		PredId = PredId0
	;
		string__int_to_string(Arity, ArityS),
		string__append_list(["can't locate ", PredName,
			"/", ArityS], ErrorMessage),
		error(ErrorMessage)
	),
	module_info_pred_info(Module, PredId, PredInfo),
	(
		pred_info_procids(PredInfo, [ProcId0])
	->
		ProcId = ProcId0
	;
		string__int_to_string(Arity, ArityS),
		string__append_list(["too many modes for pred ",
			PredName, "/", ArityS], ErrorMessage),
		error(ErrorMessage)

	),
	Call = call(PredId, ProcId, Args, not_builtin, no,
		qualified(ModuleName, PredName)),
	set__init(NonLocals0),
	set__insert_list(NonLocals0, Args, NonLocals),
	determinism_components(Detism, _CanFail, NumSolns),
	(
		NumSolns = at_most_zero
	->
		instmap_delta_init_unreachable(InstMapDelta)
	;
		instmap_delta_from_assoc_list(InstMap, InstMapDelta)
	),
	goal_info_init(NonLocals, InstMapDelta, Detism, Context,
		CallGoalInfo0),
	(
		MaybeFeature = yes(Feature),
		goal_info_add_feature(CallGoalInfo0, Feature, CallGoalInfo)
	;
		MaybeFeature = no,
		CallGoalInfo = CallGoalInfo0
	),
	CallGoal = Call - CallGoalInfo.

%-----------------------------------------------------------------------------%

predids_from_goals(Goals, PredIds) :- 
	(
		Goals = [],
		PredIds = []
	;
		Goals = [Goal | Rest],
		predids_from_goal(Goal, PredIds0),
		predids_from_goals(Rest, PredIds1),
		PredIds = PredIds0 ++ PredIds1
	).

predids_from_goal(Goal, PredIds) :-
                % Explicit lambda expression needed since
		% goal_calls_pred_id has multiple modes.
	P = (pred(PredId::out) is nondet :- goal_calls_pred_id(Goal, PredId)),
	solutions(P, PredIds).

%-----------------------------------------------------------------------------%

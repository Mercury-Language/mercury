%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2004 The University of Melbourne.
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

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module hlds__instmap.
:- import_module parse_tree__prog_data.

:- import_module assoc_list, bool, list, set, map, term.

% The predicates rename_var* take a structure and a mapping from var -> var
% and apply that translation. If a var in the input structure does not
% occur as a key in the mapping, then the variable is left unsubstituted.

	% goal_util__rename_vars_in_goals(GoalList, MustRename, Substitution,
	%	NewGoalList).
:- pred goal_util__rename_vars_in_goals(list(hlds_goal)::in, bool::in,
	map(prog_var, prog_var)::in, list(hlds_goal)::out) is det.

:- pred goal_util__rename_vars_in_goal(hlds_goal::in,
	map(prog_var, prog_var)::in, hlds_goal::out) is det.

:- pred goal_util__must_rename_vars_in_goal(hlds_goal::in,
	map(prog_var, prog_var)::in, hlds_goal::out) is det.

:- pred goal_util__rename_vars_in_var_set(set(prog_var)::in, bool::in,
	map(prog_var, prog_var)::in, set(prog_var)::out) is det.

:- pred goal_util__rename_var_list(list(var(T))::in, bool::in,
	map(var(T), var(T))::in, list(var(T))::out) is det.

:- pred goal_util__rename_var(var(V)::in, bool::in, map(var(V), var(V))::in,
	var(V)::out) is det.

% goal_util__create_variables takes a list of variables, a varset, and map
% from vars to types and an initial substitution, and creates new instances
% of each of the source variables in the substitution, adding each new
% variable to the varset and the var types map. The name and type of each new
% variable is found by looking up
% in the type map given in the 5th argument - the last input.
% (This interface will not easily admit uniqueness in the type map for this
% reason - such is the sacrifice for generality.)

	% goal_util__create_variables(OldVariables, OldVarset, InitialVarTypes,
	%	InitialSubstitution, OldVarTypes, OldVarNames,  NewVarset,
	%	NewVarTypes, NewSubstitution)
:- pred goal_util__create_variables(list(prog_var)::in, prog_varset::in,
	map(prog_var, type)::in, prog_varset::in, prog_varset::out,
	map(prog_var, type)::in, map(prog_var, type)::out,
	map(prog_var, prog_var)::in, map(prog_var, prog_var)::out) is det.

	% Return all the variables in the goal.
	% Unlike quantification:goal_vars, this predicate returns
	% even the explicitly quantified variables.
:- pred goal_util__goal_vars(hlds_goal::in, set(prog_var)::out) is det.

	% Return all the variables in the list of goals.
	% Unlike quantification:goal_vars, this predicate returns
	% even the explicitly quantified variables.
:- pred goal_util__goals_goal_vars(list(hlds_goal)::in, set(prog_var)::in,
	set(prog_var)::out) is det.

	% Return all the variables in a generic call.
:- pred goal_util__generic_call_vars(generic_call::in, list(prog_var)::out)
	is det.

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
:- pred goal_util__extra_nonlocal_typeinfos(map(tvar, type_info_locn)::in,
	map(class_constraint, prog_var)::in, map(prog_var, type)::in,
	existq_tvars::in, set(prog_var)::in, set(prog_var)::out) is det.

	% See whether the goal is a branched structure.
:- pred goal_util__goal_is_branched(hlds_goal_expr::in) is semidet.

	% Return an indication of the size of the goal.
:- pred goal_size(hlds_goal::in, int::out) is det.

	% Return an indication of the size of the list of goals.
:- pred goals_size(list(hlds_goal)::in, int::out) is det.

	% Return an indication of the size of the list of clauses.
:- pred clause_list_size(list(clause)::in, int::out) is det.

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
:- pred goal_contains_reconstruction(hlds_goal::in) is semidet.

	% goal_contains_goal(Goal, SubGoal) is true iff Goal contains SubGoal,
	% i.e. iff Goal = SubGoal or Goal contains SubGoal as a direct
	% or indirect sub-goal.
	%
:- pred goal_contains_goal(hlds_goal::in, hlds_goal::out) is multi.

	% direct_subgoal(Goal, DirectSubGoal) is true iff DirectSubGoal is
	% a direct sub-goal of Goal.
	%
:- pred direct_subgoal(hlds_goal_expr::in, hlds_goal::out) is nondet.

	% returns all the predids that are used within a goal
:- pred predids_from_goal(hlds_goal::in, list(pred_id)::out) is det.

	% returns all the predids that are used in a list of goals
:- pred predids_from_goals(list(hlds_goal)::in, list(pred_id)::out) is det.

%-----------------------------------------------------------------------------%

	% Convert a switch back into a disjunction. This is needed
	% for the magic set transformation.
	% This aborts if any of the constructors are existentially typed.
:- pred goal_util__switch_to_disjunction(prog_var::in, list(case)::in,
	instmap::in, list(hlds_goal)::out, prog_varset::in, prog_varset::out,
	map(prog_var, type)::in, map(prog_var, type)::out,
	module_info::in, module_info::out) is det.

	% Convert a case into a conjunction by adding a tag test
	% (deconstruction unification) to the case goal.
	% This aborts if the constructor is existentially typed.
:- pred goal_util__case_to_disjunct(prog_var::in, cons_id::in, hlds_goal::in,
	instmap::in, hlds_goal::out, prog_varset::in, prog_varset::out,
	map(prog_var, type)::in, map(prog_var, type)::out,
	module_info::in, module_info::out) is det.

	% Transform an if-then-else into ( Cond, Then ; \+ Cond, Else ),
	% since magic.m and rl_gen.m don't handle if-then-elses.
:- pred goal_util__if_then_else_to_disjunction(hlds_goal::in, hlds_goal::in,
	hlds_goal::in, hlds_goal_info::in, hlds_goal_expr::out) is det.

%-----------------------------------------------------------------------------%

	% goal_util__can_reorder_goals(ModuleInfo, FullyStrict, Goal1, Goal2).
	%
	% Goals can be reordered if
	% - the goals are independent
	% - the goals are not impure
	% - any possible change in termination behaviour is allowed
	%	according to the semantics options.
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

	% generate_simple_call(ModuleName, ProcName, PredOrFunc, ModeNo,
	%	Detism, Args, Features, InstMapDelta, ModuleInfo, Context,
	%	CallGoal):
	%
	% Generate a call to a builtin procedure (e.g. from the private_builtin
	% or table_builtin module). This is used by HLDS->HLDS transformation
	% passes that introduce calls to builtin procedures.
	%
	% If ModeNo = only_mode then the predicate must have exactly one
	% procedure (an error is raised if this is not the case.)
	%
	% If ModeNo = mode_no(N) then the Nth procedure is used, counting
	% from 0.
	%
:- pred goal_util__generate_simple_call(module_name::in, string::in,
	pred_or_func::in, mode_no::in, determinism::in, list(prog_var)::in,
	list(goal_feature)::in, assoc_list(prog_var, inst)::in,
	module_info::in, term__context::in, hlds_goal::out) is det.

	% generate_foreign_proc(ModuleName, ProcName, PredOrFunc,
	%	ModeNo, Detism, Attributes, Args, ExtraArgs, PrefixCode, Code,
	%	SuffixCode, Features, InstMapDelta, ModuleInfo, Context,
	%	CallGoal):
	%
	% generate_foreign_proc is similar to generate_simple_call,
	% but also assumes that the called predicate is defined via a
	% foreign_proc, that the foreign_proc's arguments are as given in
	% Args, its attributes are Attributes, and its code is Code.
	% As well as returning a foreign_code instead of a call, effectively
	% inlining the call, generate_foreign_proc also puts PrefixCode
	% before Code, SuffixCode after Code, and passes ExtraArgs as well
	% as Args.
	%
:- pred goal_util__generate_foreign_proc(module_name::in, string::in,
	pred_or_func::in, mode_no::in, determinism::in,
	pragma_foreign_proc_attributes::in,
	list(foreign_arg)::in, list(foreign_arg)::in, string::in, string::in,
	string::in, list(goal_feature)::in, assoc_list(prog_var, inst)::in,
	module_info::in, term__context::in, hlds_goal::out) is det.

:- pred goal_util__generate_unsafe_cast(prog_var::in, prog_var::in,
	prog_context::in, hlds_goal::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__det_analysis.
:- import_module check_hlds__inst_match.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__purity.
:- import_module check_hlds__type_util.
:- import_module hlds__goal_form.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_llds.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_util.

:- import_module int, string, require, varset, std_util.

%-----------------------------------------------------------------------------%

goal_util__create_variables([], _OldVarNames, _OldVarTypes,
		!Varset, !VarTypes, !Subn).
goal_util__create_variables([V | Vs], OldVarNames, OldVarTypes,
		!Varset, !VarTypes, !Subn) :-
	( map__contains(!.Subn, V) ->
		true
	;
		varset__new_var(!.Varset, NV, !:Varset),
		( varset__search_name(OldVarNames, V, Name) ->
			varset__name_var(!.Varset, NV, Name, !:Varset)
		;
			true
		),
		map__det_insert(!.Subn, V, NV, !:Subn),
		( map__search(OldVarTypes, V, VT) ->
			map__set(!.VarTypes, NV, VT, !:VarTypes)
		;
			true
		)
	),
	goal_util__create_variables(Vs, OldVarNames, OldVarTypes,
		!Varset, !VarTypes, !Subn).

%-----------------------------------------------------------------------------%

:- pred goal_util__init_subn(assoc_list(prog_var, prog_var)::in,
	map(prog_var, prog_var)::in, map(prog_var, prog_var)::out) is det.

goal_util__init_subn([], !Subn).
goal_util__init_subn([A - H | Vs], !Subn) :-
	map__set(!.Subn, H, A, !:Subn),
	goal_util__init_subn(Vs, !Subn).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_var_pair_list(assoc_list(prog_var, T)::in, bool::in,
	map(prog_var, prog_var)::in, list(pair(prog_var, T))::out) is det.

goal_util__rename_var_pair_list([], _Must, _Subn, []).
goal_util__rename_var_pair_list([V - D | VDs], Must, Subn, [N - D | NDs]) :-
	goal_util__rename_var(V, Must, Subn, N),
	goal_util__rename_var_pair_list(VDs, Must, Subn, NDs).

goal_util__rename_var_list([], _Must, _Subn, []).
goal_util__rename_var_list([V | Vs], Must, Subn, [N | Ns]) :-
	goal_util__rename_var(V, Must, Subn, N),
	goal_util__rename_var_list(Vs, Must, Subn, Ns).

goal_util__rename_var(V, Must, Subn, N) :-
	( map__search(Subn, V, N0) ->
		N = N0
	;
		(
			Must = no,
			N = V
		;
			Must = yes,
			term__var_to_int(V, VInt),
			string__format("goal_util__rename_var: " ++
				"no substitute for var %i", [i(VInt)], Msg),
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

:- pred goal_util__rename_vars_in_goal(hlds_goal::in, bool::in,
	map(prog_var, prog_var)::in, hlds_goal::out) is det.

goal_util__rename_vars_in_goal(Goal0 - GoalInfo0, Must, Subn,
		Goal - GoalInfo) :-
	goal_util__rename_vars_in_goal_expr(Goal0, Must, Subn, Goal),
	goal_util__rename_vars_in_goal_info(GoalInfo0, Must, Subn, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_vars_in_goal_expr(hlds_goal_expr::in, bool::in,
	map(prog_var, prog_var)::in, hlds_goal_expr::out) is det.

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

goal_util__rename_vars_in_goal_expr(unify(LHS0, RHS0, Mode, Unify0, Context),
		Must, Subn, unify(LHS, RHS, Mode, Unify, Context)) :-
	goal_util__rename_var(LHS0, Must, Subn, LHS),
	goal_util__rename_unify_rhs(RHS0, Must, Subn, RHS),
	goal_util__rename_unify(Unify0, Must, Subn, Unify).

goal_util__rename_vars_in_goal_expr(foreign_proc(A,B,C,Args0,Extra0,F),
		Must, Subn, foreign_proc(A,B,C,Args,Extra,F)) :-
	goal_util__rename_arg_list(Args0, Must, Subn, Args),
	goal_util__rename_arg_list(Extra0, Must, Subn, Extra).

goal_util__rename_vars_in_goal_expr(shorthand(ShorthandGoal0), Must, Subn,
		shorthand(ShrothandGoal)) :-
	goal_util__rename_vars_in_shorthand(ShorthandGoal0, Must, Subn,
		ShrothandGoal).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_vars_in_shorthand(shorthand_goal_expr::in, bool::in,
	map(prog_var, prog_var)::in, shorthand_goal_expr::out) is det.

goal_util__rename_vars_in_shorthand(bi_implication(LHS0, RHS0), Must, Subn,
		bi_implication(LHS, RHS)) :-
	goal_util__rename_vars_in_goal(LHS0, Must, Subn, LHS),
	goal_util__rename_vars_in_goal(RHS0, Must, Subn, RHS).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_arg_list(list(foreign_arg)::in, bool::in,
	map(prog_var, prog_var)::in, list(foreign_arg)::out) is det.

goal_util__rename_arg_list([], _Must, _Subn, []).
goal_util__rename_arg_list([Arg0 | Args0], Must, Subn, [Arg | Args]) :-
	goal_util__rename_arg(Arg0, Must, Subn, Arg),
	goal_util__rename_arg_list(Args0, Must, Subn, Args).

:- pred goal_util__rename_arg(foreign_arg::in, bool::in,
	map(prog_var, prog_var)::in, foreign_arg::out) is det.

goal_util__rename_arg(foreign_arg(Var0, B, C), Must, Subn,
		foreign_arg(Var, B, C)) :-
	goal_util__rename_var(Var0, Must, Subn, Var).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_vars_in_cases(list(case)::in, bool::in,
	map(prog_var, prog_var)::in, list(case)::out) is det.

goal_util__rename_vars_in_cases([], _Must, _Subn, []).
goal_util__rename_vars_in_cases([case(Cons, G0) | Gs0], Must, Subn,
		[case(Cons, G) | Gs]) :-
	goal_util__rename_vars_in_goal(G0, Must, Subn, G),
	goal_util__rename_vars_in_cases(Gs0, Must, Subn, Gs).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_unify_rhs(unify_rhs::in, bool::in,
	map(prog_var, prog_var)::in, unify_rhs::out) is det.

goal_util__rename_unify_rhs(var(Var0), Must, Subn, var(Var)) :-
	goal_util__rename_var(Var0, Must, Subn, Var).
goal_util__rename_unify_rhs(functor(Functor, E, ArgVars0), Must, Subn,
		functor(Functor, E, ArgVars)) :-
	goal_util__rename_var_list(ArgVars0, Must, Subn, ArgVars).
goal_util__rename_unify_rhs(
		lambda_goal(Purity, PredOrFunc, EvalMethod, FixModes,
			NonLocals0, Vars0, Modes, Det, Goal0),
		Must, Subn,
		lambda_goal(Purity, PredOrFunc, EvalMethod, FixModes,
			NonLocals, Vars, Modes, Det, Goal)) :-
	goal_util__rename_var_list(NonLocals0, Must, Subn, NonLocals),
	goal_util__rename_var_list(Vars0, Must, Subn, Vars),
	goal_util__rename_vars_in_goal(Goal0, Must, Subn, Goal).

:- pred goal_util__rename_unify(unification::in, bool::in,
	map(prog_var, prog_var)::in, unification::out) is det.

goal_util__rename_unify(
		construct(Var0, ConsId, Vars0, Modes, How0, Uniq, Size),
		Must, Subn,
		construct(Var, ConsId, Vars, Modes, How, Uniq, Size)) :-
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

:- pred goal_util__rename_generic_call(generic_call::in, bool::in,
	map(prog_var, prog_var)::in, generic_call::out) is det.

goal_util__rename_generic_call(higher_order(Var0, Purity, PredOrFunc, Arity),
		Must, Subn, higher_order(Var, Purity, PredOrFunc, Arity)) :-
	goal_util__rename_var(Var0, Must, Subn, Var).
goal_util__rename_generic_call(class_method(Var0, Method, ClassId, MethodId),
		Must, Subn, class_method(Var, Method, ClassId, MethodId)) :-
	goal_util__rename_var(Var0, Must, Subn, Var).
goal_util__rename_generic_call(unsafe_cast, _, _, unsafe_cast).
goal_util__rename_generic_call(aditi_builtin(Builtin, PredCallId),
		_Must, _Subn, aditi_builtin(Builtin, PredCallId)).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_var_maps(map(prog_var, T)::in, bool::in,
	map(prog_var, prog_var)::in, map(prog_var, T)::out) is det.

goal_util__rename_var_maps(Map0, Must, Subn, Map) :-
	map__to_assoc_list(Map0, AssocList0),
	goal_util__rename_var_maps_2(AssocList0, Must, Subn, AssocList),
	map__from_assoc_list(AssocList, Map).

:- pred goal_util__rename_var_maps_2(assoc_list(var(V), T)::in,
	bool::in, map(var(V), var(V))::in, assoc_list(var(V), T)::out) is det.

goal_util__rename_var_maps_2([], _Must, _Subn, []).
goal_util__rename_var_maps_2([V - L | Vs], Must, Subn, [N - L | Ns]) :-
	goal_util__rename_var(V, Must, Subn, N),
	goal_util__rename_var_maps_2(Vs, Must, Subn, Ns).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_vars_in_goal_info(hlds_goal_info::in,
	bool::in, map(prog_var, prog_var)::in, hlds_goal_info::out) is det.

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
	goal_util__goal_vars_2(Goal, set__init, Set).

:- pred goal_util__goal_vars_2(hlds_goal_expr::in,
	set(prog_var)::in, set(prog_var)::out) is det.

goal_util__goal_vars_2(unify(Var, RHS, _, Unif, _), !Set) :-
	set__insert(!.Set, Var, !:Set),
	( Unif = construct(_, _, _, _, CellToReuse, _, _) ->
		( CellToReuse = reuse_cell(cell_to_reuse(Var, _, _)) ->
			set__insert(!.Set, Var, !:Set)
		;
			true
		)
	;
		true
	),
	goal_util__rhs_goal_vars(RHS, !Set).

goal_util__goal_vars_2(generic_call(GenericCall, ArgVars, _, _), !Set) :-
	goal_util__generic_call_vars(GenericCall, Vars0),
	set__insert_list(!.Set, Vars0, !:Set),
	set__insert_list(!.Set, ArgVars, !:Set).

goal_util__goal_vars_2(call(_, _, ArgVars, _, _, _), !Set) :-
	set__insert_list(!.Set, ArgVars, !:Set).

goal_util__goal_vars_2(conj(Goals), !Set) :-
	goal_util__goals_goal_vars(Goals, !Set).

goal_util__goal_vars_2(par_conj(Goals), !Set) :-
	goal_util__goals_goal_vars(Goals, !Set).

goal_util__goal_vars_2(disj(Goals), !Set) :-
	goal_util__goals_goal_vars(Goals, !Set).

goal_util__goal_vars_2(switch(Var, _Det, Cases), !Set) :-
	set__insert(!.Set, Var, !:Set),
	goal_util__cases_goal_vars(Cases, !Set).

goal_util__goal_vars_2(some(Vars, _, Goal - _), !Set) :-
	set__insert_list(!.Set, Vars, !:Set),
	goal_util__goal_vars_2(Goal, !Set).

goal_util__goal_vars_2(not(Goal - _GoalInfo), !Set) :-
	goal_util__goal_vars_2(Goal, !Set).

goal_util__goal_vars_2(if_then_else(Vars, A - _, B - _, C - _), !Set) :-
	set__insert_list(!.Set, Vars, !:Set),
	goal_util__goal_vars_2(A, !Set),
	goal_util__goal_vars_2(B, !Set),
	goal_util__goal_vars_2(C, !Set).

goal_util__goal_vars_2(foreign_proc(_, _, _, Args, ExtraArgs, _), !Set) :-
	ArgVars = list__map(foreign_arg_var, Args),
	ExtraVars = list__map(foreign_arg_var, ExtraArgs),
	set__insert_list(!.Set, list__append(ArgVars, ExtraVars), !:Set).

goal_util__goal_vars_2(shorthand(ShorthandGoal), !Set) :-
	goal_util__goal_vars_2_shorthand(ShorthandGoal, !Set).

:- pred goal_util__goal_vars_2_shorthand(shorthand_goal_expr::in,
	set(prog_var)::in, set(prog_var)::out) is det.

goal_util__goal_vars_2_shorthand(bi_implication(LHS - _, RHS - _), !Set) :-
	goal_util__goal_vars_2(LHS, !Set),
	goal_util__goal_vars_2(RHS, !Set).

goal_util__goals_goal_vars([], !Set).
goal_util__goals_goal_vars([Goal - _ | Goals], !Set) :-
	goal_util__goal_vars_2(Goal, !Set),
	goal_util__goals_goal_vars(Goals, !Set).

:- pred goal_util__cases_goal_vars(list(case)::in,
	set(prog_var)::in, set(prog_var)::out) is det.

goal_util__cases_goal_vars([], !Set).
goal_util__cases_goal_vars([case(_, Goal - _) | Cases], !Set) :-
	goal_util__goal_vars_2(Goal, !Set),
	goal_util__cases_goal_vars(Cases, !Set).

:- pred goal_util__rhs_goal_vars(unify_rhs::in,
	set(prog_var)::in, set(prog_var)::out) is det.

goal_util__rhs_goal_vars(RHS, !Set) :-
	RHS = var(X),
	set__insert(!.Set, X, !:Set).
goal_util__rhs_goal_vars(RHS, !Set) :-
	RHS = functor(_Functor, _, ArgVars),
	set__insert_list(!.Set, ArgVars, !:Set).
goal_util__rhs_goal_vars(RHS, !Set) :-
	RHS = lambda_goal(_, _, _, _, NonLocals, LambdaVars, _, _, Goal - _),
	set__insert_list(!.Set, NonLocals, !:Set),
	set__insert_list(!.Set, LambdaVars, !:Set),
	goal_util__goal_vars_2(Goal, !Set).

goal_util__generic_call_vars(higher_order(Var, _, _, _), [Var]).
goal_util__generic_call_vars(class_method(Var, _, _, _), [Var]).
goal_util__generic_call_vars(unsafe_cast, []).
goal_util__generic_call_vars(aditi_builtin(_, _), []).

%-----------------------------------------------------------------------------%

goal_util__extra_nonlocal_typeinfos(TypeVarMap, TypeClassVarMap, VarTypes,
		ExistQVars, NonLocals, NonLocalTypeInfos) :-
	set__to_sorted_list(NonLocals, NonLocalsList),
	map__apply_to_list(NonLocalsList, VarTypes, NonLocalsTypes),
	term__vars_list(NonLocalsTypes, NonLocalTypeVars),
		% Find all the type-infos and typeclass-infos that are
		% non-local
	solutions_set((pred(Var::out) is nondet :-
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
			(
				map__search(TypeVarMap, TypeVar, Location),
				type_info_locn_var(Location, Var)
			;
				% this is probably not very efficient...
				map__member(TypeClassVarMap, Constraint, Var),
				Constraint = constraint(_Name, ArgTypes),
				term__contains_var_list(ArgTypes, TypeVar)
			)
		), NonLocalTypeInfos).

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
	Size = Size1 + Size2.

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

:- pred cases_size(list(case)::in, int::out) is det.

cases_size([], 0).
cases_size([case(_, Goal) | Cases], Size) :-
	goal_size(Goal, Size1),
	cases_size(Cases, Size2),
	Size = Size1 + Size2.

:- pred goal_expr_size(hlds_goal_expr::in, int::out) is det.

goal_expr_size(conj(Goals), Size) :-
	goals_size(Goals, Size).
goal_expr_size(par_conj(Goals), Size) :-
	goals_size(Goals, Size1),
	Size = Size1 + 1.
goal_expr_size(disj(Goals), Size) :-
	goals_size(Goals, Size1),
	Size = Size1 + 1.
goal_expr_size(switch(_, _, Goals), Size) :-
	cases_size(Goals, Size1),
	Size = Size1 + 1.
goal_expr_size(if_then_else(_, Cond, Then, Else), Size) :-
	goal_size(Cond, Size1),
	goal_size(Then, Size2),
	goal_size(Else, Size3),
	Size = Size1 + Size2 + Size3 + 1.
goal_expr_size(not(Goal), Size) :-
	goal_size(Goal, Size1),
	Size = Size1 + 1.
goal_expr_size(some(_, _, Goal), Size) :-
	goal_size(Goal, Size1),
	Size = Size1 + 1.
goal_expr_size(call(_, _, _, _, _, _), 1).
goal_expr_size(generic_call(_, _, _, _), 1).
goal_expr_size(unify(_, _, _, _, _), 1).
goal_expr_size(foreign_proc(_, _, _, _, _, _), 1).
goal_expr_size(shorthand(ShorthandGoal), Size) :-
	goal_expr_size_shorthand(ShorthandGoal, Size).

:- pred goal_expr_size_shorthand(shorthand_goal_expr::in, int::out) is det.

goal_expr_size_shorthand(bi_implication(LHS, RHS), Size) :-
	goal_size(LHS, Size1),
	goal_size(RHS, Size2),
	Size = Size1 + Size2 + 1.

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

:- pred goal_expr_contains_reconstruction(hlds_goal_expr::in) is semidet.

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

:- pred goals_contain_reconstruction(list(hlds_goal)::in) is semidet.

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

goal_util__switch_to_disjunction(_, [], _, [],
		!VarSet, !VarTypes, !ModuleInfo).
goal_util__switch_to_disjunction(Var, [case(ConsId, Goal0) | Cases], InstMap,
		[Goal | Goals], !VarSet, !VarTypes, !ModuleInfo) :-
	goal_util__case_to_disjunct(Var, ConsId, Goal0, InstMap, Goal,
		!VarSet, !VarTypes, !ModuleInfo),
	goal_util__switch_to_disjunction(Var, Cases, InstMap, Goals,
		!VarSet, !VarTypes, !ModuleInfo).

goal_util__case_to_disjunct(Var, ConsId, CaseGoal, InstMap, Disjunct,
		!VarSet, !VarTypes, !ModuleInfo) :-
	ConsArity = cons_id_arity(ConsId),
	varset__new_vars(!.VarSet, ConsArity, ArgVars, !:VarSet),
	map__lookup(!.VarTypes, Var, VarType),
	type_util__get_cons_id_arg_types(!.ModuleInfo,
		VarType, ConsId, ArgTypes),
	map__det_insert_from_corresponding_lists(!.VarTypes, ArgVars,
		ArgTypes, !:VarTypes),
	instmap__lookup_var(InstMap, Var, Inst0),
	(
		inst_expand(!.ModuleInfo, Inst0, Inst1),
		get_arg_insts(Inst1, ConsId, ConsArity, ArgInsts1)
	->
		ArgInsts = ArgInsts1
	;
		error("goal_util__case_to_disjunct - get_arg_insts failed")
	),
	InstToUniMode = (pred(ArgInst::in, ArgUniMode::out) is det :-
			ArgUniMode = ((ArgInst - free) -> (ArgInst - ArgInst))
		),
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
		ExtraInstMapDelta0, ExtraInstMapDelta, !ModuleInfo),
	goal_info_init(NonLocals, ExtraInstMapDelta,
		semidet, pure, ExtraGoalInfo),

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
	infer_goal_info_purity(CaseGoalInfo, CasePurity),
	goal_info_init(CaseNonLocals, InstMapDelta,
		Detism, CasePurity, CombinedGoalInfo),
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
	infer_goal_info_purity(CondInfo, CondPurity),
	goal_info_init(CondNonLocals, NegCondDelta, NegCondDet, CondPurity,
		NegCondInfo),

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

	goal_list_purity([Goal1, Goal2], CombinedPurity),

	goal_info_init(CombinedNonLocals, CombinedDelta,
		CombinedDetism, CombinedPurity, CombinedInfo).

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
		\+ goal_cannot_loop_or_throw(EarlierGoal)
	->
		LaterCanFail = cannot_fail
	;
		true
	),
		% Don't convert (can_fail, can_loop) into
		% (can_loop, can_fail), since this could worsen
		% the termination properties of the program.
	( EarlierCanFail = can_fail ->
		goal_cannot_loop_or_throw(ModuleInfo, LaterGoal)
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

goal_util__generate_simple_call(ModuleName, ProcName, PredOrFunc, ModeNo,
		Detism, Args, Features, InstMap, ModuleInfo, Context,
		Goal) :-
	list__length(Args, Arity),
	lookup_builtin_pred_proc_id(ModuleInfo, ModuleName, ProcName,
		PredOrFunc, Arity, ModeNo, PredId, ProcId),

	% builtin_state only uses this to work out whether
	% this is the "recursive" clause generated for the compiler
	% for each builtin, so an invalid pred_id won't cause problems.
	InvalidPredId = invalid_pred_id,
	BuiltinState = builtin_state(ModuleInfo, InvalidPredId,
		PredId, ProcId),

	GoalExpr = call(PredId, ProcId, Args, BuiltinState, no,
		qualified(ModuleName, ProcName)),
	set__init(NonLocals0),
	set__insert_list(NonLocals0, Args, NonLocals),
	determinism_components(Detism, _CanFail, NumSolns),
	( NumSolns = at_most_zero ->
		instmap_delta_init_unreachable(InstMapDelta)
	;
		instmap_delta_from_assoc_list(InstMap, InstMapDelta)
	),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_get_purity(PredInfo, Purity),
	goal_info_init(NonLocals, InstMapDelta, Detism, Purity, Context,
		GoalInfo0),
	goal_info_add_features(Features, GoalInfo0, GoalInfo),
	Goal = GoalExpr - GoalInfo.

goal_util__generate_foreign_proc(ModuleName, ProcName, PredOrFunc, ModeNo,
		Detism, Attributes, Args, ExtraArgs, PrefixCode, Code,
		SuffixCode, Features, InstMap, ModuleInfo, Context,
		Goal) :-
	list__length(Args, Arity),
	lookup_builtin_pred_proc_id(ModuleInfo, ModuleName, ProcName,
		PredOrFunc, Arity, ModeNo, PredId, ProcId),

	AllCode = PrefixCode ++ Code ++ SuffixCode,
	GoalExpr = foreign_proc(Attributes, PredId, ProcId, Args, ExtraArgs,
		ordinary(AllCode, no)),
	ArgVars = list__map(foreign_arg_var, Args),
	ExtraArgVars = list__map(foreign_arg_var, ExtraArgs),
	Vars = ArgVars ++ ExtraArgVars,
	set__list_to_set(Vars, NonLocals),
	determinism_components(Detism, _CanFail, NumSolns),
	( NumSolns = at_most_zero ->
		instmap_delta_init_unreachable(InstMapDelta)
	;
		instmap_delta_from_assoc_list(InstMap, InstMapDelta)
	),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_get_purity(PredInfo, Purity),
	goal_info_init(NonLocals, InstMapDelta, Detism, Purity, Context,
		GoalInfo0),
	goal_info_add_features(Features, GoalInfo0, GoalInfo),
	Goal = GoalExpr - GoalInfo.

generate_unsafe_cast(InArg, OutArg, Context, Goal) :-
	set__list_to_set([InArg, OutArg], NonLocals),
	instmap_delta_from_assoc_list([OutArg - ground(shared, none)],
		InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, det, pure, Context, GoalInfo),
	Goal = generic_call(unsafe_cast, [InArg, OutArg],
		[in_mode, out_mode], det) - GoalInfo.

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

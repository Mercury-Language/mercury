%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% file: polymorphism.nl
% main author: fjh

% This module is a pass over the HLDS.
% It does a syntactic transformation to implement polymorphic unifications
% using higher-order predicates.  Every polymorphic predicate is transformed
% so that it takes one additional argument for every type variable in the
% predicate's type declaration.  The argument is a higher-order predicate
% argument which is (the address of) the unification predicate for that type.
% Every polymorphic unification is replaced by a call to one of these
% predicates.
%
% For example, we translate
%
%	:- pred p(T1).
%	:- pred q(T2).
%	:- pred r(T3).
%	p(X) :- q([X]), r(0).
%
% into
%
%	:- pred p(T1, pred(T1, T1)).
%	:- pred q(T2, pred(T2, T2)).
%	:- pred r(T3, pred(T2, T2)).
%	p(X, Unify) :- q([X], list_unify(Unify)), r(0, int_unify).
%
% (except that both the input and output of the transformation are
% actually in super-homogeneous form).

%-----------------------------------------------------------------------------%

:- module polymorphism.
:- interface. 
:- import_module hlds.

:- pred polymorphism__process_module(module_info, module_info).
:- mode polymorphism__process_module(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, list, set, map, term, varset, std_util, require.
:- import_module prog_io, type_util, mode_util.

%-----------------------------------------------------------------------------%

	% This whole section just traverses the module structure.

polymorphism__process_module(ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, Preds),
	map__keys(Preds, PredIds),
	polymorphism__process_preds(PredIds, ModuleInfo0, ModuleInfo).

:- pred polymorphism__process_preds(list(pred_id), module_info, module_info).
:- mode polymorphism__process_preds(in, in, out) is det.

polymorphism__process_preds([], ModuleInfo, ModuleInfo).
polymorphism__process_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_procids(PredInfo, ProcIds),
	polymorphism__process_procs(PredId, ProcIds, ModuleInfo0, ModuleInfo1),
	polymorphism__process_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred polymorphism__process_procs(pred_id, list(proc_id),
				module_info, module_info).
:- mode polymorphism__process_procs(in, in, in, out) is det.

polymorphism__process_procs(_PredId, [], ModuleInfo, ModuleInfo).
polymorphism__process_procs(PredId, [ProcId | ProcIds], ModuleInfo0,
		ModuleInfo) :-
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),
	polymorphism__process_proc(ProcInfo0, PredInfo0, ModuleInfo0,
					ProcInfo, PredInfo1),
	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo1, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1),
	polymorphism__process_procs(PredId, ProcIds, ModuleInfo1, ModuleInfo).

%---------------------------------------------------------------------------%

	% This is the useful part of the code ;-).

:- type poly_info --->
		poly_info(
			varset,			% from the proc_info
			map(var, type),		% from the proc_info
			tvarset,		% from the proc_info
			map(tvar, var),		% specifies the unify proc var
						% for each of the pred's type
						% parameters
			module_info
		).
	
:- pred polymorphism__process_proc(proc_info, pred_info, module_info,
					proc_info, pred_info).
:- mode polymorphism__process_proc(in, in, in, out, out) is det.

polymorphism__process_proc(ProcInfo0, PredInfo0, ModuleInfo,
				ProcInfo, PredInfo) :-
	% grab the appropriate fields from the pred_info and proc_info
	pred_info_arg_types(PredInfo0, TypeVarSet0, ArgTypes0),
	proc_info_headvars(ProcInfo0, HeadVars0),
	proc_info_variables(ProcInfo0, VarSet0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	proc_info_goal(ProcInfo0, Goal0),

	% insert extra head variables to hold the address of the
	% equality predicate for each polymorphic type in the predicate's
	% type declaration
	term__vars_list(ArgTypes0, HeadTypeVars),
	polymorphism__make_head_vars(HeadTypeVars, VarSet0, VarTypes0,
				ExtraHeadVars, VarSet1, VarTypes1),
	list__append(HeadVars0, ExtraHeadVars, HeadVars),
	map__apply_to_list(ExtraHeadVars, VarTypes1, ExtraArgTypes),
	list__append(ArgTypes0, ExtraArgTypes, ArgTypes),

	% process any polymorphic calls inside the goal
	map__from_corresponding_lists(HeadTypeVars, ExtraHeadVars,
				UnifyProcMap),
	Info0 = poly_info(VarSet1, VarTypes1, TypeVarSet0,
				UnifyProcMap, ModuleInfo),
	polymorphism__process_goal(Goal0, Goal, Info0, Info),
	Info = poly_info(VarSet, VarTypes, TypeVarSet, _, _),

	% set the new values of the fields in proc_info and pred_info
	proc_info_set_headvars(ProcInfo0, HeadVars, ProcInfo1),
	proc_info_set_goal(ProcInfo1, Goal, ProcInfo2),
	proc_info_set_varset(ProcInfo2, VarSet, ProcInfo3),
	proc_info_set_vartypes(ProcInfo3, VarTypes, ProcInfo),
	pred_info_set_arg_types(PredInfo0, TypeVarSet, ArgTypes, PredInfo).
	
:- pred polymorphism__process_goal(hlds__goal, hlds__goal,
					poly_info, poly_info).
:- mode polymorphism__process_goal(in, out, in, out) is det.

polymorphism__process_goal(Goal0 - GoalInfo0, Goal) -->
	polymorphism__process_goal_2(Goal0, GoalInfo0, Goal).
	
:- pred polymorphism__process_goal_2(hlds__goal_expr, hlds__goal_info,
					hlds__goal, poly_info, poly_info).
:- mode polymorphism__process_goal_2(in, in, out, in, out) is det.

polymorphism__process_goal_2(
			call(PredId, ProcId, Args0, Builtin, Name, FollowVars),
			GoalInfo, Goal) -->
	{ term__term_list_to_var_list(Args0, ArgVars0) },
	polymorphism__process_call(PredId, ProcId, ArgVars0, 
			ArgVars, ExtraGoals),
	{ term__var_list_to_term_list(ArgVars, Args) },
	{ Call = call(PredId, ProcId, Args, Builtin, Name, FollowVars)
			- GoalInfo },
	{ list__append(ExtraGoals, [Call], GoalList) },
	{ conj_list_to_goal(GoalList, GoalInfo, Goal) }.

	% XXX handle polymorphic unifications!
polymorphism__process_goal_2(unify(A, B, C, D, E), GoalInfo,
			unify(A, B, C, D, E) - GoalInfo) --> [].

	% the rest of the clauses just process goals recursively

polymorphism__process_goal_2(conj(Goals0), GoalInfo, conj(Goals) - GoalInfo) -->
	polymorphism__process_goal_list(Goals0, Goals).
polymorphism__process_goal_2(disj(Goals0), GoalInfo, disj(Goals) - GoalInfo) -->
	polymorphism__process_goal_list(Goals0, Goals).
polymorphism__process_goal_2(not(Goal0), GoalInfo, not(Goal) - GoalInfo) -->
	polymorphism__process_goal(Goal0, Goal).
polymorphism__process_goal_2(switch(_, _, _), _, _) -->
	{ error("polymorphism__process_goal_2: switch unexpected") }.
polymorphism__process_goal_2(some(Vars, Goal0), GoalInfo,
			some(Vars, Goal) - GoalInfo) -->
	polymorphism__process_goal(Goal0, Goal).
polymorphism__process_goal_2(if_then_else(Vars, A0, B0, C0), GoalInfo,
			if_then_else(Vars, A, B, C) - GoalInfo) -->
	polymorphism__process_goal(A0, A),
	polymorphism__process_goal(B0, B),
	polymorphism__process_goal(C0, C).

:- pred polymorphism__process_goal_list(list(hlds__goal), list(hlds__goal),
					poly_info, poly_info).
:- mode polymorphism__process_goal_list(in, out, in, out) is det.

polymorphism__process_goal_list([], []) --> [].
polymorphism__process_goal_list([Goal0 | Goals0], [Goal | Goals]) -->
	polymorphism__process_goal(Goal0, Goal),
	polymorphism__process_goal_list(Goals0, Goals).

:- pred polymorphism__process_call(pred_id, proc_id, list(var),
					list(var), list(hlds__goal),
					poly_info, poly_info).
:- mode polymorphism__process_call(in, in, in, out, out, in, out) is det.

polymorphism__process_call(PredId, ProcId, ArgVars0, ArgVars, ExtraGoals,
				Info0, Info) :-
	Info0 = poly_info(VarSet0, VarTypes0, TypeVarSet0,
				UnifyProcMap, ModuleInfo),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
				PredInfo, ProcInfo),
	pred_info_arg_types(PredInfo, PredTypeVarSet, PredArgTypes0),
		% rename apart
	varset__merge(TypeVarSet0, PredTypeVarSet, PredArgTypes0,
			TypeVarSet, PredArgTypes),
	term__vars_list(PredArgTypes, PredTypeVars),
	proc_info_vartypes(ProcInfo, PredVarTypes),
	map__apply_to_list(ArgVars0, PredVarTypes, ActualArgTypes),
	map__keys(UnifyProcMap, HeadTypeVars),
	map__init(TypeSubst0),
	( type_unify_list(ActualArgTypes, PredArgTypes, HeadTypeVars,
			TypeSubst0, TypeSubst1) ->
		TypeSubst = TypeSubst1
	;
		error("polymorphism__process_goal_2: type unification failed")
	),
	term__var_list_to_term_list(PredTypeVars, PredTypes0),
	term__apply_rec_substitution_to_list(PredTypes0, TypeSubst, PredTypes),
	polymorphism__make_vars(PredTypes, UnifyProcMap, VarSet0, VarTypes0,
			ExtraVars, ExtraGoals, VarSet, VarTypes),
	list__append(ArgVars0, ExtraVars, ArgVars),
	Info = poly_info(VarSet, VarTypes, TypeVarSet,
			UnifyProcMap, ModuleInfo).

:- pred polymorphism__make_vars(list(type), map(tvar, var),
				varset, map(var, type),
				list(var), list(hlds__goal),
				varset, map(var, type)).
:- mode polymorphism__make_vars(in, in, in, in, out, out, out, out) is det.

polymorphism__make_vars([], _, VarSet, VarTypes, [], [], VarSet, VarTypes).
polymorphism__make_vars([Type|Types], UnifyProcMap, VarSet0, VarTypes0,
				ExtraVars, ExtraGoals, VarSet, VarTypes) :-
	(
		Type = term__functor(_TypeName, TypeArgs, Context)
	->
		% This occurs for code where a predicate calls a polymorphic
		% predicate with a known value of the type variable.
		% For example, given
		%
		%	:- pred p(T1).
		%	:- pred q(T2).
		%	:- pred r(T3).
		%	p(X) :- q([X]), r(0).
		%
		% we know that in the call to q/1, T2 is bound to `list(T1)',
		% and in the call to r/1, T3 is bound to `int', and so
		% we translate it into
		%	:- pred p(T1, pred(T1, T1)).
		%	:- pred q(T2, pred(T2, T2)).
		%	:- pred r(T3, pred(T3, T3)).
		%	p(X, Unify) :- q([X], list_unify(Unify)), r(int_unify).

		% create a term which holds the unification pred for
		% the type, processing any argument types recursively
		polymorphism__make_vars(TypeArgs, UnifyProcMap,
			VarSet0, VarTypes0,
			ArgVars, ExtraGoals0, VarSet1, VarTypes1),
		term__var_list_to_term_list(ArgVars, Args),
		Functor = term__atom("="), 
		ValTerm = term__functor(Functor, Args, Context),
		ConsId = cons("=", 2),
		
		% introduce new variable
		varset__new_var(VarSet1, Var, VarSet2),
		UnifyPredType = term__functor(term__atom("pred"),
					[Type, Type],
					Context),
		map__set(VarTypes1, Var, UnifyPredType, VarTypes2),
		VarTerm = term__variable(Var),

		% create a construction unification which initializes the
		% variable to the unification pred for the type
		UniMode = (free - ground -> ground - ground),
		list__length(ArgVars, NumArgVars),
		list__duplicate(NumArgVars, UniMode, UniModes),
		Unification = construct(Var, ConsId, ArgVars, UniModes),
		UnifyMode = (free -> ground) - (ground -> ground),
		UnifyContext = unify_context(explicit, []),
			% XXX the UnifyContext is wrong
		Unify = unify(VarTerm, ValTerm, UnifyMode, Unification,
					UnifyContext),

		% create a goal_info for the unification

		goal_info_init(GoalInfo0),
		set__list_to_set([Var | ArgVars], NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),
		map__init(InstMapping0),
		list__duplicate(NumArgVars, ground, ArgInsts),
			% note that we could perhaps be more accurate than
			% `ground', but hopefully it shouldn't make any
			% difference.
		map__set(InstMapping0, Var, bound([functor(Functor, ArgInsts)]),
			InstMapping),
		goal_info_set_instmap_delta(GoalInfo1, reachable(InstMapping),
			GoalInfo),

		ExtraGoal = Unify - GoalInfo,
		list__append(ExtraGoals0, [ExtraGoal], ExtraGoals1)
	;
		Type = term__variable(TypeVar1),
		map__search(UnifyProcMap, TypeVar1, UnifyProcVar)
	->
		% This occurs for code where a predicate calls a polymorphic
		% predicate with a bound but unknown value of the type variable.
		% For example, in
		%
		%	:- pred p(T1).
		%	:- pred q(T2).
		%	p(X) :- q(X).
		%
		% we know that `T2' is bound to `T1', and we translate it into
		%
		%	:- pred p(T1, pred(T1, T1)).
		%	:- pred q(T2, pred(T2, T2)).
		%	p(X, UnifyProc) :- q(X, UnifyProc).
		
		Var = UnifyProcVar,
		ExtraGoals1 = [],
		VarSet2 = VarSet0,
		VarTypes2 = VarTypes0
	;
		% This occurs for code where a predicate calls a polymorphic
		% predicate with an unbound type variable, for example
		%
		%	:- pred p.
		%	:- pred q(list(T)).
		%	p :- q([]).
		%
		% In this case T is unbound, so there cannot be any objects
		% ot type T, and so q/1 cannot possibly use the unification
		% predicate for type T.  We just pass a dummy value (zero).
		%
		%	:- pred p.
		%	:- pred q(T, pred(T, T)).
		%	p :- q([], 0).
		%
		% (This isn't really type-correct, but we're already past
		% the type-checker.  Passing 0 should ensure that we get
		% a core dump if we ever attempt to call the unify pred.)

		% introduce new variable
		varset__new_var(VarSet0, Var, VarSet2),
		UnifyPredType = term__functor(term__atom("pred"),
					[Type, Type],
					Context),
		map__set(VarTypes0, Var, UnifyPredType, VarTypes2),
		VarTerm = term__variable(Var),

		% create a construction unification which initializes the
		% variable to zero
		term__context_init(0, Context),
		Functor = term__integer(0),
		ZeroTerm = term__functor(Functor, [], Context),
		ConsId = int_const(0),
		Unification = construct(Var, ConsId, [], []),
		UnifyMode = (free -> ground) - (ground -> ground),
		UnifyContext = unify_context(explicit, []),
			% XXX the UnifyContext is wrong
		Unify = unify(VarTerm, ZeroTerm, UnifyMode, Unification,
					UnifyContext),
		goal_info_init(GoalInfo0),
		set__singleton_set(NonLocals, Var),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),
		map__init(InstMapping0),
		map__set(InstMapping0, Var, bound([functor(Functor, [])]),
			InstMapping),
		goal_info_set_instmap_delta(GoalInfo1, reachable(InstMapping),
			GoalInfo),
		Goal = Unify - GoalInfo,
		ExtraGoals1 = [Goal]
	),
	ExtraVars = [Var | ExtraVars1],
	list__append(ExtraGoals1, ExtraGoals2, ExtraGoals),
	polymorphism__make_vars(Types,  UnifyProcMap, VarSet2, VarTypes2,
				ExtraVars1, ExtraGoals2, VarSet, VarTypes).

:- pred polymorphism__make_head_vars(list(tvar), varset, map(var, type),
				list(var), varset, map(var, type)).
:- mode polymorphism__make_head_vars(in, in, in, out, out, out) is det.

polymorphism__make_head_vars([], VarSet, VarTypes, [], VarSet, VarTypes).
polymorphism__make_head_vars([TypeVar|TypeVars], VarSet0, VarTypes0,
				UnifyProcVars, VarSet, VarTypes) :-
	varset__new_var(VarSet0, Var, VarSet1),
	term__context_init(0, Context),
	Type = term__variable(TypeVar),
	UnifyPredType = term__functor(term__atom("pred"), [Type, Type],
					Context),
	map__set(VarTypes0, Var, UnifyPredType, VarTypes1),
	UnifyProcVars = [Var | UnifyProcVars1],
	polymorphism__make_head_vars(TypeVars, VarSet1, VarTypes1,
				UnifyProcVars1, VarSet, VarTypes).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

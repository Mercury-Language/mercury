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
%	:- pred p(T1, pred(T1, T1)).
%	:- pred q(T2, pred(T2, T2)).
%	:- pred r(T3, pred(T2, T2)).
%	p(X, Unify) :- q([X], list_unify(Unify)), r(0, int_unify).

%-----------------------------------------------------------------------------%

:- module polymorphism.
:- interface. 
:- import_module hlds.

:- pred polymorphism__process_module(module_info, module_info).
:- mode polymorphism__process_module(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, int, mode_util, list, require.

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
	polymorphism__process_proc(ProcInfo0, PredInfo0, ModuleInfo0, ProcInfo),
	map__set(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1),
	polymorphism__process_procs(PredId, ProcIds, ModuleInfo1, ModuleInfo).

%---------------------------------------------------------------------------%

	% This is the useful part of the code ;-).

:- type poly_info --->
		poly_info(
			varset,			% from the proc_info
			map(var, type),		% from the proc_info
			map(tvar, var),		% specifies the unify proc var
						% for each of the pred's type
						% parameters
			module_info
		).
	
:- pred polymorphism__process_proc(proc_info, pred_info, module_info,
					proc_info).
:- mode polymorphism__process_proc(in, in, in, out) is det.

polymorphism__process_proc(ProcInfo0, PredInfo, ModuleInfo, ProcInfo) :-
	% grab the appropriate fields from the pred_info and proc_info
	pred_info_arg_types(PredInfo, TypeVarSet, ArgTypes0),
	proc_info_headvars(ProcInfo0, HeadVars0),
	proc_info_variables(ProcInfo0, VarSet0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	proc_info_goal(ProcInfo0, Goal0),

	% insert extra head variables to hold the address of the
	% equality predicate for each polymorphic type in the predicate's
	% type declaration
	varset__vars(TypeVarSet, TypeVars),
	polymorphism__make_head_vars(TypeVars, VarSet0, VarTypes0,
				ExtraHeadVars, VarSet1, VarTypes1),
	list__append(HeadVars0, ExtraHeadVars, HeadVars),
	map__from_corresponding_lists(ExtraHeadVars, TypeVars, ExtraArgTypes),
	map__overlay(ArgTypes0, ExtraArgTypes, ArgTypes),

	% process any polymorphic calls inside the goal
	map__from_corresponding_lists(TypeVars, ExtraHeadVars, UnifyProcMap),
	Info0 = poly_info(VarSet1, VarTypes1, UnifyProcMap, ModuleInfo)
	polymorphism__process_goal(Goal0, Goal, Info0, Info),
	Info = poly_info(VarSet, VarTypes, _, _),

	% set the new values of the fields in proc_info
	proc_info_set_headvars(ProcInfo0, HeadVars, ProcInfo1),
	proc_info_set_goal(ProcInfo1, Goal, ProcInfo2),
	proc_info_set_varset(ProcInfo2, VarSet, ProcInfo3),
	proc_info_set_vartypes(ProcInfo3, VarTypes, ProcInfo).
	
:- pred polymorphism__process_goal(hlds__goal, hlds__goal,
					poly_info, poly_info).
:- mode polymorphism__process_goal(in, out, in, out) is det.

polymorphism__process_goal(Goal0 - GoalInfo0, Goal) -->
	polymorphism__process_goal_2(Goal0, GoalInfo0, Goal).
	
:- pred polymorphism__process_goal_2(hlds__goal_expr, goal_info, hlds__goal,
					poly_info, poly_info).
:- mode polymorphism__process_goal_2(in, in, out, in, out) is det.

polymorphism__process_goal_2(
			call(PredId, ProcId, Args0, Builtin, Name, FollowVars),
			GoalInfo, Goals) -->
	{ term__term_list_to_var_list(Args0, ArgVars0) },
	polymorphism__process_call(PredId, ProcId, ArgVars0,
			UnifyProcMap, ModuleInfo,
			ArgVars, ExtraGoals),
	{ term__var_list_to_term_list(ArgVars, Args) },
	{ Goal = call(PredId, ProcId, Args, Builtin, Name, FollowVars)
			- GoalInfo },
	{ list__append(ExtraGoals, Goal, Goals) }.

:- pred polymorphism__process_call(pred_id, proc_id, list(var), list(var)
					
:- mode polymorphism__process_call(in, in, in, out) is det.

polymorphism__process_call(PredId, ProcId, ArgVars0, ArgVars, ExtraGoals,
				Info0, Info) :-
	Info0 = poly_info(VarSet0, VarTypes0, UnifyProcMap, ModuleInfo),

	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
					PredInfo, ProcInfo),
	pred_info_arg_types(PredInfo, PredTypeVarSet, FormalArgTypes),
	varset__vars(PredTypeVarSet, PredTypeVars),
	proc_info_vartypes(VarTypes),
	map__apply_list(ArgVars0, VarTypes, ActualArgTypes),
	map__keys(UnifyProcMap, HeadTypeVars),
	map__init(TypeSubst0),
	( type__unify_list(ActualArgTypes, FormalArgTypes, HeadTypeVars,
			TypeSubst0, TypeSubst1) ->
		TypeSubst = TypeSubst1
	;
		error("polymorphism__process_goal_2: type unification failed")
	),
	polymorphism__make_vars(PredTypeVars, VarSet0, VarTypes0,
			ExtraVars, VarSet, VarTypes),
	list__append(ArgVars0, ExtraVars, ArgVars),
	map__apply_list(PredTypeVars, TypeSubst, PredTypeVars1),
	polymorphism__initialize_vars(ExtraVars, PredTypeVars1, UnifyProcMap,
			ExtraGoals),
	Info = poly_info(VarSet, VarTypes, UnifyProcMap, ModuleInfo).


polymorphism__initialize_vars([], [], ..., []).
polymorphism__initialize_vars([Var | Vars], [Type | Types], ...,
			[Goal | Goals]) :-
	%
	% three cases:
	% case 1) type is 
	%

	(
polymorphism__get_unify_proc_vars(
			[TypeVar | TypeVars], UnifyProcMap, VarSet0, VarTypes0,
			[ExtraVar | ExtraVars], ExtraGoals, VarSet, VarTypes) :-


:- type hlds__goal_expr
	--->	conj(hlds__goals)
	;	call(pred_id, proc_id, list(term), is_builtin, sym_name, follow_vars)
	;	switch(var, category, list(case))
	;	unify(term, term, unify_mode, unification, unify_context)
	;	disj(hlds__goals)
	;	not(hlds__goal)
	;	some(list(var), hlds__goal)
	;	if_then_else(list(var), hlds__goal, hlds__goal, hlds__goal).

:- pred polymorphism__make_vars(list(tvar), varset, map(var, type),
				list(var), varset, map(var, type)).
:- mode polymorphism__make_vars(in, in, in, out, out, out) is det.

polymorphism__make_vars([], VarSet, VarTypes, [], [], VarSet, VarTypes).
polymorphism__make_vars([TypeVar|TypeVars], VarSet0, VarTypes0,
				UnifyProcVars, ExtraGoals, VarSet, VarTypes) :-
	map__lookup(TypeVar, TypeSubst, TypeVal),
	(
		TypeVal = term__functor(TypeName, TypeArgs, Context)
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
		%	:- pred r(T3, pred(T2, T2)).
		%	p(X, Unify) :- q([X], list_unify(Unify)), r(int_unify).

		% create a term which holds the unification pred for
		% the type, processing any argument types recursively
		polymorphism__make_vars(TypeVars, VarSet1, VarTypes1,
			ArgVars, ExtraGoals0, VarSet2, VarTypes2),
		term__var_list_to_term_list(ArgVars, Args),
		ValTerm = term__functor(term__atom("="), Args, Context),
		ConsId = cons("=", 2),
		
		% introduce new variable
		varset__new_var(VarSet1, Var, VarSet2),
		Type = term__functor(term__atom("pred"), [TypeVar, TypeVar],
					Context),
		map__set(VarTypes1, Var, Type, VarTypes2),
		VarTerm = term__variable(Var),

		% create a construction unification which initializes the
		% variable to the unification pred for the type
		UniMode = (free - ground -> ground - ground),
		list__length(ArgVars, NumArgVars),
		list__duplicate(NumArgVars, UniMode, UniModes),
		Unification = construct(Var, ConsId, ArgVars, UniModes),
		UnifyMode = (free -> ground) - (ground -> ground),
		UnifyContext = unify__context(explicit, []),
			% XXX the UnifyContext is wrong
		Unify = unify(VarTerm, ValTerm, UniMode, Unification,
					UnifyContext),

		% create a goal_info for the unification, and append it
		% to the list of extra goals
		goal_info_init(GoalInfo0),
		set__list_to_set([Var | ArgVars], NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo),
		ExtraGoal = Unify - GoalInfo,
		list__append(ExtraGoals0, [ExtraGoal], ExtraGoals1)
	;
		TypeVal = term__variable(TypeVar1),
		map__search(UnifyProcVars, TypeVar1, UnifyProcVar)
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
		
		ExtraGoals = [],
		Var = UnifyProcVar
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
		varset__new_var(VarSet1, Var, VarSet2),
		Type = term__functor(term__atom("pred"), [TypeVar, TypeVar],
					Context),
		map__set(VarTypes1, Var, Type, VarTypes2),
		VarTerm = term__variable(Var),

		% create a construction unification which initializes the
		% variable to zero
		term__context_init(0, Context),
		ZeroTerm = term__functor(term__integer(0), [], Context),
		ConsId = int_const(0)
		Unification = construct(Var, ConsId, [], []),
		UnifyMode = (free -> ground) - (ground -> ground),
		UnifyContext = unify__context(explicit, []),
			% XXX the UnifyContext is wrong
		Unify = unify(VarTerm, ZeroTerm, UniMode, Unification,
					UnifyContext),
		goal_info_init(GoalInfo0),
		set__singleton_set(Var, NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),
		Goal = Unify - GoalInfo
	),

	UnifyProcVars = [Var | UnifyProcVars1],
	polymorphism__make_vars(TypeVars, VarSet1, VarTypes1,
				UnifyProcVars, VarSet, VarTypes).

:- pred polymorphism__make_head_vars(list(tvar), varset, map(var, type),
				list(var), varset, map(var, type)).
:- mode polymorphism__make_head_vars(in, in, in, out, out, out) is det.

polymorphism__make_head_vars([], VarSet, VarTypes, [], VarSet, VarTypes).
polymorphism__make_head_vars([TypeVar|TypeVars], VarSet0, VarTypes0,
				UnifyProcVars, VarSet, VarTypes) :-
	varset__new_var(VarSet0, Var, VarSet1),
	term__context_init(0, Context),
	Type = term__functor(term__atom("pred"), [TypeVar, TypeVar], Context),
	map__set(VarTypes0, Var, Type, VarTypes1),
	UnifyProcVars = [Var | UnifyProcVars1],
	polymorphism__make_head_vars(TypeVars, VarSet1, VarTypes1,
				UnifyProcVars, VarSet, VarTypes).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

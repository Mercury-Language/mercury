%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: unique_modes.m
% main author: fjh

% This module checks that variables with a unique mode (as opposed to
% a mostly-unique mode) really are unique, and not nondet live - i.e.,
% that they cannot be referenced on backtracking.

% Basically we just traverse each goal, keeping track of which variables
% are nondet live.  At each procedure call, we check that any arguments
% whose initial insts are required to be unique are not nondet live.
% If they are, we report an error message.

% XXX what if it would have matched ok with a different mode of the
% called predicate (e.g. if a predicate is overloaded with both
% `ui' and `in' modes)?

% XXX NOT YET FINISHED!
% The code to actually calculate the set
% of nondet live variables is currently a rather crude approximation.

%-----------------------------------------------------------------------------%

:- module unique_modes.
:- interface. 
:- import_module hlds, llds, io.

	% check every predicate in a module
:- pred unique_modes__check_module(module_info, module_info,
					io__state, io__state).
:- mode unique_modes__check_module(in, out, di, uo) is det.

	% just check a single procedure
:- pred unique_modes__check_proc(proc_info, pred_id, proc_id, module_info,
				proc_info, io__state, io__state).
:- mode unique_modes__check_proc(in, in, in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, list, map, set, std_util, require, term, varset.
:- import_module mode_util, prog_out, hlds_out, mercury_to_mercury.

%-----------------------------------------------------------------------------%

	% This section just traverses the module structure.

unique_modes__check_module(ModuleInfo0, ModuleInfo) -->
	{ module_info_preds(ModuleInfo0, Preds) },
	{ map__keys(Preds, PredIds) },
	unique_modes__check_preds(PredIds, ModuleInfo0, ModuleInfo).

:- pred unique_modes__check_preds(list(pred_id), module_info, module_info,
					io__state, io__state).
:- mode unique_modes__check_preds(in, in, out, di, uo) is det.

unique_modes__check_preds([], ModuleInfo, ModuleInfo) --> [].
unique_modes__check_preds([PredId | PredIds], ModuleInfo0, ModuleInfo) -->
	{ module_info_preds(ModuleInfo0, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ pred_info_procids(PredInfo, ProcIds) },
	unique_modes__check_procs(PredId, ProcIds, ModuleInfo0, ModuleInfo1),
	unique_modes__check_preds(PredIds, ModuleInfo1, ModuleInfo).

:- pred unique_modes__check_procs(pred_id, list(proc_id),
					module_info, module_info,
					io__state, io__state).
:- mode unique_modes__check_procs(in, in, in, out, di, uo) is det.

unique_modes__check_procs(_PredId, [], ModuleInfo, ModuleInfo) --> [].
unique_modes__check_procs(PredId, [ProcId | ProcIds], ModuleInfo0,
		ModuleInfo) -->
	{ module_info_preds(ModuleInfo0, PredTable0) },
	{ map__lookup(PredTable0, PredId, PredInfo0) },
	{ pred_info_procedures(PredInfo0, ProcTable0) },
	{ map__lookup(ProcTable0, ProcId, ProcInfo0) },

	%%% XXX detect_liveness_proc(ProcInfo0, ModuleInfo0, ProcInfo),
	unique_modes__check_proc(ProcInfo0, PredId, ProcId, ModuleInfo0,
		ProcInfo),

	{ map__set(ProcTable0, ProcId, ProcInfo, ProcTable) },
	{ pred_info_set_procedures(PredInfo0, ProcTable, PredInfo) },
	{ map__set(PredTable0, PredId, PredInfo, PredTable) },
	{ module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo1) },

	unique_modes__check_procs(PredId, ProcIds, ModuleInfo1, ModuleInfo).

%-----------------------------------------------------------------------------%

unique_modes__check_proc(ProcInfo0, PredId, ProcId, ModuleInfo, ProcInfo) -->
	%
	% first check the initial insts of all the predicate calls in the goal
	%
	{ proc_info_goal(ProcInfo0, Goal0) },
	{ proc_info_get_initial_instmap(ProcInfo0, ModuleInfo, Instmap0) },
	{ uniq_info_init(ModuleInfo, Instmap0, UniqModesInfo0) },
	{ unique_modes__check_goal(Goal0, UniqModesInfo0,
				Goal, UniqModesInfo1) },
	{ proc_info_set_goal(ProcInfo0, Goal, ProcInfo) },

	%
	% then check that the final insts of the head vars is OK
	%
	{ proc_info_headvars(ProcInfo0, Args) },
	{ proc_info_argmodes(ProcInfo0, ArgModes) },
	{ proc_info_context(ProcInfo0, Context) },
	{ mode_list_get_final_insts(ArgModes, ModuleInfo, ArgInsts) },
	{ unique_modes__check_args(Args, ArgInsts, PredId, ProcId, 1,
		Context, no, UniqModesInfo1, UniqModesInfo) },

	%
	% finally if we encountered any errors then report them
	%
	{ uniq_info_get_errors(UniqModesInfo, Errors) },
	( { Errors \= [] } ->
		unique_modes__report_errors(Errors, ModuleInfo),
		io__set_exit_status(1)
	;
		[]
	).

:- pred unique_modes__check_goal(hlds__goal, uniq_info,
				   hlds__goal, uniq_info).
:- mode unique_modes__check_goal(in, in, out, out) is det.

	% XXX we currently make the conservative assumption that
	% any non-local variable in a disjunction or nondet call
	% is nondet-live - and stays nondet-live.

unique_modes__check_goal(Goal0, UniqModesInfo0, Goal, UniqModesInfo) :-
	Goal0 = GoalExpr0 - GoalInfo0,
	goal_info_get_code_model(GoalInfo0, CodeModel),
	(
		CodeModel = model_non,
		( GoalExpr0 = disj(_) ; GoalExpr0 = call(_,_,_,_,_,_,_) )
	->
		uniq_info_get_nondet_live_vars(UniqModesInfo0, Vars0),
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		set__union(Vars0, NonLocals, Vars),
		uniq_info_set_nondet_live_vars(UniqModesInfo0, Vars,
			UniqModesInfo1)
	;
		UniqModesInfo1 = UniqModesInfo0
	),
	unique_modes__check_goal_2(Goal0, UniqModesInfo1, Goal, UniqModesInfo).

:- pred unique_modes__check_goal_2(hlds__goal, uniq_info,
				   hlds__goal, uniq_info).
:- mode unique_modes__check_goal_2(in, in, out, out) is det.

unique_modes__check_goal_2(GoalExpr0 - GoalInfo0, UniqModesInfo0,
		Goal, UniqModesInfo) :-
	(
		GoalExpr0 = disj(Goals0),
		unique_modes__check_disj(Goals0, UniqModesInfo0,
					Goals, UniqModesInfo),
		Goal = disj(Goals) - GoalInfo0
	;
		GoalExpr0 = call(PredId, ProcId, ArgVars, _IsBuiltin,
				CallContext, _SymName, _FollowVars),
		goal_info_context(GoalInfo0, Context),
		unique_modes__check_call(PredId, ProcId, ArgVars,
				Context, CallContext,
				UniqModesInfo0, UniqModesInfo1),
		unique_modes__update_instmap(GoalInfo0,
				UniqModesInfo1, UniqModesInfo),
		Goal = GoalExpr0 - GoalInfo0
	;
		GoalExpr0 = unify(LHS, RHS, _, _, _),
		unique_modes__propagate_nondet_liveness(RHS, LHS,
				UniqModesInfo0, UniqModesInfo1),
		Goal = GoalExpr0 - GoalInfo0,
		unique_modes__update_instmap(GoalInfo0,
				UniqModesInfo1, UniqModesInfo)
	;
		GoalExpr0 = pragma_c_code(_, _, _, _, _),
		Goal = GoalExpr0 - GoalInfo0,
		unique_modes__update_instmap(GoalInfo0,
				UniqModesInfo0, UniqModesInfo)
	
	% for the remaining cases, we just process goals recursively

	;
		GoalExpr0 = conj(Goals0),
		unique_modes__check_conj(Goals0, UniqModesInfo0,
					Goals, UniqModesInfo),
		conj_list_to_goal(Goals, GoalInfo0, Goal)
	;
		GoalExpr0 = not(NegGoal0),
		unique_modes__check_goal(NegGoal0, UniqModesInfo0,
						NegGoal, UniqModesInfo),
		Goal = not(NegGoal) - GoalInfo0
	;
		GoalExpr0 = switch(Var, CanFail, Cases0),
		unique_modes__check_switch(Cases0, UniqModesInfo0,
						Cases, UniqModesInfo),
		Goal = switch(Var, CanFail, Cases) - GoalInfo0
	;
		GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
		unique_modes__check_goal(Cond0, UniqModesInfo0,
					Cond, UniqModesInfo1),
		unique_modes__check_goal(Then0, UniqModesInfo1,
					Then, UniqModesInfo2),
		unique_modes__check_goal(Else0, UniqModesInfo2,
					Else, UniqModesInfo),
		Goal = if_then_else(Vars, Cond, Then, Else) - GoalInfo0
	;
		GoalExpr0 = some(Vars, SubGoal0),
		unique_modes__check_goal(SubGoal0, UniqModesInfo0,
					   SubGoal, UniqModesInfo),
		Goal = some(Vars, SubGoal) - GoalInfo0
	),
	!.

:- pred unique_modes__update_instmap(hlds__goal_info, uniq_info, uniq_info).
:- mode unique_modes__update_instmap(in, in, out) is det.

unique_modes__update_instmap(_GoalInfo, UniqModesInfo, UniqModesInfo).

/* XXX we don't use the instmap (yet - do we need to?)
unique_modes__update_instmap(GoalInfo, UniqModesInfo0, UniqModesInfo) :-
	goal_info_get_instmap_delta(GoalInfo, InstmapDelta),
	uniq_info_get_instmap(UniqModesInfo, InstMap0),
	apply_instmap_delta(InstMap0, InstmapDelta, InstMap),
	uniq_info_set_instmap(UniqModesInfo, InstMap, FinalUniqModesInfo).
*/

:- pred unique_modes__propagate_nondet_liveness(unify_rhs, var, 
			uniq_info, uniq_info).
:- mode unique_modes__propagate_nondet_liveness(in, in, in, out) is det.

unique_modes__propagate_nondet_liveness(var(RHS), LHS, UniqInfo0, UniqInfo) :-
	uniq_info_get_nondet_live_vars(UniqInfo0, NondetLiveVars0),
	( set__member(LHS, NondetLiveVars0) ->
		set__insert(NondetLiveVars0, RHS, NondetLiveVars)
	; set__member(RHS, NondetLiveVars0) ->
		set__insert(NondetLiveVars0, LHS, NondetLiveVars)
	;
		NondetLiveVars = NondetLiveVars0
	),
	uniq_info_set_nondet_live_vars(UniqInfo0, NondetLiveVars, UniqInfo).
unique_modes__propagate_nondet_liveness(functor(_, Args), Var,
		UniqInfo0, UniqInfo) :-
	uniq_info_get_nondet_live_vars(UniqInfo0, NondetLiveVars0),
	( set__member(Var, NondetLiveVars0) ->
		set__insert_list(NondetLiveVars0, Args, NondetLiveVars)
	;
		set__list_to_set(Args, ArgsSet),
		set__intersect(ArgsSet, NondetLiveVars0, NondetLiveArgs),
		\+ set__empty(NondetLiveArgs)
	->
		set__insert(NondetLiveVars0, Var, NondetLiveVars)
	;
		NondetLiveVars = NondetLiveVars0
	),
	uniq_info_set_nondet_live_vars(UniqInfo0, NondetLiveVars, UniqInfo).
unique_modes__propagate_nondet_liveness(lambda_goal(_, _, _, _), _, _, _) :-
	% lambda expressions should get preprocessed away by lambda.m
	% before we get to here
	error("unique_modes.m: unexpected lambda goal").

:- pred unique_modes__check_call(pred_id, proc_id, list(var), term_context,
			maybe(call_unify_context), uniq_info, uniq_info).
:- mode unique_modes__check_call(in, in, in, in, in, in, out) is det.

	% to check a call, we just look up the required initial insts
	% for the arguments of the call, and then check for each
	% argument if the variable is nondet-live and the required initial
	% inst was unique.

unique_modes__check_call(PredId, ProcId, Args, Context, CallContext,
		UniqInfo0, UniqInfo) :-
	uniq_info_get_module_info(UniqInfo0, ModuleInfo),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	proc_info_argmodes(ProcInfo, ArgModes),
	mode_list_get_initial_insts(ArgModes, ModuleInfo, InitialArgInsts),
	unique_modes__check_args(Args, InitialArgInsts, PredId, ProcId, 1,
		Context, CallContext, UniqInfo0, UniqInfo).

:- pred unique_modes__check_args(list(var), list(inst),
			pred_id, proc_id, int, term_context,
			maybe(call_unify_context), uniq_info, uniq_info).
:- mode unique_modes__check_args(in, in, in, in, in, in, in, in, out) is det.

unique_modes__check_args(Vars, Insts, 
		PredId, ProcId, ArgNum, Context, CallContext,
		UniqInfo0, UniqInfo) :-
	( Vars = [], Insts = [] ->
		UniqInfo = UniqInfo0
	; Vars = [Var|Vars1], Insts = [Inst|Insts1] ->
		uniq_info_get_nondet_live_vars(UniqInfo0, NondetLiveVars),
		uniq_info_get_module_info(UniqInfo0, ModuleInfo),
		(
			set__member(Var, NondetLiveVars),
			inst_is_unique(ModuleInfo, Inst),
			\+ inst_is_free(ModuleInfo, Inst)
		->
			Error = uniq_error(PredId, ProcId, ArgNum, Var,
					Context, CallContext),
			uniq_info_get_errors(UniqInfo0, Errors0),
			list__append(Errors0, [Error], Errors),
			uniq_info_set_errors(UniqInfo0, Errors, UniqInfo1)
		;
			UniqInfo1 = UniqInfo0
		),
		ArgNum1 is ArgNum + 1,
		unique_modes__check_args(Vars1, Insts1,
			PredId, ProcId, ArgNum1, Context, CallContext,
			UniqInfo1, UniqInfo)
	;
		error("mode_modes__check_args: length mismatch")
	).

%-----------------------------------------------------------------------------%

:- pred unique_modes__check_conj(list(hlds__goal),
	uniq_info, list(hlds__goal), uniq_info).
:- mode unique_modes__check_conj(in, in, out, out) is det.

unique_modes__check_conj([], UniqModesInfo, [], UniqModesInfo).
unique_modes__check_conj([Goal0 | Goals0], UniqModesInfo0, 
		[Goal | Goals], UniqModesInfo) :-
	unique_modes__check_goal(Goal0, UniqModesInfo0, Goal, UniqModesInfo1),
	unique_modes__check_conj(Goals0, UniqModesInfo1, Goals, UniqModesInfo).

%-----------------------------------------------------------------------------%

:- pred unique_modes__check_disj(list(hlds__goal), uniq_info,
	list(hlds__goal), uniq_info).
:- mode unique_modes__check_disj(in, in, out, out) is det.

unique_modes__check_disj([], UniqModesInfo, [], UniqModesInfo).
unique_modes__check_disj([Goal0 | Goals0], UniqModesInfo0,
			   [Goal | Goals], UniqModesInfo) :-
	unique_modes__check_goal(Goal0, UniqModesInfo0, Goal, UniqModesInfo1),
	unique_modes__check_disj(Goals0, UniqModesInfo1, Goals, UniqModesInfo).

%-----------------------------------------------------------------------------%

:- pred unique_modes__check_switch(list(case), uniq_info,
				     list(case), uniq_info).
:- mode unique_modes__check_switch(in, in, out, out) is det.

unique_modes__check_switch([], UniqModesInfo, [], UniqModesInfo).
unique_modes__check_switch([case(Cons, Goal0) | Cases0], UniqModesInfo0,
		[case(Cons, Goal) | Cases], UniqModesInfo) :-
	unique_modes__check_goal(Goal0, UniqModesInfo0, Goal, UniqModesInfo1),
	unique_modes__check_switch(Cases0, UniqModesInfo1,
			Cases, UniqModesInfo).

%-----------------------------------------------------------------------------%
%
% The uniq_info type and access predicates.
%
%-----------------------------------------------------------------------------%

:- type uniq_info
	--->	uniq_info(
			module_info,
			set(var),	% the nondet-live variables
			instmap,
			list(unique_modes__error)
		).

:- pred uniq_info_init(module_info::in, instmap::in, uniq_info::out) is det.

uniq_info_init(ModuleInfo, InstMap, UniqInfo) :-
	set__init(NondetLiveVars),
	Errors = [],
	UniqInfo = uniq_info(ModuleInfo, NondetLiveVars, InstMap, Errors).

:- pred uniq_info_get_module_info(uniq_info::in, module_info::out) is det.

uniq_info_get_module_info(uniq_info(ModuleInfo, _, _, _), ModuleInfo).

:- pred uniq_info_get_nondet_live_vars(uniq_info::in, set(var)::out) is det.

uniq_info_get_nondet_live_vars(uniq_info(_, NondetLiveVars, _, _),
	NondetLiveVars).

:- pred uniq_info_set_nondet_live_vars(uniq_info::in, set(var)::in,
		uniq_info::out) is det.

uniq_info_set_nondet_live_vars(uniq_info(A, _, C, D), NondetLiveVars,
		uniq_info(A, NondetLiveVars, C, D)).

:- pred uniq_info_get_instmap(uniq_info::in, instmap::out) is det.

uniq_info_get_instmap(uniq_info(_, _, InstMap, _), InstMap).

:- pred uniq_info_set_instmap(uniq_info::in, instmap::in, uniq_info::out)
	is det.

uniq_info_set_instmap(uniq_info(A, B, _, D), InstMap,
		uniq_info(A, B, InstMap, D)).

:- pred uniq_info_get_errors(uniq_info::in,
		list(unique_modes__error)::out) is det.

uniq_info_get_errors(uniq_info(_, _, _, Errors), Errors).

:- pred uniq_info_set_errors(uniq_info::in, list(unique_modes__error)::in,
		uniq_info::out) is det.

uniq_info_set_errors(uniq_info(A, B, C, _), Errors, uniq_info(A, B, C, Errors)).

%-----------------------------------------------------------------------------%
%
% Error reporting
%
% XXX should improve error message - print var name,
% and if --verbose-errors specified, give explanation
%
%-----------------------------------------------------------------------------%

:- type unique_modes__error
	--->	uniq_error(pred_id, proc_id, int, var, term_context,
			maybe(call_unify_context)).

:- pred unique_modes__report_errors(list(unique_modes__error)::in,
		module_info::in, io__state::di, io__state::uo) is det.

unique_modes__report_errors([], _) --> [].
unique_modes__report_errors([Error|Errors], ModuleInfo) -->
	unique_modes__report_error(Error, ModuleInfo),
	unique_modes__report_errors(Errors, ModuleInfo).

:- pred unique_modes__report_error(unique_modes__error::in, module_info::in,
					io__state::di, io__state::uo) is det.

unique_modes__report_error(uniq_error(PredId, ProcId, ArgNum, Var, Context,
			CallUnifyContext), ModuleInfo) -->
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
			_PredInfo, ProcInfo) },
	{ proc_info_variables(ProcInfo, VarSet) },
	unique_modes__report_call_context(PredId, ProcId, ArgNum,
			Context, CallUnifyContext, ModuleInfo),
	prog_out__write_context(Context),
	io__write_string("  Unique mode error: "),
	unique_modes__write_argument_name(VarSet, Var),
	io__write_string(" is not `unique',\n"),
	prog_out__write_context(Context),
	io__write_string("  since it's value may be needed again on backtracking.\n").

:- pred unique_modes__write_argument_name(varset, var, io__state, io__state).
:- mode unique_modes__write_argument_name(in, in, di, uo) is det.

unique_modes__write_argument_name(VarSet, VarId) -->
	( { varset__lookup_name(VarSet, VarId, _) } ->
		io__write_string("variable `"),
		mercury_output_var(VarId, VarSet),
		io__write_string("'")
	;
		io__write_string("argument")
	).

%-----------------------------------------------------------------------------%

:- pred unique_modes__report_call_context(pred_id, proc_id, int, term_context,
	maybe(call_unify_context), module_info, io__state, io__state).
:- mode unique_modes__report_call_context(in, in, in, in, in, in, di, uo)
	is det.

unique_modes__report_call_context(PredId, ProcId, ArgNum, Context,
		CallUnifyContext, ModuleInfo) -->
	(
		{ CallUnifyContext = yes(call_unify_context(LT, RT, UC)) },
		unique_modes__report_unify_context(Context, UC, ModuleInfo,
			PredId, ProcId, LT, RT)
	;
		{ CallUnifyContext = no },
		{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
				PredInfo, ProcInfo) },
		{ pred_info_name(PredInfo, PredName) },
		{ proc_info_argmodes(ProcInfo, ArgModes) },
		prog_out__write_context(Context),
		io__write_string("In argument "),
		io__write_int(ArgNum),
		io__write_string(" of `"),
		unique_modes__report_pred_name_mode(PredName, ArgModes),
		io__write_string("':\n")
	).

%-----------------------------------------------------------------------------%

:- pred unique_modes__report_unify_context(term_context, unify_context,
	module_info, pred_id, proc_id, var, unify_rhs, io__state, io__state).
:- mode unique_modes__report_unify_context(in, in, in, in, in, in, in, di, uo)
	is det.

unique_modes__report_unify_context(Context, UnifyContext, ModuleInfo, 
		PredId, ProcId, LT, RT)
		-->
	hlds_out__write_unify_context(UnifyContext, Context),
	prog_out__write_context(Context),
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo) },
	{ proc_info_variables(ProcInfo, Varset) },
	(
		{ varset__lookup_name(Varset, LT, _) }
	->
		(
			{ RT = var(RV) },
			\+ { varset__lookup_name(Varset, RV, _) }
		->
			io__write_string("  unification with `"),
			mercury_output_var(LT, Varset)
		;
			io__write_string("  unification of `"),
			mercury_output_var(LT, Varset),
			io__write_string("' and `"),
			hlds_out__write_unify_rhs(RT, ModuleInfo, Varset, 3)
		)
	;
		io__write_string("  unification with `"),
		hlds_out__write_unify_rhs(RT, ModuleInfo, Varset, 3)
	),
	io__write_string("':\n").

%-----------------------------------------------------------------------------%

:- pred unique_modes__report_pred_name_mode(string, list(mode),
		io__state, io__state).
:- mode unique_modes__report_pred_name_mode(in, in, di, uo) is det.

unique_modes__report_pred_name_mode(PredName, ArgModes) -->
	io__write_string(PredName),
	( { ArgModes \= [] } ->
		{ varset__init(InstVarSet) },	% XXX inst var names
		io__write_string("("),
		mercury_output_mode_list(ArgModes, InstVarSet),
		io__write_string(")")
	;
		[]
	).

%-----------------------------------------------------------------------------%

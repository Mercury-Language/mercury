%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: unique_modes.m
% main author: fjh

% This module checks that variables with a unique mode (as opposed to
% a mostly-unique mode) really are unique, and not nondet live - i.e.,
% that they cannot be referenced on backtracking.
% (Actually the term "nondet live" is a bit of a misnomer, because
% really we are just interested in whether something can be referenced
% on backtracking, and this can occur after backtracking in semidet
% code too, not just in nondet code.)

% Basically we just traverse each goal, keeping track of which variables
% are nondet live.  At each procedure call, we check that any arguments
% whose initial insts are required to be unique are not nondet live.
% If they are, we first try selecting a different mode of the same
% predicate, and if that fails, then we report an error message.

% Variables can become nondet live in several places:
% in negations, in the conditions of if-then-elses,
% in disjunctions, and at nondet calls.
% These are the only places at which we can resume execution
% after backtracking.

% XXX we currently make the conservative assumption that
% any non-local variable in a disjunction or nondet call
% is nondet-live - and stays nondet-live.

%-----------------------------------------------------------------------------%

:- module check_hlds__unique_modes.
:- interface. 
:- import_module hlds__hlds_module, hlds__hlds_pred, hlds__hlds_goal.
:- import_module check_hlds__mode_info.
:- import_module io, bool.

	% check every predicate in a module
:- pred unique_modes__check_module(module_info, module_info,
					io__state, io__state).
:- mode unique_modes__check_module(in, out, di, uo) is det.

	% just check a single procedure
:- pred unique_modes__check_proc(proc_id, pred_id, module_info,
				module_info, bool, io__state, io__state).
:- mode unique_modes__check_proc(in, in, in, out, out, di, uo) is det.

	% just check a single goal
:- pred unique_modes__check_goal(hlds_goal, hlds_goal, mode_info, mode_info).
:- mode unique_modes__check_goal(in, out, mode_info_di, mode_info_uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_data, check_hlds__mode_debug.
:- import_module check_hlds__modecheck_unify, check_hlds__modecheck_call.
:- import_module check_hlds__mode_util, parse_tree__prog_out, hlds__hlds_out.
:- import_module parse_tree__mercury_to_mercury, hlds__passes_aux.
:- import_module check_hlds__modes, parse_tree__prog_data.
:- import_module check_hlds__mode_errors, ll_backend__llds.
:- import_module check_hlds__unify_proc.
:- import_module (parse_tree__inst), hlds__instmap, check_hlds__inst_match.
:- import_module check_hlds__inst_util.
:- import_module term, varset.
:- import_module assoc_list, bag, int, list, map.
:- import_module require, set, std_util, string.

%-----------------------------------------------------------------------------%

unique_modes__check_module(ModuleInfo0, ModuleInfo) -->
	check_pred_modes(check_unique_modes, may_change_called_proc,
			ModuleInfo0, ModuleInfo, _UnsafeToContinue).

unique_modes__check_proc(ProcId, PredId, ModuleInfo0, ModuleInfo, Changed) -->
	modecheck_proc(ProcId, PredId,
		check_unique_modes, may_change_called_proc,
		ModuleInfo0, ModuleInfo, NumErrors, Changed),
	( { NumErrors \= 0 } ->
		io__set_exit_status(1)
	;
		[]
	).

	% XXX we currently make the conservative assumption that
	% any non-local variable in a disjunction or nondet call
	% is nondet-live - and stays nondet-live.

unique_modes__check_goal(Goal0, Goal, ModeInfo0, ModeInfo) :-
	%
	% store the current context in the mode_info
	%
	Goal0 = GoalExpr0 - GoalInfo0,
	goal_info_get_context(GoalInfo0, Context),
	term__context_init(EmptyContext),
	( Context = EmptyContext ->
		ModeInfo1 = ModeInfo0
	;
		mode_info_set_context(Context, ModeInfo0, ModeInfo1)
	),

	%
	% Grab the original instmap
	%
	mode_info_get_instmap(ModeInfo1, InstMap0),

	% 
	% Grab the original bag of nondet-live vars
	%
	mode_info_get_nondet_live_vars(ModeInfo1, NondetLiveVars0),

	% 
	% If the goal is not nondet, then nothing is nondet-live,
	% so reset the bag of nondet-live vars to be empty.
	%
	goal_info_get_determinism(GoalInfo0, Detism),
	( determinism_components(Detism, _, at_most_many) ->
		ModeInfo2 = ModeInfo1
	;
		mode_info_set_nondet_live_vars([], ModeInfo1, ModeInfo2)
	),

	%
	% Modecheck the goal
	%
	unique_modes__check_goal_2(GoalExpr0, GoalInfo0, GoalExpr,
		ModeInfo2, ModeInfo3),

	% 
	% Restore the original bag of nondet-live vars
	%
	mode_info_set_nondet_live_vars(NondetLiveVars0, ModeInfo3, ModeInfo4),

	%
	% Grab the final instmap, compute the change in insts
	% over this goal, and save that instmap_delta in the goal_info.
	%
	compute_goal_instmap_delta(InstMap0, GoalExpr,
		GoalInfo0, GoalInfo, ModeInfo4, ModeInfo),

	Goal = GoalExpr - GoalInfo.

	% Make all nondet-live variables whose current inst
	% is `unique' become `mostly_unique'.
	%
:- pred make_all_nondet_live_vars_mostly_uniq(mode_info, mode_info).
:- mode make_all_nondet_live_vars_mostly_uniq(mode_info_di, mode_info_uo)
	is det.

make_all_nondet_live_vars_mostly_uniq(ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, FullInstMap0),
	( instmap__is_reachable(FullInstMap0) ->
		instmap__vars_list(FullInstMap0, AllVars),
		select_nondet_live_vars(AllVars, ModeInfo0, NondetLiveVars),
		make_var_list_mostly_uniq(NondetLiveVars, ModeInfo0, ModeInfo)
	;
		ModeInfo = ModeInfo0
	).

:- pred select_live_vars(list(prog_var), mode_info, list(prog_var)).
:- mode select_live_vars(in, mode_info_ui, out) is det.

select_live_vars([], _, []).
select_live_vars([Var|Vars], ModeInfo, LiveVars) :-
	( mode_info_var_is_live(ModeInfo, Var, live) ->
		LiveVars = [Var | LiveVars1],
		select_live_vars(Vars, ModeInfo, LiveVars1)
	;
		select_live_vars(Vars, ModeInfo, LiveVars)
	).

:- pred select_nondet_live_vars(list(prog_var), mode_info, list(prog_var)).
:- mode select_nondet_live_vars(in, mode_info_ui, out) is det.

select_nondet_live_vars([], _, []).
select_nondet_live_vars([Var|Vars], ModeInfo, NondetLiveVars) :-
	( mode_info_var_is_nondet_live(ModeInfo, Var, live) ->
		NondetLiveVars = [Var | NondetLiveVars1],
		select_nondet_live_vars(Vars, ModeInfo, NondetLiveVars1)
	;
		select_nondet_live_vars(Vars, ModeInfo, NondetLiveVars)
	).

	% Given a list of variables, a delta instmap, and a mode_info,
	% select all the variables whose inst changed in the delta instmap
	% (other than changes which just add information,
	% e.g. `ground -> bound(42)'.)
	%
:- pred select_changed_inst_vars(list(prog_var), instmap_delta, mode_info,
		list(prog_var)).
:- mode select_changed_inst_vars(in, in, mode_info_ui, out) is det.

select_changed_inst_vars([], _DeltaInstMap, _ModeInfo, []).
select_changed_inst_vars([Var | Vars], DeltaInstMap, ModeInfo, ChangedVars) :-
	mode_info_get_module_info(ModeInfo, ModuleInfo),
	mode_info_get_instmap(ModeInfo, InstMap0),
	instmap__lookup_var(InstMap0, Var, Inst0),
	mode_info_get_var_types(ModeInfo, VarTypes),
	map__lookup(VarTypes, Var, Type),
	(
		instmap_delta_is_reachable(DeltaInstMap),
		instmap_delta_search_var(DeltaInstMap, Var, Inst),
		\+ inst_matches_final(Inst, Inst0, Type, ModuleInfo)
	->
		ChangedVars = [Var | ChangedVars1],
		select_changed_inst_vars(Vars, DeltaInstMap, ModeInfo,
			ChangedVars1)
	;
		select_changed_inst_vars(Vars, DeltaInstMap, ModeInfo,
			ChangedVars)
	).

:- pred make_var_list_mostly_uniq(list(prog_var), mode_info, mode_info).
:- mode make_var_list_mostly_uniq(in, mode_info_di, mode_info_uo) is det.

make_var_list_mostly_uniq([], ModeInfo, ModeInfo).
make_var_list_mostly_uniq([Var | Vars], ModeInfo0, ModeInfo) :-
	make_var_mostly_uniq(Var, ModeInfo0, ModeInfo1),
	make_var_list_mostly_uniq(Vars, ModeInfo1, ModeInfo).

:- pred make_var_mostly_uniq(prog_var, mode_info, mode_info).
:- mode make_var_mostly_uniq(in, mode_info_di, mode_info_uo) is det.

make_var_mostly_uniq(Var, ModeInfo0, ModeInfo) :-
	mode_info_get_instmap(ModeInfo0, InstMap0),
	mode_info_get_module_info(ModeInfo0, ModuleInfo0),
	(
		%
		% only variables which are `unique' need to be changed
		%
		instmap__is_reachable(InstMap0),
		instmap__vars_list(InstMap0, Vars),
		list__member(Var, Vars),
		instmap__lookup_var(InstMap0, Var, Inst0),
		inst_expand(ModuleInfo0, Inst0, Inst1),
		( Inst1 = ground(unique, _)
		; Inst1 = bound(unique, _)
		; Inst1 = any(unique)
		)
	->
		make_mostly_uniq_inst(Inst0, ModuleInfo0, Inst, ModuleInfo),
		mode_info_set_module_info(ModeInfo0, ModuleInfo, ModeInfo1),
		instmap__set(InstMap0, Var, Inst, InstMap),
		mode_info_set_instmap(InstMap, ModeInfo1, ModeInfo)
	;
		ModeInfo = ModeInfo0
	).

:- pred unique_modes__check_goal_2(hlds_goal_expr, hlds_goal_info,
		hlds_goal_expr, mode_info, mode_info).
:- mode unique_modes__check_goal_2(in, in, out, mode_info_di, mode_info_uo)
		is det.

unique_modes__check_goal_2(conj(List0), _GoalInfo0, conj(List)) -->
	mode_checkpoint(enter, "conj"),
	( { List0 = [] } ->	% for efficiency, optimize common case
		{ List = [] }
	;
		mode_info_add_goals_live_vars(List0),
		unique_modes__check_conj(List0, List)
	),
	mode_checkpoint(exit, "conj").

unique_modes__check_goal_2(par_conj(List0), GoalInfo0,
		par_conj(List)) -->
	mode_checkpoint(enter, "par_conj"),
	{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
	mode_info_add_live_vars(NonLocals),
		% Build a multiset of the nonlocals of the conjuncts
		% so that we can figure out which variables must be
		% made shared at the start of the parallel conjunction.
	{ make_par_conj_nonlocal_multiset(List0, NonLocalsBag) },
	unique_modes__check_par_conj(List0, NonLocalsBag, List, InstMapList),
	instmap__unify(NonLocals, InstMapList),
	mode_info_remove_live_vars(NonLocals),
	mode_checkpoint(exit, "par_conj").

unique_modes__check_goal_2(disj(List0), GoalInfo0, disj(List)) -->
	mode_checkpoint(enter, "disj"),
	( { List0 = [] } ->
		{ List = [] },
		{ instmap__init_unreachable(InstMap) },
		mode_info_set_instmap(InstMap)
	;
		%
		% If the disjunction creates a choice point (i.e. is model_non),
		% then mark all the variables which are live at the
		% start of the disjunction and whose inst is `unique'
		% as instead being only `mostly_unique', since those variables
		% may be needed again after we backtrack to that choice point
		% and resume forward execution again.
		%
		% Note: for model_det or model_semi disjunctions,
		% we may do some "shallow" backtracking from semidet
		% disjuncts.  But we handle that seperately for each
		% disjunct, in unique_modes__check_disj.
		%
		{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
		{ goal_info_get_determinism(GoalInfo0, Determinism) },
		% does this disjunction create a choice point?
		( { determinism_components(Determinism, _, at_most_many) } ->
			mode_info_add_live_vars(NonLocals),
			make_all_nondet_live_vars_mostly_uniq,
			mode_info_remove_live_vars(NonLocals)
		;
			[]
		),

		%
		% Now just modecheck each disjunct in turn, and then
		% merge the resulting instmaps.
		%
		unique_modes__check_disj(List0, Determinism, NonLocals,
			List, InstMapList),
		instmap__merge(NonLocals, InstMapList, disj)
	),
	mode_checkpoint(exit, "disj").

unique_modes__check_goal_2(if_then_else(Vs, Cond0, Then0, Else0),
		GoalInfo0, Goal) -->
	mode_checkpoint(enter, "if-then-else"),
	{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
	{ goal_get_nonlocals(Cond0, Cond_Vars) },
	{ goal_get_nonlocals(Then0, Then_Vars) },
	{ goal_get_nonlocals(Else0, Else_Vars) },
	mode_info_dcg_get_instmap(InstMap0),
	mode_info_lock_vars(if_then_else, NonLocals),

	%
	% At this point, we should set the inst of any `unique'
	% variables which occur in the condition and which
	% are live to `mostly_unique'.  However, if a variable's
	% inst was unchanged over the condition (i.e. it remains
	% `unique' on exit from the condition), then it is
	% safe to leave it as `unique' on entry to the condition.
	% The only case we need to set it to `mostly_unique' is
	% if the condition would clobber it.
	%
	% XXX actually that is not true; the code below does
	% the wrong thing for examples such as this one:
	%
	% :- mode p(di).
	% p(Var) :-
	%	(if 
	%		(if semidet_succeed then
	%			clobber(Var), fail
	%		else
	%			true
	%		)
	%	then
	%		blah
	%	else
	%		use(Var)
	%	).
	%
	mode_info_add_live_vars(Else_Vars),
	=(ModeInfo),
	{ set__to_sorted_list(Cond_Vars, Cond_Vars_List) },
	{ select_live_vars(Cond_Vars_List, ModeInfo, Cond_Live_Vars) },
	{ Cond0 = _ - Cond0_GoalInfo },
	{ goal_info_get_instmap_delta(Cond0_GoalInfo, Cond0_DeltaInstMap) },
	{ select_changed_inst_vars(Cond_Live_Vars, Cond0_DeltaInstMap,
		ModeInfo, ChangedVars) },
	make_var_list_mostly_uniq(ChangedVars),
	mode_info_remove_live_vars(Else_Vars),

	mode_info_add_live_vars(Then_Vars),
	unique_modes__check_goal(Cond0, Cond),
	mode_info_remove_live_vars(Then_Vars),
	mode_info_unlock_vars(if_then_else, NonLocals),
	mode_info_dcg_get_instmap(InstMapCond),
	( { instmap__is_reachable(InstMapCond) } ->
		unique_modes__check_goal(Then0, Then),
		mode_info_dcg_get_instmap(InstMapThen)
	;
		% We should not mode-analyse the goal, since it is unreachable.
		% Instead we optimize the goal away, so that later passes
		% won't complain about it not having unique mode information.
		{ true_goal(Then) },
		{ InstMapThen = InstMapCond }
	),
	mode_info_set_instmap(InstMap0),
	unique_modes__check_goal(Else0, Else),
	mode_info_dcg_get_instmap(InstMapElse),
	mode_info_set_instmap(InstMap0),
	instmap__merge(NonLocals, [InstMapThen, InstMapElse], if_then_else),
	{ Goal = if_then_else(Vs, Cond, Then, Else) },
	mode_checkpoint(exit, "if-then-else").

unique_modes__check_goal_2(not(A0), GoalInfo0, not(A)) -->
	mode_checkpoint(enter, "not"),
	mode_info_dcg_get_instmap(InstMap0),
	%
	% We need to mark all the variables which are live
	% after the negation as nondet-live for the negated
	% goal, since if the negated goal fails, then the
	% negation will succeed, and so these variables
	% can be accessed again after backtracking.
	%
	{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
	{ set__to_sorted_list(NonLocals, NonLocalsList) },
	=(ModeInfo0),
	{ select_live_vars(NonLocalsList, ModeInfo0, LiveNonLocals) },
	make_var_list_mostly_uniq(LiveNonLocals),
	%
	% But nothing is forward-live for the negated goal, since
	% if the goal succeeds then execution will immediately backtrack.
	% So we need to set the live variables set to empty here.
	%
	=(ModeInfo),
	{ mode_info_get_live_vars(ModeInfo, LiveVars0) },
	mode_info_set_live_vars([]),
	%
	% We need to lock the non-local variables, to ensure
	% that the negation does not bind them.
	%
	mode_info_lock_vars(negation, NonLocals),
	unique_modes__check_goal(A0, A),
	mode_info_unlock_vars(negation, NonLocals),
	mode_info_set_live_vars(LiveVars0),
	mode_info_set_instmap(InstMap0),
	mode_checkpoint(exit, "not").

unique_modes__check_goal_2(some(Vs, CanRemove, G0), _,
		some(Vs, CanRemove, G)) -->
	mode_checkpoint(enter, "some"),
	unique_modes__check_goal(G0, G),
	mode_checkpoint(exit, "some").

unique_modes__check_goal_2(generic_call(GenericCall, Args, Modes, Det),
		_GoalInfo0, Goal) -->
	mode_checkpoint(enter, "generic_call"),
	{ hlds_goal__generic_call_id(GenericCall, CallId) },
	mode_info_set_call_context(call(CallId)),
	{ determinism_components(Det, _, at_most_zero) ->
		NeverSucceeds = yes
	;
		NeverSucceeds = no
	},

	{
		GenericCall = higher_order(_, _, _),
		ArgOffset = 1
	;
		% Class method calls are introduced by the compiler
		% and should be mode correct.
		GenericCall = class_method(_, _, _, _),
		ArgOffset = 0
	;
		% `aditi_insert' and `aditi_delete' goals have type_info
		% arguments for each of the arguments of the tuple to insert
		% added to the start of the argument list by polymorphism.m.
		GenericCall = aditi_builtin(Builtin, UpdatedCallId),
		(
			Builtin = aditi_tuple_insert_delete(_, _),
			UpdatedCallId = _ - _/Arity
		->
			ArgOffset = -Arity
		;
			ArgOffset = 0
		)
	},

	unique_modes__check_call_modes(Args, Modes, ArgOffset,
		Det, NeverSucceeds),
	{ Goal = generic_call(GenericCall, Args, Modes, Det) },
	mode_info_unset_call_context,
	mode_checkpoint(exit, "generic_call").

unique_modes__check_goal_2(call(PredId, ProcId0, Args, Builtin, CallContext,
		PredName), _GoalInfo0, Goal) -->
	{ prog_out__sym_name_to_string(PredName, PredNameString) },
	{ string__append("call ", PredNameString, CallString) },
	mode_checkpoint(enter, CallString),

	=(ModeInfo),
	{ mode_info_get_call_id(ModeInfo, PredId, CallId) },
	mode_info_set_call_context(call(call(CallId))),

	unique_modes__check_call(PredId, ProcId0, Args, ProcId),
	{ Goal = call(PredId, ProcId, Args, Builtin, CallContext, PredName) },
	mode_info_unset_call_context,
	mode_checkpoint(exit, "call").

unique_modes__check_goal_2(unify(A0, B0, _, UnifyInfo0, UnifyContext),
		GoalInfo0, Goal) -->
	mode_checkpoint(enter, "unify"),
	mode_info_set_call_context(unify(UnifyContext)),

	modecheck_unification(A0, B0, UnifyInfo0, UnifyContext, GoalInfo0,
		Goal),

	mode_info_unset_call_context,
	mode_checkpoint(exit, "unify").

unique_modes__check_goal_2(switch(Var, CanFail, Cases0), GoalInfo0,
		switch(Var, CanFail, Cases)) -->
	mode_checkpoint(enter, "switch"),
	( { Cases0 = [] } ->
		{ Cases = [] },
		{ instmap__init_unreachable(InstMap) },
		mode_info_set_instmap(InstMap)
	;
		{ goal_info_get_nonlocals(GoalInfo0, NonLocals) },
		unique_modes__check_case_list(Cases0, Var, Cases, InstMapList),
		instmap__merge(NonLocals, InstMapList, disj)
	),
	mode_checkpoint(exit, "switch").

	% to modecheck a pragma_c_code, we just modecheck the proc for 
	% which it is the goal.
unique_modes__check_goal_2(foreign_proc(Attributes,
		PredId, ProcId0, Args, ArgNameMap, OrigArgTypes, PragmaCode),
		_GoalInfo, Goal) -->
	mode_checkpoint(enter, "foreign_proc"),
	=(ModeInfo),
	{ mode_info_get_call_id(ModeInfo, PredId, CallId) },
	mode_info_set_call_context(call(call(CallId))),
	unique_modes__check_call(PredId, ProcId0, Args, ProcId),
	{ Goal = foreign_proc(Attributes, PredId, ProcId, Args,
			ArgNameMap, OrigArgTypes, PragmaCode) },
	mode_info_unset_call_context,
	mode_checkpoint(exit, "foreign_proc").

unique_modes__check_goal_2(shorthand(_), _, _) -->
	% these should have been expanded out by now
	{ error("unique_modes__check_goal_2: unexpected shorthand") }.

:- pred unique_modes__check_call(pred_id, proc_id, list(prog_var), proc_id, 
			mode_info, mode_info).
:- mode unique_modes__check_call(in, in, in, out,
			mode_info_di, mode_info_uo) is det.

unique_modes__check_call(PredId, ProcId0, ArgVars, ProcId,
		ModeInfo0, ModeInfo) :-
	%
	% set the error list to empty for use below
	% (saving the old error list and instmap in variables)
	%
	mode_info_get_errors(ModeInfo0, OldErrors),
	mode_info_get_instmap(ModeInfo0, InstMap0),
	mode_info_set_errors([], ModeInfo0, ModeInfo1),

	%
	% first off, try using the existing mode
	%
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId0,
			PredInfo, ProcInfo),
	compute_arg_offset(PredInfo, ArgOffset),
	proc_info_argmodes(ProcInfo, ProcArgModes0),
	proc_info_interface_determinism(ProcInfo, InterfaceDeterminism),
	proc_info_never_succeeds(ProcInfo, NeverSucceeds),
	unique_modes__check_call_modes(ArgVars, ProcArgModes0, ArgOffset,
			InterfaceDeterminism, NeverSucceeds,
			ModeInfo1, ModeInfo2),
	( ProcInfo ^ mode_errors = [_|_] ->
		% mode error in callee for this mode
		WaitingVars = set__list_to_set(ArgVars),
		mode_info_get_instmap(ModeInfo2, InstMap),
		instmap__lookup_vars(ArgVars, InstMap, ArgInsts),
		mode_info_error(WaitingVars,
			mode_error_in_callee(ArgVars, ArgInsts,
				PredId, ProcId0,
				ProcInfo ^ mode_errors),
			ModeInfo2, ModeInfo3)
	;
		ModeInfo3 = ModeInfo2
	),

	%
	% see whether or not that worked
	% (and restore the old error list)
	%
	mode_info_get_errors(ModeInfo3, Errors),
	mode_info_set_errors(OldErrors, ModeInfo3, ModeInfo4),
	mode_info_get_may_change_called_proc(ModeInfo4, MayChangeCalledProc),
	( Errors = [] ->
		ProcId = ProcId0,
		ModeInfo = ModeInfo4
	; MayChangeCalledProc = may_not_change_called_proc ->
		% We're not allowed to try a different procedure
		% here, so just return all the errors.
		ProcId = ProcId0,
		list__append(OldErrors, Errors, AllErrors),
		mode_info_set_errors(AllErrors, ModeInfo4, ModeInfo)
	;
		%
		% If it didn't work, restore the original instmap,
		% and then call modecheck_call_pred.
		% That will try all the modes, and will infer
		% new ones if necessary. 
		%
		% We set the declared determinism for newly inferred
		% modes to be the same as the determinism inferred for
		% the existing mode selected by ordinary (non-unique)
		% mode analysis.  This means that determinism analysis
		% will report an error if the determinism changes
		% as a result of unique mode analysis.  That is OK,
		% because uniqueness should not affect determinism.
		%
		mode_info_set_instmap(InstMap0, ModeInfo4, ModeInfo5),
		proc_info_inferred_determinism(ProcInfo, Determinism),
		modecheck_call_pred(PredId, ProcId0, ArgVars, yes(Determinism),
			ProcId, NewArgVars, ExtraGoals, ModeInfo5, ModeInfo),
		
		( NewArgVars = ArgVars, ExtraGoals = no_extra_goals ->
			true
		;
			% this shouldn't happen, since modes.m should do
			% all the handling of implied modes
			% XXX it might happen, though, if the user
			% XXX writes strange code; we should report
			% XXX a proper error here
			error("unique_modes.m: call to implied mode?")
		)
	).

	% to check a call, we just look up the required initial insts
	% for the arguments of the call, and then check for each
	% argument if the variable is nondet-live and the required initial
	% inst was unique.

:- pred unique_modes__check_call_modes(list(prog_var), list(mode), int,
		determinism, bool, mode_info, mode_info).
:- mode unique_modes__check_call_modes(in, in, in, in, in,
			mode_info_di, mode_info_uo) is det.

unique_modes__check_call_modes(ArgVars, ProcArgModes, ArgOffset,
		Determinism, NeverSucceeds, ModeInfo0, ModeInfo) :-
	mode_info_get_module_info(ModeInfo0, ModuleInfo),
	mode_list_get_initial_insts(ProcArgModes, ModuleInfo,
			InitialInsts),
	NeedExactMatch = no,
	modecheck_var_has_inst_list(ArgVars, InitialInsts,
			NeedExactMatch, ArgOffset, InstVarSub,
			ModeInfo0, ModeInfo1),
	mode_list_get_final_insts(ProcArgModes, ModuleInfo, FinalInsts0),
	inst_list_apply_substitution(FinalInsts0, InstVarSub, FinalInsts),
	modecheck_set_var_inst_list(ArgVars, InitialInsts, FinalInsts,
		ArgOffset, NewArgVars, ExtraGoals, ModeInfo1, ModeInfo2),
	( NewArgVars = ArgVars, ExtraGoals = no_extra_goals ->
		true
	;	
		% this shouldn't happen, since modes.m should do
		% all the handling of implied modes
		error("unique_modes.m: call to implied mode?")
	),
	( NeverSucceeds = yes ->
		instmap__init_unreachable(InstMap),
		mode_info_set_instmap(InstMap, ModeInfo2, ModeInfo)
	;
		%
		% Check whether we are at a call to a nondet predicate.
		% If so, mark all the currently nondet-live variables
		% whose inst is `unique' as instead being only `mostly_unique'.
		%
		( determinism_components(Determinism, _, at_most_many) ->
			make_all_nondet_live_vars_mostly_uniq(ModeInfo2,
				ModeInfo)
		;
			ModeInfo = ModeInfo2
		)
	).

%-----------------------------------------------------------------------------%

:- pred unique_modes__check_conj(list(hlds_goal), list(hlds_goal),
		mode_info, mode_info).
:- mode unique_modes__check_conj(in, out, mode_info_di, mode_info_uo) is det.

	% Just process each conjunct in turn.
	% Note that we don't do any reordering of conjuncts here.

unique_modes__check_conj([], []) --> [].
unique_modes__check_conj([Goal0 | Goals0], [Goal | Goals]) -->
	{ goal_get_nonlocals(Goal0, NonLocals) },
	mode_info_remove_live_vars(NonLocals),
	unique_modes__check_goal(Goal0, Goal),
	mode_info_dcg_get_instmap(InstMap),
	( { instmap__is_unreachable(InstMap) } ->
		% We should not mode-analyse the remaining goals, since they
		% are unreachable.  Instead we optimize them away, so that
		% later passes won't complain about them not having
		% unique mode information.
		mode_info_remove_goals_live_vars(Goals0),
		{ Goals  = [] }
	;
		unique_modes__check_conj(Goals0, Goals)
	).

%-----------------------------------------------------------------------------%

	% make_par_conj_nonlocal_multiset builds a multiset (bag) of all
	% the nonlocals of the conjuncts.
:- pred make_par_conj_nonlocal_multiset(list(hlds_goal)::in,
	bag(prog_var)::out) is det.

make_par_conj_nonlocal_multiset([], Empty) :-
	bag__init(Empty).
make_par_conj_nonlocal_multiset([Goal | Goals], NonLocalsMultiSet) :-
	make_par_conj_nonlocal_multiset(Goals, NonLocalsMultiSet0),
	goal_get_nonlocals(Goal, NonLocals),
	set__to_sorted_list(NonLocals, NonLocalsList),
	bag__from_list(NonLocalsList, NonLocalsMultiSet1),
	bag__union(NonLocalsMultiSet0, NonLocalsMultiSet1,
		NonLocalsMultiSet).

	% To unique-modecheck a parallel conjunction, we find the variables
	% that are nonlocal to more than one conjunct and make them shared,
	% then we unique-modecheck the conjuncts.
	%
	% The variables that occur in more than one conjunct must be shared
	% because otherwise it would be possible to make them become clobbered
	% which would introduce an implicit dependency between the conjuncts
	% which we do not allow.
:- pred unique_modes__check_par_conj(list(hlds_goal), bag(prog_var),
		list(hlds_goal), list(pair(instmap, set(prog_var))),
		mode_info, mode_info).
:- mode unique_modes__check_par_conj(in, in, out, out,
		mode_info_di, mode_info_uo) is det.

unique_modes__check_par_conj(Goals0, NonLocalVarsBag, Goals, Instmaps) -->
	unique_modes__check_par_conj_0(NonLocalVarsBag),
	unique_modes__check_par_conj_1(Goals0, Goals, Instmaps).

		% Figure out which variables occur in more than one
		% conjunct and make them shared.
:- pred unique_modes__check_par_conj_0(bag(prog_var), mode_info, mode_info).
:- mode unique_modes__check_par_conj_0(in, mode_info_di, mode_info_uo) is det.

unique_modes__check_par_conj_0(NonLocalVarsBag, ModeInfo0, ModeInfo) :-
	bag__to_assoc_list(NonLocalVarsBag, NonLocalVarsList),
	list__filter_map((pred(Pair::in, Var::out) is semidet :-
		Pair = Var - Multiplicity,
		Multiplicity > 1
	), NonLocalVarsList, SharedList),
	mode_info_dcg_get_instmap(InstMap0, ModeInfo0, ModeInfo1),
	instmap__lookup_vars(SharedList, InstMap0, VarInsts),
	mode_info_get_module_info(ModeInfo1, ModuleInfo0),
	make_shared_inst_list(VarInsts, ModuleInfo0,
		SharedVarInsts, ModuleInfo1),
	mode_info_set_module_info(ModeInfo1, ModuleInfo1, ModeInfo2),
	instmap__set_vars(InstMap0, SharedList, SharedVarInsts, InstMap1),
	mode_info_set_instmap(InstMap1, ModeInfo2, ModeInfo).

	% Just process each conjunct in turn.
	% Because we have already done modechecking, we know that
	% there are no attempts to bind a variable in multiple
	% parallel conjuncts, so we don't need to lock/unlock variables.

:- pred unique_modes__check_par_conj_1(list(hlds_goal), list(hlds_goal),
		list(pair(instmap, set(prog_var))), mode_info, mode_info).
:- mode unique_modes__check_par_conj_1(in, out, out,
		mode_info_di, mode_info_uo) is det.

unique_modes__check_par_conj_1([], [], []) --> [].
unique_modes__check_par_conj_1([Goal0 | Goals0], [Goal | Goals],
		[InstMap - NonLocals|InstMaps]) -->
	{ goal_get_nonlocals(Goal0, NonLocals) },
	mode_info_dcg_get_instmap(InstMap0),
	unique_modes__check_goal(Goal0, Goal),
	mode_info_dcg_get_instmap(InstMap),
	mode_info_set_instmap(InstMap0),
	unique_modes__check_par_conj_1(Goals0, Goals, InstMaps).

%-----------------------------------------------------------------------------%

	% Process each of the disjunctions in turn, making sure to restore
	% the original instmap before processing the next one.
	% Collect up a list of the resulting instmaps.

:- pred unique_modes__check_disj(list(hlds_goal), determinism, set(prog_var),
		list(hlds_goal), list(instmap), mode_info, mode_info).
:- mode unique_modes__check_disj(in, in, in, out, out,
		mode_info_di, mode_info_uo) is det.

unique_modes__check_disj([], _, _, [], []) --> [].
unique_modes__check_disj([Goal0 | Goals0], DisjDetism, DisjNonLocals,
		[Goal | Goals], [InstMap | InstMaps]) -->
	mode_info_dcg_get_instmap(InstMap0),
	(
		%
		% If the disjunction was model_nondet, then we already marked
		% all the non-locals as only being mostly-unique, so we
		% don't need to do anything special here...
		%
		{ \+ determinism_components(DisjDetism, _, at_most_many) },

		%
		% ... but for model_semi or model_det disjunctions, if the
		% _disjunct_ can fail, then we still might backtrack to another
		% disjunct, so again in that case we need to mark all the
		% non-locals as being only mostly-unique rather than unique.
		%
		{ Goal0 = _ - GoalInfo0 },
		{ goal_info_get_determinism(GoalInfo0, Determinism) },
		{ determinism_components(Determinism, CanFail, _) },
		{ CanFail = can_fail }
	->
		mode_info_add_live_vars(DisjNonLocals),
		make_all_nondet_live_vars_mostly_uniq,
		mode_info_remove_live_vars(DisjNonLocals)
	;
		[]
	),
	unique_modes__check_goal(Goal0, Goal),
	mode_info_dcg_get_instmap(InstMap),
	mode_info_set_instmap(InstMap0),
	unique_modes__check_disj(Goals0, DisjDetism, DisjNonLocals,
		Goals, InstMaps).

%-----------------------------------------------------------------------------%

:- pred unique_modes__check_case_list(list(case), prog_var, list(case),
		list(instmap), mode_info, mode_info).
:- mode unique_modes__check_case_list(in, in, out, out,
		mode_info_di, mode_info_uo) is det.

unique_modes__check_case_list([], _Var, [], []) --> [].
unique_modes__check_case_list([Case0 | Cases0], Var,
			[Case | Cases], [InstMap | InstMaps]) -->
	{ Case0 = case(ConsId, Goal0) },
	{ Case = case(ConsId, Goal) },
	=(ModeInfo0),
	{ mode_info_get_instmap(ModeInfo0, InstMap0) },

	% record the fact that Var was bound to ConsId in the
	% instmap before processing this case
	modecheck_functor_test(Var, ConsId),

	mode_info_dcg_get_instmap(InstMap1),
	( { instmap__is_reachable(InstMap1) } ->
		unique_modes__check_goal(Goal0, Goal1)
	;
		% We should not mode-analyse the goal, since it is unreachable.
		% Instead we optimize the goal away, so that later passes
		% won't complain about it not having unique mode information.
		{ true_goal(Goal1) }
	),

	mode_info_dcg_get_instmap(InstMap),
	{ fixup_switch_var(Var, InstMap0, InstMap, Goal1, Goal) },

	mode_info_set_instmap(InstMap0),
	unique_modes__check_case_list(Cases0, Var, Cases, InstMaps).

%-----------------------------------------------------------------------------%

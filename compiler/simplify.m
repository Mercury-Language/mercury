%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main authors: zs, stayl.
%
% The two jobs of the simplification module are
%
%	to find and exploit opportunities for simplifying the internal form
%	of the program, both to optimize the code and to massage the code
%	into a form the code generator will accept, and
%
%	to warn the programmer about any constructs that are so simple that
%	they should not have been included in the program in the first place.
%
% Simplification is done in two passes. The first pass performs common
% structure and duplicate call elimination. The second pass performs
% excess assignment elimination and cleans up the code after the first pass.
% Two passes are required because the goal must be requantified after the
% optimizations in common.m are run so that excess assignment elimination
% works properly.
%
%-----------------------------------------------------------------------------%

:- module simplify.

:- interface.

:- import_module hlds_goal, hlds_module, hlds_pred, det_report, det_util.
:- import_module common, instmap, globals.
:- import_module io, bool, list, map, term, varset.

:- pred simplify__pred(list(simplification), pred_id, module_info, module_info,
	pred_info, pred_info, int, int, io__state, io__state).
:- mode simplify__pred(in, in, in, out, in, out, out, out, di, uo) is det.

:- pred simplify__proc(list(simplification), pred_id, proc_id,
	module_info, module_info, proc_info, proc_info, io__state, io__state).
:- mode simplify__proc(in, in, in, in, out, in, out, di, uo) is det.

:- pred simplify__process_goal(hlds_goal, hlds_goal,
		simplify_info, simplify_info).
:- mode simplify__process_goal(in, out, in, out) is det.
	
	% Find out which simplifications should be run from the options table
	% stored in the globals. The first argument states whether warnings
	% should be issued during this pass of simplification.
:- pred simplify__find_simplifications(bool, globals, list(simplification)).
:- mode simplify__find_simplifications(in, in, out) is det.

:- type simplification
	--->	warn_simple_code	% --warn-simple-code
	;	warn_duplicate_calls	% --warn-duplicate-calls
	;	do_once			% run things that should be done once
	;	excess_assigns		% remove excess assignment unifications
	;	duplicate_calls		% optimize duplicate calls
	;	constant_prop		% partially evaluate calls
	;	common_struct		% common structure elimination
	;	extra_common_struct	% do common structure elimination
					% even when it might increase stack
					% usage (used by deforestation).
	.

:- type simplify_info.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module code_aux, det_analysis, follow_code, goal_util, const_prop.
:- import_module hlds_module, hlds_data, (inst), inst_match.
:- import_module options, passes_aux, prog_data, mode_util, type_util.
:- import_module code_util, quantification, modes, purity, pd_cost.
:- import_module set, require, std_util, int.

%-----------------------------------------------------------------------------%

simplify__pred(Simplifications0, PredId, ModuleInfo0, ModuleInfo,
		PredInfo0, PredInfo, WarnCnt, ErrCnt) -->
	write_pred_progress_message("% Simplifying ", PredId, ModuleInfo0),
	{ pred_info_non_imported_procids(PredInfo0, ProcIds) },
	{ MaybeMsgs0 = no },
	{
		% Don't warn for compiler-generated procedures.
		list__member(warn_simple_code, Simplifications0),
		module_info_pred_info(ModuleInfo0, PredId, PredInfo0),
		code_util__compiler_generated(PredInfo0)
	->
		list__delete_all(Simplifications0, warn_simple_code,
			Simplifications)
	;
		Simplifications = Simplifications0
	},
	simplify__procs(Simplifications, PredId, ProcIds, ModuleInfo0,
		ModuleInfo, PredInfo0, PredInfo, MaybeMsgs0, MaybeMsgs),
	( { MaybeMsgs = yes(Msgs0 - Msgs1) } ->
		{ set__union(Msgs0, Msgs1, Msgs2) },
		{ set__to_sorted_list(Msgs2, Msgs) },
		det_report_msgs(Msgs, ModuleInfo, WarnCnt, ErrCnt)
	;
		{ WarnCnt = 0 },
		{ ErrCnt = 0 }
	).

:- pred simplify__procs(list(simplification), pred_id, list(proc_id),
		module_info, module_info, pred_info, pred_info,
		maybe(pair(set(det_msg))), maybe(pair(set(det_msg))),
		io__state, io__state).
:- mode simplify__procs(in, in, in, in, out, in, out,
		in, out, di, uo) is det.

simplify__procs(_, _, [], ModuleInfo, ModuleInfo, PredInfo, PredInfo,
		Msgs, Msgs) --> [].
simplify__procs(Simplifications, PredId, [ProcId | ProcIds], ModuleInfo0,
		ModuleInfo, PredInfo0, PredInfo, MaybeMsgs0, MaybeMsgs) -->
	{ pred_info_procedures(PredInfo0, Procs0) },
	{ map__lookup(Procs0, ProcId, Proc0) },	
	simplify__proc_2(Simplifications, PredId, ProcId, ModuleInfo0,
			ModuleInfo1, Proc0, Proc, Msgs1),
	{ map__det_update(Procs0, ProcId, Proc, Procs) },
	{ pred_info_set_procedures(PredInfo0, Procs, PredInfo1) },
	{ set__to_sorted_list(Msgs1, Msgs2) },
	{ list__filter(lambda([Msg::in] is semidet,
		det_msg_is_any_mode_msg(Msg, any_mode)),
		Msgs2, AnyModeMsgs1, AllModeMsgs1) },
	{ set__sorted_list_to_set(AnyModeMsgs1, AnyModeMsgs2) },
	{ set__sorted_list_to_set(AllModeMsgs1, AllModeMsgs2) },
	{ MaybeMsgs0 = yes(AnyModeMsgs0 - AllModeMsgs0) ->
		set__union(AnyModeMsgs0, AnyModeMsgs2, AnyModeMsgs),
		set__intersect(AllModeMsgs0, AllModeMsgs2, AllModeMsgs),
		MaybeMsgs1 = yes(AllModeMsgs - AnyModeMsgs)
	;
		MaybeMsgs1 = yes(AnyModeMsgs2 - AllModeMsgs2)
	},
	simplify__procs(Simplifications, PredId, ProcIds, ModuleInfo1, 
		ModuleInfo, PredInfo1, PredInfo, MaybeMsgs1, MaybeMsgs).

simplify__proc(Simplifications, PredId, ProcId, ModuleInfo0, ModuleInfo,
		Proc0, Proc)  -->
	write_pred_progress_message("% Simplifying ", PredId, ModuleInfo0),
	simplify__proc_2(Simplifications, PredId, ProcId, ModuleInfo0,
			ModuleInfo, Proc0, Proc, _).

:- pred simplify__proc_2(list(simplification), pred_id, proc_id, module_info,
		module_info, proc_info, proc_info, set(det_msg),
		io__state, io__state).
:- mode simplify__proc_2(in, in, in, in, out, in, out, 
		out, di, uo) is det.

simplify__proc_2(Simplifications, PredId, ProcId, ModuleInfo0, ModuleInfo,
		ProcInfo0, ProcInfo, Msgs, State0, State) :-

	globals__io_get_globals(Globals, State0, State),
	det_info_init(ModuleInfo0, PredId, ProcId, Globals, DetInfo0),
	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap0),
	proc_info_varset(ProcInfo0, VarSet0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	proc_info_goal(ProcInfo0, Goal0),

	simplify_info_init(DetInfo0, Simplifications, InstMap0,
		VarSet0, VarTypes0, Info0),
	simplify__process_goal(Goal0, Goal, Info0, Info),
	
	simplify_info_get_varset(Info, VarSet),
	simplify_info_get_var_types(Info, VarTypes),
	proc_info_set_varset(ProcInfo0, VarSet, ProcInfo1),
	proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo2),
	proc_info_set_goal(ProcInfo2, Goal, ProcInfo),
	simplify_info_get_module_info(Info, ModuleInfo),
	simplify_info_get_msgs(Info, Msgs).

simplify__process_goal(Goal0, Goal, Info0, Info) :-
	simplify_info_get_simplifications(Info0, Simplifications0),
	simplify_info_get_instmap(Info0, InstMap0),

	( (simplify_do_common(Info0); simplify_do_calls(Info0)) ->
		% On the first pass do common structure and call elimination. 
		NotOnFirstPass = [do_once, excess_assigns],

		set__delete_list(Simplifications0, NotOnFirstPass,
			Simplifications1),
		simplify_info_set_simplifications(Info0, Simplifications1,
			Info1),
		
		simplify__do_process_goal(Goal0, Goal1, Info1, Info2),

		NotOnSecondPass = [warn_simple_code, warn_duplicate_calls,
			common_struct, duplicate_calls],
		set__delete_list(Simplifications0, NotOnSecondPass,
			Simplifications2),
		simplify_info_reinit(Simplifications2, InstMap0, Info2, Info3)
	;
		Info3 = Info0,
		Goal1 = Goal0
	),
		% On the second pass do excess assignment elimination and
		% some cleaning up after the common structure pass.
	simplify__do_process_goal(Goal1, Goal, Info3, Info).

:- pred simplify__do_process_goal(hlds_goal::in, hlds_goal::out,
		simplify_info::in, simplify_info::out) is det.

simplify__do_process_goal(Goal0, Goal, Info0, Info) :-
	simplify_info_get_instmap(Info0, InstMap0),
	simplify__goal(Goal0, Goal1, Info0, Info1),
	simplify_info_get_varset(Info1, VarSet0),
	simplify_info_get_var_types(Info1, VarTypes0),
	( simplify_info_requantify(Info1) ->
		Goal1 = _ - GoalInfo1,
		goal_info_get_nonlocals(GoalInfo1, NonLocals),

		implicitly_quantify_goal(Goal1, VarSet0, VarTypes0, NonLocals,
			Goal2, VarSet, VarTypes, _),

		simplify_info_set_varset(Info1, VarSet, Info2),
		simplify_info_set_var_types(Info2, VarTypes, Info3),

		% Always recompute instmap_deltas for atomic goals - this
		% is safer in the case where unused variables should no
		% longer be included in the instmap_delta for a goal.
		% In the alias branch this is necessary anyway.
		RecomputeAtomic = yes,

		simplify_info_get_module_info(Info3, ModuleInfo1),
		recompute_instmap_delta(RecomputeAtomic, Goal2, Goal3,
			InstMap0, ModuleInfo1, ModuleInfo),
		simplify_info_set_module_info(Info3, ModuleInfo, Info)
	;
		Goal3 = Goal1,
		Info = Info1
	),
	( simplify_info_rerun_det(Info) ->
		Goal0 = _ - GoalInfo0,
		goal_info_get_determinism(GoalInfo0, Det),
		det_get_soln_context(Det, SolnContext),
		simplify_info_get_det_info(Info, DetInfo),
		det_infer_goal(Goal3, InstMap0, SolnContext,
			DetInfo, Goal, _, _)
	;
		Goal = Goal3
	).

%-----------------------------------------------------------------------------%

simplify__find_simplifications(WarnThisPass, Globals, S) :-
	simplify__find_simplifications_2(WarnThisPass, Globals, [], S).

:- pred simplify__find_simplifications_2(bool, globals, 
		list(simplification), list(simplification)).
:- mode simplify__find_simplifications_2(in, in, in, out) is det.

simplify__find_simplifications_2(WarnThisPass, Globals) -->
	( { WarnThisPass = yes } ->
		simplify__lookup_option(Globals, warn_duplicate_calls,
			warn_duplicate_calls),
		simplify__lookup_option(Globals, warn_simple_code,
			warn_simple_code)
	;
		[]
	),
	simplify__lookup_option(Globals, excess_assign, excess_assigns),
	simplify__lookup_option(Globals, common_struct, common_struct),
	simplify__lookup_option(Globals, optimize_duplicate_calls,
		duplicate_calls),
	simplify__lookup_option(Globals, constant_propagation,
		constant_prop).
	
:- pred simplify__lookup_option(globals::in, option::in, simplification::in,
		list(simplification)::in, list(simplification)::out) is det.

simplify__lookup_option(Globals, Option, Simplification,
		Simplifications0, Simplifications) :-
	globals__lookup_bool_option(Globals, Option, Result),
	( Result = yes ->
		Simplifications = [Simplification | Simplifications0]
	;
		Simplifications = Simplifications0
	).

%-----------------------------------------------------------------------------%

:- pred simplify__goal(hlds_goal, hlds_goal, simplify_info, simplify_info).
:- mode simplify__goal(in, out, in, out) is det.

simplify__goal(Goal0, Goal - GoalInfo, Info0, Info) :-
	Goal0 = _ - GoalInfo0,
	goal_info_get_determinism(GoalInfo0, Detism),
	simplify_info_get_det_info(Info0, DetInfo),
	simplify_info_get_module_info(Info0, ModuleInfo),
	(
		%
		% if --no-fully-strict,
		% replace goals with determinism failure with `fail'.
		% XXX we should warn about this (if the goal wasn't `fail')
		%
		Detism = failure,
		% ensure goal is pure or semipure
		\+ goal_info_is_impure(GoalInfo0),
		( det_info_get_fully_strict(DetInfo, no)
		; code_aux__goal_cannot_loop(ModuleInfo, Goal0)
		)
	->
		% If the goal had any non-locals we should requantify. 
		goal_info_get_nonlocals(GoalInfo0, NonLocals0),
		( set__empty(NonLocals0) ->
			Info1 = Info0
		;
			simplify_info_set_requantify(Info0, Info1)
		),
		pd_cost__goal(Goal0, CostDelta),
		simplify_info_incr_cost_delta(Info1, CostDelta, Info2),
		goal_info_get_context(GoalInfo0, Context),
		fail_goal(Context, Goal1)
	;
		%
		% if --no-fully-strict,
		% replace goals which cannot fail and have no
		% output variables with `true'.
		% However, we don't do this for erroneous goals,
		% since these may occur in conjunctions where there
		% are no producers for some variables, and the
		% code generator would fail for these.
		% XXX we should warn about this (if the goal wasn't `true')
		%

		% XXX this optimization is currently disabled for anything
		% other than unifications, since it mishandles calls to
		% existentially typed predicates. 
		% The fix for this is to run polymorphism.m before simplify.m.
		% When that is done, we can re-enable this optimization.
		Goal0 = unify(_, _, _, _, _) - _,
		
		determinism_components(Detism, cannot_fail, MaxSoln),
		MaxSoln \= at_most_zero,
		goal_info_get_instmap_delta(GoalInfo0, InstMapDelta),
		goal_info_get_nonlocals(GoalInfo0, NonLocalVars),
		simplify_info_get_instmap(Info0, InstMap0),
		det_no_output_vars(NonLocalVars, InstMap0, InstMapDelta,
			DetInfo),
		% ensure goal is pure or semipure
		\+ goal_info_is_impure(GoalInfo0),
		( det_info_get_fully_strict(DetInfo, no)
		; code_aux__goal_cannot_loop(ModuleInfo, Goal0)
		)
	->
		% If the goal had any non-locals we should requantify. 
		goal_info_get_nonlocals(GoalInfo0, NonLocals0),
		( set__empty(NonLocals0) ->
			Info1 = Info0
		;
			simplify_info_set_requantify(Info0, Info1)
		),
		pd_cost__goal(Goal0, CostDelta),
		simplify_info_incr_cost_delta(Info1, CostDelta, Info2),
		goal_info_get_context(GoalInfo0, Context),
		true_goal(Context, Goal1)
	;
		Goal1 = Goal0,
		Info2 = Info0
	),
	simplify_info_maybe_clear_structs(before, Goal1, Info2, Info3),
	Goal1 = GoalExpr1 - GoalInfo1,
	simplify__goal_2(GoalExpr1, GoalInfo1, Goal, GoalInfo2, Info3, Info4),
	simplify_info_maybe_clear_structs(after, Goal - GoalInfo2,
		Info4, Info5),
	simplify__enforce_invariant(GoalInfo2, GoalInfo, Info5, Info).

:- pred simplify__enforce_invariant(hlds_goal_info, hlds_goal_info,
		simplify_info, simplify_info).
:- mode simplify__enforce_invariant(in, out, in, out) is det.
	%
	% Ensure that the mode information and the determinism
	% information say consistent things about unreachability.
	%
simplify__enforce_invariant(GoalInfo0, GoalInfo, Info0, Info) :-
	goal_info_get_determinism(GoalInfo0, Determinism0),
	goal_info_get_instmap_delta(GoalInfo0, DeltaInstmap0),
	determinism_components(Determinism0, CanFail0, NumSolns0),
	(
		NumSolns0 = at_most_zero,
		instmap_delta_is_reachable(DeltaInstmap0)
	->
		instmap_delta_init_unreachable(UnreachableInstMapDelta),
		goal_info_set_instmap_delta(GoalInfo0, UnreachableInstMapDelta,
			GoalInfo),
		simplify_info_set_rerun_det(Info0, Info)
	;
		instmap_delta_is_unreachable(DeltaInstmap0),
		NumSolns0 \= at_most_zero
	->
		determinism_components(Determinism, CanFail0, at_most_zero),
		goal_info_set_determinism(GoalInfo0, Determinism, GoalInfo),
		simplify_info_set_rerun_det(Info0, Info)
	;
		GoalInfo = GoalInfo0,
		Info = Info0
	).

%-----------------------------------------------------------------------------%

:- pred simplify__goal_2(hlds_goal_expr, hlds_goal_info, hlds_goal_expr,
		hlds_goal_info, simplify_info, simplify_info).
:- mode simplify__goal_2(in, in, out, out, in, out) is det.

simplify__goal_2(conj(Goals0), GoalInfo0, Goal, GoalInfo, Info0, Info) :-
	simplify_info_get_instmap(Info0, InstMap0),
	simplify__conj(Goals0, [], Goals, GoalInfo0, Info0, Info1),
	simplify_info_set_instmap(Info1, InstMap0, Info2),
	( Goals = [] ->
		goal_info_get_context(GoalInfo0, Context),
		true_goal(Context, Goal - GoalInfo),
		Info = Info2
	; Goals = [SingleGoal - SingleGoalInfo] ->
		% a singleton conjunction is equivalent to the goal itself
		simplify__maybe_wrap_goal(GoalInfo0, SingleGoalInfo, 
			SingleGoal, Goal, GoalInfo, Info2, Info)
	;
		%
		% Conjunctions that cannot produce solutions may nevertheless
		% contain nondet and multidet goals. If this happens, the
		% conjunction is put inside a `some' to appease the code
		% generator.
		%
		Info = Info2,
		goal_info_get_determinism(GoalInfo0, Detism),
		(
			simplify_do_once(Info),
			determinism_components(Detism, CanFail, at_most_zero),
			simplify__contains_multisoln_goal(Goals)
		->
			determinism_components(InnerDetism,
				CanFail, at_most_many),
			goal_info_set_determinism(GoalInfo0,
				InnerDetism, InnerInfo),
			InnerGoal = conj(Goals) - InnerInfo,
			Goal = some([], InnerGoal)
		;
			Goal = conj(Goals)
		),
		GoalInfo = GoalInfo0
	).

simplify__goal_2(par_conj(Goals0, SM), GoalInfo0, Goal,
		GoalInfo, Info0, Info) :-
	(
		Goals0 = []
	->
		goal_info_get_context(GoalInfo0, Context),
		true_goal(Context, Goal - GoalInfo),
		Info = Info0
	;
		Goals0 = [SingleGoal0]
	->
		simplify__goal(SingleGoal0, SingleGoal - SingleGoalInfo,
			Info0, Info1),
		simplify__maybe_wrap_goal(GoalInfo0, SingleGoalInfo,
			SingleGoal, Goal, GoalInfo, Info1, Info)
	;
		GoalInfo = GoalInfo0,
		simplify__par_conj(Goals0, Goals, Info0, Info0, Info),
		Goal = par_conj(Goals, SM)
	).

simplify__goal_2(disj(Disjuncts0, SM), GoalInfo0,
		Goal, GoalInfo, Info0, Info) :-
	simplify_info_get_instmap(Info0, InstMap0),
	simplify__disj(Disjuncts0, [], Disjuncts, [], InstMaps,
			Info0, Info0, Info1),
	( Disjuncts = [] ->
		goal_info_get_context(GoalInfo0, Context),
		fail_goal(Context, Goal - GoalInfo),
		Info = Info1
	; Disjuncts = [SingleGoal] ->
		% a singleton disjunction is equivalent to the goal itself
		SingleGoal = Goal1 - GoalInfo1,
		simplify__maybe_wrap_goal(GoalInfo0, GoalInfo1,
			Goal1, Goal, GoalInfo, Info1, Info)
	;
		Goal = disj(Disjuncts, SM),
		simplify_info_get_module_info(Info1, ModuleInfo1),
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		merge_instmap_deltas(InstMap0, NonLocals, InstMaps,
			NewDelta, ModuleInfo1, ModuleInfo2),
		simplify_info_set_module_info(Info1, ModuleInfo2, Info),
		goal_info_set_instmap_delta(GoalInfo0, NewDelta, GoalInfo)
	).

simplify__goal_2(switch(Var, SwitchCanFail0, Cases0, SM),
		GoalInfo0, Goal, GoalInfo, Info0, Info) :-
	simplify_info_get_instmap(Info0, InstMap0),
	simplify_info_get_module_info(Info0, ModuleInfo0),
	instmap__lookup_var(InstMap0, Var, VarInst),
	( inst_is_bound_to_functors(ModuleInfo0, VarInst, Functors) ->
		functors_to_cons_ids(Functors, ConsIds0),
		list__sort(ConsIds0, ConsIds),
		delete_unreachable_cases(Cases0, ConsIds, Cases1),
		MaybeConsIds = yes(ConsIds)
	;
		Cases1 = Cases0,
		MaybeConsIds = no
	),
	simplify__switch(Var, Cases1, [], Cases, [], InstMaps, 
		SwitchCanFail0, SwitchCanFail, Info0, Info0, Info1),
	( Cases = [] ->
		% An empty switch always fails.
		pd_cost__eliminate_switch(CostDelta),
		simplify_info_incr_cost_delta(Info1, CostDelta, Info),
		goal_info_get_context(GoalInfo0, Context),
		fail_goal(Context, Goal - GoalInfo)
	; Cases = [case(ConsId, SingleGoal)] ->
		% a singleton switch is equivalent to the goal itself with 
		% a possibly can_fail unification with the functor on the front.
		cons_id_arity(ConsId, Arity),
		(
			SwitchCanFail = can_fail,
			MaybeConsIds \= yes([ConsId])
		->
			simplify__create_test_unification(Var, ConsId, Arity,
				UnifyGoal, Info1, Info2),

			% Conjoin the test and the rest of the case.
			goal_to_conj_list(SingleGoal, SingleGoalConj),
			GoalList = [UnifyGoal | SingleGoalConj],

			% Work out the nonlocals, instmap_delta 
			% and determinism of the entire conjunction.
			goal_info_get_nonlocals(GoalInfo0, NonLocals0),
			set__insert(NonLocals0, Var, NonLocals),
			goal_info_get_instmap_delta(GoalInfo0, InstMapDelta0),
			simplify_info_get_instmap(Info2, InstMap),
			instmap_delta_bind_var_to_functor(Var, ConsId, 	
				InstMap, InstMapDelta0, InstMapDelta, 
				ModuleInfo0, ModuleInfo),
			simplify_info_set_module_info(Info2, 
				ModuleInfo, Info3),	
			goal_info_get_determinism(GoalInfo0, CaseDetism),
			det_conjunction_detism(semidet, CaseDetism, Detism),
			goal_info_init(NonLocals, InstMapDelta, Detism, 
				CombinedGoalInfo),

			simplify_info_set_requantify(Info3, Info4),
			Goal = conj(GoalList),
			GoalInfo = CombinedGoalInfo
		;
			% The var can only be bound to this cons_id, so
			% a test is unnecessary.
			SingleGoal = Goal - GoalInfo,
			Info4 = Info1
		),
		pd_cost__eliminate_switch(CostDelta),
		simplify_info_incr_cost_delta(Info4, CostDelta, Info)
	;
		Goal = switch(Var, SwitchCanFail, Cases, SM),
		simplify_info_get_module_info(Info1, ModuleInfo1),
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		merge_instmap_deltas(InstMap0, NonLocals, InstMaps,
			NewDelta, ModuleInfo1, ModuleInfo2),
		simplify_info_set_module_info(Info1, ModuleInfo2, Info),
		goal_info_set_instmap_delta(GoalInfo0, NewDelta, GoalInfo)
	).

simplify__goal_2(Goal0, GoalInfo, Goal, GoalInfo, Info0, Info) :-
	Goal0 = higher_order_call(Closure, Args, _, Modes, Det, _PredOrFunc),
	( simplify_do_calls(Info0) ->
		common__optimise_higher_order_call(Closure, Args, Modes, Det,
			Goal0, GoalInfo, Goal, Info0, Info)
	; simplify_do_warn_calls(Info0) ->
		% we need to do the pass, for the warnings, but we ignore
		% the optimized goal and instead use the original one
		common__optimise_higher_order_call(Closure, Args, Modes, Det,
			Goal0, GoalInfo, _Goal1, Info0, Info),
		Goal = Goal0
	;
		Goal = Goal0,
		Info = Info0
	).

	% XXX We ought to do duplicate call elimination for class 
	% XXX method calls here.
simplify__goal_2(Goal, GoalInfo, Goal, GoalInfo, Info, Info) :-
	Goal = class_method_call(_, _, _, _, _, _).

simplify__goal_2(Goal0, GoalInfo0, Goal, GoalInfo, Info0, Info) :-
	Goal0 = call(PredId, ProcId, Args, IsBuiltin, _, _),
	simplify_info_get_module_info(Info0, ModuleInfo),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo,
		ProcInfo),

	%
	% check for calls to predicates with `pragma obsolete' declarations
	%
	(
		simplify_do_warn(Info0),
		pred_info_get_markers(PredInfo, Markers),
		check_marker(Markers, obsolete),
		%
		% Don't warn about directly recursive calls.
		% (That would cause spurious warnings, particularly
		% with builtin predicates, or preds defined using
		% pragma c_code.)
		%
		simplify_info_get_det_info(Info0, DetInfo0),
		det_info_get_pred_id(DetInfo0, ThisPredId),
		PredId \= ThisPredId
	->

		goal_info_get_context(GoalInfo0, Context1),
		simplify_info_add_msg(Info0, warn_obsolete(PredId, Context1),
			Info1)
	;
		Info1 = Info0
	),

	%
	% Check for recursive calls with the same input arguments,
	% and warn about them (since they will lead to infinite loops).
	%
	(
		simplify_do_warn(Info1),

		%
		% Is this a (directly) recursive call,
		% i.e. is the procedure being called the same as the
		% procedure we're analyzing?
		%
		simplify_info_get_det_info(Info1, DetInfo),
		det_info_get_pred_id(DetInfo, PredId),
		det_info_get_proc_id(DetInfo, ProcId),

		%
		% Don't count inline builtins.
		% (The compiler generates code for builtins that looks
		% recursive, so that you can take their address, but since
		% the recursive call actually expands into inline
		% instructions, so it's not infinite recursion.)
		%
		IsBuiltin \= inline_builtin,

		%
		% Don't warn if we're inside a lambda goal, because the
		% recursive call may not be executed.
		%
		\+ simplify_info_inside_lambda(Info1),

		%
		% Are the input arguments the same (or equivalent)?
		%
		simplify_info_get_module_info(Info1, ModuleInfo1),
		module_info_pred_proc_info(ModuleInfo1, PredId, ProcId,
			_PredInfo1, ProcInfo1),
		proc_info_headvars(ProcInfo1, HeadVars),
		proc_info_argmodes(ProcInfo1, ArgModes),
		simplify_info_get_common_info(Info1, CommonInfo1),
		simplify__input_args_are_equiv(Args, HeadVars, ArgModes,
			CommonInfo1, ModuleInfo1),

		% 
		% Don't count procs using minimal evaluation as they 
		% should always terminate if they have a finite number
		% of answers. 
		%
		\+ proc_info_eval_method(ProcInfo, eval_minimal)	
	->	
		goal_info_get_context(GoalInfo0, Context2),
		simplify_info_add_msg(Info1, warn_infinite_recursion(Context2),
				Info2)
	;
		Info2 = Info1
	),

	%
	% check for duplicate calls to the same procedure
	%
	( simplify_do_calls(Info2),
	  goal_info_is_pure(GoalInfo0)
	->	
		common__optimise_call(PredId, ProcId, Args, Goal0, GoalInfo0,
			Goal1, Info2, Info3)
	; simplify_do_warn_calls(Info0),
	  goal_info_is_pure(GoalInfo0)
	->	
		% we need to do the pass, for the warnings, but we ignore
		% the optimized goal and instead use the original one
		common__optimise_call(PredId, ProcId, Args, Goal0, GoalInfo0,
			_Goal1, Info2, Info3),
		Goal1 = Goal0
	;
		Goal1 = Goal0,
		Info3 = Info2
	),

	%
	% Try to evaluate the call at compile-time.
	%

	( simplify_do_const_prop(Info3) ->
		simplify_info_get_instmap(Info3, Instmap0),
		simplify_info_get_module_info(Info3, ModuleInfo2),
		(
			Goal1 = call(_, _, _, _, _, _),
			evaluate_builtin(PredId, ProcId, Args, GoalInfo0, 
				Goal2, GoalInfo2, Instmap0,
				ModuleInfo2, ModuleInfo3)
		->
			Goal = Goal2,
			GoalInfo = GoalInfo2,
			simplify_info_set_module_info(Info3, ModuleInfo3, Info4),
			simplify_info_set_requantify(Info4, Info)
		;
			Goal = Goal1,
			GoalInfo = GoalInfo0,
			Info = Info3
		)
	;
		Goal = Goal1,
		GoalInfo = GoalInfo0,
		Info = Info3
	).

simplify__goal_2(Goal0, GoalInfo0, Goal, GoalInfo, Info0, Info) :-
	Goal0 = unify(LT0, RT0, M, U0, C),
	(
		% A unification of the form X = X can safely be
		% optimised away.

		RT0 = var(LT0)
	->
		goal_info_get_context(GoalInfo0, Context),
		true_goal(Context, Goal - GoalInfo),
		Info = Info0
	;
		RT0 = lambda_goal(PredOrFunc, NonLocals, Vars, 
			Modes, LambdaDeclaredDet, LambdaGoal0)
	->
		simplify_info_enter_lambda(Info0, Info1),
		simplify_info_get_common_info(Info1, Common1),
		simplify_info_get_module_info(Info1, ModuleInfo),
		simplify_info_get_instmap(Info1, InstMap1),
		instmap__pre_lambda_update(ModuleInfo, Vars, Modes,
			InstMap1, InstMap2),
		simplify_info_set_instmap(Info1, InstMap2, Info2),

		% Don't attempt to pass structs into lambda_goals,
		% since that could change the curried non-locals of the 
		% lambda_goal, and that would be difficult to fix up.
		common_info_init(Common2),
		simplify_info_set_common_info(Info2, Common2, Info3),

		% Don't attempt to pass structs out of lambda_goals.
		simplify__goal(LambdaGoal0, LambdaGoal, Info3, Info4),
		simplify_info_set_common_info(Info4, Common1, Info5),
		simplify_info_set_instmap(Info5, InstMap1, Info6),
		RT = lambda_goal(PredOrFunc, NonLocals, Vars, Modes, 
			LambdaDeclaredDet, LambdaGoal),
		simplify_info_leave_lambda(Info6, Info),
		Goal = unify(LT0, RT, M, U0, C),
		GoalInfo = GoalInfo0
	;
		simplify_do_common(Info0)
	->
		common__optimise_unification(U0, LT0, RT0, M, C,
			Goal0, GoalInfo0, Goal, GoalInfo, Info0, Info)
	;
		( simplify_do_calls(Info0)
		; simplify_do_warn_calls(Info0)
		)
	->
		% We need to do the pass, to record the variable
		% equivalences used for optimizing or warning about
		% duplicate calls.  But we don't want to perform
		% the optimization, so we disregard the optimized goal
		% and instead use the original one.
		common__optimise_unification(U0, LT0, RT0, M, C,
			Goal0, GoalInfo0, _Goal1, _GoalInfo1, Info0, Info),
		Goal = Goal0,
		GoalInfo = GoalInfo0
	;
		Goal = Goal0,
		GoalInfo = GoalInfo0,
		Info = Info0
	).

	% (A -> B ; C) is logically equivalent to (A, B ; ~A, C).
	% If the determinism of A means that one of these disjuncts
	% cannot succeed, then we replace the if-then-else with the
	% other disjunct. (We could also eliminate A, but we leave
	% that to the recursive invocations.)
	%
	% The conjunction operator in the remaining disjunct ought to be
	% a sequential conjunction, because Mercury's if-then-else always
	% guarantees sequentiality, whereas conjunction only guarantees
	% sequentiality if the --no-reorder-conj option is enabled.
	%
	% However, currently reordering is only done in mode analysis,
	% not in the code generator, so we don't yet need a sequential
	% conjunction construct. This will change when constraint pushing
	% is finished, or when we start doing coroutining.

simplify__goal_2(if_then_else(Vars, Cond0, Then0, Else0, SM),
		GoalInfo0, Goal, GoalInfo, Info0, Info) :-
	Cond0 = _ - CondInfo0,

	goal_info_get_determinism(CondInfo0, CondDetism0),
	determinism_components(CondDetism0, CondCanFail0, CondSolns0),
	( CondCanFail0 = cannot_fail ->
		goal_to_conj_list(Cond0, CondList),
		goal_to_conj_list(Then0, ThenList),
		list__append(CondList, ThenList, List),
		simplify__goal(conj(List) - GoalInfo0, Goal - GoalInfo,
			Info0, Info1),
		goal_info_get_context(GoalInfo0, Context),
		simplify_info_add_msg(Info1, ite_cond_cannot_fail(Context),
			Info)
	; CondSolns0 = at_most_zero ->
		% Optimize away the condition and the `then' part.
		det_negation_det(CondDetism0, MaybeNegDetism),
		( Cond0 = not(NegCond) - _ ->
			Cond = NegCond
		;
			(
				MaybeNegDetism = yes(NegDetism1),
				(
					NegDetism1 = erroneous,
					instmap_delta_init_unreachable(
						NegInstMapDelta1)
				;
					NegDetism1 = det,
					instmap_delta_init_reachable(
						NegInstMapDelta1)
				)
			->
				NegDetism = NegDetism1,
				NegInstMapDelta = NegInstMapDelta1
			;
				error("simplify__goal_2: cannot get negated determinism")
			),
			goal_info_set_determinism(CondInfo0,
				NegDetism, NegCondInfo0),
			goal_info_set_instmap_delta(NegCondInfo0, 
				NegInstMapDelta, NegCondInfo),
			Cond = not(Cond0) - NegCondInfo
		),
		goal_to_conj_list(Else0, ElseList),
		List = [Cond | ElseList],
		simplify__goal(conj(List) - GoalInfo0, Goal - GoalInfo,
			Info0, Info1),
		goal_info_get_context(GoalInfo0, Context),
		simplify_info_add_msg(Info1, ite_cond_cannot_succeed(Context),
			Info)
	; Else0 = disj([], _) - _ ->
		% (A -> C ; fail) is equivalent to (A, C)
		goal_to_conj_list(Cond0, CondList),
		goal_to_conj_list(Then0, ThenList),
		list__append(CondList, ThenList, List),
		simplify__goal(conj(List) - GoalInfo0, Goal - GoalInfo,
			Info0, Info)
	;
		simplify_info_get_instmap(Info0, InstMap0),
		simplify__goal(Cond0, Cond, Info0, Info1),
		simplify_info_update_instmap(Info1, Cond, Info2),
		simplify__goal(Then0, Then, Info2, Info3),
		simplify_info_post_branch_update(Info0, Info3, Info4),
		simplify__goal(Else0, Else, Info4, Info5),
		simplify_info_post_branch_update(Info0, Info5, Info6),
		Cond = _ - CondInfo,
		goal_info_get_instmap_delta(CondInfo, CondDelta),
		Then = _ - ThenInfo,
		goal_info_get_instmap_delta(ThenInfo, ThenDelta),
		instmap_delta_apply_instmap_delta(CondDelta, ThenDelta,
			CondThenDelta),
		Else = _ - ElseInfo,
		goal_info_get_instmap_delta(ElseInfo, ElseDelta),
                goal_info_get_nonlocals(GoalInfo0, NonLocals),
		simplify_info_get_module_info(Info6, ModuleInfo0),
		merge_instmap_deltas(InstMap0, NonLocals,
			[CondThenDelta, ElseDelta], NewDelta,
			ModuleInfo0, ModuleInfo1),
		simplify_info_set_module_info(Info6, ModuleInfo1, Info),
		goal_info_set_instmap_delta(GoalInfo0, NewDelta, GoalInfo1),
		IfThenElse = if_then_else(Vars, Cond, Then, Else, SM),
		%
		% If-then-elses that are det or semidet may nevertheless
		% contain nondet or multidet conditions. If this happens, the
		% if-then-else must be put inside a `some' to appease the code
		% generator.
		%
		goal_info_get_determinism(GoalInfo0, IfThenElseDetism0),
		determinism_components(IfThenElseDetism0, IfThenElseCanFail,
			IfThenElseNumSolns),
		(
			simplify_do_once(Info),
			goal_info_get_determinism(CondInfo, CondDetism),
			determinism_components(CondDetism, _, at_most_many),
			IfThenElseNumSolns \= at_most_many
		->
			determinism_components(InnerDetism, IfThenElseCanFail,
				at_most_many),
			goal_info_set_determinism(GoalInfo1, InnerDetism,
				InnerInfo),
			Goal = some([], IfThenElse - InnerInfo)
		;
			Goal = IfThenElse
		),
		GoalInfo = GoalInfo1
	).

simplify__goal_2(not(Goal0), GoalInfo0, Goal, GoalInfo, Info0, Info) :-
	% Can't use calls or unifications seen within a negation,
	% since non-local variables may not be bound within the negation.
	simplify_info_get_common_info(Info0, Common),
	simplify__goal(Goal0, Goal1, Info0, Info1),
	simplify_info_set_common_info(Info1, Common, Info2),
	Goal1 = _ - GoalInfo1,
	goal_info_get_determinism(GoalInfo1, Detism),
	determinism_components(Detism, CanFail, MaxSoln),
	goal_info_get_context(GoalInfo0, Context),
	( CanFail = cannot_fail ->
		simplify_info_add_msg(Info2,
			negated_goal_cannot_fail(Context), Info3)
	; MaxSoln = at_most_zero ->
		simplify_info_add_msg(Info2,
			negated_goal_cannot_succeed(Context), Info3)
	;
		Info3 = Info2
	),
	(
		% replace `not true' with `fail'
		Goal1 = conj([]) - _GoalInfo
	->
		fail_goal(Context, Goal - GoalInfo),
		Info = Info3
	;
		% replace `not fail' with `true'
		Goal1 = disj([], _) - _GoalInfo2
	->
		true_goal(Context, Goal - GoalInfo),
		Info = Info3
	;
		% remove double negation
		Goal1 = not(SubGoal - SubGoalInfo) - _
	->
		simplify__maybe_wrap_goal(GoalInfo0, SubGoalInfo, SubGoal,
			Goal, GoalInfo, Info3, Info)
	;
		Goal = not(Goal1),
		GoalInfo = GoalInfo0,
		Info = Info3
	).

simplify__goal_2(some(Vars1, Goal1), SomeInfo, Goal, GoalInfo, Info0, Info) :-
	simplify__goal(Goal1, Goal2, Info0, Info),
	simplify__nested_somes(Vars1, Goal2, Vars, Goal3),
	Goal3 = GoalExpr3 - GoalInfo3,
	(
		goal_info_get_determinism(GoalInfo3, Detism),
		goal_info_get_determinism(SomeInfo, Detism)
	->
		% If the inner and outer detisms match the `some'
		% is unnecessary.
		Goal = GoalExpr3,
		GoalInfo = GoalInfo3
	;
		Goal = some(Vars, Goal3),
		GoalInfo = SomeInfo
	).

simplify__goal_2(Goal0, GoalInfo, Goal, GoalInfo, Info0, Info) :-
	Goal0 = pragma_c_code(_, PredId, ProcId, Args, _, _, _),
	(
		simplify_do_calls(Info0),
		goal_info_is_pure(GoalInfo)
	->	
		common__optimise_call(PredId, ProcId, Args, Goal0,
			GoalInfo, Goal, Info0, Info)
	;
		Info = Info0,
		Goal = Goal0
	).

%-----------------------------------------------------------------------------%

	% simplify__input_args_are_equiv(Args, HeadVars, Modes,
	% 		CommonInfo, ModuleInfo1):
	% Succeeds if all the input arguments (determined by looking at
	% `Modes') in `Args' are equivalent (according to the equivalence
	% class specified by `CommonInfo') to the corresponding variables
	% in HeadVars.  HeadVars, Modes, and Args should all be lists of
	% the same length.

:- pred simplify__input_args_are_equiv(list(var), list(var), list(mode),
	common_info, module_info).
:- mode simplify__input_args_are_equiv(in, in, in, in, in) is semidet.

simplify__input_args_are_equiv([], [], _, _, _).
simplify__input_args_are_equiv([Arg|Args], [HeadVar|HeadVars], [Mode|Modes],
		CommonInfo, ModuleInfo1) :-
	( mode_is_input(ModuleInfo1, Mode) ->
		common__vars_are_equivalent(Arg, HeadVar, CommonInfo)
	;
		true
	),
	simplify__input_args_are_equiv(Args, HeadVars, Modes,
			CommonInfo, ModuleInfo1).

%-----------------------------------------------------------------------------%

	% replace nested `some's with a single `some',
:- pred simplify__nested_somes(list(var)::in, hlds_goal::in,
		list(var)::out, hlds_goal::out) is det.

simplify__nested_somes(Vars0, Goal0, Vars, Goal) :-
	( Goal0 = some(Vars1, Goal1) - _ ->
		list__append(Vars0, Vars1, Vars2),
		simplify__nested_somes(Vars2, Goal1, Vars, Goal)
	;
		Vars = Vars0,
		Goal = Goal0
	).

%-----------------------------------------------------------------------------%

	% When removing a level of wrapping around a goal,
	% if the determinisms are not the same, we really
	% need to rerun determinism analysis on the
	% procedure. I think this is a similar situation
	% to inlining of erroneous goals. The safe thing
	% to do is to wrap a `some' around the inner goal if
	% the inner and outer determinisms are not the same.
	% It probably won't happen that often.
:- pred simplify__maybe_wrap_goal(hlds_goal_info::in, hlds_goal_info::in,
	hlds_goal_expr::in, hlds_goal_expr::out, hlds_goal_info::out,
	simplify_info::in, simplify_info::out)  is det.

simplify__maybe_wrap_goal(OuterGoalInfo, InnerGoalInfo,
		Goal1, Goal, GoalInfo, Info0, Info) :-
	(
		goal_info_get_determinism(InnerGoalInfo, Det),
		goal_info_get_determinism(OuterGoalInfo, Det)
	->
		Goal = Goal1,
		GoalInfo = InnerGoalInfo,
		Info = Info0
	;
		Goal = some([], Goal1 - InnerGoalInfo),
		GoalInfo = OuterGoalInfo,
		simplify_info_set_rerun_det(Info0, Info)
	).

%-----------------------------------------------------------------------------%

:- pred simplify__conj(list(hlds_goal), list(hlds_goal),
		list(hlds_goal), hlds_goal_info,
		simplify_info, simplify_info).
:- mode simplify__conj(in, in, out, in, in, out) is det.

simplify__conj([], RevGoals, Goals, _, Info, Info) :-
	list__reverse(RevGoals, Goals).
simplify__conj([Goal0 | Goals0], RevGoals0, Goals, ConjInfo, Info0, Info) :-
	% Flatten conjunctions.
	( Goal0 = conj(SubGoals) - _ ->
	    list__append(SubGoals, Goals0, Goals1),
	    simplify__conj(Goals1, RevGoals0, Goals, ConjInfo, Info0, Info)
	;
	    simplify__goal(Goal0, Goal1, Info0, Info1),
	    (
		% Flatten conjunctions.
		Goal1 = conj(SubGoals1) - _
	    ->
		simplify_info_undo_goal_updates(Info0, Info1, Info2),
		list__append(SubGoals1, Goals0, Goals1),
		simplify__conj(Goals1, RevGoals0, Goals, ConjInfo, Info2, Info)
	    ;
		% Delete unreachable goals.
		(
		    simplify_info_get_instmap(Info1, InstMap1),
		    instmap__is_unreachable(InstMap1)
		;
		    Goal1 = _ - GoalInfo1,
		    goal_info_get_determinism(GoalInfo1, Detism1),
		    determinism_components(Detism1, _, at_most_zero)
		)
	    ->
		Info = Info1,
		simplify__conjoin_goal_and_rev_goal_list(Goal1,
			RevGoals0, RevGoals1),

		( (Goal1 = disj([], _) - _ ; Goals0 = []) ->
			RevGoals = RevGoals1
		;
			% We insert an explicit failure at the end
			% of the non-succeeding conjunction. This
			% is necessary, since the unreachability of
			% the instmap could have been derived using
			% inferred determinism information. Without the
			% explicit fail goal, mode errors could result if mode
			% analysis is rerun, since according to the language
			% specification, mode analysis does not use inferred
			% determinism information when deciding what can
			% never succeed.
			Goal0 = _ - GoalInfo0,
			goal_info_get_context(GoalInfo0, Context),
			fail_goal(Context, Fail),
			simplify__conjoin_goal_and_rev_goal_list(Fail,
				RevGoals1, RevGoals)	
		),
		list__reverse(RevGoals, Goals)
	    ;
		simplify__excess_assigns(Goal1, ConjInfo,
			Goals0, Goals1, RevGoals0, RevGoals1,
			GoalNeeded, Info1, Info2),
		( GoalNeeded = yes ->
			simplify__conjoin_goal_and_rev_goal_list(Goal1,
				RevGoals1, RevGoals2)
		;
			RevGoals2 = RevGoals1
		),
		simplify_info_update_instmap(Info2, Goal1, Info3),
		simplify__conj(Goals1, RevGoals2, Goals,
			ConjInfo, Info3, Info)
	    )
	).

:- pred simplify__conjoin_goal_and_rev_goal_list(hlds_goal::in,
		hlds_goals::in, hlds_goals::out) is det.

simplify__conjoin_goal_and_rev_goal_list(Goal, RevGoals0, RevGoals) :-
	( Goal = conj(Goals) - _ ->
		list__reverse(Goals, Goals1),
		list__append(Goals1, RevGoals0, RevGoals)
	;
		RevGoals = [Goal | RevGoals0]
	).

%-----------------------------------------------------------------------------%

:- pred simplify__par_conj(list(hlds_goal), list(hlds_goal),
		simplify_info, simplify_info, simplify_info).
:- mode simplify__par_conj(in, out, in, in, out) is det.

simplify__par_conj([], [], _, Info, Info).
simplify__par_conj([Goal0 |Goals0], [Goal | Goals], Info0, Info1, Info) :-
	simplify__goal(Goal0, Goal, Info1, Info2),
	simplify_info_post_branch_update(Info0, Info2, Info3),
	simplify__par_conj(Goals0, Goals, Info0, Info3, Info).

%-----------------------------------------------------------------------------%

:- pred simplify__excess_assigns(hlds_goal::in, hlds_goal_info::in,
		hlds_goals::in, hlds_goals::out,
		hlds_goals::in, hlds_goals::out, bool::out,
		simplify_info::in, simplify_info::out) is det.

simplify__excess_assigns(Goal0, ConjInfo, Goals0, Goals,
		RevGoals0, RevGoals, GoalNeeded, Info0, Info) :-
	(
		simplify_do_excess_assigns(Info0),
		Goal0 = unify(_, _, _, Unif, _) - _,
		goal_info_get_nonlocals(ConjInfo, NonLocals),
		Unif = assign(LeftVar, RightVar),
		( \+ set__member(LeftVar, NonLocals) ->
			LocalVar = LeftVar, ReplacementVar = RightVar
		; \+ set__member(RightVar, NonLocals) ->
			LocalVar = RightVar, ReplacementVar = LeftVar
		;
			fail
		)
	->
		GoalNeeded = no,
		map__init(Subn0),
		map__det_insert(Subn0, LocalVar, ReplacementVar, Subn),
		goal_util__rename_vars_in_goals(Goals0, no,
			Subn, Goals),
		goal_util__rename_vars_in_goals(RevGoals0, no,
			Subn, RevGoals),
		simplify_info_get_varset(Info0, VarSet0),
		varset__delete_var(VarSet0, LocalVar, VarSet),
		simplify_info_set_varset(Info0, VarSet, Info)
	;
		GoalNeeded = yes,
		Goals = Goals0,
		RevGoals = RevGoals0,
		Info = Info0
	).

%-----------------------------------------------------------------------------%

:- pred simplify__switch(var, list(case), list(case), list(case), 
		list(instmap_delta), list(instmap_delta), can_fail, can_fail,
		simplify_info, simplify_info, simplify_info).
:- mode simplify__switch(in, in, in, out, in, out, in, out,
		in, in, out) is det.

simplify__switch(_, [], RevCases, Cases, InstMaps, InstMaps, 
		CanFail, CanFail, _, Info, Info) :-
	list__reverse(RevCases, Cases). 
simplify__switch(Var, [Case0 | Cases0], RevCases0, Cases, InstMaps0, InstMaps, 
		CanFail0, CanFail, Info0, Info1, Info) :-
	simplify_info_get_instmap(Info0, InstMap0),
	Case0 = case(ConsId, Goal0),
	simplify_info_get_module_info(Info1, ModuleInfo0),
	instmap__bind_var_to_functor(Var, ConsId,
		InstMap0, InstMap1, ModuleInfo0, ModuleInfo1),
	simplify_info_set_module_info(Info1, ModuleInfo1, Info2),
	simplify_info_set_instmap(Info2, InstMap1, Info3),
	simplify__goal(Goal0, Goal, Info3, Info4),

		% Remove failing branches. 
	( Goal = disj([], _) - _ ->
		RevCases = RevCases0,
		InstMaps1 = InstMaps0,
		CanFail1 = can_fail,
		Info5 = Info4
	;
		Case = case(ConsId, Goal),
		Goal = _ - GoalInfo,

		%
		% Make sure the switched on variable appears in the
		% instmap delta. This avoids an abort in merge_instmap_delta
		% if another branch further instantiates the switched-on 
		% variable. If the switched on variable does not appear in
		% this branch's instmap_delta, the inst before the goal
		% would be used, resulting in a mode error.
		%
		goal_info_get_instmap_delta(GoalInfo, InstMapDelta0),
		simplify_info_get_module_info(Info4, ModuleInfo5),
		instmap_delta_bind_var_to_functor(Var, ConsId,
			InstMap0, InstMapDelta0, InstMapDelta, 
			ModuleInfo5, ModuleInfo),
		simplify_info_set_module_info(Info4, ModuleInfo, Info5),

		InstMaps1 = [InstMapDelta | InstMaps0],
		RevCases = [Case | RevCases0],
		CanFail1 = CanFail0
	),

	simplify_info_post_branch_update(Info0, Info5, Info6),
	simplify__switch(Var, Cases0, RevCases, Cases, InstMaps1, InstMaps,
		CanFail1, CanFail, Info0, Info6, Info).

	% Create a semidet unification at the start of a singleton case
	% in a can_fail switch.
:- pred simplify__create_test_unification(var::in, cons_id::in, int::in,
		hlds_goal::out, simplify_info::in, simplify_info::out) is det.

simplify__create_test_unification(Var, ConsId, ConsArity,
		ExtraGoal - ExtraGoalInfo, Info0, Info) :-
	simplify_info_get_varset(Info0, VarSet0),
	simplify_info_get_var_types(Info0, VarTypes0),
	varset__new_vars(VarSet0, ConsArity, ArgVars, VarSet),
	map__lookup(VarTypes0, Var, VarType),
	simplify_info_get_module_info(Info0, ModuleInfo),
	type_util__get_cons_id_arg_types(ModuleInfo,
		VarType, ConsId, ArgTypes),
	map__det_insert_from_corresponding_lists(VarTypes0, ArgVars,
		ArgTypes, VarTypes),
	simplify_info_set_varset(Info0, VarSet, Info1),
	simplify_info_set_var_types(Info1, VarTypes, Info),
	simplify_info_get_instmap(Info, InstMap),
	instmap__lookup_var(InstMap, Var, Inst0),
	(
		inst_expand(ModuleInfo, Inst0, Inst1),
		get_arg_insts(Inst1, ConsId, ConsArity, ArgInsts1)
	->
		ArgInsts = ArgInsts1
	;
		error("simplify__goal_2 - get_arg_insts failed")
	),
	InstToUniMode =
		lambda([ArgInst::in, ArgUniMode::out] is det, (
			ArgUniMode = ((ArgInst - free) -> (ArgInst - ArgInst))
		)),
	list__map(InstToUniMode, ArgInsts, UniModes),
	UniMode = (Inst0 -> Inst0) - (Inst0 -> Inst0),
	UnifyContext = unify_context(explicit, []),
	Unification = deconstruct(Var, ConsId,
		ArgVars, UniModes, can_fail),
	ExtraGoal = unify(Var, functor(ConsId, ArgVars),
		UniMode, Unification, UnifyContext),
	set__singleton_set(NonLocals, Var),

		% The test can't bind any variables, so the
		% InstMapDelta should be empty.
	instmap_delta_init_reachable(InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, semidet, ExtraGoalInfo).

%-----------------------------------------------------------------------------%

:- pred simplify__disj(list(hlds_goal), list(hlds_goal), list(hlds_goal), 
	list(instmap_delta), list(instmap_delta), 
	simplify_info, simplify_info, simplify_info).
:- mode simplify__disj(in, in, out, in, out, in, in, out) is det.

simplify__disj([], RevGoals, Goals, InstMaps, InstMaps, _, Info, Info) :-
	list__reverse(RevGoals, Goals).
simplify__disj([Goal0 | Goals0], RevGoals0, Goals,  PostBranchInstMaps0,
		PostBranchInstMaps, Info0, Info1, Info) :-
	simplify__goal(Goal0, Goal, Info1, Info2),
	Goal = _ - GoalInfo,

	(
		% Don't prune or warn about impure disjuncts 
		% that can't succeed.
		\+ goal_info_is_impure(GoalInfo),
		goal_info_get_determinism(GoalInfo, Detism),
		determinism_components(Detism, _CanFail, MaxSolns),
		MaxSolns = at_most_zero
	->
		( 
			simplify_do_warn(Info2),
			% Don't warn where the initial goal was fail,
			% since that can result from mode analysis
			% pruning away cases in a switch which cannot
			% succeed due to sub-typing in the modes.
			Goal0 \= disj([], _) - _
		->
			goal_info_get_context(GoalInfo, Context),
			simplify_info_add_msg(Info2, 
				zero_soln_disjunct(Context), Info3)
		;
			Info3 = Info2
		),

		%
		% Prune away non-succeeding disjuncts where possible.
		%

		( 
			(
				Goal0 = disj([], _) - _
			;
				% Only remove disjuncts that might loop
				% or call error/1 if --no-fully-strict.
				simplify_info_get_det_info(Info3, DetInfo),
				det_info_get_fully_strict(DetInfo, no)
			)
		->
			RevGoals1 = RevGoals0,
			PostBranchInstMaps1 = PostBranchInstMaps0
		;			
			RevGoals1 = [Goal | RevGoals0],
			goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
			PostBranchInstMaps1 = 
				[InstMapDelta | PostBranchInstMaps0]
		)
	;
		Info3 = Info2,
		RevGoals1 = [Goal | RevGoals0],
		goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
		PostBranchInstMaps1 = [InstMapDelta | PostBranchInstMaps0]
	),

	simplify_info_post_branch_update(Info0, Info3, Info4),
	simplify__disj(Goals0, RevGoals1, Goals, PostBranchInstMaps1,
			PostBranchInstMaps, Info0, Info4, Info).

	% Disjunctions that cannot succeed more than once when viewed from the
	% outside generally need some fixing up, and/or some warnings to be
	% issued.

	% We previously converted them all to if-then-elses using the code
	% below, however converting disjs that have output variables but
	% that nevertheless cannot succeed more than one
	% (e.g. cc_nondet or cc_multi disjs) into if-then-elses
	% may cause problems with other parts of the compiler that
	% assume that an if-then-else is mode-correct, i.e. that
	% the condition doesn't bind variables.

	/****
			goal_info_get_determinism(GoalInfo, Detism),
			determinism_components(Detism, _CanFail, MaxSoln),
			MaxSoln \= at_most_many
		->
			goal_info_get_instmap_delta(GoalInfo, DeltaInstMap),
			goal_info_get_nonlocals(GoalInfo, NonLocalVars),
			(
				det_no_output_vars(NonLocalVars, InstMap0,
					DeltaInstMap, DetInfo)
			->
				OutputVars = no
			;
				OutputVars = yes
			),
			simplify__fixup_disj(Disjuncts, Detism, OutputVars,
				GoalInfo, SM, InstMap0, DetInfo, Goal,
				MsgsA, Msgs)
		;
	****/

:- pred simplify__fixup_disj(list(hlds_goal), determinism, bool,
	hlds_goal_info, follow_vars, hlds_goal_expr,
	simplify_info, simplify_info).
:- mode simplify__fixup_disj(in, in, in, in, in, out, in, out) is det.

simplify__fixup_disj(Disjuncts, _, _OutputVars, GoalInfo, SM,
		Goal, Info0, Info) :-
	det_disj_to_ite(Disjuncts, GoalInfo, SM, IfThenElse),
	simplify__goal(IfThenElse, Simplified, Info0, Info),
	Simplified = Goal - _.

	% det_disj_to_ite is used to transform disjunctions that occur
	% in prunable contexts into if-then-elses.
	% For example, it would transform
	%
	%	( Disjunct1
	%	; Disjunct2
	%	; Disjunct3
	%	)
	% into
	%	( Disjunct1 ->
	%		true
	%	; Disjunct2 ->
	%		true
	%	;
	%		Disjunct3
	%	).

:- pred det_disj_to_ite(list(hlds_goal), hlds_goal_info, follow_vars,
	hlds_goal).
:- mode det_disj_to_ite(in, in, in, out) is det.

det_disj_to_ite([], _GoalInfo, _SM, _) :-
	error("reached base case of det_disj_to_ite").
det_disj_to_ite([Disjunct | Disjuncts], GoalInfo, SM, Goal) :-
	( Disjuncts = [] ->
		Goal = Disjunct
	;
		Cond = Disjunct,
		Cond = _CondGoal - CondGoalInfo,

		true_goal(Then),

		det_disj_to_ite(Disjuncts, GoalInfo, SM, Rest),
		Rest = _RestGoal - RestGoalInfo,

		goal_info_get_nonlocals(CondGoalInfo, CondNonLocals),
		goal_info_get_nonlocals(RestGoalInfo, RestNonLocals),
		set__union(CondNonLocals, RestNonLocals, NonLocals),
		goal_info_set_nonlocals(GoalInfo, NonLocals, NewGoalInfo0),

		goal_info_get_instmap_delta(GoalInfo, InstMapDelta0),
		instmap_delta_restrict(InstMapDelta0, NonLocals, InstMapDelta),
		goal_info_set_instmap_delta(NewGoalInfo0, InstMapDelta,
			NewGoalInfo1),

		goal_info_get_determinism(CondGoalInfo, CondDetism),
		goal_info_get_determinism(RestGoalInfo, RestDetism),
		determinism_components(CondDetism, CondCanFail, CondMaxSoln),
		determinism_components(RestDetism, RestCanFail, RestMaxSoln),
		det_disjunction_canfail(CondCanFail, RestCanFail, CanFail),
		det_disjunction_maxsoln(CondMaxSoln, RestMaxSoln, MaxSoln0),
		( MaxSoln0 = at_most_many ->
			MaxSoln = at_most_one
		;
			MaxSoln = MaxSoln0
		),
		determinism_components(Detism, CanFail, MaxSoln),
		goal_info_set_determinism(NewGoalInfo1, Detism, NewGoalInfo),

		Goal = if_then_else([], Cond, Then, Rest, SM) - NewGoalInfo
	).

%-----------------------------------------------------------------------------%

:- pred simplify__contains_multisoln_goal(list(hlds_goal)::in) is semidet.

simplify__contains_multisoln_goal(Goals) :-
	list__member(_Goal - GoalInfo, Goals),
	goal_info_get_determinism(GoalInfo, Detism),
	determinism_components(Detism, _, at_most_many).

%-----------------------------------------------------------------------------%

:- type simplify_info
	--->	simplify_info(
			det_info,
			set(det_msg),
			set(simplification),
			common_info,	% Info about common subexpressions.
			instmap,
			varset,
			map(var, type),
			bool,		% Does the goal need requantification.
			bool,		% Do we need to recompute
					% instmap_deltas for atomic goals
			bool,		% Does determinism analysis need to
					% be rerun.
			int,		% Measure of the improvement in
					% the goal from simplification.
			int		% Count of the number of lambdas
					% which enclose the current goal.
		).

simplify_info_init(DetInfo, Simplifications0, InstMap,
		VarSet, VarTypes, Info) :-
	common_info_init(CommonInfo),
	set__init(Msgs),
	set__list_to_set(Simplifications0, Simplifications),
	Info = simplify_info(DetInfo, Msgs, Simplifications, CommonInfo,
			InstMap, VarSet, VarTypes, no, no, no, 0, 0). 

	% Reinitialise the simplify_info before reprocessing a goal.
:- pred simplify_info_reinit(set(simplification)::in, instmap::in,
		simplify_info::in, simplify_info::out) is det.

simplify_info_reinit(Simplifications, InstMap0, Info0, Info) :-
	Info0 = simplify_info(DetInfo, Msgs, _, _, _,
		VarSet, VarTypes, _, _, _, CostDelta, _),
	common_info_init(Common),
	Info = simplify_info(DetInfo, Msgs, Simplifications, Common, InstMap0,
		VarSet, VarTypes, no, no, no, CostDelta, 0).

	% exported for common.m
:- interface.

:- import_module prog_data.
:- import_module set.

:- pred simplify_info_init(det_info, list(simplification), instmap,
		varset, map(var, type), simplify_info).
:- mode simplify_info_init(in, in, in, in, in, out) is det.

:- pred simplify_info_get_det_info(simplify_info::in, det_info::out) is det.
:- pred simplify_info_get_msgs(simplify_info::in, set(det_msg)::out) is det.
:- pred simplify_info_get_instmap(simplify_info::in, instmap::out) is det.
:- pred simplify_info_get_simplifications(simplify_info::in,
		set(simplification)::out) is det.
:- pred simplify_info_get_common_info(simplify_info::in,
		common_info::out) is det.
:- pred simplify_info_get_varset(simplify_info::in, varset::out) is det.
:- pred simplify_info_get_var_types(simplify_info::in,
		map(var, type)::out) is det.
:- pred simplify_info_requantify(simplify_info::in) is semidet.
:- pred simplify_info_recompute_atomic(simplify_info::in) is semidet.
:- pred simplify_info_rerun_det(simplify_info::in) is semidet.
:- pred simplify_info_get_cost_delta(simplify_info::in, int::out) is det.

:- pred simplify_info_get_module_info(simplify_info::in,
		module_info::out) is det.

:- implementation.

simplify_info_get_det_info(simplify_info(Det, _,_,_,_,_,_,_,_,_,_,_), Det). 
simplify_info_get_msgs(simplify_info(_, Msgs, _,_,_,_,_,_,_,_,_,_), Msgs).
simplify_info_get_simplifications(simplify_info(_,_,Simplify,_,_,_,_,_,_,_,_,_),
	Simplify). 
simplify_info_get_common_info(simplify_info(_,_,_,Common, _,_,_,_,_,_,_,_),
	Common).
simplify_info_get_instmap(simplify_info(_,_,_,_, InstMap,_,_,_,_,_,_,_),
	InstMap). 
simplify_info_get_varset(simplify_info(_,_,_,_,_, VarSet, _,_,_,_,_,_), VarSet).
simplify_info_get_var_types(simplify_info(_,_,_,_,_,_, VarTypes, _,_,_,_,_),
	VarTypes). 
simplify_info_requantify(simplify_info(_,_,_,_,_,_,_, yes, _,_,_,_)).
simplify_info_recompute_atomic(simplify_info(_,_,_,_,_,_,_,_, yes,_,_,_)).
simplify_info_rerun_det(simplify_info(_,_,_,_,_,_,_,_,_, yes,_,_)).
simplify_info_get_cost_delta(simplify_info(_,_,_,_,_,_,_,_,_,_,CostDelta, _),
	CostDelta).

simplify_info_get_module_info(Info, ModuleInfo) :-
	simplify_info_get_det_info(Info, DetInfo),
	det_info_get_module_info(DetInfo, ModuleInfo).

:- interface.

:- pred simplify_info_set_det_info(simplify_info::in,
		det_info::in, simplify_info::out) is det.
:- pred simplify_info_set_msgs(simplify_info::in,
		set(det_msg)::in, simplify_info::out) is det.
:- pred simplify_info_set_simplifications(simplify_info::in,
		set(simplification)::in, simplify_info::out) is det.
:- pred simplify_info_set_instmap(simplify_info::in,
		instmap::in, simplify_info::out) is det.
:- pred simplify_info_set_common_info(simplify_info::in, common_info::in,
		simplify_info::out) is det.
:- pred simplify_info_set_varset(simplify_info::in, varset::in,
		simplify_info::out) is det.
:- pred simplify_info_set_var_types(simplify_info::in, map(var, type)::in,
		simplify_info::out) is det.
:- pred simplify_info_set_requantify(simplify_info::in,
		simplify_info::out) is det.
:- pred simplify_info_set_recompute_atomic(simplify_info::in,
		simplify_info::out) is det.
:- pred simplify_info_set_rerun_det(simplify_info::in,
		simplify_info::out) is det.
:- pred simplify_info_add_msg(simplify_info::in, det_msg::in,
		simplify_info::out) is det.
:- pred simplify_info_do_add_msg(simplify_info::in, det_msg::in, 
		simplify_info::out) is det.
:- pred simplify_info_set_cost_delta(simplify_info::in, int::in,
		simplify_info::out) is det.
:- pred simplify_info_incr_cost_delta(simplify_info::in,
		int::in, simplify_info::out) is det.

:- pred simplify_info_enter_lambda(simplify_info::in, simplify_info::out)
		is det.
:- pred simplify_info_leave_lambda(simplify_info::in, simplify_info::out)
		is det.
:- pred simplify_info_inside_lambda(simplify_info::in) is semidet.

:- pred simplify_info_set_module_info(simplify_info::in,
		module_info::in, simplify_info::out) is det.

:- implementation.

simplify_info_set_det_info(simplify_info(_, B, C, D, E, F, G, H, I, J, K, L),
		Det, simplify_info(Det, B, C, D, E, F, G, H, I, J, K, L)).
simplify_info_set_msgs(simplify_info(A, _, C, D, E, F, G, H, I, J, K, L), Msgs,
		simplify_info(A, Msgs, C, D, E, F, G, H, I, J, K, L)). 
simplify_info_set_simplifications(
		simplify_info(A, B, _, D, E, F, G, H, I, J, K, L),
		Simp, simplify_info(A, B, Simp, D, E, F, G, H, I, J, K, L)).
simplify_info_set_instmap(simplify_info(A, B, C, D, _, F, G, H, I, J, K, L), 
		InstMap, 
		simplify_info(A, B, C, D, InstMap, F, G, H, I, J, K, L)). 
simplify_info_set_common_info(simplify_info(A, B, C, _, E, F, G, H, I, J, K, L),
		Common, 
		simplify_info(A, B, C, Common, E, F, G, H, I, J, K, L)). 
simplify_info_set_varset(simplify_info(A, B, C, D, E, _, G, H, I, J, K, L), 
		VarSet, 
		simplify_info(A, B, C, D, E, VarSet, G, H, I, J, K, L)). 
simplify_info_set_var_types(simplify_info(A, B, C, D, E, F, _, H, I, J, K, L),
		VarTypes, simplify_info(A, B, C, D, E, F, VarTypes, H,I,J,K,L)).
simplify_info_set_requantify(simplify_info(A, B, C, D, E, F, G, _, I, J, K, L),
		simplify_info(A, B, C, D, E, F, G, yes, I, J, K, L)). 
simplify_info_set_recompute_atomic(simplify_info(A, B, C, D, E, F, G,H,_,J,K,L),
		simplify_info(A, B, C, D, E, F, G, H, yes, J, K, L)). 
simplify_info_set_rerun_det(simplify_info(A, B, C, D, E, F, G,H,I,_,K,L),
		simplify_info(A, B, C, D, E, F, G, H, I, yes, K, L)). 
simplify_info_set_cost_delta(simplify_info(A, B, C, D, E, F, G, H, I, J, _, L),
		Delta, simplify_info(A, B, C, D, E, F, G, H, I, J, Delta, L)). 

simplify_info_incr_cost_delta(
		simplify_info(A, B, C, D, E, F,G,H,I,J, Delta0, L),
		Incr, simplify_info(A, B, C, D, E, F, G, H, I, J, Delta, L)) :-
	Delta is Delta0 + Incr.

simplify_info_add_msg(Info0, Msg, Info) :-
	( simplify_do_warn(Info0) ->
		simplify_info_do_add_msg(Info0, Msg, Info)
	;
		Info = Info0
	).

simplify_info_do_add_msg(Info0, Msg, Info) :-
	simplify_info_get_msgs(Info0, Msgs0),
	set__insert(Msgs0, Msg, Msgs),
	simplify_info_set_msgs(Info0, Msgs, Info).

simplify_info_enter_lambda(
		simplify_info(A, B, C, D, E, F, G, H, I, J, K, LambdaCount0),
		simplify_info(A, B, C, D, E, F, G, H, I, J, K, LambdaCount)) :-
	LambdaCount is LambdaCount0 + 1.
simplify_info_leave_lambda(
		simplify_info(A, B, C, D, E, F, G, H, I, J, K, LambdaCount0),
		simplify_info(A, B, C, D, E, F, G, H, I, J, K, LambdaCount)) :-
	LambdaCount1 is LambdaCount0 - 1,
	(
		LambdaCount1 >= 0
	->
		LambdaCount = LambdaCount1
	;
		error("simplify_info_leave_lambda: Left too many lambdas")
	).
simplify_info_inside_lambda(
		simplify_info(_,_,_,_,_,_,_,_,_,_,_,LambdaCount)) :-
	LambdaCount > 0.

simplify_info_set_module_info(Info0, ModuleInfo, Info) :-
	simplify_info_get_det_info(Info0, DetInfo0),
	det_info_set_module_info(DetInfo0, ModuleInfo, DetInfo),
	simplify_info_set_det_info(Info0, DetInfo, Info).

:- interface.

:- pred simplify_do_warn(simplify_info::in) is semidet.
:- pred simplify_do_warn_calls(simplify_info::in) is semidet.
:- pred simplify_do_once(simplify_info::in) is semidet.
:- pred simplify_do_common(simplify_info::in) is semidet.
:- pred simplify_do_excess_assigns(simplify_info::in) is semidet.
:- pred simplify_do_calls(simplify_info::in) is semidet.
:- pred simplify_do_const_prop(simplify_info::in) is semidet.
:- pred simplify_do_more_common(simplify_info::in) is semidet.

:- implementation.

simplify_do_warn(Info) :-
	simplify_info_get_simplifications(Info, Simplifications),
	set__member(warn_simple_code, Simplifications).
simplify_do_warn_calls(Info) :-
	simplify_info_get_simplifications(Info, Simplifications),
	set__member(warn_duplicate_calls, Simplifications).
simplify_do_once(Info) :-
	simplify_info_get_simplifications(Info, Simplifications),
	set__member(do_once, Simplifications).
simplify_do_excess_assigns(Info) :-
	simplify_info_get_simplifications(Info, Simplifications),
	set__member(excess_assigns, Simplifications).
simplify_do_calls(Info) :-
	simplify_info_get_simplifications(Info, Simplifications),
	set__member(duplicate_calls, Simplifications).
simplify_do_const_prop(Info) :-
	simplify_info_get_simplifications(Info, Simplifications),
	set__member(constant_prop, Simplifications).
simplify_do_common(Info) :-
	simplify_info_get_simplifications(Info, Simplifications), 
	set__member(common_struct, Simplifications).
simplify_do_more_common(Info) :-
	simplify_info_get_simplifications(Info, Simplifications),
	set__member(extra_common_struct, Simplifications).

:- pred simplify_info_update_instmap(simplify_info::in, hlds_goal::in,
		simplify_info::out) is det.

simplify_info_update_instmap(
		simplify_info(A, B, C, D, InstMap0, F, G, H, I, J, K, L), Goal,
		simplify_info(A, B, C, D, InstMap, F, G, H, I, J, K, L)) :-
	update_instmap(Goal, InstMap0, InstMap).

:- type before_after
	--->	before
	;	after.

	% Clear the common_info structs accumulated since the last goal that
	% could cause a stack flush. This is done to avoid replacing a
	% deconstruction with assignments to the arguments where this
	% would cause more variables to be live across the stack flush.
	% Calls and construction unifications are not treated in this
	% way since it is nearly always better to optimize them away.
	% When doing deforestation, it may be better to remove
	% as many common structures as possible.
:- pred simplify_info_maybe_clear_structs(before_after::in, hlds_goal::in,
		simplify_info::in, simplify_info::out) is det.

simplify_info_maybe_clear_structs(BeforeAfter, Goal, Info0, Info) :-
	(
		( code_util__cannot_stack_flush(Goal) 
		; simplify_do_more_common(Info0)
		)
	->
		Info = Info0
	;
		% First check to see if a call is common and can be replaced 
		% by a series of unifications.
		simplify_do_common(Info0),
		(
			BeforeAfter = after
		; 
			BeforeAfter = before,
			Goal = GoalExpr - _,
			GoalExpr \= call(_, _, _, _, _, _),
			GoalExpr \= higher_order_call(_, _, _, _, _, _),
			GoalExpr \= class_method_call(_, _, _, _, _, _),
			GoalExpr \= pragma_c_code(_, _, _, _, _, _, _)
		)
	->
		simplify_info_get_common_info(Info0, CommonInfo0),
		common_info_clear_structs(CommonInfo0, CommonInfo),
		simplify_info_set_common_info(Info0, CommonInfo, Info)
	;
		Info = Info0
	).

	% Reset the instmap and seen calls for the next branch.
:- pred simplify_info_post_branch_update(simplify_info::in, simplify_info::in,
		simplify_info::out) is det.

simplify_info_post_branch_update(PreBranchInfo, PostBranchInfo0, Info) :-
	simplify_info_get_instmap(PreBranchInfo, InstMap),
	simplify_info_set_instmap(PostBranchInfo0, InstMap, PostBranchInfo1),
	simplify_info_get_common_info(PreBranchInfo, Common),
	simplify_info_set_common_info(PostBranchInfo1, Common, Info).

	% Undo updates to the simplify_info before redoing 
	% simplification on a goal. 
:- pred simplify_info_undo_goal_updates(simplify_info::in, simplify_info::in,
		simplify_info::out) is det.

simplify_info_undo_goal_updates(Info1, Info2, Info) :-
	simplify_info_get_common_info(Info1, CommonInfo0),
	simplify_info_set_common_info(Info2, CommonInfo0, Info3),
	simplify_info_get_instmap(Info1, InstMap),
	simplify_info_set_instmap(Info3, InstMap, Info).

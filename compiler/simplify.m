%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2002 The University of Melbourne.
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

:- module check_hlds__simplify.

:- interface.

:- import_module hlds__hlds_goal, hlds__hlds_module, hlds__hlds_pred.
:- import_module check_hlds__det_report, check_hlds__det_util.
:- import_module check_hlds__common, hlds__instmap, libs__globals.
:- import_module io, bool, list, map.

:- pred simplify__pred(list(simplification), pred_id, module_info, module_info,
	pred_info, pred_info, int, int, io__state, io__state).
:- mode simplify__pred(in, in, in, out, in, out, out, out, di, uo) is det.

:- pred simplify__proc(list(simplification), pred_id, proc_id,
	module_info, module_info, proc_info, proc_info, io__state, io__state).
:- mode simplify__proc(in, in, in, in, out, in, out, di, uo) is det.

:- pred simplify__proc_2(list(simplification), pred_id, proc_id, module_info,
		module_info, proc_info, proc_info, set(det_msg)).
:- mode simplify__proc_2(in, in, in, in, out, in, out, out) is det.

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

:- import_module parse_tree__inst, parse_tree__prog_data.
:- import_module parse_tree__prog_util.
:- import_module hlds__hlds_module, hlds__hlds_data, hlds__passes_aux.
:- import_module hlds__goal_util, hlds__goal_form, hlds__special_pred.
:- import_module hlds__quantification.
:- import_module check_hlds__type_util.
:- import_module check_hlds__mode_util, check_hlds__inst_match.
:- import_module check_hlds__det_analysis.
:- import_module check_hlds__modes, check_hlds__purity.
:- import_module check_hlds__unify_proc.
:- import_module check_hlds__polymorphism.
:- import_module transform_hlds__const_prop.
:- import_module transform_hlds__pd_cost.
:- import_module ll_backend__code_util, ll_backend__follow_code.
:- import_module libs__options.

:- import_module int, set, require, std_util, varset, term.

%-----------------------------------------------------------------------------%

simplify__pred(Simplifications0, PredId, ModuleInfo0, ModuleInfo,
		PredInfo0, PredInfo, WarnCnt, ErrCnt) -->
	write_pred_progress_message("% Simplifying ", PredId, ModuleInfo0),
	{ pred_info_non_imported_procids(PredInfo0, ProcIds) },
	{ MaybeMsgs0 = no },
	{
		% Don't warn for compiler-generated procedures.
		list__member(warn_simple_code, Simplifications0),
		code_util__compiler_generated(PredInfo0)
	->
		list__delete_all(Simplifications0, warn_simple_code,
			Simplifications)
	;
		Simplifications = Simplifications0
	},
	{ simplify__procs(Simplifications, PredId, ProcIds, ModuleInfo0,
		ModuleInfo, PredInfo0, PredInfo, MaybeMsgs0, MaybeMsgs) },
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
		maybe(pair(set(det_msg))), maybe(pair(set(det_msg)))).
:- mode simplify__procs(in, in, in, in, out, in, out,
		in, out) is det.

simplify__procs(_, _, [], ModuleInfo, ModuleInfo, PredInfo, PredInfo,
		Msgs, Msgs). 
simplify__procs(Simplifications, PredId, [ProcId | ProcIds], ModuleInfo0,
		ModuleInfo, PredInfo0, PredInfo, MaybeMsgs0, MaybeMsgs) :-
	pred_info_procedures(PredInfo0, Procs0),
	map__lookup(Procs0, ProcId, Proc0),
	simplify__proc_2(Simplifications, PredId, ProcId, ModuleInfo0,
		ModuleInfo1, Proc0, Proc, Msgs1),
	map__det_update(Procs0, ProcId, Proc, Procs),
	pred_info_set_procedures(PredInfo0, Procs, PredInfo1),
	set__to_sorted_list(Msgs1, Msgs2),
	list__filter(lambda([Msg::in] is semidet,
		det_msg_is_any_mode_msg(Msg, any_mode)),
		Msgs2, AnyModeMsgs1, AllModeMsgs1),
	set__sorted_list_to_set(AnyModeMsgs1, AnyModeMsgs2),
	set__sorted_list_to_set(AllModeMsgs1, AllModeMsgs2),
	( MaybeMsgs0 = yes(AnyModeMsgs0 - AllModeMsgs0) ->
		set__union(AnyModeMsgs0, AnyModeMsgs2, AnyModeMsgs),
		set__intersect(AllModeMsgs0, AllModeMsgs2, AllModeMsgs),
		MaybeMsgs1 = yes(AllModeMsgs - AnyModeMsgs)
	;
		MaybeMsgs1 = yes(AnyModeMsgs2 - AllModeMsgs2)
	),
	simplify__procs(Simplifications, PredId, ProcIds, ModuleInfo1, 
		ModuleInfo, PredInfo1, PredInfo, MaybeMsgs1, MaybeMsgs).

simplify__proc(Simplifications, PredId, ProcId, ModuleInfo0, ModuleInfo,
		Proc0, Proc)  -->
	write_pred_progress_message("% Simplifying ", PredId, ModuleInfo0),
	{ simplify__proc_2(Simplifications, PredId, ProcId, ModuleInfo0,
			ModuleInfo, Proc0, Proc, _) }.

simplify__proc_2(Simplifications, PredId, ProcId, ModuleInfo0, ModuleInfo,
		ProcInfo0, ProcInfo, Msgs) :-
	module_info_globals(ModuleInfo0, Globals),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	det_info_init(ModuleInfo0, VarTypes0, PredId, ProcId, Globals,
		DetInfo0),
	proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0, InstMap0),
	proc_info_varset(ProcInfo0, VarSet0),
	proc_info_inst_varset(ProcInfo0, InstVarSet0),
	proc_info_typeinfo_varmap(ProcInfo0, TVarMap0),
	proc_info_typeclass_info_varmap(ProcInfo0, TCVarMap0),
	proc_info_goal(ProcInfo0, Goal0),

	simplify_info_init(DetInfo0, Simplifications, InstMap0,
		VarSet0, InstVarSet0, TVarMap0, TCVarMap0, Info0),
	simplify__process_goal(Goal0, Goal, Info0, Info),
	
	simplify_info_get_varset(Info, VarSet),
	simplify_info_get_var_types(Info, VarTypes),
	simplify_info_get_type_info_varmap(Info, TVarMap),
	simplify_info_get_typeclass_info_varmap(Info, TCVarMap),
	proc_info_set_varset(ProcInfo0, VarSet, ProcInfo1),
	proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo2),
	proc_info_set_goal(ProcInfo2, Goal, ProcInfo3),
	proc_info_set_typeinfo_varmap(ProcInfo3, TVarMap, ProcInfo4),
	proc_info_set_typeclass_info_varmap(ProcInfo4, TCVarMap, ProcInfo),
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
		implicitly_quantify_goal(Goal1, VarSet0, VarTypes0,
			NonLocals, Goal2, VarSet, VarTypes, _),

		simplify_info_set_varset(Info1, VarSet, Info2),
		simplify_info_set_var_types(Info2, VarTypes, Info3),

		% Always recompute instmap_deltas for atomic goals - this
		% is safer in the case where unused variables should no
		% longer be included in the instmap_delta for a goal.
		% In the alias branch this is necessary anyway.
		RecomputeAtomic = yes,

		simplify_info_get_module_info(Info3, ModuleInfo3),
		recompute_instmap_delta(RecomputeAtomic, Goal2, Goal3,
			VarTypes, Info3^inst_varset, InstMap0, ModuleInfo3,
			ModuleInfo4),
		simplify_info_set_module_info(Info3, ModuleInfo4, Info4)
	;
		Goal3 = Goal1,
		Info4 = Info1
	),
	( simplify_info_rerun_det(Info4) ->
		Goal0 = _ - GoalInfo0,
		goal_info_get_determinism(GoalInfo0, Det),
		det_get_soln_context(Det, SolnContext),

		% det_infer_goal looks up the proc_info in the module_info
		% for the vartypes, so we'd better stick them back in the
		% module_info.
		simplify_info_get_module_info(Info4, ModuleInfo5),
		simplify_info_get_varset(Info4, VarSet4),
		simplify_info_get_var_types(Info4, VarTypes4),
		simplify_info_get_det_info(Info4, DetInfo4),
		det_info_get_pred_id(DetInfo4, PredId),
		det_info_get_proc_id(DetInfo4, ProcId),
		module_info_pred_proc_info(ModuleInfo5, PredId, ProcId,
			PredInfo, ProcInfo0),
		proc_info_set_vartypes(ProcInfo0, VarTypes4, ProcInfo1),
		proc_info_set_varset(ProcInfo1, VarSet4, ProcInfo),
		module_info_set_pred_proc_info(ModuleInfo5, PredId, ProcId,
			PredInfo, ProcInfo, ModuleInfo6),
		simplify_info_set_module_info(Info4, ModuleInfo6, Info),

		simplify_info_get_det_info(Info, DetInfo),
		det_infer_goal(Goal3, InstMap0, SolnContext,
			DetInfo, Goal, _, _)
	;
		Info = Info4,
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
		%
		Detism = failure,
		% ensure goal is pure or semipure
		\+ goal_info_is_impure(GoalInfo0),
		( det_info_get_fully_strict(DetInfo, no)
		; goal_cannot_loop(ModuleInfo, Goal0)
		)
	->
		% warn about this, unless the goal was an explicit
		% `fail', or some goal containing `fail'.

		goal_info_get_context(GoalInfo0, Context),
		(
			simplify_do_warn(Info0),
			\+ (
				goal_contains_goal(Goal0, SubGoal),
				SubGoal = disj([]) - _
			)
		->
			simplify_info_add_msg(Info0,
				goal_cannot_succeed(Context), Info1)
		;
			Info1 = Info0
		),
		
		% If the goal had any non-locals we should requantify. 
		goal_info_get_nonlocals(GoalInfo0, NonLocals0),
		( set__empty(NonLocals0) ->
			Info2 = Info1
		;
			simplify_info_set_requantify(Info1, Info2)
		),
		pd_cost__goal(Goal0, CostDelta),
		simplify_info_incr_cost_delta(Info2, CostDelta, Info3),
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
		%
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
		; goal_cannot_loop(ModuleInfo, Goal0)
		)
	->
/******************
The following warning is disabled, because it often results in spurious
warnings.  Sometimes predicate calls are used just to constrain the types,
to avoid type ambiguities or unbound type variables, and in such cases,
it is perfectly legitimate for a call to be det and to have no outputs.
There's no simple way of telling those cases from cases for which we
really ought to warn.
		% warn about this, if the goal wasn't `true', wasn't `!',
		% and wasn't a deconstruction unification.
		% We don't warn about deconstruction unifications
		% with no outputs that always succeed, because that
		% would result in bogus warnings, since switch detection
		% converts deconstruction unifications that can fail
		% into ones that always succeed by moving the test into
		% the switch.
		% We also don't warn about conjunctions or existential
		% quantifications, because it seems that warnings in those
		% cases are usually spurious.
		(
			simplify_do_warn(Info0),
			% Goal0 \= conj([]) - _,
			\+ (Goal0 = call(_, _, _, _, _, SymName) - _,
			    unqualify_name(SymName, "!")),
			Goal0 \= conj(_) - _,
			Goal0 \= some(_, _) - _,
			\+ (Goal0 = unify(_, _, _, Unification, _) - _,
			    Unification = deconstruct(_, _, _, _, _))
		->
			simplify_info_add_msg(Info0,
				det_goal_has_no_outputs(Context), Info1)
		;
			Info1 = Info0
		),
******************/
		Info0 = Info1,
		
		% If the goal had any non-locals we should requantify. 
		goal_info_get_nonlocals(GoalInfo0, NonLocals0),
		( set__empty(NonLocals0) ->
			Info2 = Info1
		;
			simplify_info_set_requantify(Info1, Info2)
		),
		pd_cost__goal(Goal0, CostDelta),
		simplify_info_incr_cost_delta(Info2, CostDelta, Info3),
		goal_info_get_context(GoalInfo0, Context),
		true_goal(Context, Goal1)
	;
		Goal1 = Goal0,
		Info3 = Info0
	),

	%
	% Remove unnecessary explicit quantifications before working
	% out whether the goal can cause a stack flush.
	%
	( Goal1 = some(SomeVars, CanRemove, SomeGoal1) - GoalInfo1 ->
		simplify__nested_somes(CanRemove, SomeVars, SomeGoal1,
			GoalInfo1, Goal2)
	;
		Goal2 = Goal1	
	),
	simplify_info_maybe_clear_structs(before, Goal2, Info3, Info4),
	Goal2 = GoalExpr2 - GoalInfo2,
	simplify__goal_2(GoalExpr2, GoalInfo2, Goal, GoalInfo3, Info4, Info5),
	simplify_info_maybe_clear_structs(after, Goal - GoalInfo3,
		Info5, Info6),
	simplify__enforce_invariant(GoalInfo3, GoalInfo, Info6, Info).

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
	simplify__excess_assigns_in_conj(GoalInfo0,
		Goals0, Goals1, Info0, Info1a),
	simplify__conj(Goals1, [], Goals, GoalInfo0, Info1a, Info1),
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
			Goal = some([], can_remove, InnerGoal)
		;
			Goal = conj(Goals)
		),
		GoalInfo = GoalInfo0
	).

simplify__goal_2(par_conj(Goals0), GoalInfo0, Goal,
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
		Goal = par_conj(Goals)
	).

simplify__goal_2(disj(Disjuncts0), GoalInfo0,
		Goal, GoalInfo, Info0, Info) :-
	simplify_info_get_instmap(Info0, InstMap0),
	simplify__disj(Disjuncts0, [], Disjuncts, [], InstMaps,
			Info0, Info0, Info1),
	( Disjuncts = [] ->
		goal_info_get_context(GoalInfo0, Context),
		fail_goal(Context, Goal - GoalInfo),
		Info2 = Info1
	; Disjuncts = [SingleGoal] ->
		% a singleton disjunction is equivalent to the goal itself
		SingleGoal = Goal1 - GoalInfo1,
		simplify__maybe_wrap_goal(GoalInfo0, GoalInfo1,
			Goal1, Goal, GoalInfo, Info1, Info2)
	;
		Goal = disj(Disjuncts),
		simplify_info_get_module_info(Info1, ModuleInfo1),
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		simplify_info_get_var_types(Info1, VarTypes),
		merge_instmap_deltas(InstMap0, NonLocals, VarTypes, InstMaps,
			NewDelta, ModuleInfo1, ModuleInfo2),
		simplify_info_set_module_info(Info1, ModuleInfo2, Info2),
		goal_info_set_instmap_delta(GoalInfo0, NewDelta, GoalInfo)
	),
	(
		list__length(Disjuncts) \=
			list__length(Disjuncts0) `with_type` int
	->
		%
		% If we pruned some disjuncts, variables used by those
		% disjuncts may no longer be non-local to the disjunction.
		% Also, the determinism may have changed (especially
		% if we pruned all the disjuncts).
		% If the disjunction now can't succeed, it is necessary
		% to recompute instmap_deltas and rerun determinism
		% analysis to avoid aborts in the code generator
		% because the disjunction now cannot produce variables
		% it did before.
		%
		simplify_info_set_requantify(Info2, Info3),
		simplify_info_set_rerun_det(Info3, Info)
	;
		Info = Info2
	).

simplify__goal_2(switch(Var, SwitchCanFail0, Cases0),
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
		simplify_info_incr_cost_delta(Info1, CostDelta, Info5),
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
			%
			% Don't optimize in the case of an existentially
			% typed constructor because currently 
			% simplify__create_test_unification does not
			% handle the existential type variables
			% in the types of the constructor arguments
			% or their type-infos.
			%
		    simplify_info_get_var_types(Info1, VarTypes1),
		    map__lookup(VarTypes1, Var, Type),
		    simplify_info_get_module_info(Info1, ModuleInfo1),
		    ( 
			type_util__is_existq_cons(ModuleInfo1,
					Type, ConsId)
		    ->
		    	Goal = switch(Var, SwitchCanFail, Cases),
			goal_info_get_nonlocals(GoalInfo0, NonLocals),
			simplify_info_get_var_types(Info1, VarTypes),
			merge_instmap_deltas(InstMap0, NonLocals, VarTypes,
				InstMaps, NewDelta, ModuleInfo1, ModuleInfo2),
			simplify_info_set_module_info(Info1,
				ModuleInfo2, Info4),
			goal_info_set_instmap_delta(GoalInfo0,
				NewDelta, GoalInfo)
		    ;
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
			instmap_delta_bind_var_to_functor(Var, Type, ConsId, 	
				InstMap, InstMapDelta0, InstMapDelta, 
				ModuleInfo1, ModuleInfo),
			simplify_info_set_module_info(Info2, 
				ModuleInfo, Info3),	
			goal_info_get_determinism(GoalInfo0, CaseDetism),
			det_conjunction_detism(semidet, CaseDetism, Detism),
			goal_info_init(NonLocals, InstMapDelta, Detism, 
				CombinedGoalInfo0),
			goal_list_purity(GoalList, Purity),
			add_goal_info_purity_feature(CombinedGoalInfo0,
				Purity, CombinedGoalInfo),

			simplify_info_set_requantify(Info3, Info4),
			Goal = conj(GoalList),
			GoalInfo = CombinedGoalInfo
		    )
		;
		    % The var can only be bound to this cons_id, so
		    % a test is unnecessary.
		    SingleGoal = Goal - GoalInfo,
		    Info4 = Info1
		),
		pd_cost__eliminate_switch(CostDelta),
		simplify_info_incr_cost_delta(Info4, CostDelta, Info5)
	;
		Goal = switch(Var, SwitchCanFail, Cases),
		simplify_info_get_module_info(Info1, ModuleInfo1),
		goal_info_get_nonlocals(GoalInfo0, NonLocals),
		simplify_info_get_var_types(Info1, VarTypes),
		merge_instmap_deltas(InstMap0, NonLocals, VarTypes, InstMaps,
			NewDelta, ModuleInfo1, ModuleInfo2),
		simplify_info_set_module_info(Info1, ModuleInfo2, Info5),
		goal_info_set_instmap_delta(GoalInfo0, NewDelta, GoalInfo)
	),
	( list__length(Cases) \= list__length(Cases0) `with_type` int ->
		%
		% If we pruned some cases, variables used by those
		% cases may no longer be non-local to the switch.
		% Also, the determinism may have changed (especially
		% if we pruned all the cases).
		% If the switch now can't succeed, it is necessary
		% to recompute instmap_deltas and rerun determinism
		% analysis to avoid aborts in the code generator
		% because the switch now cannot produce variables it
		% did before.
		%
		simplify_info_set_requantify(Info5, Info6),
		simplify_info_set_rerun_det(Info6, Info)
	;
		Info = Info5
	).

simplify__goal_2(Goal0, GoalInfo, Goal, GoalInfo, Info0, Info) :-
	Goal0 = generic_call(GenericCall, Args, Modes, Det),
	(
		simplify_do_calls(Info0),
		% XXX We should do duplicate call elimination for
		% class method calls here.
		GenericCall = higher_order(Closure, _, _)
	->
		common__optimise_higher_order_call(Closure, Args, Modes, Det,
			Goal0, GoalInfo, Goal, Info0, Info)
	;
		simplify_do_warn_calls(Info0),
		GenericCall = higher_order(Closure, _, _)
	->
		% We need to do the pass, for the warnings, but we ignore
		% the optimized goal and instead use the original one.
		common__optimise_higher_order_call(Closure, Args, Modes, Det,
			Goal0, GoalInfo, _Goal1, Info0, Info),
		Goal = Goal0
	;
		Goal = Goal0,
		Info = Info0
	).

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
		% pragma foreign.)
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
			PredInfo1, ProcInfo1),
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
		\+ proc_info_eval_method(ProcInfo, eval_minimal),

		% Don't warn about Aditi relations.
		\+ hlds_pred__pred_info_is_aditi_relation(PredInfo1)
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
		RT0 = lambda_goal(PredOrFunc, EvalMethod, FixModes,
			NonLocals, Vars, Modes, LambdaDeclaredDet, LambdaGoal0)
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
		RT = lambda_goal(PredOrFunc, EvalMethod, FixModes, NonLocals,
			Vars, Modes, LambdaDeclaredDet, LambdaGoal),
		simplify_info_leave_lambda(Info6, Info),
		Goal = unify(LT0, RT, M, U0, C),
		GoalInfo = GoalInfo0
	;
		U0 = complicated_unify(UniMode, CanFail, TypeInfoVars)
	->
		( RT0 = var(V) ->
			simplify__process_compl_unify(LT0, V,
				UniMode, CanFail, TypeInfoVars,
				C, GoalInfo0, Goal1,
				Info0, Info),
			Goal1 = Goal - GoalInfo
		;
			error("simplify.m: invalid RHS for complicated unify")
		)
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
	% Note however that rerunning determinism analysis, which
	% we do at the end of simplification, may introduce more
	% occurrences of these; since we don't iterate simplification
	% and determinism anaysis until a fixpoint is reached,
	% we don't guarantee to eliminate all such if-then-elses.
	% Hence the code generator must be prepared to handle the
	% case when the condition of an if-then-else has determinism
	% `det' or `failure'.
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

simplify__goal_2(if_then_else(Vars, Cond0, Then0, Else0),
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
		(
			Cond0 = not(NegCond) - _,
			% XXX BUG! This optimization is only safe if it
			% preserves mode correctness, which means in particular
			% that the the negated goal must not clobber any
			% variables.
			% For now I've just disabled the optimization.
			semidet_fail
		->
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
	; Else0 = disj([]) - _ ->
		% (A -> C ; fail) is equivalent to (A, C)
		goal_to_conj_list(Cond0, CondList),
		goal_to_conj_list(Then0, ThenList),
		list__append(CondList, ThenList, List),
		simplify__goal(conj(List) - GoalInfo0, Goal - GoalInfo,
			Info0, Info)
	;
		%
		% recursively simplify the sub-goals,
		% and rebuild the resulting if-then-else
		%
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
		simplify_info_get_var_types(Info6, VarTypes),
		merge_instmap_deltas(InstMap0, NonLocals, VarTypes,
			[CondThenDelta, ElseDelta], NewDelta,
			ModuleInfo0, ModuleInfo1),
		simplify_info_set_module_info(Info6, ModuleInfo1, Info7),
		goal_info_set_instmap_delta(GoalInfo0, NewDelta, GoalInfo1),
		IfThenElse = if_then_else(Vars, Cond, Then, Else),

		goal_info_get_determinism(GoalInfo0, IfThenElseDetism0),
		determinism_components(IfThenElseDetism0, IfThenElseCanFail,
			IfThenElseNumSolns),

		goal_info_get_determinism(CondInfo, CondDetism),
		determinism_components(CondDetism, CondCanFail, CondSolns),
		(
			%
			% check again if we can apply one of the above
			% simplifications after having simplified the 
			% sub-goals (we need to do this to ensure that
			% the goal is fully simplified, to maintain the
			% invariants that the MLDS back-end depends on)
			%
			( CondCanFail = cannot_fail
			; CondSolns = at_most_zero
			; Else = disj([]) - _ 
			)
		->
			simplify_info_undo_goal_updates(Info0, Info7, Info8),
			simplify__goal_2(IfThenElse, GoalInfo1,
				Goal, GoalInfo, Info8, Info)
		;
			(
				%
				% If-then-elses that are det or semidet may
				% nevertheless contain nondet or multidet
				% conditions. If this happens, the if-then-else
				% must be put inside a `some' to appease the
				% code generator.  (Both the MLDS and LLDS
				% back-ends rely on this.)
				%
				simplify_do_once(Info),
				CondSolns = at_most_many,
				IfThenElseNumSolns \= at_most_many
			->
				determinism_components(InnerDetism,
					IfThenElseCanFail, at_most_many),
				goal_info_set_determinism(GoalInfo1,
					InnerDetism, InnerInfo),
				Goal = some([], can_remove,
					IfThenElse - InnerInfo)
			;
				Goal = IfThenElse
			),
			GoalInfo = GoalInfo1,
			Info = Info7
		)
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
		Goal1 = disj([]) - _GoalInfo2
	->
		true_goal(Context, Goal - GoalInfo),
		Info = Info3
	;
		% remove double negation
		Goal1 = not(SubGoal - SubGoalInfo) - _,
		% XXX BUG! This optimization is only safe if it preserves
		% mode correctness, which means in particular that the
		% the negated goal must not clobber any variables.
		% For now I've just disabled the optimization.
		semidet_fail
	->
		simplify__maybe_wrap_goal(GoalInfo0, SubGoalInfo, SubGoal,
			Goal, GoalInfo, Info3, Info)
	;
		Goal = not(Goal1),
		GoalInfo = GoalInfo0,
		Info = Info3
	).

simplify__goal_2(some(Vars1, CanRemove0, Goal1), SomeInfo,
		GoalExpr, GoalInfo, Info0, Info) :-
	simplify_info_get_common_info(Info0, Common),
	simplify__goal(Goal1, Goal2, Info0, Info1),
	simplify__nested_somes(CanRemove0, Vars1, Goal2, SomeInfo, Goal),
	Goal = GoalExpr - GoalInfo,
	( Goal = some(_, _, _) - _ ->
		% Replacing calls, constructions or deconstructions
		% outside a commit with references to variables created
		% inside the commit would increase the set of output
		% variables of the goal inside the commit. This is not
		% allowed because it could change the determinism.
		%
		% Thus we need to reset the common_info to what it
		% was before processing the goal inside the commit,
		% to ensure that we don't make any such replacements
		% when processing the rest of the goal.
		simplify_info_set_common_info(Info1, Common, Info)
	;
		Info = Info1
	).

simplify__goal_2(Goal0, GoalInfo, Goal, GoalInfo, Info0, Info) :-
	Goal0 = foreign_proc(_, PredId, ProcId, Args, _, _, _),
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

simplify__goal_2(shorthand(_), _, _, _, _, _) :-
	% these should have been expanded out by now
	error("simplify__goal_2: unexpected shorthand").

%-----------------------------------------------------------------------------%

:- pred simplify__process_compl_unify(prog_var, prog_var,
		uni_mode, can_fail, list(prog_var), unify_context,
		hlds_goal_info, hlds_goal, simplify_info, simplify_info).
:- mode simplify__process_compl_unify(in, in, in, in, in, in, in, out,
		in, out) is det.

simplify__process_compl_unify(XVar, YVar, UniMode, CanFail, _OldTypeInfoVars,
		Context, GoalInfo0, Goal) -->
	=(Info0),
	{ simplify_info_get_module_info(Info0, ModuleInfo) },
	{ simplify_info_get_var_types(Info0, VarTypes) },
	{ map__lookup(VarTypes, XVar, Type) },
	( { Type = term__variable(TypeVar) } ->
		%
		% Convert polymorphic unifications into calls to
		% `unify/2', the general unification predicate, passing
		% the appropriate type_info
		% 	unify(TypeInfoVar, X, Y)
		% where TypeInfoVar is the type_info variable
		% associated with the type of the variables that
		% are being unified.
		%
		simplify__type_info_locn(TypeVar, TypeInfoVar, ExtraGoals),
		{ simplify__call_generic_unify(TypeInfoVar, XVar, YVar,
			ModuleInfo, Context, GoalInfo0, Call) }

	; { type_is_higher_order(Type, _, _, _) } ->
		%
		% convert higher-order unifications into calls to
		% builtin_unify_pred (which calls error/1)
		%
		{ SymName = unqualified("builtin_unify_pred") },
		{ ArgVars = [XVar, YVar] },
		{ module_info_get_predicate_table(ModuleInfo,
			PredicateTable) },
		{
			mercury_private_builtin_module(PrivateBuiltin),
			predicate_table_search_pred_m_n_a(
			    PredicateTable,
			    PrivateBuiltin, "builtin_unify_pred", 2,
			    [PredId0])
		->
			PredId = PredId0
		;
			error("can't locate private_builtin:builtin_unify_pred/2")
		},
		{ hlds_pred__in_in_unification_proc_id(ProcId) },
		{ CallContext = call_unify_context(XVar, var(YVar), Context) },
		{ Call0 = call(PredId, ProcId, ArgVars, not_builtin,
			yes(CallContext), SymName) },
		simplify__goal_2(Call0, GoalInfo0, Call1, GoalInfo),
		{ Call = Call1 - GoalInfo },
		{ ExtraGoals = [] }
	;
		{ type_to_ctor_and_args(Type, TypeCtorPrime, TypeArgsPrime) ->
			TypeCtor = TypeCtorPrime,
			TypeArgs = TypeArgsPrime
		;
			error("simplify: type_to_ctor_and_args failed")
		},
		{ determinism_components(Det, CanFail, at_most_one) },
		{ unify_proc__lookup_mode_num(ModuleInfo, TypeCtor, UniMode,
			Det, ProcId) },
		{ module_info_globals(ModuleInfo, Globals) },
		{ globals__lookup_bool_option(Globals, special_preds,
			SpecialPreds) },
		(
			{ hlds_pred__in_in_unification_proc_id(ProcId) },
			{
				SpecialPreds = no
			;
				SpecialPreds = yes,

				%
				% For most imported types we only generate
				% unification predicate declarations if they
				% are needed for complicated unifications
				% other than proc_id 0.
				% higher_order.m will specialize these cases
				% if possible.
				%
				special_pred_is_generated_lazily(ModuleInfo,
					TypeCtor)
			}
		->
			simplify__make_type_info_vars([Type], TypeInfoVars,
				ExtraGoals),
			{ TypeInfoVars = [TypeInfoVarPrime] ->
				TypeInfoVar = TypeInfoVarPrime
			;
				error("simplify__process_compl_unify: more than one typeinfo for one type var")
			},
			{ simplify__call_generic_unify(TypeInfoVar, XVar, YVar,
				ModuleInfo, Context, GoalInfo0, Call) }
		;
			%
			% Convert other complicated unifications into
			% calls to specific unification predicates,
			% inserting extra typeinfo arguments if necessary.
			%

			simplify__make_type_info_vars(TypeArgs,
				TypeInfoVars, ExtraGoals),
			{ simplify__call_specific_unify(TypeCtor, TypeInfoVars,
				XVar, YVar, ProcId, ModuleInfo, Context,
				GoalInfo0, Call0, CallGoalInfo0) },
			simplify__goal_2(Call0, CallGoalInfo0,
				Call1, CallGoalInfo1),
			{ Call = Call1 - CallGoalInfo1 }
		)
	),
	{ list__append(ExtraGoals, [Call], ConjList) },
	{ conj_list_to_goal(ConjList, GoalInfo0, Goal) }.

:- pred simplify__call_generic_unify(prog_var::in, prog_var::in,  prog_var::in, 
	module_info::in, unify_context::in, hlds_goal_info::in, hlds_goal::out)
	is det.

simplify__call_generic_unify(TypeInfoVar, XVar, YVar, ModuleInfo, Context,
		GoalInfo0, Call) :-
	ArgVars = [TypeInfoVar, XVar, YVar],
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	mercury_public_builtin_module(MercuryBuiltin),
	( predicate_table_search_pred_m_n_a(PredicateTable,
		MercuryBuiltin, "unify", 2, [CallPredId])
	->
		PredId = CallPredId
	;
		error("simplify.m: can't find `builtin:unify/2'")
	),
	% Note: the mode for polymorphic unifications
	% should be `in, in'. 
	% (This should have been checked by mode analysis.)
	hlds_pred__in_in_unification_proc_id(ProcId),

	SymName = unqualified("unify"),
	code_util__builtin_state(ModuleInfo, PredId, ProcId, BuiltinState),
	CallContext = call_unify_context(XVar, var(YVar), Context),
	goal_info_get_nonlocals(GoalInfo0, NonLocals0),
	set__insert(NonLocals0, TypeInfoVar, NonLocals),
	goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo),
	Call = call(PredId, ProcId, ArgVars, BuiltinState, yes(CallContext),
		SymName) - GoalInfo.

:- pred simplify__call_specific_unify(type_ctor::in, list(prog_var)::in,
	prog_var::in, prog_var::in, proc_id::in,
	module_info::in, unify_context::in, hlds_goal_info::in,
	hlds_goal_expr::out, hlds_goal_info::out) is det.

simplify__call_specific_unify(TypeCtor, TypeInfoVars, XVar, YVar, ProcId,
		ModuleInfo, Context, GoalInfo0, CallExpr, CallGoalInfo) :-
	% create the new call goal
	list__append(TypeInfoVars, [XVar, YVar], ArgVars),
	module_info_get_special_pred_map(ModuleInfo, SpecialPredMap),
	map__lookup(SpecialPredMap, unify - TypeCtor, PredId),
	SymName = unqualified("__Unify__"),
	CallContext = call_unify_context(XVar, var(YVar), Context),
	CallExpr = call(PredId, ProcId, ArgVars, not_builtin,
		yes(CallContext), SymName),

	% add the extra type_info vars to the nonlocals for the call
	goal_info_get_nonlocals(GoalInfo0, NonLocals0),
	set__insert_list(NonLocals0, TypeInfoVars, NonLocals),
	goal_info_set_nonlocals(GoalInfo0, NonLocals, CallGoalInfo).

:- pred simplify__make_type_info_vars(list(type)::in, list(prog_var)::out,
	list(hlds_goal)::out, simplify_info::in, simplify_info::out) is det.

simplify__make_type_info_vars(Types, TypeInfoVars, TypeInfoGoals,
		Info0, Info) :-
	%
	% Extract the information from simplify_info
	%
	simplify_info_get_det_info(Info0, DetInfo0),
	simplify_info_get_varset(Info0, VarSet0),
	simplify_info_get_var_types(Info0, VarTypes0),
	det_info_get_module_info(DetInfo0, ModuleInfo0),
	det_info_get_pred_id(DetInfo0, PredId),
	det_info_get_proc_id(DetInfo0, ProcId),

	%
	% Put the varset and vartypes from the simplify_info
	% back in the proc_info
	%
	module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
		PredInfo0, ProcInfo0),
	proc_info_set_vartypes(ProcInfo0, VarTypes0, ProcInfo1),
	proc_info_set_varset(ProcInfo1, VarSet0, ProcInfo2),

	%
	% Call polymorphism.m to create the type_infos
	%
	create_poly_info(ModuleInfo0, PredInfo0, ProcInfo2, PolyInfo0),
	term__context_init(Context),
	polymorphism__make_type_info_vars(Types, Context,
		TypeInfoVars, TypeInfoGoals, PolyInfo0, PolyInfo),
	poly_info_extract(PolyInfo, PredInfo0, PredInfo,
		ProcInfo0, ProcInfo, ModuleInfo1),

	%
	% Get the new varset and vartypes from the proc_info
	% and put them back in the simplify_info.
	%
	proc_info_vartypes(ProcInfo, VarTypes),
	proc_info_varset(ProcInfo, VarSet),
	simplify_info_set_var_types(Info0, VarTypes, Info1),
	simplify_info_set_varset(Info1, VarSet, Info2),

	%
	% Put the new proc_info and pred_info back
	% in the module_info and put the new module_info
	% back in the simplify_info.
	%
	module_info_set_pred_proc_info(ModuleInfo1, PredId, ProcId,
		PredInfo, ProcInfo, ModuleInfo),
	simplify_info_set_module_info(Info2, ModuleInfo, Info).

:- pred simplify__type_info_locn(tvar, prog_var, list(hlds_goal),
		simplify_info, simplify_info).
:- mode simplify__type_info_locn(in, out, out, in, out) is det.

simplify__type_info_locn(TypeVar, TypeInfoVar, Goals) -->
	=(Info0),
	{ simplify_info_get_type_info_varmap(Info0, TypeInfoMap) },
	{ map__lookup(TypeInfoMap, TypeVar, TypeInfoLocn) },
	(
			% If the typeinfo is available in a variable,
			% just use it
		{ TypeInfoLocn = type_info(TypeInfoVar) },
		{ Goals = [] }
	;
			% If the typeinfo is in a typeclass_info
			% then we need to extract it
		{ TypeInfoLocn =
			typeclass_info(TypeClassInfoVar, Index) },
		simplify__extract_type_info(TypeVar, TypeClassInfoVar, Index,
			Goals, TypeInfoVar)
	).

:- pred simplify__extract_type_info(tvar, prog_var, int,
		list(hlds_goal), prog_var, simplify_info, simplify_info).
:- mode simplify__extract_type_info(in, in, in, out, out, in, out) is det.

simplify__extract_type_info(TypeVar, TypeClassInfoVar, Index,
		Goals, TypeInfoVar, Info0, Info) :-
	simplify_info_get_module_info(Info0, ModuleInfo),
	simplify_info_get_varset(Info0, VarSet0),
	simplify_info_get_var_types(Info0, VarTypes0),

	polymorphism__gen_extract_type_info(TypeVar, TypeClassInfoVar, Index,
		ModuleInfo, Goals, TypeInfoVar,
		VarSet0, VarTypes0, VarSet, VarTypes),

	simplify_info_set_var_types(Info0, VarTypes, Info1),
	simplify_info_set_varset(Info1, VarSet, Info).

%-----------------------------------------------------------------------------%

	% simplify__input_args_are_equiv(Args, HeadVars, Modes,
	% 		CommonInfo, ModuleInfo1):
	% Succeeds if all the input arguments (determined by looking at
	% `Modes') in `Args' are equivalent (according to the equivalence
	% class specified by `CommonInfo') to the corresponding variables
	% in HeadVars.  HeadVars, Modes, and Args should all be lists of
	% the same length.

:- pred simplify__input_args_are_equiv(list(prog_var), list(prog_var),
		list(mode), common_info, module_info).
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
:- pred simplify__nested_somes(can_remove::in, list(prog_var)::in,
		hlds_goal::in, hlds_goal_info::in, hlds_goal::out) is det.

simplify__nested_somes(CanRemove0, Vars1, Goal0, OrigGoalInfo, Goal) :-
	simplify__nested_somes_2(CanRemove0, Vars1, Goal0,
		CanRemove, Vars, Goal1),
	Goal1 = GoalExpr1 - GoalInfo1,
	(
		goal_info_get_determinism(GoalInfo1, Detism),
		goal_info_get_determinism(OrigGoalInfo, Detism),
		CanRemove = can_remove
	->
		% If the inner and outer detisms match the `some'
		% is unnecessary.
		Goal = GoalExpr1 - GoalInfo1
	;
		Goal = some(Vars, CanRemove, Goal1) - OrigGoalInfo
	).

:- pred simplify__nested_somes_2(can_remove::in, list(prog_var)::in,
		hlds_goal::in, can_remove::out, list(prog_var)::out,
		hlds_goal::out) is det.

simplify__nested_somes_2(CanRemove0, Vars0, Goal0, CanRemove, Vars, Goal) :-
	( Goal0 = some(Vars1, CanRemove1, Goal1) - _ ->
		(
			( CanRemove0 = cannot_remove
			; CanRemove1 = cannot_remove
			)
		->
			CanRemove2 = cannot_remove
		;
			CanRemove2 = can_remove
		),
		list__append(Vars0, Vars1, Vars2),
		simplify__nested_somes_2(CanRemove2, Vars2, Goal1,
			CanRemove, Vars, Goal)
	;
		CanRemove = CanRemove0,
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
		Goal = some([], can_remove, Goal1 - InnerGoalInfo),
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

		( (Goal1 = disj([]) - _ ; Goals0 = []) ->
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
		simplify__conjoin_goal_and_rev_goal_list(Goal1,
			RevGoals0, RevGoals1),
		simplify_info_update_instmap(Info1, Goal1, Info2),
		simplify__conj(Goals0, RevGoals1, Goals,
			ConjInfo, Info2, Info)
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

:- pred simplify__excess_assigns_in_conj(hlds_goal_info::in,
		list(hlds_goal)::in, list(hlds_goal)::out,
		simplify_info::in, simplify_info::out) is det.

simplify__excess_assigns_in_conj(ConjInfo, Goals0, Goals,
		Info0, Info) :-
	( simplify_do_excess_assigns(Info0) ->
		goal_info_get_nonlocals(ConjInfo, ConjNonLocals),
		map__init(Subn0),
		simplify__find_excess_assigns_in_conj(ConjNonLocals,
			Goals0, [], RevGoals, Subn0, Subn1),
		( map__is_empty(Subn1) ->
			Goals = Goals0,
			Info = Info0
		;
			renaming_transitive_closure(Subn1, Subn),
			list__reverse(RevGoals, Goals1),
			MustSub = no,
			goal_util__rename_vars_in_goals(Goals1, MustSub,
				Subn, Goals),
			simplify_info_get_varset(Info0, VarSet0),
			map__keys(Subn0, RemovedVars),
			varset__delete_vars(VarSet0, RemovedVars, VarSet),
			simplify_info_set_varset(Info0, VarSet, Info1),
			simplify_info_get_type_info_varmap(Info1, TVarMap0),
			apply_substitutions_to_var_map(TVarMap0,
				map__init, map__init, Subn, TVarMap),
			simplify_info_set_type_info_varmap(Info1, TVarMap,
				Info2),
			simplify_info_get_typeclass_info_varmap(Info2,
				TCVarMap0),
			apply_substitutions_to_typeclass_var_map(TCVarMap0,
				map__init, map__init, Subn, TCVarMap),
			simplify_info_set_typeclass_info_varmap(Info2,
				TCVarMap, Info)
		)
	;
		Goals = Goals0,
		Info = Info0
	).

:- type var_renaming == map(prog_var, prog_var).

:- pred simplify__find_excess_assigns_in_conj(set(prog_var)::in,
	list(hlds_goal)::in, list(hlds_goal)::in, list(hlds_goal)::out,
	var_renaming::in, var_renaming::out) is det.

simplify__find_excess_assigns_in_conj(_, [], RevGoals, RevGoals,
			Subn, Subn).
simplify__find_excess_assigns_in_conj(ConjNonLocals, [Goal | Goals],
			RevGoals0, RevGoals, Subn0, Subn) :-
	( goal_is_excess_assign(ConjNonLocals, Goal, Subn0, Subn1) ->
		RevGoals1 = RevGoals0,
		Subn2 = Subn1
	;
		RevGoals1 = [Goal | RevGoals0],
		Subn2 = Subn0
	),
	simplify__find_excess_assigns_in_conj(ConjNonLocals, Goals,
		RevGoals1, RevGoals, Subn2, Subn).

:- pred goal_is_excess_assign(set(prog_var)::in, hlds_goal::in,
	var_renaming::in, var_renaming::out) is semidet.

goal_is_excess_assign(ConjNonLocals, Goal0, Subn0, Subn) :-
	Goal0 = unify(_, _, _, Unif, _) - _,
	Unif = assign(LeftVar0, RightVar0),

	%
	% Check if we've already substituted
	% one or both of the variables.
	%
	find_renamed_var(Subn0, LeftVar0, LeftVar),
	find_renamed_var(Subn0, RightVar0, RightVar),
	( \+ set__member(LeftVar, ConjNonLocals) ->
		map__det_insert(Subn0, LeftVar, RightVar, Subn)
	; \+ set__member(RightVar, ConjNonLocals) ->
		map__det_insert(Subn0, RightVar, LeftVar, Subn)
	;
		fail
	).

:- pred find_renamed_var(var_renaming, prog_var, prog_var).
:- mode find_renamed_var(in, in, out) is det.

find_renamed_var(Subn, Var0, Var) :-
	( map__search(Subn, Var0, Var1) ->
		find_renamed_var(Subn, Var1, Var)
	;
		Var = Var0
	).

	% Collapse chains of renamings.
:- pred renaming_transitive_closure(var_renaming, var_renaming).
:- mode renaming_transitive_closure(in, out) is det.

renaming_transitive_closure(VarRenaming0, VarRenaming) :-
	map__map_values(
		(pred(_::in, Value0::in, Value::out) is det :-
			find_renamed_var(VarRenaming0, Value0, Value)
		), VarRenaming0, VarRenaming).

%-----------------------------------------------------------------------------%

:- pred simplify__switch(prog_var, list(case), list(case), list(case), 
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
	simplify_info_get_var_types(Info1, VarTypes),
	map__lookup(VarTypes, Var, Type),
	instmap__bind_var_to_functor(Var, Type, ConsId,
		InstMap0, InstMap1, ModuleInfo0, ModuleInfo1),
	simplify_info_set_module_info(Info1, ModuleInfo1, Info2),
	simplify_info_set_instmap(Info2, InstMap1, Info3),
	simplify__goal(Goal0, Goal, Info3, Info4),

		% Remove failing branches. 
	( Goal = disj([]) - _ ->
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
		instmap_delta_bind_var_to_functor(Var, Type, ConsId,
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
	% This will abort if the cons_id is existentially typed.
:- pred simplify__create_test_unification(prog_var::in, cons_id::in, int::in,
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
		ArgVars, UniModes, can_fail, no),
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
			Goal0 \= disj([]) - _
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
				Goal0 = disj([]) - _
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
				GoalInfo, InstMap0, DetInfo, Goal,
				MsgsA, Msgs)
		;
	****/

:- pred simplify__fixup_disj(list(hlds_goal), determinism, bool,
	hlds_goal_info, hlds_goal_expr,
	simplify_info, simplify_info).
:- mode simplify__fixup_disj(in, in, in, in, out, in, out) is det.

simplify__fixup_disj(Disjuncts, _, _OutputVars, GoalInfo, Goal, Info0, Info) :-
	det_disj_to_ite(Disjuncts, GoalInfo, IfThenElse),
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

:- pred det_disj_to_ite(list(hlds_goal), hlds_goal_info, hlds_goal).
:- mode det_disj_to_ite(in, in, out) is det.

det_disj_to_ite([], _GoalInfo, _) :-
	error("reached base case of det_disj_to_ite").
det_disj_to_ite([Disjunct | Disjuncts], GoalInfo, Goal) :-
	( Disjuncts = [] ->
		Goal = Disjunct
	;
		Cond = Disjunct,
		Cond = _CondGoal - CondGoalInfo,

		true_goal(Then),

		det_disj_to_ite(Disjuncts, GoalInfo, Rest),
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

		Goal = if_then_else([], Cond, Then, Rest) - NewGoalInfo
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
			det_info	::	det_info,
			msgs		::	set(det_msg),
			simplifications	::	set(simplification),
			common_info	::	common_info,
					% Info about common subexpressions.
			instmap		::	instmap,
			varset		::	prog_varset,
			inst_varset	::	inst_varset,
			requantify	::	bool,
					% Does the goal need requantification.
			recompute_atomic ::	bool,
					% Do we need to recompute
					% instmap_deltas for atomic goals
			rerun_det	::	bool,
					% Does determinism analysis need to
					% be rerun.
			cost_delta	::	int,
					% Measure of the improvement in
					% the goal from simplification.
			lambdas		::	int,
					% Count of the number of lambdas
					% which enclose the current goal.
			type_info_varmap ::	type_info_varmap,
			typeclass_info_varmap ::	typeclass_info_varmap
		).

simplify_info_init(DetInfo, Simplifications0, InstMap,
		VarSet, InstVarSet, TVarMap, TCVarMap, Info) :-
	common_info_init(CommonInfo),
	set__init(Msgs),
	set__list_to_set(Simplifications0, Simplifications),
	Info = simplify_info(DetInfo, Msgs, Simplifications, CommonInfo,
		InstMap, VarSet, InstVarSet, no, no, no, 0, 0,
		TVarMap, TCVarMap). 

	% Reinitialise the simplify_info before reprocessing a goal.
:- pred simplify_info_reinit(set(simplification)::in, instmap::in,
		simplify_info::in, simplify_info::out) is det.

simplify_info_reinit(Simplifications, InstMap0) -->
	{ common_info_init(Common) },
	^simplifications := Simplifications,
	^common_info := Common,
	^instmap := InstMap0,
	^requantify := no,
	^recompute_atomic := no,
	^rerun_det := no,
	^lambdas := 0.

	% exported for common.m
:- interface.

:- import_module parse_tree__prog_data.
:- import_module set.

:- pred simplify_info_init(det_info::in, list(simplification)::in, instmap::in,
		prog_varset::in, inst_varset::in, 
		type_info_varmap::in, typeclass_info_varmap::in,
		simplify_info::out) is det.

:- pred simplify_info_get_det_info(simplify_info::in, det_info::out) is det.
:- pred simplify_info_get_msgs(simplify_info::in, set(det_msg)::out) is det.
:- pred simplify_info_get_instmap(simplify_info::in, instmap::out) is det.
:- pred simplify_info_get_simplifications(simplify_info::in,
		set(simplification)::out) is det.
:- pred simplify_info_get_common_info(simplify_info::in,
		common_info::out) is det.
:- pred simplify_info_get_varset(simplify_info::in, prog_varset::out) is det.
:- pred simplify_info_get_var_types(simplify_info::in,
		vartypes::out) is det.
:- pred simplify_info_requantify(simplify_info::in) is semidet.
:- pred simplify_info_recompute_atomic(simplify_info::in) is semidet.
:- pred simplify_info_rerun_det(simplify_info::in) is semidet.
:- pred simplify_info_get_cost_delta(simplify_info::in, int::out) is det.
:- pred simplify_info_get_type_info_varmap(simplify_info::in,
		type_info_varmap::out) is det.
:- pred simplify_info_get_typeclass_info_varmap(simplify_info::in,
		typeclass_info_varmap::out) is det.

:- pred simplify_info_get_module_info(simplify_info::in,
		module_info::out) is det.
:- pred simplify_info_get_pred_info(simplify_info::in,
		pred_info::out) is det.

:- implementation.

simplify_info_get_det_info(SI, SI^det_info).
simplify_info_get_msgs(SI, SI^msgs).
simplify_info_get_simplifications(SI, SI^simplifications).
simplify_info_get_common_info(SI, SI^common_info).
simplify_info_get_instmap(SI, SI^instmap).
simplify_info_get_varset(SI, SI^varset).
simplify_info_get_var_types(SI, VarTypes) :-
	det_info_get_vartypes(SI^det_info, VarTypes).
simplify_info_requantify(SI) :-
	SI^requantify = yes.
simplify_info_recompute_atomic(SI) :-
	SI^recompute_atomic = yes.
simplify_info_rerun_det(SI) :-
	SI^rerun_det = yes.
simplify_info_get_cost_delta(SI, SI^cost_delta).
simplify_info_get_type_info_varmap(SI, SI^type_info_varmap).
simplify_info_get_typeclass_info_varmap(SI, SI^typeclass_info_varmap).

simplify_info_get_module_info(Info, ModuleInfo) :-
	simplify_info_get_det_info(Info, DetInfo),
	det_info_get_module_info(DetInfo, ModuleInfo).

simplify_info_get_pred_info(Info, PredInfo) :-
	simplify_info_get_det_info(Info, DetInfo),
	det_info_get_module_info(DetInfo, ModuleInfo),
	det_info_get_pred_id(DetInfo, PredId),
	module_info_pred_info(ModuleInfo, PredId, PredInfo).

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
:- pred simplify_info_set_varset(simplify_info::in, prog_varset::in,
		simplify_info::out) is det.
:- pred simplify_info_set_var_types(simplify_info::in, map(prog_var, type)::in,
		simplify_info::out) is det.
:- pred simplify_info_set_requantify(simplify_info::in,
		simplify_info::out) is det.
:- pred simplify_info_set_recompute_atomic(simplify_info::in,
		simplify_info::out) is det.
:- pred simplify_info_set_rerun_det(simplify_info::in,
		simplify_info::out) is det.
:- pred simplify_info_set_type_info_varmap(simplify_info::in,
		type_info_varmap::in, simplify_info::out) is det.
:- pred simplify_info_set_typeclass_info_varmap(simplify_info::in,
		typeclass_info_varmap::in, simplify_info::out) is det.

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

simplify_info_set_det_info(SI, Det, SI^det_info := Det).
simplify_info_set_msgs(SI, Msgs, SI^msgs := Msgs). 
simplify_info_set_simplifications(SI, Simp, SI^simplifications := Simp).
simplify_info_set_instmap(SI, InstMap, SI^instmap := InstMap). 
simplify_info_set_common_info(SI, Common, SI^common_info := Common). 
simplify_info_set_varset(SI, VarSet, SI^varset := VarSet). 
simplify_info_set_var_types(SI, VarTypes, SI^det_info := DetInfo) :-
	det_info_set_vartypes(SI ^ det_info, VarTypes, DetInfo).
simplify_info_set_requantify(SI, SI^requantify := yes).
simplify_info_set_recompute_atomic(SI, SI^recompute_atomic := yes).
simplify_info_set_rerun_det(SI, SI^rerun_det := yes).
simplify_info_set_cost_delta(SI, Delta, SI^cost_delta := Delta).
simplify_info_set_type_info_varmap(SI, Map, SI^type_info_varmap := Map).
simplify_info_set_typeclass_info_varmap(SI, Map,
		SI^typeclass_info_varmap := Map).

simplify_info_incr_cost_delta(SI, Incr, SI^cost_delta := SI^cost_delta + Incr).

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

simplify_info_enter_lambda(SI, SI^lambdas := SI^lambdas + 1).
simplify_info_leave_lambda(SI, SI^lambdas := LambdaCount) :-
	LambdaCount1 is SI^lambdas - 1,
	(
		LambdaCount1 >= 0
	->
		LambdaCount = LambdaCount1
	;
		error("simplify_info_leave_lambda: Left too many lambdas")
	).
simplify_info_inside_lambda(SI) :-
	SI^lambdas > 0.

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

simplify_info_update_instmap(SI, Goal, SI^instmap := InstMap) :-
	update_instmap(Goal, SI^instmap, InstMap).

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
		( code_util__cannot_flush(Goal) 
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
			GoalExpr \= generic_call(_, _, _, _),
			GoalExpr \= foreign_proc(_, _, _, _, _, _, _)
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

%-----------------------------------------------------------------------------%

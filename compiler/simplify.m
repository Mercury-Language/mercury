%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main author: zs.
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
% structure elimination and branch merging. The second pass performs
% excess assignment elimination and cleans up the code after the first pass.
% Two passes are required because the goal must be requantified after the
% optimizations in common.m are run so that excess assignment elimination
% works properly.
%
%-----------------------------------------------------------------------------%
:- module simplify.

:- interface.

:- import_module common, hlds_pred, det_report, det_util.
:- import_module io.

:- pred simplify__proc(simplify, pred_id, proc_id, module_info, module_info,
	proc_info, proc_info, int, int, io__state, io__state).
:- mode simplify__proc(in, in, in, in, out, in, out, out, out, di, uo) is det.

:- pred simplify__goal(hlds_goal, hlds_goal,
		simplify_info, simplify_info).
:- mode simplify__goal(in, out, in, out) is det.

:- pred simplify_info_init(det_info, simplify, instmap,
		varset, map(var, type), simplify_info).
:- mode simplify_info_init(in, in, in, in, in, out) is det.

:- type simplify_info.

:- type simplify
	---> 	simplify(
			bool,	% --warn-simple-code
			bool,	% --warn-duplicate-calls
			bool,	% run things that should be done once only
			bool,	% attempt to merge adjacent switches 
			bool,	% common subexpression elimination
			bool,	% remove excess assignment unifications
			bool	% optimize duplicate calls
		).	

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_out.

:- import_module code_aux, det_analysis, follow_code, goal_util.
:- import_module hlds_module, hlds_goal, hlds_data, instmap, inst_match.
:- import_module globals, options, passes_aux, prog_data, mode_util, type_util.
:- import_module code_util, quantification, modes.
:- import_module bool, list, set, map, require, std_util, term, varset.

%-----------------------------------------------------------------------------%

simplify__proc(Simplify, PredId, ProcId, ModuleInfo0, ModuleInfo,
		Proc0, Proc, WarnCnt, ErrCnt, State0, State) :-
	globals__io_get_globals(Globals, State0, State1),
	det_info_init(ModuleInfo0, PredId, ProcId, Globals, DetInfo),
	proc_info_get_initial_instmap(Proc0, ModuleInfo0, InstMap0),
	proc_info_variables(Proc0, VarSet0),
	proc_info_vartypes(Proc0, VarTypes0),
	simplify_info_init(DetInfo, Simplify, InstMap0,
		VarSet0, VarTypes0, Info0),
	write_pred_progress_message("% Simplifying ", PredId, ModuleInfo0,
		State1, State2),
	Simplify = simplify(Warn, WarnCalls, Once, Switch, _, Excess, Calls),
	( simplify_do_common(Info0) ->
		% On the first pass do common structure elimination and
		% branch merging.
		simplify_info_set_simplify(Info0,
			simplify(Warn, WarnCalls, no, Switch, yes, no, Calls),
			Info1),
		simplify__proc_2(Proc0, Proc1, ModuleInfo0, ModuleInfo1,
			Info1, Info2, State2, State3),
		simplify_info_get_msgs(Info2, Msgs1),
		proc_info_variables(Proc1, VarSet1),
		proc_info_vartypes(Proc1, VarTypes1),
		simplify_info_init(DetInfo,
			simplify(no, no, Once, no, no, Excess, no),
			InstMap0, VarSet1, VarTypes1, Info3),
		simplify_info_set_msgs(Info3, Msgs1, Info4),
		%proc_info_goal(Proc1, OutGoal),
		%hlds_out__write_goal(OutGoal, ModuleInfo1, VarSet1, yes,
		%	2, ".", State3, State4)
		State4 = State3
	;
		Info4 = Info0,
		Proc1 = Proc0,
		ModuleInfo1 = ModuleInfo0,
		State4 = State2
	),
		% On the second pass do excess assignment elimination and
		% some cleaning up after the common structure and branch 
		% merging pass.
	simplify__proc_2(Proc1, Proc, ModuleInfo1, ModuleInfo,
			Info4, Info, State4, State5),
	simplify_info_get_msgs(Info, Msgs2),
	set__to_sorted_list(Msgs2, Msgs),
	( (Warn = yes ; WarnCalls = yes) ->
		det_report_msgs(Msgs, ModuleInfo, WarnCnt,
			ErrCnt, State5, State)
	;
		WarnCnt = 0,
		ErrCnt = 0,
		State = State5
	).

:- pred simplify__proc_2(proc_info::in, proc_info::out, module_info::in,
		module_info::out, simplify_info::in, simplify_info::out,
		io__state::di, io__state::uo) is det.

simplify__proc_2(Proc0, Proc, ModuleInfo0, ModuleInfo,
		Info0, Info, State0, State) :-
	proc_info_goal(Proc0, Goal0),
	simplify__goal(Goal0, Goal, Info0, Info),
	simplify_info_get_varset(Info, VarSet),
	simplify_info_get_var_types(Info, VarTypes),
	proc_info_set_goal(Proc0, Goal, Proc1),
	proc_info_set_variables(Proc1, VarSet, Proc2),
	proc_info_set_vartypes(Proc2, VarTypes, Proc3),
	( simplify_info_requantify(Info) ->
		requantify_proc(Proc3, Proc4),
		( simplify_info_recompute_atomic(Info) ->
			RecomputeAtomic = yes
		;
			RecomputeAtomic = no
		),
		proc_info_goal(Proc4, Goal2),
		proc_info_get_initial_instmap(Proc4,
			ModuleInfo0, InstMap0),
		recompute_instmap_delta(RecomputeAtomic, Goal2, Goal3,
			InstMap0, ModuleInfo0, ModuleInfo),
		proc_info_set_goal(Proc4, Goal3, Proc),
		State = State0
	;
		Proc = Proc3,
		ModuleInfo = ModuleInfo0,
		State = State0
	).

%-----------------------------------------------------------------------------%

simplify__goal(Goal0, Goal - GoalInfo, Info0, Info) :-
	Goal0 = _GoalExpr0 - GoalInfo0,
	simplify_info_get_det_info(Info0, DetInfo),
	goal_info_get_determinism(GoalInfo0, Detism),
	simplify_info_get_module_info(Info0, ModuleInfo),
	(
		%
		% if --no-fully-strict,
		% replace goals with determinism failure with `fail'.
		% XXX we should warn about this (if the goal wasn't `fail')
		%
		Detism = failure,
		( det_info_get_fully_strict(DetInfo, no)
		; code_aux__goal_cannot_loop(ModuleInfo, Goal0)
		)
	->
		fail_goal(Goal1)
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
		determinism_components(Detism, cannot_fail, MaxSoln),
		MaxSoln \= at_most_zero,
		goal_info_get_instmap_delta(GoalInfo0, InstMapDelta),
		goal_info_get_nonlocals(GoalInfo0, NonLocalVars),
		simplify_info_get_instmap(Info0, InstMap0),
		det_no_output_vars(NonLocalVars, InstMap0, InstMapDelta,
			DetInfo),
		( det_info_get_fully_strict(DetInfo, no)
		; code_aux__goal_cannot_loop(ModuleInfo, Goal0)
		)
	->
		true_goal(Goal1)
	;
		Goal1 = Goal0
	),
	simplify_info_maybe_clear_structs(before, Goal1,
		Info0, Info1),
	Goal1 = GoalExpr1 - GoalInfo1,
	simplify__goal_2(GoalExpr1, GoalInfo1, Goal, GoalInfo, Info1, Info2),
	simplify_info_maybe_clear_structs(after, Goal - GoalInfo, Info2, Info).

%-----------------------------------------------------------------------------%

:- pred simplify__goal_2(hlds_goal_expr, hlds_goal_info, hlds_goal_expr,
		hlds_goal_info, simplify_info, simplify_info).
:- mode simplify__goal_2(in, in, out, out, in, out) is det.

simplify__goal_2(conj(Goals0), GoalInfo0, Goal, GoalInfo0, Info0, Info) :-
	simplify_info_reset_branch_info(Info0, Info1, PostBranchInstMaps),
	simplify_info_get_instmap(Info1, InstMap0),
	simplify__conj(Goals0, [], Goals, GoalInfo0, Info1, Info2),
	simplify_info_set_branch_info(Info2, PostBranchInstMaps, Info3),
	simplify_info_set_instmap(Info3, InstMap0, Info),
	( Goals = [SingleGoal] ->
		% a singleton conjunction is equivalent to the goal itself
		SingleGoal = Goal - _
	;
		%
		% Conjunctions that cannot produce solutions may nevertheless
		% contain nondet and multidet goals. If this happens, the
		% conjunction is put inside a `some' to appease the code
		% generator.
		%
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
		)
	).

simplify__goal_2(disj(Disjuncts0, SM), GoalInfo0,
		Goal, GoalInfo, Info0, Info) :-
	( Disjuncts0 = [] ->
		Goal = disj([], SM),
		GoalInfo = GoalInfo0,
		Info = Info0
	; Disjuncts0 = [SingleGoal0] ->
		% a singleton disjunction is equivalent to the goal itself
		simplify__goal(SingleGoal0, Goal1 - GoalInfo1, Info0, Info),
		(
			% If the determinisms are not the same, we really
			% need to rerun determinism analysis on the
			% procedure. I think this is a similar situation
			% to inlining of erroneous goals. The safe thing
			% to do is to disable the optimisation if the
			% inner and outer determinisms are not the same.
			% It probably won't happen that often.
			goal_info_get_determinism(GoalInfo0, Det),
			goal_info_get_determinism(GoalInfo1, Det)
		->
			Goal = Goal1,
			GoalInfo = GoalInfo1
		;
			Goal = disj([Goal1 - GoalInfo1], SM),
			GoalInfo = GoalInfo0
		)
	;
		simplify__disj(Disjuncts0, Disjuncts, [], InstMaps,
			Info0, Info0, Info1),
		simplify_info_create_branch_info(Info0, Info1, InstMaps, Info),
		(
	/****
	XXX This optimization is not correct, see comment below
	    at the definition of fixup_disj
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
			Goal = disj(Disjuncts, SM),
			GoalInfo = GoalInfo0
		)
	).

simplify__goal_2(switch(Var, SwitchCanFail, Cases0, SM),
		GoalInfo0, Goal, GoalInfo, Info0, Info) :-
	simplify_info_get_instmap(Info0, InstMap0),
	simplify_info_get_module_info(Info0, ModuleInfo),
	instmap__lookup_var(InstMap0, Var, VarInst),
	( inst_is_bound_to_functors(ModuleInfo, VarInst, Functors) ->
		functors_to_cons_ids(Functors, ConsIds0),
		list__sort(ConsIds0, ConsIds),
		delete_unreachable_cases(Cases0, ConsIds, Cases1),
		MaybeConsIds = yes(ConsIds)
	;
		Cases1 = Cases0,
		MaybeConsIds = no
	),
	( Cases1 = [] ->
		% An empty switch always fails.
		fail_goal(Goal - GoalInfo),
		Info = Info0
	; Cases1 = [case(ConsId, SingleGoal0)] ->
		% a singleton switch is equivalent to the goal itself with 
		% a possibly can_fail unification with the functor on the front.
		cons_id_arity(ConsId, Arity),
		(
			SwitchCanFail = can_fail,
			MaybeConsIds \= yes([ConsId])
		->
			simplify__create_test_unification(Var, ConsId, Arity,
				UnifyGoal, Info0, Info1),
			conjoin_goals(UnifyGoal, SingleGoal0, Goal1)
		;
			% The var can only be bound to this cons_id, so
			% a test is unnecessary.
			Goal1 = SingleGoal0,
			Info1 = Info0
		),
		simplify__goal(Goal1, Goal - GoalInfo, Info1, Info)
	;
		GoalInfo = GoalInfo0,
		simplify__switch(Var, Cases1, Cases, [], InstMaps,
			Info0, Info0, Info1),
		simplify_info_create_branch_info(Info0, Info1, InstMaps, Info),
		Goal = switch(Var, SwitchCanFail, Cases, SM)
	).

simplify__goal_2(Goal0, GoalInfo, Goal, GoalInfo, Info0, Info) :-
	Goal0 = higher_order_call(Closure, Args, _, Modes, Det),
	( simplify_do_calls(Info0) ->
		common__optimise_higher_order_call(Closure, Args, Modes, Det,
			Goal0, GoalInfo, Goal, Info0, Info)
	;
		Goal = Goal0,
		Info = Info0
	).

simplify__goal_2(Goal0, GoalInfo, Goal, GoalInfo, Info0, Info) :-
	Goal0 = call(PredId, ProcId, Args, IsBuiltin, _, _),

	%
	% check for calls to predicates with `pragma obsolete' declarations
	%
	(
		simplify_do_warn(Info0),
		simplify_info_get_module_info(Info0, ModuleInfo),
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_get_marker_list(PredInfo, Markers),
		list__member(request(obsolete), Markers)
	->

		goal_info_get_context(GoalInfo, Context1),
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
		% Are the input arguments the same (or equivalent)?
		%
		simplify_info_get_module_info(Info1, ModuleInfo1),
		module_info_pred_proc_info(ModuleInfo1, PredId, ProcId,
			_PredInfo1, ProcInfo1),
		proc_info_headvars(ProcInfo1, HeadVars),
		proc_info_argmodes(ProcInfo1, ArgModes),
		simplify_info_get_common_info(Info1, CommonInfo1),
		simplify__input_args_are_equiv(Args, HeadVars, ArgModes,
			CommonInfo1, ModuleInfo1)
	->
		goal_info_get_context(GoalInfo, Context2),
		simplify_info_add_msg(Info1, warn_infinite_recursion(Context2),
				Info2)
	;
		Info2 = Info1
	),

	%
	% check for duplicate calls to the same procedure
	%
	( simplify_do_calls(Info2) ->
		common__optimise_call(PredId, ProcId, Args, Goal0, GoalInfo,
			Goal, Info2, Info)
	;
		Goal = Goal0,
		Info = Info2
	).

simplify__goal_2(Goal0, GoalInfo0, Goal, GoalInfo, Info0, Info) :-
	Goal0 = unify(LT0, RT0, M, U0, C),
	(
		RT0 = lambda_goal(PredOrFunc, Vars, Modes, LambdaDeclaredDet,
			LambdaGoal0)
	->
		simplify_info_get_common_info(Info0, Common0),
		simplify_info_get_module_info(Info0, ModuleInfo),
		simplify_info_get_instmap(Info0, InstMap0),
		instmap__pre_lambda_update(ModuleInfo, Vars, Modes,
			InstMap0, InstMap1),
		simplify_info_set_instmap(Info0, InstMap1, Info1),

		% Don't attempt to pass structs into lambda_goals,
		% since that could change the curried non-locals of the 
		% lambda_goal, and that would be difficult to fix up.
		common_info_init(Common1),
		simplify_info_set_common_info(Info1, Common1, Info2),

		% Don't attempt to pass structs out of lambda_goals.
		simplify__goal(LambdaGoal0, LambdaGoal, Info2, Info3),
		simplify_info_set_common_info(Info3, Common0, Info4),
		simplify_info_set_instmap(Info4, InstMap0, Info),
		RT = lambda_goal(PredOrFunc, Vars, Modes, LambdaDeclaredDet,
			LambdaGoal),
		Goal = unify(LT0, RT, M, U0, C),
		GoalInfo = GoalInfo0
	;
		simplify_do_common(Info0)
	->
		common__optimise_unification(U0, LT0, RT0, M, C,
			Goal0, GoalInfo0, Goal, GoalInfo, Info0, Info)
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

	goal_info_get_determinism(CondInfo0, CondDetism),
	determinism_components(CondDetism, CondCanFail, CondSolns),
	( CondCanFail = cannot_fail ->
		goal_to_conj_list(Cond0, CondList),
		goal_to_conj_list(Then0, ThenList),
		list__append(CondList, ThenList, List),
		simplify__goal(conj(List) - GoalInfo0, Goal - GoalInfo,
			Info0, Info1),
		goal_info_get_context(GoalInfo, Context),
		simplify_info_add_msg(Info1, ite_cond_cannot_fail(Context),
			Info)
	; CondSolns = at_most_zero ->
		% Optimize away the condition and the `then' part.
		goal_info_get_determinism(CondInfo0, Detism),
		det_negation_det(Detism, MaybeNegDetism),
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
		goal_info_get_context(GoalInfo, Context),
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
		simplify_info_create_branch_info(Info0, Info6,
			[ElseDelta, CondThenDelta], Info),
		Goal = if_then_else(Vars, Cond, Then, Else, SM),
		GoalInfo = GoalInfo0
	).

simplify__goal_2(not(Goal0), GoalInfo, Goal, GoalInfo, Info0, Info) :-
	% Can't use calls or unifications seen within a negation,
	% since non-local variables may not be bound within the negation.
	simplify_info_get_common_info(Info0, Common),
	simplify__goal(Goal0, Goal1, Info0, Info1),
	simplify_info_set_common_info(Info1, Common, Info2),
	Goal1 = _ - GoalInfo1,
	goal_info_get_determinism(GoalInfo1, Detism),
	determinism_components(Detism, CanFail, MaxSoln),
	( CanFail = cannot_fail ->
		goal_info_get_context(GoalInfo, Context),
		simplify_info_add_msg(Info2,
			negated_goal_cannot_fail(Context), Info)
	; MaxSoln = at_most_zero ->
		goal_info_get_context(GoalInfo, Context),
		simplify_info_add_msg(Info2,
			negated_goal_cannot_succeed(Context), Info)
	;
		Info = Info2
	),
	(
		% replace `not true' with `fail'
		Goal1 = conj([]) - _GoalInfo
	->
		map__init(Empty),
		Goal = disj([], Empty)
	;
		% replace `not fail' with `true'
		Goal1 = disj([], _) - _GoalInfo2
	->
		Goal = conj([])
	;
		% remove double negation
		Goal1 = not(SubGoal - _) - _
	->
		Goal = SubGoal
	;
		Goal = not(Goal1)
	).

simplify__goal_2(some(Vars1, Goal1), SomeInfo, Goal, SomeInfo, Info0, Info) :-
	simplify__goal(Goal1, Goal2, Info0, Info),
	simplify__nested_somes(Vars1, Goal2, Vars, Goal3),
	Goal = some(Vars, Goal3).

simplify__goal_2(Goal0, GoalInfo, Goal, GoalInfo, Info0, Info) :-
	Goal0 = pragma_c_code(_, _, PredId, ProcId, Args, _, _, _),
	( simplify_do_calls(Info0) ->
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
	    simplify_info_reset_branch_info(Info0, Info1, BranchInstMaps),
	    simplify__goal(Goal0, Goal1, Info1, Info2),
	    (
		% Flatten conjunctions.
		Goal1 = conj(SubGoals1) - _
	    ->
		simplify_info_undo_goal_updates(Info1, Info2, Info3),
		simplify_info_set_branch_info(Info3, BranchInstMaps, Info4),
		list__append(SubGoals1, Goals0, Goals1),
		simplify__conj(Goals1, RevGoals0, Goals, ConjInfo, Info4, Info)
	    ;
		% Merge branching goals where the branches of the first goal
		% contain extra information about the switched on variable
		% of the second goal.
		simplify__merge_adjacent_switches(Goal1, Goal, RevGoals0,
			RevGoals1, BranchInstMaps, Info2, Info3)
	    ->
		simplify__conj([Goal | Goals0], RevGoals1, Goals,
			ConjInfo, Info3, Info)
	    ;
		% Delete unreachable goals.
		simplify_info_get_instmap(Info2, InstMap1),
		instmap__is_unreachable(InstMap1)
	    ->
		Info = Info2,
		simplify__conjoin_goal_and_rev_goal_list(Goal1,
			RevGoals0, RevGoals),
		list__reverse(RevGoals, Goals)
	    ;
		Goal1 = GoalExpr - _, 
		( goal_util__goal_is_branched(GoalExpr) ->
			Info4 = Info2,
			GoalNeeded = yes,
			Goals1 = Goals0,
			RevGoals1 = RevGoals0
		;
			simplify_info_set_branch_info(Info2, BranchInstMaps,
				Info3),
			simplify__excess_assigns(Goal1, ConjInfo,
				Goals0, Goals1, RevGoals0, RevGoals1,
				GoalNeeded, Info3, Info4)
		),
		( GoalNeeded = yes ->
			simplify__conjoin_goal_and_rev_goal_list(Goal1,
				RevGoals1, RevGoals2)
		;
			RevGoals2 = RevGoals1
		),
		simplify_info_update_instmap(Info4, Goal1, Info5),
		simplify__conj(Goals1, RevGoals2, Goals,
			ConjInfo, Info5, Info)
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

	% Check the post-branch instmaps from the last branching structure in
	% the conjunction to see if there was more information about the
	% switched on variable at the end of each branch than there is at the
	% start of the current switch. If there is, it may be worth merging the
	% goals.
:- pred simplify__merge_adjacent_switches(hlds_goal::in, hlds_goal::out,
		hlds_goals::in, hlds_goals::out, maybe(branch_info)::in,
		simplify_info::in, simplify_info::out) is semidet.

simplify__merge_adjacent_switches(SwitchGoal, Goal, RevGoals0, RevGoals,
		MaybeBranchInfo, Info0, Info) :-
	MaybeBranchInfo = yes(branch_info(RevInstMapDeltas,
		PreGoalCommon, PreGoalInstMap)),
	simplify_do_switch(Info0),
	list__reverse(RevInstMapDeltas, BranchInstMaps),
	BranchInstMaps \= [],
	BranchInstMaps \= [_],
	SwitchGoal = switch(Var, _, Cases2, SM) - _,
	move_follow_code_select(RevGoals0, RevFollowGoals, RevGoals1),
	RevGoals1 = [BranchedGoal | RevGoals],
	BranchedGoal = BranchedGoalExpr - BranchedGoalInfo,
	goal_util__goal_is_branched(BranchedGoalExpr),
	simplify_info_get_instmap(Info0, InstMap),
	simplify__check_branches_for_extra_info(Var, InstMap,
		BranchInstMaps, Cases2, [], RevCaseList),
	list__reverse(RevCaseList, CaseList),
	list__reverse(RevFollowGoals, FollowGoals),
	(
		BranchedGoalExpr = switch(Var1, CanFail1, Cases1, _)
	->
		simplify__merge_switch_into_cases(Cases1,
			FollowGoals, CaseList, Cases),
		GoalExpr = switch(Var1, CanFail1, Cases, SM)
	;
		BranchedGoalExpr = if_then_else(Vars, Cond,
			Then0, Else0, IteSM)
	->
		CaseList = [ThenCase, ElseCase],
		simplify__merge_switch_into_goal(Then0, FollowGoals,
			ThenCase, Then),
		simplify__merge_switch_into_goal(Else0, FollowGoals,
			ElseCase, Else),
		GoalExpr = if_then_else(Vars, Cond, Then, Else, IteSM)
	;
		BranchedGoalExpr = disj(Disjuncts0, DisjSM)
	->
		simplify__merge_switch_into_goals(Disjuncts0,
			FollowGoals, CaseList, Disjuncts),
		GoalExpr = disj(Disjuncts, DisjSM) 
	;
		error("simplify__merge_adjacent_switches")
	),
	list__append(FollowGoals, [SwitchGoal], NewGoals),
	simplify__approximate_goal_info(NewGoals, BranchedGoalInfo, GoalInfo),
	Goal = GoalExpr - GoalInfo,
	simplify_info_set_requantify(Info0, Info1),
	simplify_info_set_common_info(Info1, PreGoalCommon, Info2),
	simplify_info_set_instmap(Info2, PreGoalInstMap, Info3),
	simplify_info_reset_branch_info(Info3, Info, _).

	% This just checks if every case in the second switch either fails
	% or matches only one case given the information in the branches of
	% the first branching goal. Returns the goal for the case that
	% applies for each branch's instmap delta.
:- pred simplify__check_branches_for_extra_info(var::in,
		instmap::in, list(instmap_delta)::in, list(case)::in,
		list(hlds_goal)::in, list(hlds_goal)::out) is semidet.

simplify__check_branches_for_extra_info(_, _, [], _, CaseList, CaseList).
simplify__check_branches_for_extra_info(Var, InstMap,
		[BranchInstMap | BranchInstMaps], Cases, CaseList0, CaseList) :-
	instmap__lookup_var(InstMap, Var, InstMapInst),
	instmap_delta_lookup_var(BranchInstMap, Var, BranchInstMapInst),
	simplify__inst_contains_more_information(BranchInstMapInst,
		InstMapInst, Cases, ThisCase),
	simplify__check_branches_for_extra_info(Var, InstMap,
		BranchInstMaps, Cases, [ThisCase | CaseList0], CaseList).

:- pred simplify__inst_contains_more_information((inst)::in,
		(inst)::in, list(case)::in, hlds_goal::out) is semidet.

simplify__inst_contains_more_information(not_reached, _, _, Goal) :-
	fail_goal(Goal).
simplify__inst_contains_more_information(bound(_, BoundInsts),
		_, Cases0, Goal) :-
	functors_to_cons_ids(BoundInsts, ConsIds0),
	list__sort(ConsIds0, ConsIds),
	delete_unreachable_cases(Cases0, ConsIds, Cases),
	(
		Cases = [],
		fail_goal(Goal)
	;
		Cases = [case(_, Goal)]
	).

:- pred simplify__merge_switch_into_goals(hlds_goals::in, hlds_goals::in,
		list(hlds_goal)::in, hlds_goals::out) is det.

simplify__merge_switch_into_goals([], _, [], []).
simplify__merge_switch_into_goals([], _, [_|_], []) :-
	error("simplify__merge_switch_into_goals").
simplify__merge_switch_into_goals([_|_], _, [], []) :-
	error("simplify__merge_switch_into_goals").
simplify__merge_switch_into_goals([Goal0 | Goals0], Builtins, 
		[SwitchGoal | SwitchGoals], [Goal | Goals]) :-
	simplify__merge_switch_into_goal(Goal0, Builtins, SwitchGoal, Goal),
	simplify__merge_switch_into_goals(Goals0, Builtins, SwitchGoals, Goals).

:- pred simplify__merge_switch_into_cases(list(case)::in, hlds_goals::in,
		list(hlds_goal)::in, list(case)::out) is det.

simplify__merge_switch_into_cases([], _, [], []).
simplify__merge_switch_into_cases([], _, [_|_], []) :-
	error("simplify__merge_switch_into_cases").
simplify__merge_switch_into_cases([_|_], _, [], []) :-
	error("simplify__merge_switch_into_cases").
simplify__merge_switch_into_cases([case(ConsId, Goal0) | Cases0], Builtins, 
		[SwitchGoal | SwitchGoals], [case(ConsId, Goal) | Cases]) :-
	simplify__merge_switch_into_goal(Goal0, Builtins, SwitchGoal, Goal),
	simplify__merge_switch_into_cases(Cases0, Builtins, SwitchGoals, Cases).

:- pred simplify__merge_switch_into_goal(hlds_goal::in, hlds_goals::in,
		hlds_goal::in, hlds_goal::out) is det.

simplify__merge_switch_into_goal(Goal0, Builtins, SwitchGoal, Goal) :-
	conjoin_goal_and_goal_list(Goal0, Builtins, Goal1),
	conjoin_goals(Goal1, SwitchGoal, Goal2),
	Goal2 = GoalExpr - GoalInfo0,
	( GoalExpr = conj(Goals) -> 
		simplify__approximate_goal_info(Goals, GoalInfo0, GoalInfo)
	;
		GoalInfo = GoalInfo0
	),
	Goal = GoalExpr - GoalInfo.

	% Create a conservative goal_info so that simplification can
	% safely be re-run on the resulting goal. A full recomputation over
	% the entire goal is done later.
:- pred simplify__approximate_goal_info(list(hlds_goal)::in,
		hlds_goal_info::in, hlds_goal_info::out) is det.

simplify__approximate_goal_info(NewGoals, GoalInfo0, GoalInfo) :-
	ComputeGoalInfo =
	    lambda([Goal::in, GInfo0::in, GInfo::out] is det, (
		Goal = _ - GInfo1,
		goal_info_get_nonlocals(GInfo0, NonLocals0),
		goal_info_get_instmap_delta(GInfo0, InstMapDelta0),
		goal_info_get_instmap_delta(GInfo1, InstMapDelta1),
		instmap_delta_apply_instmap_delta(InstMapDelta0,
			InstMapDelta1, InstMapDelta),
		goal_info_get_nonlocals(GInfo1, NonLocals1),
		set__union(NonLocals0, NonLocals1, NonLocals),
	    	goal_info_set_instmap_delta(GInfo0, InstMapDelta, GInfo2),
		goal_info_set_nonlocals(GInfo2, NonLocals, GInfo3),
		goal_info_get_determinism(GInfo3, Detism0),
		goal_info_get_determinism(GInfo1, Detism1),
		determinism_components(Detism0, CanFail0, MaxSolns0),
		determinism_components(Detism1, CanFail1, MaxSolns1),
		det_conjunction_maxsoln(MaxSolns0, MaxSolns1, MaxSolns),
		det_conjunction_canfail(CanFail0, CanFail1, CanFail),
		determinism_components(Detism, CanFail, MaxSolns),
	    	goal_info_set_determinism(GInfo3, Detism, GInfo)
	    )),
	list__foldl(ComputeGoalInfo, NewGoals, GoalInfo0, GoalInfo).

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
		simplify_info_reset_branch_info(Info0, Info1, BranchInfo0),
		(
			BranchInfo0 = yes(
				branch_info(InstMapDeltas0,
					Common, PreBranchInstMap0))
		->
			simplify_info_get_instmap(Info1, InstMap0),
			instmap__apply_sub(PreBranchInstMap0, no,
				Subn, PreBranchInstMap),
			instmap__apply_sub(InstMap0, no, Subn, InstMap),
			Lambda = lambda([Delta0::in, Delta::out] is det, (
			    instmap_delta_apply_sub(Delta0, no, Subn, Delta)
			)),
			list__map(Lambda, InstMapDeltas0, InstMapDeltas),
			simplify_info_set_instmap(Info1, InstMap, Info2),
			simplify_info_set_branch_info(Info2,
				yes(branch_info(InstMapDeltas,
				Common, PreBranchInstMap)), Info3)
		;
			Info3 = Info1
		),
		simplify_info_get_varset(Info3, VarSet0),
		varset__delete_var(VarSet0, LocalVar, VarSet),
		simplify_info_set_varset(Info3, VarSet, Info)
	;
		GoalNeeded = yes,
		Goals = Goals0,
		RevGoals = RevGoals0,
		Info = Info0
	).

%-----------------------------------------------------------------------------%

:- pred simplify__switch(var, list(case), list(case), list(instmap_delta),
	list(instmap_delta), simplify_info, simplify_info, simplify_info).
:- mode simplify__switch(in, in, out, in, out, in, in, out) is det.

simplify__switch(_, [], [], InstMaps, InstMaps, _, Info, Info). 
simplify__switch(Var, [Case0 | Cases0], [Case | Cases],
		InstMaps0, InstMaps, Info0, Info1, Info) :-
	simplify_info_get_instmap(Info0, InstMap0),
	Case0 = case(ConsId, Goal0),
	simplify_info_get_module_info(Info1, ModuleInfo0),
	instmap__bind_var_to_functor(Var, ConsId,
		InstMap0, InstMap1, ModuleInfo0, ModuleInfo),
	simplify_info_set_module_info(Info1, ModuleInfo, Info2),
	simplify_info_set_instmap(Info2, InstMap1, Info3),
	simplify__goal(Goal0, Goal, Info3, Info4),
	simplify_info_post_branch_update(Info0, Info4, Info5),
	Case = case(ConsId, Goal),
	Goal0 = _ - GoalInfo,
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	simplify__switch(Var, Cases0, Cases, [InstMapDelta | InstMaps0],
		InstMaps, Info0, Info5, Info).

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
			ArgUniMode = ((ArgInst - ArgInst) -> (free - ArgInst))
		)),
	list__map(InstToUniMode, ArgInsts, UniModes),
	UniMode = (Inst0 -> Inst0) - (Inst0 -> Inst0),
	UnifyContext = unify_context(explicit, []),
	Unification = deconstruct(Var, ConsId,
		ArgVars, UniModes, can_fail),
	ExtraGoal = unify(Var, functor(ConsId, ArgVars),
		UniMode, Unification, UnifyContext),
	goal_info_init(ExtraGoalInfo0),
	set__singleton_set(NonLocals, Var),
	goal_info_set_nonlocals(ExtraGoalInfo0, NonLocals, ExtraGoalInfo1),
	goal_info_set_determinism(ExtraGoalInfo1, semidet, ExtraGoalInfo).

%-----------------------------------------------------------------------------%

:- pred simplify__disj(list(hlds_goal), list(hlds_goal), list(instmap_delta),
	list(instmap_delta), simplify_info, simplify_info, simplify_info).
:- mode simplify__disj(in, out, in, out, in, in, out) is det.

simplify__disj([], [], InstMaps, InstMaps, _, Info, Info).
simplify__disj([Goal0 |Goals0], [Goal | Goals], PostBranchInstMaps0,
		PostBranchInstMaps, Info0, Info1, Info) :-
	simplify__goal(Goal0, Goal, Info1, Info2),
	simplify_info_post_branch_update(Info0, Info2, Info3),
	Goal0 = _ - GoalInfo,
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	simplify__disj(Goals0, Goals, [InstMapDelta | PostBranchInstMaps0],
			PostBranchInstMaps, Info0, Info3, Info4),
	(
		simplify_do_warn(Info4),
		Goal = _ - GoalInfo,
		goal_info_get_determinism(GoalInfo, Detism),
		determinism_components(Detism, _, MaxSolns),
		MaxSolns = at_most_zero
	->
		goal_info_get_context(GoalInfo, Context),
		simplify_info_add_msg(Info4, zero_soln_disjunct(Context),
			Info)
	;
		Info = Info4
	).

	% Disjunctions that cannot succeed more than once when viewed from the
	% outside generally need some fixing up, and/or some warnings to be
	% issued.

	% Currently we just convert them all to if-then-elses.

	% XXX converting disjs that have output variables but that
	% nevertheless cannot succeed more than one
	% (e.g. cc_nondet or cc_multi disjs) into if-then-elses
	% may cause problems with other parts of the compiler that
	% assume that an if-then-else is mode-correct, i.e. that
	% the condition doesn't bind variables.

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

		goal_info_init(ThenGoalInfo0),
		instmap_delta_init_reachable(InstMap1),
		goal_info_set_instmap_delta(ThenGoalInfo0, InstMap1,
			ThenGoalInfo1),
		goal_info_set_determinism(ThenGoalInfo1, det, ThenGoalInfo),
		Then = conj([]) - ThenGoalInfo,

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
			simplify,	% How much simplification to do.
			common_info,	% Info about common subexpressions.
			instmap,
			varset,
			map(var, type),
			bool,		% Does the goal need requantification.
			bool,		% Does mode analysis need rerunning
					% rather than recompute_instmap_delta.
			maybe(branch_info)	% Final instmaps at the end
					% of each branch of the last 
					% branching goal
		).

	% info used to merge adjacent switches and prepare for rerunning
	% simplification on the resulting goal.
:- type branch_info
	--->	branch_info(
			list(instmap_delta),	% instmap_delta for each branch
			common_info,		% from before goal
			instmap			% from before goal
		).

simplify_info_init(DetInfo, Simplify, InstMap, VarSet, VarTypes, Info) :-
	common_info_init(CommonInfo),
	set__init(Msgs),
	Info = simplify_info(DetInfo, Msgs, Simplify, CommonInfo,
			InstMap, VarSet, VarTypes, no, no, no). 

	% exported for common.m
:- interface.

:- pred simplify_info_get_det_info(simplify_info::in, det_info::out) is det.
:- pred simplify_info_get_msgs(simplify_info::in, set(det_msg)::out) is det.
:- pred simplify_info_get_instmap(simplify_info::in, instmap::out) is det.
:- pred simplify_info_get_simplify(simplify_info::in, simplify::out) is det.
:- pred simplify_info_get_common_info(simplify_info::in,
		common_info::out) is det.
:- pred simplify_info_get_varset(simplify_info::in, varset::out) is det.
:- pred simplify_info_get_var_types(simplify_info::in,
		map(var, type)::out) is det.
:- pred simplify_info_requantify(simplify_info::in) is semidet.
:- pred simplify_info_recompute_atomic(simplify_info::in) is semidet.
:- pred simplify_info_get_branch_info(simplify_info::in,
		maybe(branch_info)::out) is det.

:- pred simplify_info_get_module_info(simplify_info::in,
		module_info::out) is det.

:- implementation.

simplify_info_get_det_info(simplify_info(Det, _,_,_,_,_,_,_,_,_), Det). 
simplify_info_get_msgs(simplify_info(_, Msgs, _,_,_,_,_,_,_,_), Msgs).
simplify_info_get_simplify(simplify_info(_,_,Simplify,_,_,_,_,_,_,_),
	Simplify). 
simplify_info_get_common_info(simplify_info(_,_,_,Common, _,_,_,_,_,_),
	Common).
simplify_info_get_instmap(simplify_info(_,_,_,_, InstMap,_,_,_,_,_), InstMap). 
simplify_info_get_varset(simplify_info(_,_,_,_,_, VarSet, _,_,_,_), VarSet). 
simplify_info_get_var_types(simplify_info(_,_,_,_,_,_, VarTypes, _,_,_),
	VarTypes). 
simplify_info_requantify(simplify_info(_,_,_,_,_,_,_, yes, _,_)).
simplify_info_recompute_atomic(simplify_info(_,_,_,_,_,_,_,_, yes,_)).
simplify_info_get_branch_info(simplify_info(_,_,_,_,_,_,_,_,_, BranchInfo),
	BranchInfo).

simplify_info_get_module_info(Info, ModuleInfo) :-
	simplify_info_get_det_info(Info, DetInfo),
	det_info_get_module_info(DetInfo, ModuleInfo).

:- interface.

:- type branch_info.

:- pred simplify_info_set_det_info(simplify_info::in,
		det_info::in, simplify_info::out) is det.
:- pred simplify_info_set_msgs(simplify_info::in,
		set(det_msg)::in, simplify_info::out) is det.
:- pred simplify_info_set_simplify(simplify_info::in,
		simplify::in, simplify_info::out) is det.
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
:- pred simplify_info_reset_branch_info(simplify_info::in, simplify_info::out,
		maybe(branch_info)::out) is det.
:- pred simplify_info_set_branch_info(simplify_info::in,
		maybe(branch_info)::in, simplify_info::out) is det.
:- pred simplify_info_add_msg(simplify_info::in, det_msg::in,
		simplify_info::out) is det.

:- pred simplify_info_set_module_info(simplify_info::in,
		module_info::in, simplify_info::out) is det.

:- implementation.

simplify_info_set_det_info(simplify_info(_, B, C, D, E, F, G, H, I,J), Det,
		simplify_info(Det, B, C, D, E, F, G, H, I,J)).
simplify_info_set_msgs(simplify_info(A, _, C, D, E, F, G, H, I,J), Msgs,
		simplify_info(A, Msgs, C, D, E, F, G, H, I, J)). 
simplify_info_set_simplify(simplify_info(A, B, _, D, E, F, G, H, I,J), Simp,
		simplify_info(A, B, Simp, D, E, F, G, H, I,J)).
simplify_info_set_instmap(simplify_info(A, B, C, D, _, F, G, H, I, J), InstMap,
		simplify_info(A, B, C, D, InstMap, F, G, H, I, J)). 
simplify_info_set_common_info(simplify_info(A, B, C, _, E, F, G, H, I, J),
		Common, simplify_info(A, B, C, Common, E, F, G, H, I, J)). 
simplify_info_set_varset(simplify_info(A, B, C, D, E, _, G, H, I, J), VarSet,
		simplify_info(A, B, C, D, E, VarSet, G, H, I, J)). 
simplify_info_set_var_types(simplify_info(A, B, C, D, E, F, _, H, I, J),
		VarTypes, simplify_info(A, B, C, D, E, F, VarTypes, H, I, J)). 
simplify_info_set_requantify(simplify_info(A, B, C, D, E, F, G, _, I, J),
		simplify_info(A, B, C, D, E, F, G, yes, I, J)). 
simplify_info_set_recompute_atomic(simplify_info(A, B, C, D, E, F, G, H, _, J),
		simplify_info(A, B, C, D, E, F, G, H, yes, J)). 
simplify_info_reset_branch_info(simplify_info(A, B, C, D, E, F, G, H, I, Info),
		simplify_info(A, B, C, D, E, F, G, H, I, no), Info). 
simplify_info_set_branch_info(simplify_info(A, B, C, D, E, F, G, H, I, _),
		Info, simplify_info(A, B, C, D, E, F, G, H, I, Info)). 

simplify_info_add_msg(Info0, Msg, Info) :-
	( simplify_do_warn(Info0) ->
		simplify_info_get_msgs(Info0, Msgs0),
		set__insert(Msgs0, Msg, Msgs),
		simplify_info_set_msgs(Info0, Msgs, Info)
	;
		Info = Info0
	).

simplify_info_set_module_info(Info0, ModuleInfo, Info) :-
	simplify_info_get_det_info(Info0, DetInfo0),
	det_info_set_module_info(DetInfo0, ModuleInfo, DetInfo),
	simplify_info_set_det_info(Info0, DetInfo, Info).

:- interface.

:- pred simplify_do_warn(simplify_info::in) is semidet.
:- pred simplify_do_warn_calls(simplify_info::in) is semidet.
:- pred simplify_do_once(simplify_info::in) is semidet.
:- pred simplify_do_switch(simplify_info::in) is semidet.
:- pred simplify_do_common(simplify_info::in) is semidet.
:- pred simplify_do_excess_assigns(simplify_info::in) is semidet.
:- pred simplify_do_calls(simplify_info::in) is semidet.

:- implementation.

simplify_do_warn(Info) :-
	simplify_info_get_simplify(Info, Simplify),
	Simplify = simplify(yes, _, _, _, _, _, _).
simplify_do_warn_calls(Info) :-
	simplify_info_get_simplify(Info, Simplify),
	Simplify = simplify(_, yes, _, _, _, _, _).
simplify_do_once(Info) :-
	simplify_info_get_simplify(Info, Simplify),
	Simplify = simplify(_, _, yes, _, _, _, _).
simplify_do_switch(Info) :-
	simplify_info_get_simplify(Info, Simplify),
	Simplify = simplify(_, _, _, yes, _, _, _).
simplify_do_common(Info) :-
	simplify_info_get_simplify(Info, Simplify), 
	Simplify = simplify(_, _, _, _, yes, _, _).
simplify_do_excess_assigns(Info) :-
	simplify_info_get_simplify(Info, Simplify),
	Simplify = simplify(_, _, _, _, _, yes, _).
simplify_do_calls(Info) :-
	simplify_info_get_simplify(Info, Simplify),
	Simplify = simplify(_, _, _, _, _, _, yes).

:- pred simplify_info_update_instmap(simplify_info::in, hlds_goal::in,
		simplify_info::out) is det.

simplify_info_update_instmap(
		simplify_info(A, B, C, D, InstMap0, F, G, H, I, J), Goal,
		simplify_info(A, B, C, D, InstMap, F, G, H, I, J)) :-
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
:- pred simplify_info_maybe_clear_structs(before_after::in, hlds_goal::in,
		simplify_info::in, simplify_info::out) is det.

simplify_info_maybe_clear_structs(BeforeAfter, Goal, Info0, Info) :-
	( code_util__cannot_stack_flush(Goal) ->
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
			GoalExpr \= higher_order_call(_, _, _, _, _),
			GoalExpr \= pragma_c_code(_, _, _, _, _, _, _, _)
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
	
:- pred simplify_info_create_branch_info(simplify_info::in, simplify_info::in,
		list(instmap_delta)::in, simplify_info::out) is det.

simplify_info_create_branch_info(Info0, Info1, InstMapDeltas, Info) :-
	simplify_info_get_common_info(Info0, Common),
	simplify_info_get_instmap(Info0, InstMap),
	BranchInfo = yes(branch_info(InstMapDeltas, Common, InstMap)),
	simplify_info_set_branch_info(Info1, BranchInfo, Info).

	% Undo updates to the simplify_info before redoing 
	% simplification on a goal. 
:- pred simplify_info_undo_goal_updates(simplify_info::in, simplify_info::in,
		simplify_info::out) is det.

simplify_info_undo_goal_updates(Info1, Info2, Info) :-
	simplify_info_get_common_info(Info1, CommonInfo0),
	simplify_info_set_common_info(Info2, CommonInfo0, Info3),
	simplify_info_get_branch_info(Info1, BranchInfo),
	simplify_info_set_branch_info(Info3, BranchInfo, Info4),
	simplify_info_get_instmap(Info1, InstMap),
	simplify_info_set_instmap(Info4, InstMap, Info).

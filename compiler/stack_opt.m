%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File stack_opt.
%
% Author: zs.
%
% The input to this module is a HLDS structure with annotations on three kinds
% of goals:
%
% - calls, including generic calls and foreign_proc goals which may
%   call back to Mercury, should have need_across_call annotations;
%
% - goals that have resume points before them (the conditions of if-then-elses
%   and the non-last disjuncts of disjunction) should have need_in_resume
%   annotations on them, provided that the resume point has a label that
%   expects its variables to be on the stack;
%
% - parallel conjunctions should have need_in_par_conj annotations.
%
% The code in this module puts stack_save_map annotations on goals that have
% need_across_call annotations, on if-then-else goals whose condition has a
% need_in_resume annotation, and on disjunction goals whose first disjunct has
% a need_in_resume annotation. The stack_save map annotation tells the
% code generator which of the relevant variables need to be saved in their own
% stack slots, and which can be accessed through other variables on the stack.
%
% The code in this module processes procedures one by one. It makes two passes
% over each procedure.
%
% The first pass traverses the procedure body backward, building a graph
% structure as it goes along. The nodes of the graphs are *anchors*. Points
% at which stack flushes may be required are anchors, and so are the beginnings
% and ends of branched control structures and of the procedure body itself.
% The graph associates with the edge between two anchors the set of variables
% accessed by the program fragment between those two anchors.
%
% When the traversal reaches a deconstruction unification, we sweep forward
% over the graph. During this sweep, we build a set of *paths*, with the
% intention that this set should contain an element for each path that control
% can take from the starting unification to the end of the procedure body.
% Each path is a sequence of *intervals*. An interval starts either at the
% starting unification or at a stack flush point; it ends at a stack flush
% point or the end of the procedure body. An interval is associated with one
% or more edges in the graph; the first of these associated edges will not
% have a left anchor yet.
%
% We give each path to the matching algorithm one by one. The matching
% algorithm finds out which set of variables should be accessed via
% the cell variable on that path. Since the decisions made for different
% paths are not independent, we have to apply a fixpoint iteration until
% we get a consistent set of answers.
%
% The first pass (whose main predicate is optimize_live_sets_in_goal) records
% its results in the var_save_info field of the opt_info data structure it
% passes around. This field then becomes the main input to the second pass
% (whose main predicate is record_decisions_in_goal), which performs the
% source-to-source transformation that makes each segment access via the cell
% variable the field variables that have been selected to be so accessed
% by the first pass.
%
% The principles of this optimization are documented in the paper "Using the
% heap to eliminate stack accesses" by Zoltan Somogyi and Peter Stuckey.
%
%-----------------------------------------------------------------------------%

:- module ll_backend__stack_opt.

:- interface.

:- import_module hlds__hlds_module, hlds__hlds_pred.
:- import_module io.

:- pred stack_opt_cell(pred_id::in, proc_id::in, proc_info::in, proc_info::out,
	module_info::in, module_info::out, io__state::di, io__state::uo)
	is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__prog_data, parse_tree__mercury_to_mercury.
:- import_module hlds__hlds_data, hlds__hlds_goal, hlds__hlds_llds.
:- import_module hlds__quantification, hlds__instmap.
:- import_module hlds__goal_util, hlds__hlds_out.
:- import_module check_hlds__type_util, check_hlds__mode_util.
:- import_module check_hlds__inst_match, check_hlds__goal_path.
:- import_module ll_backend__arg_info, ll_backend__call_gen.
:- import_module ll_backend__liveness, ll_backend__live_vars.
:- import_module ll_backend__store_alloc.
:- import_module backend_libs__code_model.
:- import_module backend_libs__matching.
:- import_module libs__trace_params, libs__globals, libs__options.

:- import_module counter, bool, int, list, assoc_list.
:- import_module map, set, std_util, require, term, varset.

% The opt_stack_alloc structure is constructed by live_vars.m. It contains
% the set of vars that definitely need their own stack slots, and which this
% optimization should not try to make reachable from a heap cell. At the
% moment, the only variables we treat this way are those that are required to
% be on the stack by a parallel conjunction.

:- type opt_stack_alloc --->
	opt_stack_alloc(
		par_conj_own_slots	:: set(prog_var)
	).

:- type save_point_type
	--->	call_site
	;	resume_point.

:- type save_point --->
	save_point(
		save_point_type,
		goal_path
	).

:- type branch_construct
	--->	ite
	;	disj
	;	switch
	;	neg
	;	par_conj.

:- type resume_save_status
	--->	has_resume_save
	;	has_no_resume_save.

:- type anchor
	--->	proc_start
	;	proc_end
	;	branch_start(branch_construct, goal_path)
	;	cond_then(goal_path)
	;	branch_end(branch_construct, goal_path)
	;	call_site(goal_path).

:- type interval_id	--->	interval_id(int).

:- type branch_end_info --->
	branch_end_info(
		flushed_after_branch	:: set(prog_var),
		accessed_after_branch	:: set(prog_var),
		interval_after_branch	:: interval_id
	).

:- type insert_spec --->
	insert_spec(
		hlds_goal,
		set(prog_var)
	).

:- type insert_map		==	map(anchor, list(insert_spec)).

:- type anchor_follow_info	==	pair(set(prog_var), set(interval_id)).

:- type opt_params --->
	opt_params(
		module_info		:: module_info,
		var_types		:: vartypes,
		matching_params		:: matching_params,
		all_path_node_ratio	:: int,
		fixpoint_loop		:: bool,
		full_path		:: bool,
		on_stack		:: bool,
		opt_at_most_zero_calls	:: bool,
		non_candidate_vars	:: set(prog_var)
	).

:- type matching_result --->
	matching_result(
		prog_var,
		cons_id,
		list(prog_var),
		set(prog_var),
		goal_path,
		set(interval_id),
		set(interval_id),
		set(anchor),
		set(anchor)
	).

:- type opt_info --->
	opt_info(
		opt_params		:: opt_params,
		flushed_later		:: set(prog_var),
		accessed_later		:: set(prog_var),
		branch_resume_map	:: map(goal_path, resume_save_status),
		branch_end_map		:: map(goal_path, branch_end_info),
		cond_end_map		:: map(goal_path, interval_id),
		cur_interval		:: interval_id,
		interval_counter	:: counter,
		open_intervals		:: set(interval_id),
		anchor_follow_map	:: map(anchor, anchor_follow_info),
		model_non_anchors	:: set(anchor),
		left_anchor_inserts	:: insert_map,
		interval_start		:: map(interval_id, anchor),
		interval_end		:: map(interval_id, anchor),
		interval_succ		:: map(interval_id, list(interval_id)),
		interval_vars		:: map(interval_id, set(prog_var)),
		interval_delvars	:: map(interval_id,
						list(set(prog_var))),
		matching_results	:: list(matching_result)
	).

:- type maybe_needs_flush
	--->	needs_flush
	;	doesnt_need_flush.

stack_opt_cell(PredId, ProcId, ProcInfo0, ProcInfo, ModuleInfo0, ModuleInfo,
		IO0, IO) :-
	detect_liveness_proc(PredId, ProcId, ModuleInfo0, ProcInfo0, ProcInfo1,
		IO0, IO1),
	initial_liveness(ProcInfo1, PredId, ModuleInfo0, Liveness0),
	module_info_globals(ModuleInfo0, Globals),
	module_info_pred_info(ModuleInfo0, PredId, PredInfo),
	body_should_use_typeinfo_liveness(PredInfo, Globals, TypeInfoLiveness),
	globals__lookup_bool_option(Globals, opt_no_return_calls,
		OptNoReturnCalls),
	AllocData = alloc_data(ModuleInfo0, ProcInfo1, TypeInfoLiveness,
		OptNoReturnCalls),
	goal_path__fill_slots(ProcInfo1, ModuleInfo0, ProcInfo2),
	proc_info_goal(ProcInfo2, Goal2),
	OptStackAlloc0 = init_opt_stack_alloc,
	set__init(FailVars),
	set__init(NondetLiveness0),
	build_live_sets_in_goal(Goal2, OptStackAlloc0,
		Liveness0, NondetLiveness0, FailVars, AllocData,
		Goal, OptStackAlloc, _Liveness, _NondetLiveness),
	proc_info_set_goal(ProcInfo2, Goal, ProcInfo3),
	allocate_store_maps(for_stack_opt, ProcInfo3, PredId, ModuleInfo0,
		ProcInfo4),
	globals__lookup_int_option(Globals, debug_stack_opt, DebugStackOpt),
	pred_id_to_int(PredId, PredIdInt),
	maybe_write_progress_message("\nbefore stack opt cell",
		DebugStackOpt, PredIdInt, ProcInfo4, ModuleInfo0, IO1, IO2),
	optimize_live_sets(ModuleInfo0, ProcInfo4, OptStackAlloc, ProcInfo5,
		Changed, DebugStackOpt, PredIdInt, IO2, IO3),
	(
		Changed = yes,
		maybe_write_progress_message(
			"\nafter stack opt transformation",
			DebugStackOpt, PredIdInt, ProcInfo5, ModuleInfo0,
			IO3, IO4),
		requantify_proc(ProcInfo5, ProcInfo6),
		maybe_write_progress_message(
			"\nafter stack opt requantify",
			DebugStackOpt, PredIdInt, ProcInfo6, ModuleInfo0,
			IO4, IO5),
		recompute_instmap_delta_proc(yes, ProcInfo6, ProcInfo,
			ModuleInfo0, ModuleInfo),
		maybe_write_progress_message(
			"\nafter stack opt recompute instmaps",
			DebugStackOpt, PredIdInt, ProcInfo, ModuleInfo,
			IO5, IO)
	;
		Changed = no,
		ProcInfo = ProcInfo0,
		ModuleInfo = ModuleInfo0,
		IO = IO3
	).

:- func init_opt_stack_alloc = opt_stack_alloc.

init_opt_stack_alloc = opt_stack_alloc(set__init).

:- pred optimize_live_sets(module_info::in, proc_info::in, opt_stack_alloc::in,
	proc_info::out, bool::out, int::in, int::in,
	io__state::di, io__state::uo) is det.

optimize_live_sets(ModuleInfo, ProcInfo0, OptAlloc, ProcInfo, Changed,
		DebugStackOpt, PredIdInt, IO0, IO) :-
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	proc_info_varset(ProcInfo0, VarSet0),
	OptAlloc = opt_stack_alloc(ParConjOwnSlot),
	arg_info__partition_proc_args(ProcInfo0, ModuleInfo,
		InputArgs, OutputArgs, UnusedArgs),
	HeadVars = set__union_list([InputArgs, OutputArgs, UnusedArgs]),
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals,
		optimize_saved_vars_cell_candidate_headvars, CandHeadvars),
	(
		CandHeadvars = no,
		set__union(HeadVars, ParConjOwnSlot, NonCandidateVars)
	;
		CandHeadvars = yes,
		NonCandidateVars = ParConjOwnSlot
	),
	Counter0 = counter__init(1),
	counter__allocate(CurInterval, Counter0, Counter1),
	CurIntervalId = interval_id(CurInterval),
	EndMap0 = map__det_insert(map__init, CurIntervalId, proc_end),
	InsertMap0 = map__init,
	StartMap0 = map__init,
	SuccMap0 = map__det_insert(map__init, CurIntervalId, []),
	VarsMap0 = map__det_insert(map__init, CurIntervalId, OutputArgs),
	globals__lookup_int_option(Globals,
		optimize_saved_vars_cell_cv_store_cost, CellVarStoreCost),
	globals__lookup_int_option(Globals,
		optimize_saved_vars_cell_cv_load_cost, CellVarLoadCost),
	globals__lookup_int_option(Globals,
		optimize_saved_vars_cell_fv_store_cost, FieldVarStoreCost),
	globals__lookup_int_option(Globals,
		optimize_saved_vars_cell_fv_load_cost, FieldVarLoadCost),
	globals__lookup_int_option(Globals,
		optimize_saved_vars_cell_op_ratio, OpRatio),
	globals__lookup_int_option(Globals,
		optimize_saved_vars_cell_node_ratio, NodeRatio),
	globals__lookup_bool_option(Globals,
		optimize_saved_vars_cell_include_all_candidates, InclAllCand),
	MatchingParams = matching_params(CellVarStoreCost, CellVarLoadCost,
		FieldVarStoreCost, FieldVarLoadCost, OpRatio, NodeRatio,
		InclAllCand),
	globals__lookup_int_option(Globals,
		optimize_saved_vars_cell_all_path_node_ratio,
		AllPathNodeRatio),
	globals__lookup_bool_option(Globals,
		optimize_saved_vars_cell_loop, FixpointLoop),
	globals__lookup_bool_option(Globals,
		optimize_saved_vars_cell_full_path, FullPath),
	globals__lookup_bool_option(Globals,
		optimize_saved_vars_cell_on_stack, OnStack),
	globals__get_trace_level(Globals, TraceLevel),
	OptNoReturnCalls = trace_level_is_none(TraceLevel),
	OptParams = opt_params(ModuleInfo, VarTypes0, MatchingParams,
		AllPathNodeRatio, FixpointLoop, FullPath, OnStack,
		OptNoReturnCalls, NonCandidateVars),
	OptInfo0 = opt_info(OptParams, set__init, OutputArgs, map__init,
		map__init, map__init, CurIntervalId, Counter1,
		set__make_singleton_set(CurIntervalId),
		map__init, set__init, InsertMap0, StartMap0, EndMap0,
		SuccMap0, VarsMap0, map__init, []),
	optimize_live_sets_in_goal(Goal0, OptInfo0, OptInfo),
	( DebugStackOpt = PredIdInt ->
		dump_opt_info(OptInfo, IO0, IO)
	;
		IO = IO0
	),
	InsertMap = OptInfo ^ left_anchor_inserts,
	( map__is_empty(InsertMap) ->
		ProcInfo = ProcInfo0,
		Changed = no
	;
		VarInfo0 = var_info(VarSet0, VarTypes0),
		record_decisions_in_goal(Goal0, Goal1, VarInfo0, VarInfo,
			map__init, RenameMap, InsertMap),
		apply_headvar_correction(HeadVars, RenameMap, Goal1, Goal),
		VarInfo = var_info(VarSet, VarTypes),
		proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
		proc_info_set_varset(ProcInfo1, VarSet, ProcInfo2),
		proc_info_set_vartypes(ProcInfo2, VarTypes, ProcInfo),
		Changed = yes
	).

%-----------------------------------------------------------------------------%

:- pred optimize_live_sets_in_goal(hlds_goal::in,
	opt_info::in, opt_info::out) is det.

optimize_live_sets_in_goal(conj(Goals) - _GoalInfo) -->
	optimize_live_sets_in_conj(Goals).

optimize_live_sets_in_goal(par_conj(Goals) - _GoalInfo) -->
	optimize_live_sets_in_par_conj(Goals).

optimize_live_sets_in_goal(disj(Goals) - GoalInfo) -->
	( { Goals = [FirstDisjunct | _] } ->
		reached_branch_end(GoalInfo, yes(FirstDisjunct), disj,
			StartAnchor, EndAnchor, BeforeId, AfterId,
			MaybeResumeVars),
		optimize_live_sets_in_disj(Goals, doesnt_need_flush,
			StartAnchor, EndAnchor, BeforeId, AfterId,
			OpenIntervals),
		leave_branch_start(disj, StartAnchor, BeforeId,
			MaybeResumeVars, OpenIntervals)
	;
		% We could reset the set of variables in the current interval
		% to the empty set, since any variable accesses after a fail
		% goal (which is what an empty disjunction represent) will not
		% be executed at runtime. However, simplify should have removed
		% any goals in the current branch from after the fail, so the
		% set of variables in the current interval will already be
		% the empty set.
		no_open_intervals
	).

optimize_live_sets_in_goal(switch(Var, _Det, Cases) - GoalInfo) -->
	reached_branch_end(GoalInfo, no, switch,
		StartAnchor, EndAnchor, BeforeId, AfterId, MaybeResumeVars),
	optimize_live_sets_in_cases(Cases, StartAnchor, EndAnchor,
		BeforeId, AfterId, OpenIntervalsList),
	{ OpenIntervals = set__union_list(OpenIntervalsList) },
	leave_branch_start(switch, StartAnchor, BeforeId, MaybeResumeVars,
		OpenIntervals),
	require_in_regs([Var]),
	require_access([Var]).

optimize_live_sets_in_goal(not(Goal) - GoalInfo) -->
	reached_branch_end(GoalInfo, yes(Goal), neg,
		StartAnchor, EndAnchor, BeforeId, AfterId, MaybeResumeVars),
	enter_branch_tail(EndAnchor, AfterId),
	optimize_live_sets_in_goal(Goal),
	reached_branch_start(needs_flush, StartAnchor, BeforeId,
		OpenIntervals),
	leave_branch_start(neg, StartAnchor, BeforeId, MaybeResumeVars,
		OpenIntervals).

optimize_live_sets_in_goal(if_then_else(_, Cond, Then, Else) - GoalInfo) -->
	reached_branch_end(GoalInfo, yes(Cond), ite, StartAnchor, EndAnchor,
		BeforeId, AfterId, MaybeResumeVars),
	enter_branch_tail(EndAnchor, AfterId),
	optimize_live_sets_in_goal(Then),
	reached_cond_then(GoalInfo),
	optimize_live_sets_in_goal(Cond),
	reached_branch_start(doesnt_need_flush, StartAnchor, BeforeId,
		CondOpenIntervals), 
	enter_branch_tail(EndAnchor, AfterId),
	optimize_live_sets_in_goal(Else),
	reached_branch_start(needs_flush, StartAnchor, BeforeId,
		_ElseOpenIntervals),
	leave_branch_start(ite, StartAnchor, BeforeId, MaybeResumeVars,
		CondOpenIntervals).

optimize_live_sets_in_goal(some(_Vars, _CanRemove, Goal) - _GoalInfo) -->
	optimize_live_sets_in_goal(Goal).

optimize_live_sets_in_goal(Goal - GoalInfo) -->
	OptParams =^ opt_params,
	{ Goal = generic_call(GenericCall, ArgVars0, ArgModes0, Detism) },
	{ goal_info_get_maybe_need_across_call(GoalInfo, MaybeNeedAcrossCall) },
	{ VarTypes = OptParams ^ var_types },
	{ list__map(map__lookup(VarTypes), ArgVars0, ArgTypes0) },
	{ call_gen__maybe_remove_aditi_state_args(GenericCall,
		ArgVars0, ArgTypes0, ArgModes0,
		ArgVars, ArgTypes, ArgModes) },
	{ ModuleInfo = OptParams ^ module_info },
	{ arg_info__compute_in_and_out_vars(ModuleInfo, ArgVars,
		ArgModes, ArgTypes, InputArgs, _OutputArgs) },
	{ determinism_to_code_model(Detism, CodeModel) },
	{ call_gen__generic_call_info(CodeModel, GenericCall, _,
		GenericVarsArgInfos, _) },
	{ assoc_list__keys(GenericVarsArgInfos, GenericVars) },
	{ list__append(GenericVars, InputArgs, Inputs) },
	optimize_live_sets_at_call(Inputs, MaybeNeedAcrossCall, GoalInfo).

optimize_live_sets_in_goal(Goal - GoalInfo) -->
	{ Goal = call(PredId, ProcId, ArgVars, Builtin, _, _) },
	OptParams =^ opt_params,
	{ ModuleInfo = OptParams ^ module_info },
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		_PredInfo, ProcInfo) },
	{ VarTypes = OptParams ^ var_types },
	{ arg_info__partition_proc_call_args(ProcInfo, VarTypes,
		ModuleInfo, ArgVars, InputArgs, _, _) },
	{ set__to_sorted_list(InputArgs, Inputs) },
	( { Builtin = inline_builtin } ->
		require_in_regs(Inputs),
		require_access(Inputs)
	;
		{ goal_info_get_maybe_need_across_call(GoalInfo,
			MaybeNeedAcrossCall) },
		optimize_live_sets_at_call(Inputs, MaybeNeedAcrossCall,
			GoalInfo)
	).

optimize_live_sets_in_goal(Goal - GoalInfo) -->
	{ Goal = foreign_proc(_Attributes, PredId, ProcId,
		ArgVars, _ArgNames, _OrigArgTypes, _PragmaCode) },
	OptParams =^ opt_params,
	{ ModuleInfo = OptParams ^ module_info },
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		_PredInfo, ProcInfo) },
	{ VarTypes = OptParams ^ var_types },
	{ arg_info__partition_proc_call_args(ProcInfo, VarTypes,
		ModuleInfo, ArgVars, InputArgs, _, _) },
	{ set__to_sorted_list(InputArgs, Inputs) },
	(
		{ goal_info_maybe_get_maybe_need_across_call(GoalInfo,
			MaybeNeedAcrossCall) },
		{ MaybeNeedAcrossCall = yes(_) }
	->
		optimize_live_sets_at_call(Inputs, MaybeNeedAcrossCall,
			GoalInfo)
	;
		require_in_regs(Inputs),
		require_access(Inputs)
	).

optimize_live_sets_in_goal(Goal - GoalInfo) -->
	{ Goal = unify(_, _, _, Unification, _) },
	(
		{ Unification = construct(CellVar, _ConsId, ArgVars, _,
			HowToConstruct, _, _) },
		{ HowToConstruct = reuse_cell(_) ->
			error("optimize_live_sets_in_goal: reuse")
		;
			true
		},
		require_in_regs(ArgVars),
		require_access([CellVar | ArgVars])
		% use_cell(CellVar, ArgVars, ConsId, Goal - GoalInfo)
		% We cannot use such cells, because some of the ArgVars
		% may need to be saved on the stack before this construction.
	;
		{ Unification = deconstruct(CellVar, ConsId, ArgVars,
			ArgModes, _, _) },
		OptParams =^ opt_params,
		{ ModuleInfo = OptParams ^ module_info },
		( { shared_left_to_right_deconstruct(ModuleInfo, ArgModes) } ->
			use_cell(CellVar, ArgVars, ConsId, Goal - GoalInfo)
		;
			[]
		),
		require_in_regs([CellVar]),
		require_access([CellVar | ArgVars])
	;
		{ Unification = assign(ToVar, FromVar) },
		require_in_regs([FromVar]),
		require_access([FromVar, ToVar])
	;
		{ Unification = simple_test(Var1, Var2) },
		require_in_regs([Var1, Var2]),
		require_access([Var1, Var2])
	;
		{ Unification = complicated_unify(_, _, _) },
		{ error("optimize_live_sets_in_goal: complicated_unify") }
	).

optimize_live_sets_in_goal(shorthand(_) - _) -->
	{ error("shorthand in optimize_live_sets_in_goal") }.

:- pred shared_left_to_right_deconstruct(module_info::in, list(uni_mode)::in)
	is semidet.

shared_left_to_right_deconstruct(_, []).
shared_left_to_right_deconstruct(ModuleInfo, [ArgMode | ArgsModes]) :-
	ArgMode = ((InitCell - InitArg) -> (FinalCell - FinalArg)),
	mode_is_fully_input(ModuleInfo, InitCell -> FinalCell),
	mode_is_output(ModuleInfo, InitArg -> FinalArg),
	inst_is_not_partly_unique(ModuleInfo, FinalCell),
	inst_is_not_partly_unique(ModuleInfo, FinalArg),
	shared_left_to_right_deconstruct(ModuleInfo, ArgsModes).

%-----------------------------------------------------------------------------%

:- pred optimize_live_sets_at_call(list(prog_var)::in,
	maybe(need_across_call)::in, hlds_goal_info::in,
	opt_info::in, opt_info::out) is det.

optimize_live_sets_at_call(Inputs, MaybeNeedAcrossCall, GoalInfo) -->
	(
		{ MaybeNeedAcrossCall = yes(NeedAcrossCall) },
		{ NeedAcrossCall = need_across_call(ForwardVars,
			ResumeVars, NondetLiveVars) },
		{ VarsOnStack0 = set__union_list([ForwardVars, ResumeVars,
			NondetLiveVars]) },
		{ goal_info_get_goal_path(GoalInfo, GoalPath) },
		{ CallAnchor = call_site(GoalPath) },
		get_cur_interval(AfterCallId),
		new_interval_id(BeforeCallId),
		record_interval_start(AfterCallId, CallAnchor),
		record_interval_end(BeforeCallId, CallAnchor),
		{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
		OptParams =^ opt_params,
		(
			{ instmap_delta_is_reachable(InstMapDelta)
			; OptParams ^ opt_at_most_zero_calls = no
			}
		->
			record_interval_succ(BeforeCallId, AfterCallId),
			{ VarsOnStack = VarsOnStack0 }
		;
			% If the call cannot succeed, then execution cannot
			% get from BeforeCallId to AfterCallId.
			record_interval_no_succ(BeforeCallId),
			{ VarsOnStack = set__init }
		),
		set_cur_interval(BeforeCallId),
		assign_open_intervals_to_anchor(CallAnchor),
		{ goal_info_get_code_model(GoalInfo, CodeModel) },
		( { CodeModel = model_non } ->
			record_model_non_anchor(CallAnchor)
		;
			[]
		),
		one_open_interval(BeforeCallId),
		require_flushed(VarsOnStack),
		require_in_regs(Inputs),
		require_access(Inputs)
	;
		{ MaybeNeedAcrossCall = no },
		{ error("optimize_live_sets_at_call: no need across call") }
	).

%-----------------------------------------------------------------------------%

:- pred optimize_live_sets_in_conj(list(hlds_goal)::in,
	opt_info::in, opt_info::out) is det.

optimize_live_sets_in_conj([]) --> [].
optimize_live_sets_in_conj([Goal | Goals]) -->
	optimize_live_sets_in_conj(Goals),
	optimize_live_sets_in_goal(Goal).

:- pred optimize_live_sets_in_par_conj(list(hlds_goal)::in,
	opt_info::in, opt_info::out) is det.

optimize_live_sets_in_par_conj([]) --> [].
optimize_live_sets_in_par_conj([Goal | Goals]) -->
	% XXX zs: I am not sure that passing opt_info from the first goal to
	% the rest is OK. Maybe we should pass the initial opt_info to all the
	% conjuncts, and then merge the resulting opt_infos.
	optimize_live_sets_in_par_conj(Goals),
	optimize_live_sets_in_goal(Goal).

:- pred optimize_live_sets_in_disj(list(hlds_goal)::in, maybe_needs_flush::in,
	anchor::in, anchor::in, interval_id::in, interval_id::in,
	set(interval_id)::out, opt_info::in, opt_info::out) is det.

optimize_live_sets_in_disj([], _, _, _, _, _, set__init) --> [].
optimize_live_sets_in_disj([Goal | Goals], MaybeNeedsFlush,
		StartAnchor, EndAnchor, BeforeId, AfterId, OpenIntervals) -->
	enter_branch_tail(EndAnchor, AfterId),
	optimize_live_sets_in_goal(Goal),
	reached_branch_start(MaybeNeedsFlush, StartAnchor, BeforeId,
		OpenIntervals),
	optimize_live_sets_in_disj(Goals, needs_flush, StartAnchor, EndAnchor,
		BeforeId, AfterId, _OpenIntervals).

:- pred optimize_live_sets_in_cases(list(case)::in,
	anchor::in, anchor::in, interval_id::in, interval_id::in,
	list(set(interval_id))::out, opt_info::in, opt_info::out) is det.

optimize_live_sets_in_cases([], _, _, _, _, []) --> [].
optimize_live_sets_in_cases([case(_Var, Goal) | Cases], StartAnchor, EndAnchor,
		BeforeId, AfterId, [OpenIntervals | OpenIntervalsList]) -->
	enter_branch_tail(EndAnchor, AfterId),
	optimize_live_sets_in_goal(Goal),
	reached_branch_start(doesnt_need_flush, StartAnchor, BeforeId,
		OpenIntervals),
	optimize_live_sets_in_cases(Cases, StartAnchor, EndAnchor,
		BeforeId, AfterId, OpenIntervalsList).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- instance stack_alloc_info(opt_stack_alloc) where [
	pred(at_call_site/4) is opt_at_call_site,
	pred(at_resume_site/4) is opt_at_resume_site,
	pred(at_par_conj/4) is opt_at_par_conj
].

:- pred opt_at_call_site(need_across_call::in, hlds_goal_info::in,
	opt_stack_alloc::in, opt_stack_alloc::out) is det.

opt_at_call_site(_NeedAtCall, _GoalInfo, StackAlloc, StackAlloc).

:- pred opt_at_resume_site(need_in_resume::in, hlds_goal_info::in,
	opt_stack_alloc::in, opt_stack_alloc::out) is det.

opt_at_resume_site(_NeedAtResume, _GoalInfo, StackAlloc, StackAlloc).

:- pred opt_at_par_conj(need_in_par_conj::in, hlds_goal_info::in,
	opt_stack_alloc::in, opt_stack_alloc::out) is det.

opt_at_par_conj(NeedParConj, _GoalInfo, StackAlloc0, StackAlloc) :-
	NeedParConj = need_in_par_conj(StackVars),
	ParConjOwnSlots0 = StackAlloc0 ^ par_conj_own_slots,
	ParConjOwnSlots = set__union(StackVars, ParConjOwnSlots0),
	StackAlloc = StackAlloc0 ^ par_conj_own_slots := ParConjOwnSlots.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred reached_branch_end(hlds_goal_info::in, maybe(hlds_goal)::in,
	branch_construct::in, anchor::out, anchor::out,
	interval_id::out, interval_id::out, maybe(set(prog_var))::out,
	opt_info::in, opt_info::out) is det.

reached_branch_end(GoalInfo, MaybeResumeGoal, Construct,
		StartAnchor, EndAnchor, BeforeIntervalId, AfterIntervalId,
		MaybeResumeVars) -->
	{ goal_info_get_goal_path(GoalInfo, GoalPath) },
	record_branch_end_info(GoalPath),
	{
		MaybeResumeGoal = yes(_ResumeGoalExpr - ResumeGoalInfo),
		goal_info_maybe_get_resume_point(ResumeGoalInfo, ResumePoint),
		ResumePoint = resume_point(ResumeVars, ResumeLocs),
		ResumeLocs \= orig_only
	->
		HasResumeSave = has_resume_save,
		MaybeResumeVars = yes(ResumeVars)
	;
		HasResumeSave = has_no_resume_save,
		MaybeResumeVars = no
	},
	record_branch_resume(GoalPath, HasResumeSave),
	( { goal_info_maybe_get_store_map(GoalInfo, StoreMap) } ->
		{ map__sorted_keys(StoreMap, StoreMapVarList) },
		{ set__sorted_list_to_set(StoreMapVarList, StoreMapVars) },
		require_flushed(StoreMapVars)
	;
		{ error("reached_branch_end: no store map") }
	),
	{ EndAnchor = branch_end(Construct, GoalPath) },
	{ StartAnchor = branch_start(Construct, GoalPath) },
	assign_open_intervals_to_anchor(EndAnchor),
	{ goal_info_get_code_model(GoalInfo, CodeModel) },
	( { CodeModel = model_non } ->
		record_model_non_anchor(EndAnchor)
	;
		[]
	),
	no_open_intervals,
	get_cur_interval(AfterIntervalId),
	record_interval_start(AfterIntervalId, EndAnchor),
	new_interval_id(BeforeIntervalId).

:- pred enter_branch_tail(anchor::in, interval_id::in,
	opt_info::in, opt_info::out) is det.

enter_branch_tail(EndAnchor, AfterId) -->
	new_interval_id(BranchTailId),
	record_interval_end(BranchTailId, EndAnchor),
	record_interval_succ(BranchTailId, AfterId),
	set_cur_interval(BranchTailId),
	one_open_interval(BranchTailId).

:- pred reached_branch_start(maybe_needs_flush::in, anchor::in,
	interval_id::in, set(interval_id)::out, opt_info::in, opt_info::out)
	is det.

reached_branch_start(MaybeNeedsFlush, StartAnchor, BeforeId, OpenIntervals) -->
	get_cur_interval(BranchStartId),
	record_interval_start(BranchStartId, StartAnchor),
	record_interval_succ(BeforeId, BranchStartId),
	get_open_intervals(OpenIntervals),
	(
		{ MaybeNeedsFlush = doesnt_need_flush }
	;
		{ MaybeNeedsFlush = needs_flush },
		assign_open_intervals_to_anchor(StartAnchor)
	).

:- pred reached_cond_then(hlds_goal_info::in, opt_info::in, opt_info::out)
	is det.

reached_cond_then(GoalInfo) -->
	{ goal_info_get_goal_path(GoalInfo, GoalPath) },
	record_cond_end(GoalPath),
	get_cur_interval(ThenStartId),
	record_interval_start(ThenStartId, CondThenAnchor),
	new_interval_id(CondTailId),
	{ CondThenAnchor = cond_then(GoalPath) },
	record_interval_end(CondTailId, CondThenAnchor),
	record_interval_succ(CondTailId, ThenStartId),
	set_cur_interval(CondTailId),
	get_open_intervals(OpenIntervals0),
	{ OpenIntervals = set__insert(OpenIntervals0, CondTailId) },
	set_open_intervals(OpenIntervals).

:- pred leave_branch_start(branch_construct::in, anchor::in, interval_id::in,
	maybe(set(prog_var))::in, set(interval_id)::in,
	opt_info::in, opt_info::out) is det.

leave_branch_start(_BranchConstruct, StartArchor, BeforeId, MaybeResumeVars,
		OpenIntervals) -->
	record_interval_end(BeforeId, StartArchor),
	(
		{ MaybeResumeVars = yes(ResumeVars) },
		require_flushed(ResumeVars)
	;
		{ MaybeResumeVars = no }
	),
	set_cur_interval(BeforeId),
	set_open_intervals(OpenIntervals).

:- pred get_open_intervals(set(interval_id)::out,
	opt_info::in, opt_info::out) is det.

get_open_intervals(OpenIntervals, OptInfo, OptInfo) :-
	OpenIntervals = OptInfo ^ open_intervals.

:- pred set_open_intervals(set(interval_id)::in,
	opt_info::in, opt_info::out) is det.

set_open_intervals(OpenIntervals, OptInfo0, OptInfo) :-
	OptInfo = OptInfo0 ^ open_intervals := OpenIntervals.

:- pred no_open_intervals(opt_info::in, opt_info::out) is det.

no_open_intervals(OptInfo0, OptInfo) :-
	OptInfo = OptInfo0 ^ open_intervals := set__init.

:- pred one_open_interval(interval_id::in, opt_info::in, opt_info::out) is det.

one_open_interval(IntervalId, OptInfo0, OptInfo) :-
	OptInfo = OptInfo0 ^ open_intervals :=
		set__make_singleton_set(IntervalId).

:- pred assign_open_intervals_to_anchor(anchor::in,
	opt_info::in, opt_info::out) is det.

assign_open_intervals_to_anchor(Anchor, OptInfo0, OptInfo) :-
	AnchorFollowMap0 = OptInfo0 ^ anchor_follow_map,
	IntervalVarMap = OptInfo0 ^ interval_vars,
	CurOpenIntervals = OptInfo0 ^ open_intervals,
	set__fold(gather_interval_vars(IntervalVarMap), CurOpenIntervals,
		set__init, CurOpenIntervalVars),
	( map__search(AnchorFollowMap0, Anchor, AnchorFollowInfo0) ->
		AnchorFollowInfo0 = OpenIntervalVars0 - OpenIntervals0,
		OpenIntervalVars =
			set__union(OpenIntervalVars0, CurOpenIntervalVars),
		OpenIntervals =
			set__union(OpenIntervals0, CurOpenIntervals),
		AnchorFollowInfo = OpenIntervalVars - OpenIntervals,
		map__det_update(AnchorFollowMap0, Anchor, AnchorFollowInfo,
			AnchorFollowMap)
	;
		AnchorFollowInfo = CurOpenIntervalVars - CurOpenIntervals,
		map__det_insert(AnchorFollowMap0, Anchor, AnchorFollowInfo,
			AnchorFollowMap)
	),
	OptInfo = OptInfo0 ^ anchor_follow_map :=
		AnchorFollowMap.

:- pred gather_interval_vars(map(interval_id, set(prog_var))::in,
	interval_id::in, set(prog_var)::in, set(prog_var)::out) is det.

gather_interval_vars(IntervalVarMap, IntervalId,
		OpenIntervalVars0, OpenIntervalVars) :-
	map__lookup(IntervalVarMap, IntervalId, IntervalVars),
	OpenIntervalVars = set__union(OpenIntervalVars0, IntervalVars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred get_cur_interval(interval_id::out, opt_info::in, opt_info::out)
	is det.

get_cur_interval(OptInfo ^ cur_interval, OptInfo, OptInfo).

:- pred set_cur_interval(interval_id::in, opt_info::in, opt_info::out) is det.

set_cur_interval(CurInterval, OptInfo,
	OptInfo ^ cur_interval := CurInterval).

:- pred new_interval_id(interval_id::out, opt_info::in, opt_info::out) is det.

new_interval_id(Id, OptInfo0, OptInfo) :-
	Counter0 = OptInfo0 ^ interval_counter,
	IntervalVars0 = OptInfo0 ^ interval_vars,
	counter__allocate(Num, Counter0, Counter),
	Id = interval_id(Num),
	map__det_insert(IntervalVars0, Id, set__init, IntervalVars),
	OptInfo = (OptInfo0 ^ interval_counter := Counter)
		^ interval_vars := IntervalVars.

:- pred record_branch_end_info(goal_path::in,
	opt_info::in, opt_info::out) is det.

record_branch_end_info(GoalPath, OptInfo0, OptInfo) :-
	FlushedLater = OptInfo0 ^ flushed_later,
	AccessedLater = OptInfo0 ^ accessed_later,
	CurInterval = OptInfo0 ^ cur_interval,
	BranchEndMap0 = OptInfo0 ^ branch_end_map,
	BranchEndInfo = branch_end_info(FlushedLater, AccessedLater,
		CurInterval),
	map__det_insert(BranchEndMap0, GoalPath, BranchEndInfo, BranchEndMap),
	OptInfo = OptInfo0 ^ branch_end_map := BranchEndMap.

:- pred record_cond_end(goal_path::in, opt_info::in, opt_info::out) is det.

record_cond_end(GoalPath, OptInfo0, OptInfo) :-
	CurInterval = OptInfo0 ^ cur_interval,
	CondEndMap0 = OptInfo0 ^ cond_end_map,
	map__det_insert(CondEndMap0, GoalPath, CurInterval, CondEndMap),
	OptInfo = OptInfo0 ^ cond_end_map := CondEndMap.

:- pred record_interval_end(interval_id::in, anchor::in,
	opt_info::in, opt_info::out) is det.

record_interval_end(Id, End, OptInfo0, OptInfo) :-
	EndMap0 = OptInfo0 ^ interval_end,
	map__det_insert(EndMap0, Id, End, EndMap),
	OptInfo = OptInfo0 ^ interval_end := EndMap.

:- pred record_interval_start(interval_id::in, anchor::in,
	opt_info::in, opt_info::out) is det.

record_interval_start(Id, Start, OptInfo0, OptInfo) :-
	StartMap0 = OptInfo0 ^ interval_start,
	map__det_insert(StartMap0, Id, Start, StartMap),
	OptInfo = OptInfo0 ^ interval_start := StartMap.

:- pred record_interval_succ(interval_id::in, interval_id::in,
	opt_info::in, opt_info::out) is det.

record_interval_succ(Id, Succ, OptInfo0, OptInfo) :-
	SuccMap0 = OptInfo0 ^ interval_succ,
	( map__search(SuccMap0, Id, Succ0) ->
		map__det_update(SuccMap0, Id, [Succ | Succ0], SuccMap)
	;
		map__det_insert(SuccMap0, Id, [Succ], SuccMap)
	),
	OptInfo = OptInfo0 ^ interval_succ := SuccMap.

:- pred record_interval_no_succ(interval_id::in,
	opt_info::in, opt_info::out) is det.

record_interval_no_succ(Id, OptInfo0, OptInfo) :-
	SuccMap0 = OptInfo0 ^ interval_succ,
	( map__search(SuccMap0, Id, _Succ0) ->
		error("record_interval_no_succ: already in succ map")
	;
		map__det_insert(SuccMap0, Id, [], SuccMap)
	),
	OptInfo = OptInfo0 ^ interval_succ := SuccMap.

:- pred record_interval_vars(interval_id::in, list(prog_var)::in,
	opt_info::in, opt_info::out) is det.

record_interval_vars(Id, NewVars, OptInfo0, OptInfo) :-
	VarsMap0 = OptInfo0 ^ interval_vars,
	( map__search(VarsMap0, Id, Vars0) ->
		Vars = set__insert_list(Vars0, NewVars),
		map__det_update(VarsMap0, Id, Vars, VarsMap)
	;
		set__list_to_set(NewVars, Vars),
		map__det_insert(VarsMap0, Id, Vars, VarsMap)
	),
	OptInfo = OptInfo0 ^ interval_vars := VarsMap.

:- pred delete_interval_vars(interval_id::in, set(prog_var)::in,
	set(prog_var)::out, opt_info::in, opt_info::out) is det.

delete_interval_vars(Id, ToDeleteVars, DeletedVars, OptInfo0, OptInfo) :-
	VarsMap0 = OptInfo0 ^ interval_vars,
	map__lookup(VarsMap0, Id, Vars0),
	DeletedVars = set__intersect(Vars0, ToDeleteVars),
	Vars = set__difference(Vars0, DeletedVars),
	map__det_update(VarsMap0, Id, Vars, VarsMap),
	OptInfo1 = OptInfo0 ^ interval_vars := VarsMap,

	% The deletions are recorded only for debugging. The algorithm itself
	% does not need this information to be recorded.
	DeleteMap0 = OptInfo1 ^ interval_delvars,
	( map__search(DeleteMap0, Id, Deletions0) ->
		Deletions = [DeletedVars | Deletions0],
		map__det_update(DeleteMap0, Id, Deletions, DeleteMap)
	;
		Deletions = [DeletedVars],
		map__det_insert(DeleteMap0, Id, Deletions, DeleteMap)
	),
	OptInfo = OptInfo1 ^ interval_delvars := DeleteMap.

:- pred lookup_interval_end(interval_id::in, anchor::out,
	opt_info::in, opt_info::out) is det.

lookup_interval_end(Id, End, OptInfo, OptInfo) :-
	EndMap = OptInfo ^ interval_end,
	map__lookup(EndMap, Id, End).

:- pred lookup_interval_succ(interval_id::in, list(interval_id)::out,
	opt_info::in, opt_info::out) is det.

lookup_interval_succ(Id, Succ, OptInfo, OptInfo) :-
	SuccMap = OptInfo ^ interval_succ,
	map__lookup(SuccMap, Id, Succ).

:- pred lookup_interval_vars(interval_id::in, set(prog_var)::out,
	opt_info::in, opt_info::out) is det.

lookup_interval_vars(Id, Vars, OptInfo, OptInfo) :-
	VarsMap = OptInfo ^ interval_vars,
	map__lookup(VarsMap, Id, Vars).

:- pred require_in_regs(list(prog_var)::in, opt_info::in, opt_info::out)
	is det.

require_in_regs(Vars, OptInfo0, OptInfo) :-
	CurIntervalId = OptInfo0 ^ cur_interval,
	record_interval_vars(CurIntervalId, Vars, OptInfo0, OptInfo).

:- pred require_flushed(set(prog_var)::in,
	opt_info::in, opt_info::out) is det.

require_flushed(Vars, OptInfo0, OptInfo) :-
	FlushedLater0 = OptInfo0 ^ flushed_later,
	FlushedLater = set__union(FlushedLater0, Vars),
	OptInfo = OptInfo0 ^ flushed_later := FlushedLater.

:- pred require_access(list(prog_var)::in,
	opt_info::in, opt_info::out) is det.

require_access(Vars, OptInfo0, OptInfo) :-
	AccessedLater0 = OptInfo0 ^ accessed_later,
	AccessedLater = set__insert_list(AccessedLater0, Vars),
	OptInfo = OptInfo0 ^ accessed_later := AccessedLater.

:- pred record_branch_resume(goal_path::in, resume_save_status::in,
	opt_info::in, opt_info::out) is det.

record_branch_resume(GoalPath, ResumeSaveStatus, OptInfo0, OptInfo) :-
	BranchResumeMap0 = OptInfo0 ^ branch_resume_map,
	map__det_insert(BranchResumeMap0, GoalPath, ResumeSaveStatus,
		BranchResumeMap),
	OptInfo = OptInfo0 ^ branch_resume_map := BranchResumeMap.

:- pred record_model_non_anchor(anchor::in, opt_info::in, opt_info::out)
	is det.

record_model_non_anchor(Anchor, OptInfo0, OptInfo) :-
	ModelNonAnchors0 = OptInfo0 ^ model_non_anchors,
	ModelNonAnchors = set__insert(ModelNonAnchors0, Anchor),
	OptInfo = OptInfo0 ^ model_non_anchors := ModelNonAnchors.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type match_path_info
	--->	match_path_info(
			set(prog_var),		% The set of vars referenced in
						% the first interval, before
						% the first flush point.
			list(set(prog_var))	% The set of vars referenced in
						% later intervals, after the
						% first flush point.
		).

:- type match_info
	--->	match_info(
			list(match_path_info),	% Information about the
						% variables used along each
						% path.
			set(prog_var),		% The variables used after the
						% deconstruction goes out of
						% scope.
			bool,			% Have we stepped over a
						% model_non goal?
			set(anchor),		% The set of save points
						% to which the results of the
						% matching applies.
			set(interval_id)
		).

:- pred use_cell(prog_var::in, list(prog_var)::in, cons_id::in, hlds_goal::in,
	opt_info::in, opt_info::out) is det.

use_cell(CellVar, FieldVarList, ConsId, Goal) -->
	FlushedLater =^ flushed_later,
	OptParams =^ opt_params,
	{ NonCandidateVars = OptParams ^ non_candidate_vars },
	{ set__list_to_set(FieldVarList, FieldVars) },
	{ set__intersect(FieldVars, FlushedLater, FlushedLaterFieldVars) },
	{ set__difference(FlushedLaterFieldVars, NonCandidateVars,
		CandidateArgVars0) },
	(
		{ set__empty(CandidateArgVars0) }
	->
		[]
	;
		{ ConsId = cons(_Name, _Arity) },
		{ VarTypes = OptParams ^ var_types },
		{ map__lookup(VarTypes, CellVar, Type) },
		(
			{ type_is_tuple(Type, _) }
		->
			{ FreeOfCost = no }
		;
			{ type_to_ctor_and_args(Type, TypeCtor, _) },
			{ ModuleInfo = OptParams ^ module_info },
			{ module_info_types(ModuleInfo, TypeTable) },
			{ map__lookup(TypeTable, TypeCtor, TypeDefn) },
			{ hlds_data__get_type_defn_body(TypeDefn, TypeBody) },
			{ TypeBody = du_type(_, ConsTable, _, _) }
		->
			{ map__lookup(ConsTable, ConsId, ConsTag) },
			{ ConsTag = no_tag ->
				FreeOfCost = yes
			;
				FreeOfCost = no
			}
		;
			{ fail }
		)
	->
		{ RelevantVars = set__insert(FieldVars, CellVar) },
		find_all_branches_from_cur_interval(RelevantVars, MatchInfo),
		{ MatchInfo = match_info(PathsInfo, RelevantAfterVars,
			AfterModelNon, InsertAnchors, InsertIntervals) },
		(
			{ FreeOfCost = yes },
			{ set__difference(CandidateArgVars0, RelevantAfterVars,
				ViaCellVars) },
			record_matching_result(CellVar, ConsId,
				FieldVarList, ViaCellVars, Goal,
				InsertAnchors, InsertIntervals)
		;
			{ FreeOfCost = no },
			(
				{ AfterModelNon = no },
				{ OnStack = OptParams ^ on_stack },
				{ set__difference(CandidateArgVars0,
					RelevantAfterVars, CandidateArgVars) },
				{
					OnStack = yes,
					( set__member(CellVar, FlushedLater) ->
						CellVarFlushedLater = yes
					;
						CellVarFlushedLater = no
					)
				;
					OnStack = no,
					(
						list__member(PathInfo,
							PathsInfo),
						PathInfo = match_path_info(_,
							Segments),
						list__member(Segment,
							Segments),
						set__member(CellVar, Segment)
					->
						CellVarFlushedLater = yes
					;
						CellVarFlushedLater = no
					)
				},
				{ apply_matching(CellVar, CellVarFlushedLater,
					OptParams, PathsInfo, CandidateArgVars,
					ViaCellVars) },
				record_matching_result(CellVar, ConsId,
					FieldVarList, ViaCellVars, Goal,
					InsertAnchors, InsertIntervals)
			;
				{ AfterModelNon = yes }
			)
		)
	;
		[]
	).

:- pred apply_matching(prog_var::in, bool::in, opt_params::in,
	list(match_path_info)::in, set(prog_var)::in, set(prog_var)::out)
	is det.

apply_matching(CellVar, CellVarFlushedLater, OptParams, PathInfos,
		CandidateArgVars0, ViaCellVars) :-
	apply_matching_loop(CellVar, CellVarFlushedLater, OptParams, PathInfos,
		CandidateArgVars0, BenefitNodeSets, CostNodeSets,
		ViaCellVars0),
	BenefitNodes = set__union_list(BenefitNodeSets),
	CostNodes = set__union_list(CostNodeSets),
	set__count(BenefitNodes, NumBenefitNodes),
	set__count(CostNodes, NumCostNodes),
	AllPathNodeRatio = OptParams ^ all_path_node_ratio,
	( NumBenefitNodes * 100 >= NumCostNodes * AllPathNodeRatio ->
		ViaCellVars = ViaCellVars0
	;
		ViaCellVars = set__init
	).

:- pred apply_matching_loop(prog_var::in, bool::in, opt_params::in,
	list(match_path_info)::in, set(prog_var)::in,
	list(set(benefit_node))::out, list(set(cost_node))::out,
	set(prog_var)::out) is det.

apply_matching_loop(CellVar, CellVarFlushedLater, OptParams, PathInfos,
		CandidateArgVars0, BenefitNodeSets, CostNodeSets,
		ViaCellVars) :-
	list__map3(apply_matching_for_path(CellVar, CellVarFlushedLater,
		OptParams, CandidateArgVars0), PathInfos,
		BenefitNodeSets0, CostNodeSets0, PathViaCellVars),
	( list__all_same(PathViaCellVars) ->
		BenefitNodeSets = BenefitNodeSets0,
		CostNodeSets = CostNodeSets0,
		( PathViaCellVars = [ViaCellVarsPrime | _] ->
			ViaCellVars = ViaCellVarsPrime
		;
			ViaCellVars = set__init
		)
	;
		CandidateArgVars1 = set__intersect_list(PathViaCellVars),
		FixpointLoop = OptParams ^ fixpoint_loop,
		(
			FixpointLoop = no,
			BenefitNodeSets = BenefitNodeSets0,
			CostNodeSets = CostNodeSets0,
			ViaCellVars = CandidateArgVars1
		;
			FixpointLoop = yes,
			apply_matching_loop(CellVar, CellVarFlushedLater,
				OptParams, PathInfos, CandidateArgVars1,
				BenefitNodeSets, CostNodeSets, ViaCellVars)
		)
	).

:- pred apply_matching_for_path(prog_var::in, bool::in, opt_params::in,
	set(prog_var)::in, match_path_info::in,
	set(benefit_node)::out, set(cost_node)::out, set(prog_var)::out)
	is det.

apply_matching_for_path(CellVar, CellVarFlushedLater, OptParams,
		CandidateArgVars, PathInfo, BenefitNodes, CostNodes,
		ViaCellVars) :-
	( set__empty(CandidateArgVars) ->
		BenefitNodes = set__init,
		CostNodes = set__init,
		ViaCellVars = set__init
	;
		PathInfo = match_path_info(FirstSegment, LaterSegments),
		MatchingParams = OptParams ^ matching_params,
		find_via_cell_vars(CellVar, CandidateArgVars,
			CellVarFlushedLater, FirstSegment, LaterSegments,
			MatchingParams, BenefitNodes, CostNodes, ViaCellVars)
	).

:- pred record_matching_result(prog_var::in, cons_id::in, list(prog_var)::in,
	set(prog_var)::in, hlds_goal::in, set(anchor)::in,
	set(interval_id)::in, opt_info::in, opt_info::out) is det.

record_matching_result(CellVar, ConsId, ArgVars, ViaCellVars, Goal,
		PotentialAnchors, PotentialIntervals, OptInfo0, OptInfo) :-
	( set__empty(ViaCellVars) ->
		OptInfo = OptInfo0
	;
		set__to_sorted_list(PotentialIntervals, PotentialIntervalList),
		set__to_sorted_list(PotentialAnchors, PotentialAnchorList),
		list__foldl2(
			record_cell_var_for_interval(CellVar, ViaCellVars),
			PotentialIntervalList, OptInfo0, OptInfo1,
			set__init, InsertIntervals),
		list__foldl2(
			add_anchor_inserts(Goal, ViaCellVars, InsertIntervals),
			PotentialAnchorList, OptInfo1, OptInfo2,
			set__init, InsertAnchors),
		Goal = _ - GoalInfo,
		goal_info_get_goal_path(GoalInfo, GoalPath),
		MatchingResult = matching_result(CellVar, ConsId,
			ArgVars, ViaCellVars, GoalPath,
			PotentialIntervals, InsertIntervals,
			PotentialAnchors, InsertAnchors),
		MatchingResults0 = OptInfo2 ^ matching_results,
		MatchingResults = [MatchingResult | MatchingResults0],
		OptInfo = OptInfo2 ^ matching_results := MatchingResults
	).

:- pred record_cell_var_for_interval(prog_var::in, set(prog_var)::in,
	interval_id::in, opt_info::in, opt_info::out,
	set(interval_id)::in, set(interval_id)::out) is det.

record_cell_var_for_interval(CellVar, ViaCellVars, IntervalId,
		OptInfo0, OptInfo, InsertIntervals0, InsertIntervals) :-
	record_interval_vars(IntervalId, [CellVar], OptInfo0, OptInfo1),
	delete_interval_vars(IntervalId, ViaCellVars, DeletedVars,
		OptInfo1, OptInfo),
	( set__non_empty(DeletedVars) ->
		set__insert(InsertIntervals0, IntervalId, InsertIntervals)
	;
		InsertIntervals = InsertIntervals0
	).

:- pred add_anchor_inserts(hlds_goal::in, set(prog_var)::in,
	set(interval_id)::in, anchor::in, opt_info::in, opt_info::out,
	set(anchor)::in, set(anchor)::out) is det.

add_anchor_inserts(Goal, ArgVarsViaCellVar, InsertIntervals, Anchor,
		OptInfo0, OptInfo, InsertAnchors0, InsertAnchors) :-
	map__lookup(OptInfo0 ^ anchor_follow_map, Anchor, AnchorFollow),
	AnchorFollow = _ - AnchorIntervals,
	set__intersect(AnchorIntervals, InsertIntervals,
		AnchorInsertIntervals),
	( set__non_empty(AnchorInsertIntervals) ->
		Insert = insert_spec(Goal, ArgVarsViaCellVar),
		InsertMap0 = OptInfo0 ^ left_anchor_inserts,
		( map__search(InsertMap0, Anchor, Inserts0) ->
			Inserts = [Insert | Inserts0],
			map__det_update(InsertMap0, Anchor, Inserts, InsertMap)
		;
			Inserts = [Insert],
			map__det_insert(InsertMap0, Anchor, Inserts, InsertMap)
		),
		OptInfo = OptInfo0 ^ left_anchor_inserts := InsertMap,
		set__insert(InsertAnchors0, Anchor, InsertAnchors)
	;
		OptInfo = OptInfo0,
		InsertAnchors = InsertAnchors0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type current_segment_first_flush
	--->	current_is_before_first_flush
	;	current_is_after_first_flush.

:- type path
	--->	path(
			flush_state		:: current_segment_first_flush,
			current_segment		:: set(prog_var),
			first_segment		:: set(prog_var),
			other_segments		:: list(set(prog_var)),
			flush_anchors		:: set(anchor),
			occurring_intervals	:: set(interval_id)
		).

:- type all_paths
	--->	all_paths(
			set(path),	% The set of all paths so far.
			bool,		% Have we stepped over model_non goals?
			set(prog_var)	% The vars which are known to be used 
					% after the deconstruction goes out of
					% scope.
		).

:- pred extract_match_and_save_info(path::in, match_path_info::out,
	set(anchor)::out, set(interval_id)::out) is det.

extract_match_and_save_info(Path0, MatchPathInfo, Anchors, Intervals) :-
	Path = close_path(Path0),
	FirstSegment = Path ^ first_segment,
	OtherSegments = Path ^ other_segments,
	MatchPathInfo = match_path_info(FirstSegment, OtherSegments),
	Anchors = Path ^ flush_anchors,
	Intervals = Path ^ occurring_intervals.

:- func close_path(path) = path.

close_path(Path0) = Path :-
	Path0 = path(FlushState, CurSegment, FirstSegment0, OtherSegments0,
		FlushAnchors, IntervalIds),
	( FlushState = current_is_before_first_flush ->
		require(set__empty(FirstSegment0),
			"close_path: FirstSegment0 not empty"),
		FirstSegment = CurSegment,
		OtherSegments = OtherSegments0
	; set__empty(CurSegment) ->
		FirstSegment = FirstSegment0,
		OtherSegments = OtherSegments0
	;
		FirstSegment = FirstSegment0,
		OtherSegments = [CurSegment | OtherSegments0]
	),
	Path = path(current_is_after_first_flush, set__init,
		FirstSegment, OtherSegments, FlushAnchors, IntervalIds).

:- func add_interval_to_path(interval_id, set(prog_var), path) = path.

add_interval_to_path(IntervalId, Vars, Path0) = Path :-
	( set__empty(Vars) ->
		Path = Path0
	;
		CurSegment0 = Path0 ^ current_segment,
		CurSegment = set__union(Vars, CurSegment0),
		OccurringIntervals0 = Path0 ^ occurring_intervals,
		OccurringIntervals = set__insert(OccurringIntervals0,
			IntervalId),
		Path = (Path0 ^ current_segment := CurSegment)
			^ occurring_intervals := OccurringIntervals
	).

:- func add_anchor_to_path(anchor, path) = path.

add_anchor_to_path(Anchor, Path0) = Path :-
	Anchors0 = Path0 ^ flush_anchors,
	Anchors = set__insert(Anchors0, Anchor),
	Path = Path0 ^ flush_anchors := Anchors.

:- func anchor_requires_close(opt_info, anchor) = bool.

anchor_requires_close(_, proc_start) = no.
anchor_requires_close(_, proc_end) = yes.
anchor_requires_close(OptInfo, branch_start(_, GoalPath)) =
		resume_save_status_requires_close(ResumeSaveStatus) :-
	map__lookup(OptInfo ^ branch_resume_map, GoalPath, ResumeSaveStatus).
anchor_requires_close(_, cond_then(_)) = no.
anchor_requires_close(_, branch_end(BranchConstruct, _)) =
	( BranchConstruct = neg ->
		no
	;
		yes
	).
anchor_requires_close(_, call_site(_)) = yes.

:- func resume_save_status_requires_close(resume_save_status) = bool.

resume_save_status_requires_close(has_resume_save) = yes.
resume_save_status_requires_close(has_no_resume_save) = no.

:- pred may_have_no_successor(anchor::in) is semidet.

may_have_no_successor(Anchor) :-
	may_have_no_successor(Anchor, yes).

:- pred may_have_no_successor(anchor::in, bool::out) is det.

may_have_no_successor(proc_start, no).
may_have_no_successor(proc_end, yes).
may_have_no_successor(branch_start(_, _), no).
may_have_no_successor(cond_then(_), no).
may_have_no_successor(branch_end(_, _), no).
may_have_no_successor(call_site(_), yes).	% if the call cannot succeed

:- pred may_have_one_successor(anchor::in) is semidet.

may_have_one_successor(Anchor) :-
	may_have_one_successor(Anchor, yes).

:- pred may_have_one_successor(anchor::in, bool::out) is det.

may_have_one_successor(proc_start, yes).
may_have_one_successor(proc_end, no).
may_have_one_successor(branch_start(_, _), yes).
may_have_one_successor(cond_then(_), yes).
may_have_one_successor(branch_end(_, _), yes).
may_have_one_successor(call_site(_), yes).

:- pred may_have_more_successors(anchor::in) is semidet.

may_have_more_successors(Anchor) :-
	may_have_more_successors(Anchor, yes).

:- pred may_have_more_successors(anchor::in, bool::out) is det.

may_have_more_successors(proc_start, no).
may_have_more_successors(proc_end, no).
may_have_more_successors(branch_start(Type, _), MayHave) :-
	( Type = neg ->
		MayHave = no
	;
		MayHave = yes
	).
may_have_more_successors(cond_then(_), no).
may_have_more_successors(branch_end(_, _), no).
may_have_more_successors(call_site(_), no).

:- pred lookup_inserts(insert_map::in, anchor::in, list(insert_spec)::out)
	is det.

lookup_inserts(InsertMap, Anchor, Inserts) :-
	( map__search(InsertMap, Anchor, InsertsPrime) ->
		Inserts = InsertsPrime
	;
		Inserts = []
	).

%-----------------------------------------------------------------------------%

:- pred find_all_branches_from_cur_interval(set(prog_var)::in,
	match_info::out, opt_info::in, opt_info::out) is det.

find_all_branches_from_cur_interval(RelevantVars, MatchInfo,
		OptInfo, OptInfo) :-
	IntervalId = OptInfo ^ cur_interval,
	map__lookup(OptInfo ^ interval_vars, IntervalId, IntervalVars),
	IntervalRelevantVars = set__intersect(RelevantVars, IntervalVars),
	Path0 = path(current_is_before_first_flush, IntervalRelevantVars,
		set__init, [], set__init, set__init),
	AllPaths0 = all_paths(set__make_singleton_set(Path0), no, set__init),
	find_all_branches(RelevantVars, IntervalId, no, OptInfo,
		AllPaths0, AllPaths),
	AllPaths = all_paths(Paths, AfterModelNon, RelevantAfter),
	set__to_sorted_list(Paths, PathList),
	list__map3(extract_match_and_save_info, PathList,
		MatchInputs, FlushAnchorSets, OccurringIntervalSets),
	FlushAnchors = set__union_list(FlushAnchorSets),
	OccurringIntervals = set__union_list(OccurringIntervalSets),
	MatchInfo = match_info(MatchInputs, RelevantAfter, AfterModelNon,
		FlushAnchors, OccurringIntervals).

:- pred find_all_branches(set(prog_var)::in, interval_id::in,
	maybe(anchor)::in, opt_info::in,
	all_paths::in, all_paths::out) is det.

find_all_branches(RelevantVars, IntervalId, MaybeSearchAnchor0,
		OptInfo, AllPaths0, AllPaths) :-
	map__lookup(OptInfo ^ interval_end, IntervalId, End),
	map__lookup(OptInfo ^ interval_succ, IntervalId, SuccessorIds),
	(
		SuccessorIds = [],
		require(may_have_no_successor(End),
			"find_all_branches: unexpected no successor"),
		% require(unify(MaybeSearchAnchor0, no),
		% 	"find_all_branches: no successor while in search"),
		% that test may fail if we come to a call that cannot succeed
		AllPaths = AllPaths0
	;
		SuccessorIds = [SuccessorId | MoreSuccessorIds],
		(
			MoreSuccessorIds = [],
			require(may_have_one_successor(End),
				"find_all_branches: unexpected one successor")
		;
			MoreSuccessorIds = [_ | _],
			require(may_have_more_successors(End),
				"find_all_branches: unexpected more successors")
		),
		(
			MaybeSearchAnchor0 = yes(SearchAnchor0),
			End = SearchAnchor0
		->
			AllPaths0 = all_paths(Paths0, AfterModelNon, _),
			AllPaths = all_paths(Paths0, AfterModelNon, set__init)
		;
			End = branch_end(_, EndGoalPath),
			map__lookup(OptInfo ^ branch_end_map, EndGoalPath,
				BranchEndInfo),
			OnStackAfterBranch =
				BranchEndInfo ^ flushed_after_branch,
			AccessedAfterBranch =
				BranchEndInfo ^ accessed_after_branch,
			NeededAfterBranch = set__union(OnStackAfterBranch,
				AccessedAfterBranch),
			RelevantAfter = set__intersect(RelevantVars,
				NeededAfterBranch),
			set__non_empty(RelevantAfter)
		->
			AllPaths0 = all_paths(Paths0, AfterModelNon, _),
			AllPaths = all_paths(Paths0, AfterModelNon,
				RelevantAfter)
		;
			find_all_branches_from(End, RelevantVars,
				MaybeSearchAnchor0, OptInfo,
				[SuccessorId | MoreSuccessorIds],
				AllPaths0, AllPaths)
		)
	).

:- pred find_all_branches_from(anchor::in, set(prog_var)::in,
	maybe(anchor)::in, opt_info::in, list(interval_id)::in,
	all_paths::in, all_paths::out) is det.

find_all_branches_from(End, RelevantVars, MaybeSearchAnchor0, OptInfo,
		SuccessorIds, AllPaths0, AllPaths) :-
	( anchor_requires_close(OptInfo, End) = yes ->
		AllPaths0 = all_paths(Paths0, AfterModelNon,
			RelevantAfter),
		Paths1 = set__map(close_path, Paths0),
		AllPaths1 = all_paths(Paths1, AfterModelNon,
			RelevantAfter)
	;
		AllPaths1 = AllPaths0
	),
	OptParams = OptInfo ^ opt_params,
	FullPath = OptParams ^ full_path,
	(
		FullPath = yes,
		End = branch_start(disj, EndGoalPath)
	->
		MaybeSearchAnchor1 = yes(branch_end(disj, EndGoalPath)),
		one_after_another(RelevantVars, MaybeSearchAnchor1,
			OptInfo, SuccessorIds, AllPaths1, AllPaths2),
		map__lookup(OptInfo ^ branch_end_map, EndGoalPath,
			BranchEndInfo),
		ContinueId = BranchEndInfo ^ interval_after_branch,
		apply_interval_find_all_branches(RelevantVars,
			MaybeSearchAnchor0, OptInfo,
			AllPaths2, ContinueId, AllPaths)
	;
		FullPath = yes,
		End = branch_start(ite, EndGoalPath)
	->
		( SuccessorIds = [ElseStartIdPrime, CondStartIdPrime] ->
			ElseStartId = ElseStartIdPrime,
			CondStartId = CondStartIdPrime
		;
			error("find_all_branches_from: ite not else, cond")
		),
		MaybeSearchAnchorCond = yes(cond_then(EndGoalPath)),
		apply_interval_find_all_branches(RelevantVars,
			MaybeSearchAnchorCond, OptInfo, AllPaths1,
			CondStartId, AllPaths2),
		MaybeSearchAnchorEnd = yes(branch_end(ite, EndGoalPath)),
		CondEndMap = OptInfo ^ cond_end_map,
		map__lookup(CondEndMap, EndGoalPath, ThenStartId),
		one_after_another(RelevantVars, MaybeSearchAnchorEnd, OptInfo,
			[ThenStartId, ElseStartId], AllPaths2, AllPaths3),
		map__lookup(OptInfo ^ branch_end_map, EndGoalPath,
			BranchEndInfo),
		ContinueId = BranchEndInfo ^ interval_after_branch,
		apply_interval_find_all_branches(RelevantVars,
			MaybeSearchAnchor0, OptInfo,
			AllPaths3, ContinueId, AllPaths)
	;
		End = branch_start(BranchType, EndGoalPath)
	->
		MaybeSearchAnchor1 = yes(branch_end(BranchType, EndGoalPath)),
		list__map(apply_interval_find_all_branches(RelevantVars,
			MaybeSearchAnchor1, OptInfo, AllPaths1),
			SuccessorIds, AllPathsList),
		consolidate_after_join(AllPathsList, AllPaths2),
		map__lookup(OptInfo ^ branch_end_map, EndGoalPath,
			BranchEndInfo),
		ContinueId = BranchEndInfo ^ interval_after_branch,
		apply_interval_find_all_branches(RelevantVars,
			MaybeSearchAnchor0, OptInfo,
			AllPaths2, ContinueId, AllPaths)
	;
		( SuccessorIds = [SuccessorId] ->
			apply_interval_find_all_branches(RelevantVars,
				MaybeSearchAnchor0, OptInfo,
				AllPaths1, SuccessorId, AllPaths)
		;
			error("more successor ids")
		)
	).

:- pred one_after_another(set(prog_var)::in, maybe(anchor)::in, opt_info::in,
	list(interval_id)::in, all_paths::in, all_paths::out) is det.

one_after_another(_, _, _, [], AllPaths, AllPaths).
one_after_another(RelevantVars, MaybeSearchAnchor1, OptInfo,
		[SuccessorId | MoreSuccessorIds], AllPaths0, AllPaths) :-
	apply_interval_find_all_branches(RelevantVars, MaybeSearchAnchor1,
		OptInfo, AllPaths0, SuccessorId, AllPaths1),
	one_after_another(RelevantVars, MaybeSearchAnchor1, OptInfo,
		MoreSuccessorIds, AllPaths1, AllPaths).

:- pred apply_interval_find_all_branches(set(prog_var)::in,
	maybe(anchor)::in, opt_info::in, all_paths::in,
	interval_id::in, all_paths::out) is det.

apply_interval_find_all_branches(RelevantVars, MaybeSearchAnchor0,
		OptInfo, AllPaths0, IntervalId, AllPaths) :-
	map__lookup(OptInfo ^ interval_vars, IntervalId, IntervalVars),
	RelevantIntervalVars = set__intersect(RelevantVars, IntervalVars),
	AllPaths0 = all_paths(Paths0, AfterModelNon0, RelevantAfter),
	Paths1 = set__map(
		add_interval_to_path(IntervalId, RelevantIntervalVars),
		Paths0),
	map__lookup(OptInfo ^ interval_start, IntervalId, Start),
	(
		% Check if intervals starting at Start use any RelevantVars.
		( Start = call_site(_)
		; Start = branch_end(_, _)
		; Start = branch_start(_, _)
		),
		map__search(OptInfo ^ anchor_follow_map, Start, StartInfo),
		StartInfo = AnchorFollowVars - _,
		set__intersect(RelevantVars, AnchorFollowVars, NeededVars),
		set__non_empty(NeededVars)
	->
		Paths2 = set__map(
			add_anchor_to_path(Start),
			Paths1)
	;
		Paths2 = Paths1
	),
	( set__member(Start, OptInfo ^ model_non_anchors) ->
		AfterModelNon = yes
	;
		AfterModelNon = AfterModelNon0
	),
	AllPaths2 = all_paths(Paths2, AfterModelNon, RelevantAfter),
	find_all_branches(RelevantVars, IntervalId,
		MaybeSearchAnchor0, OptInfo, AllPaths2, AllPaths).

:- pred consolidate_after_join(list(all_paths)::in,
	all_paths::out) is det.

consolidate_after_join([], _) :-
	error("consolidate_after_join: no paths to join").
consolidate_after_join([First | Rest], AllPaths) :-
	PathsList = list__map(project_paths_from_all_paths, [First | Rest]),
	Paths0 = set__union_list(PathsList),
	Paths = compress_paths(Paths0),
	AfterModelNonList = list__map(project_after_model_non_from_all_paths,
		[First | Rest]),
	bool__or_list(AfterModelNonList, AfterModelNon),
	AllPaths = all_paths(Paths, AfterModelNon, set__init).

:- func project_paths_from_all_paths(all_paths) = set(path).

project_paths_from_all_paths(all_paths(Paths, _, _)) = Paths.

:- func project_after_model_non_from_all_paths(all_paths) = bool.

project_after_model_non_from_all_paths(all_paths(_, AfterModelNon, _)) =
	AfterModelNon.

:- func compress_paths(set(path)) = set(path).

compress_paths(Paths) = Paths.
	% XXX should reduce the cardinality of Paths below a threshold.
	% XXX should try to preserve the current segment.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type var_info
	--->	var_info(
			varset		:: prog_varset,
			vartypes	:: vartypes
		).

:- type rename_map	==	map(prog_var, prog_var).

:- pred record_decisions_in_goal(hlds_goal::in, hlds_goal::out,
	var_info::in, var_info::out, rename_map::in, rename_map::out,
	insert_map::in) is det.

record_decisions_in_goal(conj(Goals0) - GoalInfo, conj(Goals) - GoalInfo,
		VarInfo0, VarInfo, VarRename0, VarRename, InsertMap) :-
	record_decisions_in_conj(Goals0, Goals, VarInfo0, VarInfo,
		VarRename0, VarRename, InsertMap).

record_decisions_in_goal(par_conj(Goals0) - GoalInfo,
		par_conj(Goals) - GoalInfo, VarInfo0, VarInfo,
		VarRename0, map__init, InsertMap) :-
	record_decisions_in_par_conj(Goals0, Goals, VarInfo0, VarInfo,
		VarRename0, InsertMap).

record_decisions_in_goal(Goal0,  Goal, VarInfo0, VarInfo,
		VarRename0, VarRename, InsertMap) :-
	Goal0 = disj(Goals0) - GoalInfo0,
	construct_anchors(disj, Goal0, StartAnchor, EndAnchor),
	( Goals0 = [FirstGoal0 | LaterGoals0] ->
		record_decisions_in_goal(FirstGoal0, FirstGoal,
			VarInfo0, VarInfo1, VarRename0, _, InsertMap),
		lookup_inserts(InsertMap, StartAnchor, StartInserts),
		record_decisions_in_disj(LaterGoals0, LaterGoals,
			VarInfo1, VarInfo2, VarRename0, StartInserts,
			InsertMap),
		Goals = [FirstGoal | LaterGoals],
		Goal1 = disj(Goals) - GoalInfo0,
		lookup_inserts(InsertMap, EndAnchor, Inserts),
		insert_goals_after(Goal1, Goal, VarInfo2, VarInfo,
			VarRename, Inserts)
	;
		Goal = disj(Goals0) - GoalInfo0,
		VarInfo = VarInfo0,
		VarRename = VarRename0
	).

record_decisions_in_goal(Goal0, Goal, VarInfo0, VarInfo, VarRename0, VarRename,
		InsertMap) :-
	Goal0 = switch(Var0, Det, Cases0) - GoalInfo0,
	record_decisions_in_cases(Cases0, Cases, VarInfo0, VarInfo1,
		VarRename0, InsertMap),
	rename_var(Var0, no, VarRename0, Var),
	Goal1 = switch(Var, Det, Cases) - GoalInfo0,
	construct_anchors(switch, Goal0, _StartAnchor, EndAnchor),
	lookup_inserts(InsertMap, EndAnchor, Inserts),
	insert_goals_after(Goal1, Goal, VarInfo1, VarInfo,
		VarRename, Inserts).

record_decisions_in_goal(Goal0, Goal, VarInfo0, VarInfo, VarRename0, VarRename,
		InsertMap) :-
	Goal0 = not(NegGoal0) - GoalInfo0,
	record_decisions_in_goal(NegGoal0, NegGoal, VarInfo0, VarInfo1,
		VarRename0, _, InsertMap),
	Goal1 = not(NegGoal) - GoalInfo0,
	construct_anchors(neg, Goal0, _StartAnchor, EndAnchor),
	lookup_inserts(InsertMap, EndAnchor, Inserts),
	% XXX
	insert_goals_after(Goal1, Goal, VarInfo1, VarInfo,
		VarRename, Inserts).

record_decisions_in_goal(Goal0, Goal, VarInfo0, VarInfo, VarRename0, VarRename,
		InsertMap) :-
	Goal0 = if_then_else(Vars0, Cond0, Then0, Else0) - GoalInfo0,
	construct_anchors(ite, Goal0, StartAnchor, EndAnchor),
	rename_var_list(Vars0, no, VarRename0, Vars),
	record_decisions_in_goal(Cond0, Cond, VarInfo0, VarInfo1,
		VarRename0, VarRename1, InsertMap),
	record_decisions_in_goal(Then0, Then, VarInfo1, VarInfo2,
		VarRename1, _, InsertMap),
	lookup_inserts(InsertMap, StartAnchor, StartInserts),
	make_inserted_goals(VarInfo2, VarInfo3, map__init, VarRenameElse,
		StartInserts, StartInsertGoals),
	record_decisions_in_goal(Else0, Else1, VarInfo3, VarInfo4,
		VarRenameElse, _, InsertMap),
	Else0 = _ - ElseGoalInfo0,
	conj_list_to_goal(list__append(StartInsertGoals, [Else1]),
		ElseGoalInfo0, Else),
	Goal1 = if_then_else(Vars, Cond, Then, Else) - GoalInfo0,
	lookup_inserts(InsertMap, EndAnchor, EndInserts),
	insert_goals_after(Goal1, Goal, VarInfo4, VarInfo,
		VarRename, EndInserts).

record_decisions_in_goal(some(Vars0, CanRemove, Goal0) - GoalInfo,
		some(Vars, CanRemove, Goal) - GoalInfo, VarInfo0, VarInfo,
		VarRename0, VarRename, InsertMap) :-
	rename_var_list(Vars0, no, VarRename0, Vars),
	record_decisions_in_goal(Goal0, Goal, VarInfo0, VarInfo,
		VarRename0, VarRename, InsertMap).

record_decisions_in_goal(Goal0, Goal, VarInfo0, VarInfo,
		VarRename0, VarRename, InsertMap) :-
	Goal0 = generic_call(_,_,_,_) - _,
	record_decisions_at_call_site(Goal0, Goal, VarInfo0, VarInfo,
		VarRename0, VarRename, yes, InsertMap).

record_decisions_in_goal(Goal0, Goal, VarInfo0, VarInfo, VarRename0, VarRename,
		InsertMap) :-
	Goal0 = call(_, _, _, Builtin, _, _) - _,
	( Builtin = inline_builtin ->
		MustHaveMap = no
	;
		MustHaveMap = yes
	),
	record_decisions_at_call_site(Goal0, Goal, VarInfo0, VarInfo,
		VarRename0, VarRename, MustHaveMap, InsertMap).

record_decisions_in_goal(Goal0, Goal, VarInfo0, VarInfo, VarRename0, VarRename,
		InsertMap) :-
	Goal0 = foreign_proc(_,_,_,_,_,_,_) - _,
	record_decisions_at_call_site(Goal0, Goal, VarInfo0, VarInfo,
		VarRename0, VarRename, no, InsertMap).

record_decisions_in_goal(Goal0 - GoalInfo0, Goal - GoalInfo, VarInfo, VarInfo,
		VarRename, VarRename, _InsertMap) :-
	Goal0 = unify(_,_,_,_,_),
	rename_vars_in_goal(Goal0 - GoalInfo0, VarRename, Goal - GoalInfo).

record_decisions_in_goal(shorthand(_) - _, _, _, _, _, _, _) :-
	error("shorthand in record_decisions_in_goal").

%-----------------------------------------------------------------------------%

:- pred insert_goals_after(hlds_goal::in, hlds_goal::out,
	var_info::in, var_info::out, rename_map::out,
	list(insert_spec)::in) is det.

insert_goals_after(BranchesGoal, Goal, VarInfo0, VarInfo, VarRename,
		Inserts) :-
	make_inserted_goals(VarInfo0, VarInfo, map__init, VarRename,
		Inserts, InsertGoals),
	BranchesGoal = _ - BranchesGoalInfo,
	conj_list_to_goal([BranchesGoal | InsertGoals], BranchesGoalInfo,
		Goal).

:- pred make_inserted_goals(var_info::in, var_info::out,
	rename_map::in, rename_map::out, list(insert_spec)::in,
	list(hlds_goal)::out) is det.

make_inserted_goals(VarInfo, VarInfo, VarRename, VarRename, [], []).
make_inserted_goals(VarInfo0, VarInfo, VarRename0, VarRename,
		[Spec | Specs], [Goal | Goals]) :-
	make_inserted_goal(VarInfo0, VarInfo1, VarRename0, VarRename1,
		Spec, Goal),
	make_inserted_goals(VarInfo1, VarInfo, VarRename1, VarRename,
		Specs, Goals).

:- pred make_inserted_goal(var_info::in, var_info::out,
	rename_map::in, rename_map::out, insert_spec::in,
	hlds_goal::out) is det.

make_inserted_goal(VarInfo0, VarInfo, VarRename0, VarRename, Spec, Goal) :-
	Spec = insert_spec(Goal0, VarsToExtract),
	Goal0 = GoalExpr0 - GoalInfo0,
	(
		GoalExpr0 = unify(_, _, _, Unification0, _),
		Unification0 = deconstruct(_, _, ArgVars, _, _, _)
	->
		Unification1 = Unification0 ^ deconstruct_can_fail
			:= cannot_fail,
		GoalExpr1 = GoalExpr0 ^ unify_kind := Unification1,
		goal_info_set_determinism(GoalInfo0, det, GoalInfo1),
		goal_info_add_feature(GoalInfo1, stack_opt, GoalInfo2),
		Goal2 = GoalExpr1 - GoalInfo2,
		VarInfo0 = var_info(VarSet0, VarTypes0),
		create_shadow_vars(ArgVars, VarsToExtract, VarSet0, VarSet,
			VarTypes0, VarTypes, map__init, NewRename,
			map__init, VoidRename),
		VarInfo = var_info(VarSet, VarTypes),
		map__merge(VarRename0, NewRename, VarRename),
		% We rename the original goal with the 
		rename_vars_in_goal(Goal2, VarRename, Goal3),
		rename_vars_in_goal(Goal3, VoidRename, Goal)
	;
		error("make_inserted_goal: not a deconstruct")
	).

:- pred create_shadow_vars(list(prog_var)::in, set(prog_var)::in,
	prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
	rename_map::in, rename_map::out, rename_map::in, rename_map::out)
	is det.

create_shadow_vars([], _, VarSet, VarSet, VarTypes, VarTypes,
		VarRename, VarRename, VoidRename, VoidRename).
create_shadow_vars([Arg | Args], VarsToExtract, VarSet0, VarSet,
		VarTypes0, VarTypes, VarRename0, VarRename,
		VoidRename0, VoidRename) :-
	create_shadow_var(Arg, VarsToExtract, VarSet0, VarSet1,
		VarTypes0, VarTypes1, VarRename0, VarRename1,
		VoidRename0, VoidRename1),
	create_shadow_vars(Args, VarsToExtract, VarSet1, VarSet,
		VarTypes1, VarTypes, VarRename1, VarRename,
		VoidRename1, VoidRename).

:- pred create_shadow_var(prog_var::in, set(prog_var)::in,
	prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
	rename_map::in, rename_map::out, rename_map::in, rename_map::out)
	is det.

create_shadow_var(Arg, VarsToExtract, VarSet0, VarSet, VarTypes0, VarTypes,
		VarRename0, VarRename, VoidRename0, VoidRename) :-
	varset__lookup_name(VarSet0, Arg, Name),
	varset__new_named_var(VarSet0, Name, Shadow, VarSet),
	map__lookup(VarTypes0, Arg, Type),
	map__det_insert(VarTypes0, Shadow, Type, VarTypes),
	( set__member(Arg, VarsToExtract) ->
		map__det_insert(VarRename0, Arg, Shadow, VarRename),
		VoidRename = VoidRename0
	;
		VarRename = VarRename0,
		map__det_insert(VoidRename0, Arg, Shadow, VoidRename)
	).

%-----------------------------------------------------------------------------%

:- pred record_decisions_at_call_site(hlds_goal::in, hlds_goal::out,
	var_info::in, var_info::out, rename_map::in, rename_map::out,
	bool::in, insert_map::in) is det.

record_decisions_at_call_site(Goal0, Goal, VarInfo0, VarInfo,
		VarRename0, VarRename, MustHaveMap, InsertMap) :-
	Goal0 = _ - GoalInfo0,
	rename_vars_in_goal(Goal0, VarRename0, Goal1),
	(
		goal_info_maybe_get_maybe_need_across_call(GoalInfo0,
			MaybeNeedAcrossCall),
		MaybeNeedAcrossCall = yes(_NeedAcrossCall)
	->
		goal_info_get_goal_path(GoalInfo0, GoalPath),
		Anchor = call_site(GoalPath),
		lookup_inserts(InsertMap, Anchor, Inserts),
		insert_goals_after(Goal1, Goal, VarInfo0, VarInfo,
			VarRename, Inserts)
	;
		(
			MustHaveMap = no,
			Goal = Goal1,
			VarInfo = VarInfo0,
			VarRename = VarRename0
		;
			MustHaveMap = yes,
			error("record_decisions_at_call_site: no save map")
		)
	).

%-----------------------------------------------------------------------------%

:- pred record_decisions_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
	var_info::in, var_info::out, rename_map::in, rename_map::out,
	insert_map::in) is det.

record_decisions_in_conj([], [], VarInfo, VarInfo, VarRename, VarRename, _).
record_decisions_in_conj([Goal0 | Goals0], Goals, VarInfo0, VarInfo,
		VarRename0, VarRename, InsertMap) :-
	record_decisions_in_goal(Goal0, Goal1, VarInfo0, VarInfo1,
		VarRename0, VarRename1, InsertMap),
	record_decisions_in_conj(Goals0, Goals1, VarInfo1, VarInfo,
		VarRename1, VarRename, InsertMap),
	( Goal1 = conj(SubGoals) - _ ->
		Goals = list__append(SubGoals, Goals1)
	;
		Goals = [Goal1 | Goals1]
	).

:- pred record_decisions_in_par_conj(list(hlds_goal)::in, list(hlds_goal)::out,
	var_info::in, var_info::out, rename_map::in, insert_map::in) is det.

record_decisions_in_par_conj([], [], VarInfo, VarInfo, _, _).
record_decisions_in_par_conj([Goal0 | Goals0], [Goal | Goals],
		VarInfo0, VarInfo, VarRename0, InsertMap) :-
	record_decisions_in_goal(Goal0, Goal, VarInfo0, VarInfo1,
		VarRename0, _, InsertMap),
	record_decisions_in_par_conj(Goals0, Goals, VarInfo1, VarInfo,
		VarRename0, InsertMap).

:- pred record_decisions_in_disj(list(hlds_goal)::in, list(hlds_goal)::out,
	var_info::in, var_info::out, rename_map::in, list(insert_spec)::in,
	insert_map::in) is det.

record_decisions_in_disj([], [], VarInfo, VarInfo, _, _, _).
record_decisions_in_disj([Goal0 | Goals0], [Goal | Goals], VarInfo0, VarInfo,
		VarRename0, Inserts, InsertMap) :-
	make_inserted_goals(VarInfo0, VarInfo1, map__init, VarRename1,
		Inserts, InsertGoals),
	Goal0 = _ - GoalInfo0,
	record_decisions_in_goal(Goal0, Goal1, VarInfo1, VarInfo2,
		VarRename1, _, InsertMap),
	conj_list_to_goal(list__append(InsertGoals, [Goal1]),
		GoalInfo0, Goal),
	record_decisions_in_disj(Goals0, Goals, VarInfo2, VarInfo,
		VarRename0, Inserts, InsertMap).

:- pred record_decisions_in_cases(list(case)::in, list(case)::out,
	var_info::in, var_info::out, rename_map::in, insert_map::in) is det.

record_decisions_in_cases([], [], VarInfo, VarInfo, _, _).
record_decisions_in_cases([case(Var, Goal0) | Cases0],
		[case(Var, Goal) | Cases], VarInfo0, VarInfo,
		VarRename0, InsertMap) :-
	record_decisions_in_goal(Goal0, Goal, VarInfo0, VarInfo1,
		VarRename0, _, InsertMap),
	record_decisions_in_cases(Cases0, Cases, VarInfo1, VarInfo,
		VarRename0, InsertMap).

%-----------------------------------------------------------------------------%

% The final RenameMap may ask for some of the head variables to be renamed.
% Doing so is inconvenient, e.g. because the debugger wants head variables to
% have names of a fixed form. Instead, we exploit the fact that the
% transformation does not care about actual variable names or even numbers;
% all it cares about wrt renaming is that the variables it has renamed apart
% should stay renamed apart. We therefore swap the roles of the original and
% the renamed variable in the goal representing the procedure body. The
% resulting procedure definition will be isomorphic to the one we would have
% get by applying the original renaming to the headvars.

:- pred apply_headvar_correction(set(prog_var)::in, rename_map::in,
	hlds_goal::in, hlds_goal::out) is det.

apply_headvar_correction(HeadVarSet, RenameMap, Goal0, Goal) :-
	set__to_sorted_list(HeadVarSet, HeadVars),
	build_headvar_subst(HeadVars, RenameMap, map__init, Subst),
	( map__is_empty(Subst) ->
		Goal = Goal0
	;
		goal_util__rename_vars_in_goal(Goal0, Subst, Goal)
	).

:- pred build_headvar_subst(list(prog_var)::in, rename_map::in,
	map(prog_var, prog_var)::in, map(prog_var, prog_var)::out) is det.

build_headvar_subst([], _RenameMap, Subst, Subst).
build_headvar_subst([HeadVar | HeadVars], RenameMap, Subst0, Subst) :-
	( map__search(RenameMap, HeadVar, Replacement) ->
		map__det_insert(Subst0, Replacement, HeadVar, Subst1),
		map__det_insert(Subst1, HeadVar, Replacement, Subst2)
	;
		Subst2 = Subst0
	),
	build_headvar_subst(HeadVars, RenameMap, Subst2, Subst).

%-----------------------------------------------------------------------------%

:- pred construct_anchors(branch_construct::in, hlds_goal::in,
	anchor::out, anchor::out) is det.

construct_anchors(Construct, Goal, StartAnchor, EndAnchor) :-
	Goal = _ - GoalInfo,
	goal_info_get_goal_path(GoalInfo, GoalPath),
	StartAnchor = branch_start(Construct, GoalPath),
	EndAnchor = branch_end(Construct, GoalPath).

%-----------------------------------------------------------------------------%

% This predicate can help debug the correctness of the transformation.

:- pred maybe_write_progress_message(string::in, int::in, int::in,
	proc_info::in, module_info::in, io__state::di, io__state::uo) is det.

maybe_write_progress_message(Message, DebugStackOpt, PredIdInt, ProcInfo,
		ModuleInfo) -->
	( { DebugStackOpt = PredIdInt } ->
		io__write_string(Message),
		io__write_string(":\n"),
		{ proc_info_goal(ProcInfo, Goal) },
		{ proc_info_varset(ProcInfo, VarSet) },
		hlds_out__write_goal(Goal, ModuleInfo, VarSet, yes, 0, "\n"),
		io__write_string("\n")
	;
		[]
	).

%-----------------------------------------------------------------------------%

% These predicates can help debug the performance of the transformation.

:- pred dump_opt_info(opt_info::in, io__state::di, io__state::uo) is det.

dump_opt_info(OptInfo) -->
	{ map__keys(OptInfo ^ interval_start, StartIds) },
	{ map__keys(OptInfo ^ interval_end, EndIds) },
	{ map__keys(OptInfo ^ interval_vars, VarsIds) },
	{ map__keys(OptInfo ^ interval_succ, SuccIds) },
	{ list__condense([StartIds, EndIds, VarsIds, SuccIds], IntervalIds0) },
	{ list__sort_and_remove_dups(IntervalIds0, IntervalIds) },
	io__write_string("INTERVALS:\n"),
	list__foldl(dump_interval_info(OptInfo), IntervalIds),

	{ map__to_assoc_list(OptInfo ^ anchor_follow_map, AnchorFollows) },
	io__write_string("\nANCHOR FOLLOW:\n"),
	list__foldl(dump_anchor_follow, AnchorFollows),

	{ map__to_assoc_list(OptInfo ^ left_anchor_inserts, Inserts) },
	io__write_string("\nANCHOR INSERT:\n"),
	list__foldl(dump_anchor_inserts, Inserts),

	io__write_string("\nMATCHING RESULTS:\n"),
	list__foldl(dump_matching_result, OptInfo ^ matching_results),
	io__write_string("\n").

:- pred dump_interval_info(opt_info::in, interval_id::in,
	io__state::di, io__state::uo) is det.

dump_interval_info(OptInfo, IntervalId) -->
	io__write_string("\ninterval "),
	io__write_int(interval_id_to_int(IntervalId)),
	io__write_string(": "),
	( { map__search(OptInfo ^ interval_succ, IntervalId, SuccIds) } ->
		{ SuccNums = list__map(interval_id_to_int, SuccIds) },
		io__write_string("succ ["),
		write_int_list(SuccNums),
		io__write_string("]\n")
	;
		io__write_string("no succ\n")
	),
	( { map__search(OptInfo ^ interval_start, IntervalId, Start) } ->
		io__write_string("start "),
		io__write(Start),
		io__write_string("\n")
	;
		io__write_string("no start\n")
	),
	( { map__search(OptInfo ^ interval_end, IntervalId, End) } ->
		io__write_string("end "),
		io__write(End),
		io__write_string("\n")
	;
		io__write_string("no end\n")
	),
	( { map__search(OptInfo ^ interval_vars, IntervalId, Vars) } ->
		{ list__map(term__var_to_int, set__to_sorted_list(Vars),
			VarNums) },
		io__write_string("vars ["),
		write_int_list(VarNums),
		io__write_string("]\n")
	;
		io__write_string("no vars\n")
	),
	( { map__search(OptInfo ^ interval_delvars, IntervalId, Deletions) } ->
		io__write_string("deletions"),
		list__foldl(dump_deletion, Deletions),
		io__write_string("\n")
	;
		[]
	).

:- pred dump_deletion(set(prog_var)::in, io__state::di, io__state::uo) is det.

dump_deletion(Vars) -->
	{ list__map(term__var_to_int, set__to_sorted_list(Vars), VarNums) },
	io__write_string(" ["),
	write_int_list(VarNums),
	io__write_string("]").

:- pred dump_anchor_follow(pair(anchor, anchor_follow_info)::in,
	io__state::di, io__state::uo) is det.

dump_anchor_follow(Anchor - AnchorFollowInfo) -->
	{ AnchorFollowInfo = Vars - Intervals },
	io__write_string("\n"),
	io__write(Anchor),
	io__write_string(" =>\n"),
	{ list__map(term__var_to_int, set__to_sorted_list(Vars), VarNums) },
	io__write_string("vars ["),
	write_int_list(VarNums),
	io__write_string("]\nintervals: "),
	{ set__to_sorted_list(Intervals, IntervalList) },
	write_int_list(list__map(interval_id_to_int, IntervalList)),
	io__write_string("\n").

:- pred dump_anchor_inserts(pair(anchor, list(insert_spec))::in,
	io__state::di, io__state::uo) is det.

dump_anchor_inserts(Anchor - InsertSpecs) -->
	io__write_string("\ninsertions after "),
	io__write(Anchor),
	io__write_string(":\n"),
	list__foldl(dump_insert, InsertSpecs).

:- pred dump_insert(insert_spec::in, io__state::di, io__state::uo) is det.

dump_insert(insert_spec(Goal, Vars)) -->
	{ list__map(term__var_to_int, set__to_sorted_list(Vars), VarNums) },
	io__write_string("vars ["),
	write_int_list(VarNums),
	io__write_string("]: "),
	(
		{ Goal = unify(_, _, _, Unification, _) - _ },
		{ Unification = deconstruct(CellVar, ConsId, ArgVars, _,_,_) }
	->
		{ term__var_to_int(CellVar, CellVarNum) },
		io__write_int(CellVarNum),
		io__write_string(" => "),
		mercury_output_cons_id(ConsId, does_not_need_brackets),
		io__write_string("("),
		{ list__map(term__var_to_int, ArgVars, ArgVarNums) },
		write_int_list(ArgVarNums),
		io__write_string(")\n")
	;
		io__write_string("BAD INSERT GOAL\n")
	).

:- pred dump_matching_result(matching_result::in,
	io__state::di, io__state::uo) is det.

dump_matching_result(MatchingResult) -->
	{ MatchingResult = matching_result(CellVar, ConsId,
		ArgVars, ViaCellVars, GoalPath,
		PotentialIntervals, InsertIntervals,
		PotentialAnchors, InsertAnchors) },
	io__write_string("\nmatching result at "),
	io__write(GoalPath),
	io__write_string("\n"),
	{ term__var_to_int(CellVar, CellVarNum) },
	{ list__map(term__var_to_int, ArgVars, ArgVarNums) },
	{ list__map(term__var_to_int, set__to_sorted_list(ViaCellVars),
		ViaCellVarNums) },
	io__write_int(CellVarNum),
	io__write_string(" => "),
	mercury_output_cons_id(ConsId, does_not_need_brackets),
	io__write_string("("),
	write_int_list(ArgVarNums),
	io__write_string("): via cell "),
	write_int_list(ViaCellVarNums),
	io__write_string("\n"),

	io__write_string("potential intervals: "),
	{ PotentialIntervalNums = list__map(interval_id_to_int,
		set__to_sorted_list(PotentialIntervals)) },
	write_int_list(PotentialIntervalNums),
	io__write_string("\n"),
	io__write_string("insert intervals: "),
	{ InsertIntervalNums = list__map(interval_id_to_int,
		set__to_sorted_list(InsertIntervals)) },
	write_int_list(InsertIntervalNums),
	io__write_string("\n"),

	io__write_string("potential anchors: "),
	io__write_list(set__to_sorted_list(PotentialAnchors), " ", io__write),
	io__write_string("\n"),
	io__write_string("insert anchors: "),
	io__write_list(set__to_sorted_list(InsertAnchors), " ", io__write),
	io__write_string("\n").

:- pred write_int_list(list(int)::in, io__state::di, io__state::uo) is det.

write_int_list(List) --> io__write_list(List, ", ", io__write_int).

:- func interval_id_to_int(interval_id) = int.

interval_id_to_int(interval_id(Num)) = Num.

%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File interval.
%
% Author: zs.
%
% This module contains a predicate to build up interval information for a
% procedure; in particular the start and end points of intervals and the set
% of variables needed in that interval.  It also contains a procedure to
% insert deconstruction unifications into a goal, given a map of insertions
% to make after particular anchors.  More detailed information is in
% stack_opt.m, from where this code was extracted.
%
% A description of intervals is in the paper "Using the heap to eliminate
% stack accesses" by Zoltan Somogyi and Peter Stuckey:
%
%    Definition 3: An interval is a sequence of atomic goals delimited by a
%    left-right pair of anchors, satisfying the property that if forward
%    execution starts at the left anchor and continues without encountering
%    failure (which would initiate backtracking, i.e. backward execution),
%    the next anchor it reaches is the right anchor of the pair.  We
%    consider a call to be part of the atomic goals of the interval only if
%    the call site is the right anchor of the interval, not the left anchor.
%
%-----------------------------------------------------------------------------%

:- module backend_libs__interval.

:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module parse_tree__prog_data.

:- import_module bool.
:- import_module counter.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.

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

:- type interval_params --->
	interval_params(
		module_info		:: module_info,
		var_types		:: vartypes,
		at_most_zero_calls	:: bool
	).

:- type interval_info --->
	interval_info(
		interval_params		:: interval_params,
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
		interval_start		:: map(interval_id, anchor),
		interval_end		:: map(interval_id, anchor),
		interval_succ		:: map(interval_id, list(interval_id)),
		interval_vars		:: map(interval_id, set(prog_var)),
		interval_delvars	:: map(interval_id,
						list(set(prog_var)))
	).

:- type maybe_needs_flush
	--->	needs_flush
	;	doesnt_need_flush.

:- typeclass build_interval_info_acc(T) where [
        pred use_cell(prog_var::in, list(prog_var)::in, cons_id::in,
                hlds_goal::in, interval_info::in, interval_info::out,
		T::in, T::out) is det
].

:- pred build_interval_info_in_goal(hlds_goal::in, interval_info::in,
	interval_info::out, T::in, T::out) is det
	<= build_interval_info_acc(T).

:- pred record_interval_vars(interval_id::in, list(prog_var)::in,
	interval_info::in, interval_info::out) is det.

:- pred delete_interval_vars(interval_id::in, set(prog_var)::in,
	set(prog_var)::out, interval_info::in, interval_info::out) is det.

:- type rename_map	==	map(prog_var, prog_var).

:- pred record_decisions_in_goal(hlds_goal::in, hlds_goal::out,
	prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
	rename_map::in, rename_map::out, insert_map::in,
	maybe(goal_feature)::in) is det.

:- pred make_inserted_goal(prog_varset::in, prog_varset::out,
	vartypes::in, vartypes::out, rename_map::in, rename_map::out,
	insert_spec::in, maybe(goal_feature)::in, hlds_goal::out) is det.

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

:- pred dump_interval_info(interval_info::in, io::di, io::uo) is det.

:- pred write_int_list(list(int)::in, io::di, io::uo) is det.

:- func interval_id_to_int(interval_id) = int.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__inst_match.
:- import_module check_hlds__mode_util.
:- import_module hlds__arg_info.
:- import_module hlds__code_model.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_llds.
:- import_module hlds__hlds_out.
:- import_module hlds__instmap.
:- import_module hlds__quantification.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module ll_backend.
:- import_module ll_backend__call_gen.
:- import_module ll_backend__liveness.
:- import_module ll_backend__live_vars.
:- import_module ll_backend__store_alloc.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_type.

:- import_module assoc_list.
:- import_module require.
:- import_module svmap.
:- import_module svset.
:- import_module svvarset.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

build_interval_info_in_goal(conj(Goals) - _GoalInfo, !IntervalInfo, !Acc) :-
	build_interval_info_in_conj(Goals, !IntervalInfo, !Acc).

build_interval_info_in_goal(par_conj(Goals) - _GoalInfo, !IntervalInfo,
		!Acc) :-
	build_interval_info_in_par_conj(Goals, !IntervalInfo, !Acc).

build_interval_info_in_goal(disj(Goals) - GoalInfo, !IntervalInfo, !Acc) :-
	( Goals = [FirstDisjunct | _] ->
		reached_branch_end(GoalInfo, yes(FirstDisjunct), disj,
			StartAnchor, EndAnchor, BeforeId, AfterId,
			MaybeResumeVars, !IntervalInfo, !Acc),
		build_interval_info_in_disj(Goals, doesnt_need_flush,
			StartAnchor, EndAnchor, BeforeId, AfterId,
			OpenIntervals, !IntervalInfo, !Acc),
		leave_branch_start(disj, StartAnchor, BeforeId,
			MaybeResumeVars, OpenIntervals, !IntervalInfo)
	;
		% We could reset the set of variables in the current interval
		% to the empty set, since any variable accesses after a fail
		% goal (which is what an empty disjunction represent) will not
		% be executed at runtime. However, simplify should have removed
		% any goals in the current branch from after the fail, so the
		% set of variables in the current interval will already be
		% the empty set.
		no_open_intervals(!IntervalInfo)
	).

build_interval_info_in_goal(switch(Var, _Det, Cases) - GoalInfo,
		!IntervalInfo, !Acc) :-
	reached_branch_end(GoalInfo, no, switch,
		StartAnchor, EndAnchor, BeforeId, AfterId, MaybeResumeVars,
		!IntervalInfo, !Acc),
	build_interval_info_in_cases(Cases, StartAnchor, EndAnchor,
		BeforeId, AfterId, OpenIntervalsList, !IntervalInfo, !Acc),
	OpenIntervals = set__union_list(OpenIntervalsList),
	leave_branch_start(switch, StartAnchor, BeforeId, MaybeResumeVars,
		OpenIntervals, !IntervalInfo),
	require_in_regs([Var], !IntervalInfo),
	require_access([Var], !IntervalInfo).

build_interval_info_in_goal(not(Goal) - GoalInfo, !IntervalInfo, !Acc) :-
	reached_branch_end(GoalInfo, yes(Goal), neg,
		StartAnchor, EndAnchor, BeforeId, AfterId, MaybeResumeVars,
		!IntervalInfo, !Acc),
	enter_branch_tail(EndAnchor, AfterId, !IntervalInfo),
	build_interval_info_in_goal(Goal, !IntervalInfo, !Acc),
	reached_branch_start(needs_flush, StartAnchor, BeforeId,
		OpenIntervals, !IntervalInfo, !Acc),
	leave_branch_start(neg, StartAnchor, BeforeId, MaybeResumeVars,
		OpenIntervals, !IntervalInfo).

build_interval_info_in_goal(if_then_else(_, Cond, Then, Else) - GoalInfo,
                !IntervalInfo, !Acc) :-
	reached_branch_end(GoalInfo, yes(Cond), ite, StartAnchor, EndAnchor,
		BeforeId, AfterId, MaybeResumeVars, !IntervalInfo, !Acc),
	enter_branch_tail(EndAnchor, AfterId, !IntervalInfo),
	build_interval_info_in_goal(Then, !IntervalInfo, !Acc),
	reached_cond_then(GoalInfo, !IntervalInfo),
	build_interval_info_in_goal(Cond, !IntervalInfo, !Acc),
	reached_branch_start(doesnt_need_flush, StartAnchor, BeforeId,
		CondOpenIntervals, !IntervalInfo, !Acc),
	enter_branch_tail(EndAnchor, AfterId, !IntervalInfo),
	build_interval_info_in_goal(Else, !IntervalInfo, !Acc),
	reached_branch_start(needs_flush, StartAnchor, BeforeId,
		_ElseOpenIntervals, !IntervalInfo, !Acc),
	leave_branch_start(ite, StartAnchor, BeforeId, MaybeResumeVars,
		CondOpenIntervals, !IntervalInfo).

build_interval_info_in_goal(scope(_Reason, Goal) - _GoalInfo, !IntervalInfo,
		!Acc) :-
	build_interval_info_in_goal(Goal, !IntervalInfo, !Acc).

build_interval_info_in_goal(Goal - GoalInfo, !IntervalInfo, !Acc) :-
	Goal = generic_call(GenericCall, ArgVars, ArgModes, Detism),
	goal_info_get_maybe_need_across_call(GoalInfo, MaybeNeedAcrossCall),
	IntParams = !.IntervalInfo ^ interval_params,
	VarTypes = IntParams ^ var_types,
	list__map(map__lookup(VarTypes), ArgVars, ArgTypes),
	ModuleInfo = IntParams ^ module_info,
	arg_info__compute_in_and_out_vars(ModuleInfo, ArgVars,
		ArgModes, ArgTypes, InputArgs, _OutputArgs),
	determinism_to_code_model(Detism, CodeModel),

	% unsafe_casts are generated inline.
	( GenericCall = unsafe_cast ->
		require_in_regs(InputArgs, !IntervalInfo),
		require_access(InputArgs, !IntervalInfo)
	;
		call_gen__generic_call_info(CodeModel, GenericCall, _,
			GenericVarsArgInfos, _),
		assoc_list__keys(GenericVarsArgInfos, GenericVars),
		list__append(GenericVars, InputArgs, Inputs),
		build_interval_info_at_call(Inputs,
                        MaybeNeedAcrossCall, GoalInfo, !IntervalInfo, !Acc)
	).

build_interval_info_in_goal(Goal - GoalInfo, !IntervalInfo, !Acc) :-
	Goal = call(PredId, ProcId, ArgVars, Builtin, _, _),
	IntParams = !.IntervalInfo ^ interval_params,
	ModuleInfo = IntParams ^ module_info,
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		_PredInfo, ProcInfo),
	VarTypes = IntParams ^ var_types,
	arg_info__partition_proc_call_args(ProcInfo, VarTypes,
		ModuleInfo, ArgVars, InputArgs, _, _),
	set__to_sorted_list(InputArgs, Inputs),
	( Builtin = inline_builtin ->
		require_in_regs(Inputs, !IntervalInfo),
		require_access(Inputs, !IntervalInfo)
	;
		goal_info_get_maybe_need_across_call(GoalInfo,
			MaybeNeedAcrossCall),
		build_interval_info_at_call(Inputs, MaybeNeedAcrossCall,
			GoalInfo, !IntervalInfo, !Acc)
	).

build_interval_info_in_goal(Goal - GoalInfo, !IntervalInfo, !Acc) :-
	Goal = foreign_proc(_Attributes, PredId, ProcId, Args, ExtraArgs,
		_PragmaCode),
	IntParams = !.IntervalInfo ^ interval_params,
	ModuleInfo = IntParams ^ module_info,
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		_PredInfo, ProcInfo),
	VarTypes = IntParams ^ var_types,
	ArgVars = list__map(foreign_arg_var, Args),
	ExtraVars = list__map(foreign_arg_var, ExtraArgs),
	arg_info__partition_proc_call_args(ProcInfo, VarTypes,
		ModuleInfo, ArgVars, InputArgVarSet, _, _),
	set__to_sorted_list(InputArgVarSet, InputArgVars),
	list__append(InputArgVars, ExtraVars, InputVars),
	(
		goal_info_maybe_get_maybe_need_across_call(GoalInfo,
			MaybeNeedAcrossCall),
		MaybeNeedAcrossCall = yes(_)
	->
		build_interval_info_at_call(InputVars, MaybeNeedAcrossCall,
			GoalInfo, !IntervalInfo, !Acc)
	;
		require_in_regs(InputVars, !IntervalInfo),
		require_access(InputVars, !IntervalInfo)
	).

build_interval_info_in_goal(Goal - GoalInfo, !IntervalInfo, !Acc) :-
	Goal = unify(_, _, _, Unification, _),
	(
		Unification = construct(CellVar, _ConsId, ArgVars, _,
			HowToConstruct, _, _),
		( HowToConstruct = reuse_cell(_) ->
			error("build_interval_info_in_goal: reuse")
		;
			true
		),
		require_in_regs(ArgVars, !IntervalInfo),
		require_access([CellVar | ArgVars], !IntervalInfo)
		% use_cell(CellVar, ArgVars, ConsId, Goal - GoalInfo,
		%	!IntervalInfo)
		% We cannot use such cells, because some of the ArgVars
		% may need to be saved on the stack before this construction.
	;
		Unification = deconstruct(CellVar, ConsId, ArgVars,
			ArgModes, _, _),
		IntParams = !.IntervalInfo ^ interval_params,
		ModuleInfo = IntParams ^ module_info,
		( shared_left_to_right_deconstruct(ModuleInfo, ArgModes) ->
                        use_cell(CellVar, ArgVars, ConsId, Goal - GoalInfo,
                                !IntervalInfo, !Acc)
		;
			true
		),
		require_in_regs([CellVar], !IntervalInfo),
		require_access([CellVar | ArgVars], !IntervalInfo)
	;
		Unification = assign(ToVar, FromVar),
		require_in_regs([FromVar], !IntervalInfo),
		require_access([FromVar, ToVar], !IntervalInfo)
	;
		Unification = simple_test(Var1, Var2),
		require_in_regs([Var1, Var2], !IntervalInfo),
		require_access([Var1, Var2], !IntervalInfo)
	;
		Unification = complicated_unify(_, _, _),
		error("build_interval_info_in_goal: complicated_unify")
	).

build_interval_info_in_goal(shorthand(_) - _, !IntervalInfo, !Acc) :-
	error("shorthand in build_interval_info_in_goal").

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

:- pred build_interval_info_at_call(list(prog_var)::in,
	maybe(need_across_call)::in, hlds_goal_info::in,
	interval_info::in, interval_info::out, T::in, T::out) is det
	<= build_interval_info_acc(T).

build_interval_info_at_call(Inputs, MaybeNeedAcrossCall, GoalInfo,
		!IntervalInfo, !Acc) :-
	(
		MaybeNeedAcrossCall = yes(NeedAcrossCall),
		NeedAcrossCall = need_across_call(ForwardVars,
			ResumeVars, NondetLiveVars),
		VarsOnStack0 = set__union_list([ForwardVars, ResumeVars,
			NondetLiveVars]),
		goal_info_get_goal_path(GoalInfo, GoalPath),
		CallAnchor = call_site(GoalPath),
		get_cur_interval(AfterCallId, !.IntervalInfo),
		new_interval_id(BeforeCallId, !IntervalInfo),
		record_interval_start(AfterCallId, CallAnchor, !IntervalInfo),
		record_interval_end(BeforeCallId, CallAnchor, !IntervalInfo),
		goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
		IntParams = !.IntervalInfo ^ interval_params,
		(
			( instmap_delta_is_reachable(InstMapDelta)
			; IntParams ^ at_most_zero_calls = no
			)
		->
			record_interval_succ(BeforeCallId, AfterCallId,
				!IntervalInfo),
			VarsOnStack = VarsOnStack0
		;
			% If the call cannot succeed, then execution cannot
			% get from BeforeCallId to AfterCallId.
			record_interval_no_succ(BeforeCallId, !IntervalInfo),
			VarsOnStack = set__init
		),
		set_cur_interval(BeforeCallId, !IntervalInfo),
		assign_open_intervals_to_anchor(CallAnchor, !IntervalInfo),
		goal_info_get_code_model(GoalInfo, CodeModel),
		( CodeModel = model_non ->
			record_model_non_anchor(CallAnchor, !IntervalInfo)
		;
			true
		),
		one_open_interval(BeforeCallId, !IntervalInfo),
		require_flushed(VarsOnStack, !IntervalInfo),
		require_in_regs(Inputs, !IntervalInfo),
		require_access(Inputs, !IntervalInfo)
	;
		MaybeNeedAcrossCall = no,
		error("build_interval_info_at_call: no need across call")
	).

%-----------------------------------------------------------------------------%

:- pred build_interval_info_in_conj(list(hlds_goal)::in,
	interval_info::in, interval_info::out, T::in, T::out) is det
	<= build_interval_info_acc(T).

build_interval_info_in_conj([], !IntervalInfo, !Acc).
build_interval_info_in_conj([Goal | Goals], !IntervalInfo, !Acc) :-
	build_interval_info_in_conj(Goals, !IntervalInfo, !Acc),
	build_interval_info_in_goal(Goal, !IntervalInfo, !Acc).

:- pred build_interval_info_in_par_conj(list(hlds_goal)::in,
	interval_info::in, interval_info::out, T::in, T::out) is det
	<= build_interval_info_acc(T).

build_interval_info_in_par_conj([], !IntervalInfo, !Acc).
build_interval_info_in_par_conj([Goal | Goals], !IntervalInfo, !Acc) :-
	% XXX zs: I am not sure that passing interval_info from the first goal
	% to the rest is OK. Maybe we should pass the initial interval_info to
	% all the conjuncts, and then merge the resulting interval_infos.
	build_interval_info_in_par_conj(Goals, !IntervalInfo, !Acc),
	build_interval_info_in_goal(Goal, !IntervalInfo, !Acc).

:- pred build_interval_info_in_disj(list(hlds_goal)::in, maybe_needs_flush::in,
	anchor::in, anchor::in, interval_id::in, interval_id::in,
	set(interval_id)::out, interval_info::in, interval_info::out,
	T::in, T::out) is det <= build_interval_info_acc(T).

build_interval_info_in_disj([], _, _, _, _, _, set__init, !IntervalInfo, !Acc).
build_interval_info_in_disj([Goal | Goals], MaybeNeedsFlush,
		StartAnchor, EndAnchor, BeforeId, AfterId, OpenIntervals,
                !IntervalInfo, !Acc) :-
	enter_branch_tail(EndAnchor, AfterId, !IntervalInfo),
	build_interval_info_in_goal(Goal, !IntervalInfo, !Acc),
	reached_branch_start(MaybeNeedsFlush, StartAnchor, BeforeId,
		OpenIntervals, !IntervalInfo, !Acc),
	build_interval_info_in_disj(Goals, needs_flush, StartAnchor, EndAnchor,
		BeforeId, AfterId, _OpenIntervals, !IntervalInfo, !Acc).

:- pred build_interval_info_in_cases(list(case)::in,
	anchor::in, anchor::in, interval_id::in, interval_id::in,
	list(set(interval_id))::out, interval_info::in, interval_info::out, 
        T::in, T::out) is det <= build_interval_info_acc(T).

build_interval_info_in_cases([], _, _, _, _, [], !IntervalInfo, !Acc).
build_interval_info_in_cases([case(_Var, Goal) | Cases],
		StartAnchor, EndAnchor, BeforeId, AfterId,
		[OpenIntervals | OpenIntervalsList], !IntervalInfo, !Acc) :-
	enter_branch_tail(EndAnchor, AfterId, !IntervalInfo),
	build_interval_info_in_goal(Goal, !IntervalInfo, !Acc),
	reached_branch_start(doesnt_need_flush, StartAnchor, BeforeId,
		OpenIntervals, !IntervalInfo, !Acc),
	build_interval_info_in_cases(Cases, StartAnchor, EndAnchor,
		BeforeId, AfterId, OpenIntervalsList, !IntervalInfo, !Acc).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred reached_branch_end(hlds_goal_info::in, maybe(hlds_goal)::in,
	branch_construct::in, anchor::out, anchor::out,
	interval_id::out, interval_id::out, maybe(set(prog_var))::out,
	interval_info::in, interval_info::out, T::in, T::out) is det
	<= build_interval_info_acc(T).

reached_branch_end(GoalInfo, MaybeResumeGoal, Construct,
		StartAnchor, EndAnchor, BeforeIntervalId, AfterIntervalId,
		MaybeResumeVars, !IntervalInfo, !Acc) :-
	goal_info_get_goal_path(GoalInfo, GoalPath),
	record_branch_end_info(GoalPath, !IntervalInfo),
	(
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
	),
	record_branch_resume(GoalPath, HasResumeSave, !IntervalInfo),
	( goal_info_maybe_get_store_map(GoalInfo, StoreMap) ->
		map__sorted_keys(StoreMap, StoreMapVarList),
		set__sorted_list_to_set(StoreMapVarList, StoreMapVars),
		require_flushed(StoreMapVars, !IntervalInfo)
	;
		error("reached_branch_end: no store map")
	),
	EndAnchor = branch_end(Construct, GoalPath),
	StartAnchor = branch_start(Construct, GoalPath),
	assign_open_intervals_to_anchor(EndAnchor, !IntervalInfo),
	goal_info_get_code_model(GoalInfo, CodeModel),
	( CodeModel = model_non ->
		record_model_non_anchor(EndAnchor, !IntervalInfo)
	;
		true
	),
	no_open_intervals(!IntervalInfo),
	get_cur_interval(AfterIntervalId, !.IntervalInfo),
	record_interval_start(AfterIntervalId, EndAnchor, !IntervalInfo),
	new_interval_id(BeforeIntervalId, !IntervalInfo).

:- pred enter_branch_tail(anchor::in, interval_id::in,
	interval_info::in, interval_info::out) is det.

enter_branch_tail(EndAnchor, AfterId, !IntervalInfo) :-
	new_interval_id(BranchTailId, !IntervalInfo),
	record_interval_end(BranchTailId, EndAnchor, !IntervalInfo),
	record_interval_succ(BranchTailId, AfterId, !IntervalInfo),
	set_cur_interval(BranchTailId, !IntervalInfo),
	one_open_interval(BranchTailId, !IntervalInfo).

:- pred reached_branch_start(maybe_needs_flush::in, anchor::in,
	interval_id::in, set(interval_id)::out, interval_info::in,
	interval_info::out, T::in, T::out) is det
	<= build_interval_info_acc(T).

reached_branch_start(MaybeNeedsFlush, StartAnchor, BeforeId, OpenIntervals,
		!IntervalInfo, !Acc) :-
	get_cur_interval(BranchStartId, !.IntervalInfo),
	record_interval_start(BranchStartId, StartAnchor, !IntervalInfo),
	record_interval_succ(BeforeId, BranchStartId, !IntervalInfo),
	get_open_intervals(!.IntervalInfo, OpenIntervals),
	(
		MaybeNeedsFlush = doesnt_need_flush
	;
		MaybeNeedsFlush = needs_flush,
		assign_open_intervals_to_anchor(StartAnchor, !IntervalInfo)
	).

:- pred reached_cond_then(hlds_goal_info::in, interval_info::in,
	interval_info::out) is det.

reached_cond_then(GoalInfo, !IntervalInfo) :-
	goal_info_get_goal_path(GoalInfo, GoalPath),
	record_cond_end(GoalPath, !IntervalInfo),
	get_cur_interval(ThenStartId, !.IntervalInfo),
	record_interval_start(ThenStartId, CondThenAnchor, !IntervalInfo),
	new_interval_id(CondTailId, !IntervalInfo),
	CondThenAnchor = cond_then(GoalPath),
	record_interval_end(CondTailId, CondThenAnchor, !IntervalInfo),
	record_interval_succ(CondTailId, ThenStartId, !IntervalInfo),
	set_cur_interval(CondTailId, !IntervalInfo),
	get_open_intervals(!.IntervalInfo, OpenIntervals0),
	svset__insert(CondTailId, OpenIntervals0, OpenIntervals),
	set_open_intervals(OpenIntervals, !IntervalInfo).

:- pred leave_branch_start(branch_construct::in, anchor::in, interval_id::in,
	maybe(set(prog_var))::in, set(interval_id)::in,
	interval_info::in, interval_info::out) is det.

leave_branch_start(_BranchConstruct, StartArchor, BeforeId, MaybeResumeVars,
		OpenIntervals, !IntervalInfo) :-
	record_interval_end(BeforeId, StartArchor, !IntervalInfo),
	(
		MaybeResumeVars = yes(ResumeVars),
		require_flushed(ResumeVars, !IntervalInfo)
	;
		MaybeResumeVars = no
	),
	set_cur_interval(BeforeId, !IntervalInfo),
	set_open_intervals(OpenIntervals, !IntervalInfo).

:- pred get_open_intervals(interval_info::in, set(interval_id)::out) is det.

get_open_intervals(IntervalInfo, OpenIntervals) :-
	OpenIntervals = IntervalInfo ^ open_intervals.

:- pred set_open_intervals(set(interval_id)::in,
	interval_info::in, interval_info::out) is det.

set_open_intervals(OpenIntervals, !IntervalInfo) :-
	!:IntervalInfo = !.IntervalInfo ^ open_intervals := OpenIntervals.

:- pred no_open_intervals(interval_info::in, interval_info::out) is det.

no_open_intervals(!IntervalInfo) :-
	!:IntervalInfo = !.IntervalInfo ^ open_intervals := set__init.

:- pred one_open_interval(interval_id::in, interval_info::in,
	interval_info::out) is det.

one_open_interval(IntervalId, !IntervalInfo) :-
	!:IntervalInfo = !.IntervalInfo ^ open_intervals :=
		set__make_singleton_set(IntervalId).

:- pred assign_open_intervals_to_anchor(anchor::in,
	interval_info::in, interval_info::out) is det.

assign_open_intervals_to_anchor(Anchor, !IntervalInfo) :-
	AnchorFollowMap0 = !.IntervalInfo ^ anchor_follow_map,
	IntervalVarMap = !.IntervalInfo ^ interval_vars,
	CurOpenIntervals = !.IntervalInfo ^ open_intervals,
	set__fold(gather_interval_vars(IntervalVarMap), CurOpenIntervals,
		set__init, CurOpenIntervalVars),
	( map__search(AnchorFollowMap0, Anchor, AnchorFollowInfo0) ->
		AnchorFollowInfo0 = OpenIntervalVars0 - OpenIntervals0,
		OpenIntervalVars =
			set__union(OpenIntervalVars0, CurOpenIntervalVars),
		OpenIntervals =
			set__union(OpenIntervals0, CurOpenIntervals),
		AnchorFollowInfo = OpenIntervalVars - OpenIntervals,
		svmap__det_update(Anchor, AnchorFollowInfo,
			AnchorFollowMap0, AnchorFollowMap)
	;
		AnchorFollowInfo = CurOpenIntervalVars - CurOpenIntervals,
		svmap__det_insert(Anchor, AnchorFollowInfo,
			AnchorFollowMap0, AnchorFollowMap)
	),
	!:IntervalInfo = !.IntervalInfo ^ anchor_follow_map := AnchorFollowMap.

:- pred gather_interval_vars(map(interval_id, set(prog_var))::in,
	interval_id::in, set(prog_var)::in, set(prog_var)::out) is det.

gather_interval_vars(IntervalVarMap, IntervalId, !OpenIntervalVars) :-
	map__lookup(IntervalVarMap, IntervalId, IntervalVars),
	!:OpenIntervalVars = set__union(!.OpenIntervalVars, IntervalVars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred get_cur_interval(interval_id::out, interval_info::in) is det.

get_cur_interval(IntervalInfo ^ cur_interval, IntervalInfo).

:- pred set_cur_interval(interval_id::in, interval_info::in,
	interval_info::out) is det.

set_cur_interval(CurInterval, IntervalInfo,
	IntervalInfo ^ cur_interval := CurInterval).

:- pred new_interval_id(interval_id::out, interval_info::in,
	interval_info::out) is det.

new_interval_id(Id, !IntervalInfo) :-
	Counter0 = !.IntervalInfo ^ interval_counter,
	IntervalVars0 = !.IntervalInfo ^ interval_vars,
	counter__allocate(Num, Counter0, Counter),
	Id = interval_id(Num),
	svmap__det_insert(Id, set__init, IntervalVars0, IntervalVars),
	!:IntervalInfo = !.IntervalInfo ^ interval_counter := Counter,
	!:IntervalInfo = !.IntervalInfo ^ interval_vars := IntervalVars.

:- pred record_branch_end_info(goal_path::in,
	interval_info::in, interval_info::out) is det.

record_branch_end_info(GoalPath, !IntervalInfo) :-
	FlushedLater = !.IntervalInfo ^ flushed_later,
	AccessedLater = !.IntervalInfo ^ accessed_later,
	CurInterval = !.IntervalInfo ^ cur_interval,
	BranchEndMap0 = !.IntervalInfo ^ branch_end_map,
	BranchEndInfo = branch_end_info(FlushedLater, AccessedLater,
		CurInterval),
	svmap__det_insert(GoalPath, BranchEndInfo,
		BranchEndMap0, BranchEndMap),
	!:IntervalInfo = !.IntervalInfo ^ branch_end_map := BranchEndMap.

:- pred record_cond_end(goal_path::in, interval_info::in, interval_info::out)
	is det.

record_cond_end(GoalPath, !IntervalInfo) :-
	CurInterval = !.IntervalInfo ^ cur_interval,
	CondEndMap0 = !.IntervalInfo ^ cond_end_map,
	svmap__det_insert(GoalPath, CurInterval, CondEndMap0, CondEndMap),
	!:IntervalInfo = !.IntervalInfo ^ cond_end_map := CondEndMap.

:- pred record_interval_end(interval_id::in, anchor::in,
	interval_info::in, interval_info::out) is det.

record_interval_end(Id, End, !IntervalInfo) :-
	EndMap0 = !.IntervalInfo ^ interval_end,
	svmap__det_insert(Id, End, EndMap0, EndMap),
	!:IntervalInfo = !.IntervalInfo ^ interval_end := EndMap.

:- pred record_interval_start(interval_id::in, anchor::in,
	interval_info::in, interval_info::out) is det.

record_interval_start(Id, Start, !IntervalInfo) :-
	StartMap0 = !.IntervalInfo ^ interval_start,
	svmap__det_insert(Id, Start, StartMap0, StartMap),
	!:IntervalInfo = !.IntervalInfo ^ interval_start := StartMap.

:- pred record_interval_succ(interval_id::in, interval_id::in,
	interval_info::in, interval_info::out) is det.

record_interval_succ(Id, Succ, !IntervalInfo) :-
	SuccMap0 = !.IntervalInfo ^ interval_succ,
	( map__search(SuccMap0, Id, Succ0) ->
		svmap__det_update(Id, [Succ | Succ0], SuccMap0, SuccMap)
	;
		svmap__det_insert(Id, [Succ], SuccMap0, SuccMap)
	),
	!:IntervalInfo = !.IntervalInfo ^ interval_succ := SuccMap.

:- pred record_interval_no_succ(interval_id::in,
	interval_info::in, interval_info::out) is det.

record_interval_no_succ(Id, !IntervalInfo) :-
	SuccMap0 = !.IntervalInfo ^ interval_succ,
	( map__search(SuccMap0, Id, _Succ0) ->
		error("record_interval_no_succ: already in succ map")
	;
		svmap__det_insert(Id, [], SuccMap0, SuccMap)
	),
	!:IntervalInfo = !.IntervalInfo ^ interval_succ := SuccMap.

record_interval_vars(Id, NewVars, !IntervalInfo) :-
	VarsMap0 = !.IntervalInfo ^ interval_vars,
	( map__search(VarsMap0, Id, Vars0) ->
		svset__insert_list(NewVars, Vars0, Vars),
		svmap__det_update(Id, Vars, VarsMap0, VarsMap)
	;
		set__list_to_set(NewVars, Vars),
		svmap__det_insert(Id, Vars, VarsMap0, VarsMap)
	),
	!:IntervalInfo = !.IntervalInfo ^ interval_vars := VarsMap.

delete_interval_vars(Id, ToDeleteVars, DeletedVars, !IntervalInfo) :-
	VarsMap0 = !.IntervalInfo ^ interval_vars,
	map__lookup(VarsMap0, Id, Vars0),
	DeletedVars = set__intersect(Vars0, ToDeleteVars),
	Vars = set__difference(Vars0, DeletedVars),
	svmap__det_update(Id, Vars, VarsMap0, VarsMap),
	!:IntervalInfo = !.IntervalInfo ^ interval_vars := VarsMap,

	% The deletions are recorded only for debugging. The algorithm itself
	% does not need this information to be recorded.
	DeleteMap0 = !.IntervalInfo ^ interval_delvars,
	( map__search(DeleteMap0, Id, Deletions0) ->
		Deletions = [DeletedVars | Deletions0],
		svmap__det_update(Id, Deletions, DeleteMap0, DeleteMap)
	;
		Deletions = [DeletedVars],
		svmap__det_insert(Id, Deletions, DeleteMap0, DeleteMap)
	),
	!:IntervalInfo = !.IntervalInfo ^ interval_delvars := DeleteMap.

:- pred require_in_regs(list(prog_var)::in, interval_info::in,
	interval_info::out) is det.

require_in_regs(Vars, !IntervalInfo) :-
	CurIntervalId = !.IntervalInfo ^ cur_interval,
	record_interval_vars(CurIntervalId, Vars, !IntervalInfo).

:- pred require_flushed(set(prog_var)::in,
	interval_info::in, interval_info::out) is det.

require_flushed(Vars, !IntervalInfo) :-
	FlushedLater0 = !.IntervalInfo ^ flushed_later,
	FlushedLater = set__union(FlushedLater0, Vars),
	!:IntervalInfo = !.IntervalInfo ^ flushed_later := FlushedLater.

:- pred require_access(list(prog_var)::in,
	interval_info::in, interval_info::out) is det.

require_access(Vars, !IntervalInfo) :-
	AccessedLater0 = !.IntervalInfo ^ accessed_later,
	svset__insert_list(Vars, AccessedLater0, AccessedLater),
	!:IntervalInfo = !.IntervalInfo ^ accessed_later := AccessedLater.

:- pred record_branch_resume(goal_path::in, resume_save_status::in,
	interval_info::in, interval_info::out) is det.

record_branch_resume(GoalPath, ResumeSaveStatus, !IntervalInfo) :-
	BranchResumeMap0 = !.IntervalInfo ^ branch_resume_map,
	svmap__det_insert(GoalPath, ResumeSaveStatus,
		BranchResumeMap0, BranchResumeMap),
	!:IntervalInfo = !.IntervalInfo ^ branch_resume_map := BranchResumeMap.

:- pred record_model_non_anchor(anchor::in, interval_info::in,
	interval_info::out) is det.

record_model_non_anchor(Anchor, !IntervalInfo) :-
	ModelNonAnchors0 = !.IntervalInfo ^ model_non_anchors,
	svset__insert(Anchor, ModelNonAnchors0, ModelNonAnchors),
	!:IntervalInfo = !.IntervalInfo ^ model_non_anchors := ModelNonAnchors.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type var_info
	--->	var_info(
			varset		:: prog_varset,
			vartypes	:: vartypes
		).

record_decisions_in_goal(!Goal, VarSet0, VarSet, VarTypes0, VarTypes,
		!VarRename, InsertMap, MaybeFeature) :-
	record_decisions_in_goal(!Goal, var_info(VarSet0, VarTypes0),
		var_info(VarSet, VarTypes), !VarRename, InsertMap,
		MaybeFeature).

:- pred record_decisions_in_goal(hlds_goal::in, hlds_goal::out,
	var_info::in, var_info::out, rename_map::in, rename_map::out,
	insert_map::in, maybe(goal_feature)::in) is det.

record_decisions_in_goal(Goal0, Goal, !VarInfo, !VarRename, InsertMap,
                MaybeFeature) :-
	Goal0 = conj(Goals0) - GoalInfo,
	record_decisions_in_conj(Goals0, Goals, !VarInfo, !VarRename,
		InsertMap, MaybeFeature),
	Goal = conj(Goals) - GoalInfo.

record_decisions_in_goal(Goal0, Goal, !VarInfo, VarRename0, map__init,
		InsertMap, MaybeFeature) :-
	Goal0 = par_conj(Goals0) - GoalInfo,
	record_decisions_in_par_conj(Goals0, Goals, !VarInfo, VarRename0,
		InsertMap, MaybeFeature),
	Goal = par_conj(Goals) - GoalInfo.

record_decisions_in_goal(Goal0,  Goal, !VarInfo, !VarRename, InsertMap,
                MaybeFeature) :-
	Goal0 = disj(Goals0) - GoalInfo0,
	construct_anchors(disj, Goal0, StartAnchor, EndAnchor),
	( Goals0 = [FirstGoal0 | LaterGoals0] ->
		record_decisions_in_goal(FirstGoal0, FirstGoal, !VarInfo,
			!.VarRename, _, InsertMap, MaybeFeature),
		lookup_inserts(InsertMap, StartAnchor, StartInserts),
		record_decisions_in_disj(LaterGoals0, LaterGoals,
			!VarInfo, !.VarRename, StartInserts, InsertMap,
                        MaybeFeature),
		Goals = [FirstGoal | LaterGoals],
		Goal1 = disj(Goals) - GoalInfo0,
		lookup_inserts(InsertMap, EndAnchor, Inserts),
		insert_goals_after(Goal1, Goal, !VarInfo, !:VarRename,
			Inserts, MaybeFeature)
	;
		Goal = disj(Goals0) - GoalInfo0
	).

record_decisions_in_goal(Goal0, Goal, !VarInfo, !VarRename, InsertMap,
                MaybeFeature) :-
	Goal0 = switch(Var0, Det, Cases0) - GoalInfo0,
	record_decisions_in_cases(Cases0, Cases, !VarInfo, !.VarRename,
		InsertMap, MaybeFeature),
	rename_var(Var0, no, !.VarRename, Var),
	Goal1 = switch(Var, Det, Cases) - GoalInfo0,
	construct_anchors(switch, Goal0, _StartAnchor, EndAnchor),
	lookup_inserts(InsertMap, EndAnchor, Inserts),
	insert_goals_after(Goal1, Goal, !VarInfo, !:VarRename, Inserts,
                MaybeFeature).

record_decisions_in_goal(Goal0, Goal, !VarInfo, !VarRename, InsertMap,
                MaybeFeature) :-
	Goal0 = not(NegGoal0) - GoalInfo0,
	record_decisions_in_goal(NegGoal0, NegGoal, !VarInfo, !.VarRename, _,
		InsertMap, MaybeFeature),
	Goal1 = not(NegGoal) - GoalInfo0,
	construct_anchors(neg, Goal0, _StartAnchor, EndAnchor),
	lookup_inserts(InsertMap, EndAnchor, Inserts),
	% XXX
	insert_goals_after(Goal1, Goal, !VarInfo, !:VarRename, Inserts,
                MaybeFeature).

record_decisions_in_goal(Goal0, Goal, !VarInfo, !VarRename, InsertMap,
                MaybeFeature) :-
	Goal0 = if_then_else(Vars0, Cond0, Then0, Else0) - GoalInfo0,
	construct_anchors(ite, Goal0, StartAnchor, EndAnchor),
	rename_var_list(Vars0, no, !.VarRename, Vars),
	record_decisions_in_goal(Cond0, Cond, !VarInfo, !VarRename, InsertMap,
                MaybeFeature),
	record_decisions_in_goal(Then0, Then, !VarInfo, !.VarRename, _,
		InsertMap, MaybeFeature),
	lookup_inserts(InsertMap, StartAnchor, StartInserts),
	make_inserted_goals(!VarInfo, map__init, VarRenameElse,
		StartInserts, MaybeFeature, StartInsertGoals),
	record_decisions_in_goal(Else0, Else1, !VarInfo, VarRenameElse, _,
		InsertMap, MaybeFeature),
	Else0 = _ - ElseGoalInfo0,
	conj_list_to_goal(list__append(StartInsertGoals, [Else1]),
		ElseGoalInfo0, Else),
	Goal1 = if_then_else(Vars, Cond, Then, Else) - GoalInfo0,
	lookup_inserts(InsertMap, EndAnchor, EndInserts),
	insert_goals_after(Goal1, Goal, !VarInfo, !:VarRename, EndInserts,
                MaybeFeature).

record_decisions_in_goal(Goal0, Goal, !VarInfo, !VarRename, InsertMap,
		MaybeFeature) :-
	Goal0 = scope(Reason0, SubGoal0) - GoalInfo,
	(
		Reason0 = exist_quant(Vars0),
		rename_var_list(Vars0, no, !.VarRename, Vars),
		Reason = exist_quant(Vars)
	;
		Reason0 = promise_purity(_, _),
		Reason = Reason0
	;
		Reason0 = promise_equivalent_solutions(_),
		Reason = Reason0
	;
		Reason0 = commit(_),
		Reason = Reason0
	;
		Reason0 = barrier(_),
		Reason = Reason0
	;
		Reason0 = from_ground_term(Var0),
		rename_var(Var0, no, !.VarRename, Var),
		Reason = from_ground_term(Var)
	),
	record_decisions_in_goal(SubGoal0, SubGoal, !VarInfo, !VarRename,
		InsertMap, MaybeFeature),
	Goal = scope(Reason, SubGoal) - GoalInfo.

record_decisions_in_goal(Goal0, Goal, !VarInfo, !VarRename, InsertMap,
                MaybeFeature) :-
	Goal0 = generic_call(GenericCall, _ , _, _) - _,
	% unsafe_casts are generated inline.
	( GenericCall = unsafe_cast ->
		MustHaveMap = no
	;
		MustHaveMap = yes
	),
	record_decisions_at_call_site(Goal0, Goal, !VarInfo, !VarRename,
		MustHaveMap, InsertMap, MaybeFeature).

record_decisions_in_goal(Goal0, Goal, !VarInfo, !VarRename, InsertMap,
                MaybeFeature) :-
	Goal0 = call(_, _, _, Builtin, _, _) - _,
	( Builtin = inline_builtin ->
		MustHaveMap = no
	;
		MustHaveMap = yes
	),
	record_decisions_at_call_site(Goal0, Goal, !VarInfo, !VarRename,
		MustHaveMap, InsertMap, MaybeFeature).

record_decisions_in_goal(Goal0, Goal, !VarInfo, !VarRename, InsertMap,
                MaybeFeature) :-
	Goal0 = foreign_proc(_, _, _, _, _, _) - _,
	record_decisions_at_call_site(Goal0, Goal, !VarInfo,
		!VarRename, no, InsertMap, MaybeFeature).

record_decisions_in_goal(Goal0, Goal, !VarInfo, !VarRename, _InsertMap,
                _MaybeFeature) :-
	Goal0 = unify(_, _, _, _, _) - _,
	rename_vars_in_goal(Goal0, !.VarRename, Goal).

record_decisions_in_goal(shorthand(_) - _, _, !VarInfo, !VarRename, _, _) :-
	error("shorthand in record_decisions_in_goal").

%-----------------------------------------------------------------------------%

:- pred lookup_inserts(insert_map::in, anchor::in, list(insert_spec)::out)
	is det.

lookup_inserts(InsertMap, Anchor, Inserts) :-
	( map__search(InsertMap, Anchor, InsertsPrime) ->
		Inserts = InsertsPrime
	;
		Inserts = []
	).

:- pred insert_goals_after(hlds_goal::in, hlds_goal::out,
	var_info::in, var_info::out, rename_map::out,
	list(insert_spec)::in, maybe(goal_feature)::in) is det.

insert_goals_after(BranchesGoal, Goal, !VarInfo, VarRename, Inserts,
                MaybeFeature) :-
	make_inserted_goals(!VarInfo, map__init, VarRename,
		Inserts, MaybeFeature, InsertGoals),
	BranchesGoal = _ - BranchesGoalInfo,
	conj_list_to_goal([BranchesGoal | InsertGoals], BranchesGoalInfo,
		Goal).

:- pred make_inserted_goals(var_info::in, var_info::out,
	rename_map::in, rename_map::out, list(insert_spec)::in,
        maybe(goal_feature)::in, list(hlds_goal)::out) is det.

make_inserted_goals(!VarInfo, !VarRename, [], _MaybeFeature, []).
make_inserted_goals(!VarInfo, !VarRename, [Spec | Specs], MaybeFeature,
                [Goal | Goals]) :-
	make_inserted_goal(!VarInfo, !VarRename, Spec, MaybeFeature, Goal),
	make_inserted_goals(!VarInfo, !VarRename, Specs, MaybeFeature, Goals).

:- pred make_inserted_goal(var_info::in, var_info::out,
	rename_map::in, rename_map::out, insert_spec::in,
        maybe(goal_feature)::in, hlds_goal::out) is det.

make_inserted_goal(!VarInfo, !VarRename, Spec, MaybeFeature, Goal) :-
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
        (
            MaybeFeature = yes(Feature),
            goal_info_add_feature(GoalInfo1, Feature, GoalInfo2)
        ;
            MaybeFeature = no,
            GoalInfo2 = GoalInfo1
        ),
		Goal2 = GoalExpr1 - GoalInfo2,
		!.VarInfo = var_info(VarSet0, VarTypes0),
		create_shadow_vars(ArgVars, VarsToExtract, VarSet0, VarSet,
			VarTypes0, VarTypes, map__init, NewRename,
			map__init, VoidRename),
		!:VarInfo = var_info(VarSet, VarTypes),
		map__merge(!.VarRename, NewRename, !:VarRename),
		% We rename the original goal
		rename_vars_in_goal(Goal2, !.VarRename, Goal3),
		rename_vars_in_goal(Goal3, VoidRename, Goal)
	;
		error("make_inserted_goal: not a deconstruct")
	).

make_inserted_goal(VarSet0, VarSet, VarTypes0, VarTypes, !RenameMap,
		InsertSpec, MaybeFeature, Goal) :-
	make_inserted_goal(var_info(VarSet0, VarTypes0),
		var_info(VarSet, VarTypes), !RenameMap, InsertSpec,
		MaybeFeature, Goal).

:- pred create_shadow_vars(list(prog_var)::in, set(prog_var)::in,
	prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
	rename_map::in, rename_map::out, rename_map::in, rename_map::out)
	is det.

create_shadow_vars([], _, !VarSet, !VarTypes, !VarRename, !VoidRename).
create_shadow_vars([Arg | Args], VarsToExtract, !VarSet, !VarTypes,
		!VarRename, !VoidRename) :-
	create_shadow_var(Arg, VarsToExtract, !VarSet, !VarTypes,
		!VarRename, !VoidRename),
	create_shadow_vars(Args, VarsToExtract, !VarSet, !VarTypes,
		!VarRename, !VoidRename).

:- pred create_shadow_var(prog_var::in, set(prog_var)::in,
	prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
	rename_map::in, rename_map::out, rename_map::in, rename_map::out)
	is det.

create_shadow_var(Arg, VarsToExtract, !VarSet, !VarTypes,
		!VarRename, !VoidRename) :-
	varset__lookup_name(!.VarSet, Arg, Name),
	svvarset__new_named_var(Name, Shadow, !VarSet),
	map__lookup(!.VarTypes, Arg, Type),
	svmap__det_insert(Shadow, Type, !VarTypes),
	( set__member(Arg, VarsToExtract) ->
		svmap__det_insert(Arg, Shadow, !VarRename)
	;
		svmap__det_insert(Arg, Shadow, !VoidRename)
	).

%-----------------------------------------------------------------------------%

:- pred record_decisions_at_call_site(hlds_goal::in, hlds_goal::out,
	var_info::in, var_info::out, rename_map::in, rename_map::out,
	bool::in, insert_map::in, maybe(goal_feature)::in) is det.

record_decisions_at_call_site(Goal0, Goal, !VarInfo, !VarRename,
		MustHaveMap, InsertMap, MaybeFeature) :-
	Goal0 = _ - GoalInfo0,
	rename_vars_in_goal(Goal0, !.VarRename, Goal1),
	(
		goal_info_maybe_get_maybe_need_across_call(GoalInfo0,
			MaybeNeedAcrossCall),
		MaybeNeedAcrossCall = yes(_NeedAcrossCall)
	->
		goal_info_get_goal_path(GoalInfo0, GoalPath),
		Anchor = call_site(GoalPath),
		lookup_inserts(InsertMap, Anchor, Inserts),
		insert_goals_after(Goal1, Goal, !VarInfo, !:VarRename, Inserts,
                        MaybeFeature)
	;
		(
			MustHaveMap = no,
			Goal = Goal1
		;
			MustHaveMap = yes,
			error("record_decisions_at_call_site: no save map")
		)
	).

%-----------------------------------------------------------------------------%

:- pred record_decisions_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
	var_info::in, var_info::out, rename_map::in, rename_map::out,
	insert_map::in, maybe(goal_feature)::in) is det.

record_decisions_in_conj([], [], !VarInfo, !VarRename, _, _).
record_decisions_in_conj([Goal0 | Goals0], Goals, !VarInfo, !VarRename,
		InsertMap, MaybeFeature) :-
	record_decisions_in_goal(Goal0, Goal1, !VarInfo, !VarRename,
		InsertMap, MaybeFeature),
	record_decisions_in_conj(Goals0, Goals1, !VarInfo, !VarRename,
		InsertMap, MaybeFeature),
	( Goal1 = conj(SubGoals) - _ ->
		Goals = list__append(SubGoals, Goals1)
	;
		Goals = [Goal1 | Goals1]
	).

:- pred record_decisions_in_par_conj(list(hlds_goal)::in, list(hlds_goal)::out,
	var_info::in, var_info::out, rename_map::in, insert_map::in,
        maybe(goal_feature)::in) is det.

record_decisions_in_par_conj([], [], !VarInfo, _, _, _).
record_decisions_in_par_conj([Goal0 | Goals0], [Goal | Goals], !VarInfo,
		VarRename0, InsertMap, MaybeFeature) :-
	record_decisions_in_goal(Goal0, Goal, !VarInfo, VarRename0, _,
		InsertMap, MaybeFeature),
	record_decisions_in_par_conj(Goals0, Goals, !VarInfo, VarRename0,
		InsertMap, MaybeFeature).

:- pred record_decisions_in_disj(list(hlds_goal)::in, list(hlds_goal)::out,
	var_info::in, var_info::out, rename_map::in, list(insert_spec)::in,
	insert_map::in, maybe(goal_feature)::in) is det.

record_decisions_in_disj([], [], !VarInfo, _, _, _, _).
record_decisions_in_disj([Goal0 | Goals0], [Goal | Goals], !VarInfo,
		VarRename0, Inserts, InsertMap, MaybeFeature) :-
	make_inserted_goals(!VarInfo, map__init, VarRename1,
		Inserts, MaybeFeature, InsertGoals),
	Goal0 = _ - GoalInfo0,
	record_decisions_in_goal(Goal0, Goal1, !VarInfo, VarRename1, _,
		InsertMap, MaybeFeature),
	conj_list_to_goal(list__append(InsertGoals, [Goal1]), GoalInfo0, Goal),
	record_decisions_in_disj(Goals0, Goals, !VarInfo, VarRename0,
		Inserts, InsertMap, MaybeFeature).

:- pred record_decisions_in_cases(list(case)::in, list(case)::out,
	var_info::in, var_info::out, rename_map::in, insert_map::in,
        maybe(goal_feature)::in) is det.

record_decisions_in_cases([], [], !VarInfo, _, _, _).
record_decisions_in_cases([case(Var, Goal0) | Cases0],
		[case(Var, Goal) | Cases], !VarInfo, VarRename0, InsertMap,
                MaybeFeature) :-
	record_decisions_in_goal(Goal0, Goal, !VarInfo, VarRename0, _,
		InsertMap, MaybeFeature),
	record_decisions_in_cases(Cases0, Cases, !VarInfo, VarRename0,
		InsertMap, MaybeFeature).

%-----------------------------------------------------------------------------%

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

build_headvar_subst([], _RenameMap, !Subst).
build_headvar_subst([HeadVar | HeadVars], RenameMap, !Subst) :-
	( map__search(RenameMap, HeadVar, Replacement) ->
		svmap__det_insert(Replacement, HeadVar, !Subst),
		svmap__det_insert(HeadVar, Replacement, !Subst)
	;
		true
	),
	build_headvar_subst(HeadVars, RenameMap, !Subst).

%-----------------------------------------------------------------------------%

:- pred construct_anchors(branch_construct::in, hlds_goal::in,
	anchor::out, anchor::out) is det.

construct_anchors(Construct, Goal, StartAnchor, EndAnchor) :-
	Goal = _ - GoalInfo,
	goal_info_get_goal_path(GoalInfo, GoalPath),
	StartAnchor = branch_start(Construct, GoalPath),
	EndAnchor = branch_end(Construct, GoalPath).

%-----------------------------------------------------------------------------%

% For debugging purposes.

dump_interval_info(IntervalInfo, !IO) :-
	map__keys(IntervalInfo ^ interval_start, StartIds),
	map__keys(IntervalInfo ^ interval_end, EndIds),
	map__keys(IntervalInfo ^ interval_vars, VarsIds),
	map__keys(IntervalInfo ^ interval_succ, SuccIds),
	list__condense([StartIds, EndIds, VarsIds, SuccIds], IntervalIds0),
	list__sort_and_remove_dups(IntervalIds0, IntervalIds),
	io__write_string("INTERVALS:\n", !IO),
	list__foldl(dump_interval_info(IntervalInfo), IntervalIds, !IO),

	map__to_assoc_list(IntervalInfo ^ anchor_follow_map, AnchorFollows),
	io__write_string("\nANCHOR FOLLOW:\n", !IO),
	list__foldl(dump_anchor_follow, AnchorFollows, !IO).

:- pred dump_interval_info(interval_info::in, interval_id::in, io::di, io::uo)
	is det.

dump_interval_info(IntervalInfo, IntervalId, !IO) :-
	io__write_string("\ninterval ", !IO),
	io__write_int(interval_id_to_int(IntervalId), !IO),
	io__write_string(": ", !IO),
	( map__search(IntervalInfo ^ interval_succ, IntervalId, SuccIds) ->
		SuccNums = list__map(interval_id_to_int, SuccIds),
		io__write_string("succ [", !IO),
		write_int_list(SuccNums, !IO),
		io__write_string("]\n", !IO)
	;
		io__write_string("no succ\n", !IO)
	),
	( map__search(IntervalInfo ^ interval_start, IntervalId, Start) ->
		io__write_string("start ", !IO),
		io__write(Start, !IO),
		io__write_string("\n", !IO)
	;
		io__write_string("no start\n", !IO)
	),
	( map__search(IntervalInfo ^ interval_end, IntervalId, End) ->
		io__write_string("end ", !IO),
		io__write(End, !IO),
		io__write_string("\n", !IO)
	;
		io__write_string("no end\n", !IO)
	),
	( map__search(IntervalInfo ^ interval_vars, IntervalId, Vars) ->
		list__map(term__var_to_int, set__to_sorted_list(Vars),
			VarNums),
		io__write_string("vars [", !IO),
		write_int_list(VarNums, !IO),
		io__write_string("]\n", !IO)
	;
		io__write_string("no vars\n", !IO)
	),
	( map__search(IntervalInfo ^ interval_delvars, IntervalId, Deletions)
	->
		io__write_string("deletions", !IO),
		list__foldl(dump_deletion, Deletions, !IO),
		io__write_string("\n", !IO)
	;
		true
	).

:- pred dump_deletion(set(prog_var)::in, io::di, io::uo) is det.

dump_deletion(Vars, !IO) :-
	list__map(term__var_to_int, set__to_sorted_list(Vars), VarNums),
	io__write_string(" [", !IO),
	write_int_list(VarNums, !IO),
	io__write_string("]", !IO).

:- pred dump_anchor_follow(pair(anchor, anchor_follow_info)::in,
	io::di, io::uo) is det.

dump_anchor_follow(Anchor - AnchorFollowInfo, !IO) :-
	AnchorFollowInfo = Vars - Intervals,
	io__write_string("\n", !IO),
	io__write(Anchor, !IO),
	io__write_string(" =>\n", !IO),
	list__map(term__var_to_int, set__to_sorted_list(Vars), VarNums),
	io__write_string("vars [", !IO),
	write_int_list(VarNums, !IO),
	io__write_string("]\nintervals: ", !IO),
	set__to_sorted_list(Intervals, IntervalList),
	write_int_list(list__map(interval_id_to_int, IntervalList), !IO),
	io__write_string("\n", !IO).

write_int_list(List, !IO) :-
	io__write_list(List, ", ", io__write_int, !IO).

interval_id_to_int(interval_id(Num)) = Num.

%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File stack_alloc.m
%
% Authors: zs, conway.
%
% This module allocates stack slots to the variables that need to be saved
% across a call, across a goal that may fail, or in a parallel conjunction.
%
% The jobs is done in two steps. First we traverse the predicate definition
% looking for sets of variables that must be saved on the stack at the same
% time. If --optimize-stack-slots is set, then this phase is done by
% stack_opt.m; if --optimize-stack-slots is not set, then it is done by
% this module. Then we use a graph colouring algorithm to find an allocation
% of stack slots (colours) to variables such that in each set of variables
% that must be saved at the same time, each variable has a different colour.

%-----------------------------------------------------------------------------%

:- module ll_backend__stack_alloc.

:- interface.

:- import_module hlds__hlds_module, hlds__hlds_pred.
:- import_module io.

:- pred allocate_stack_slots_in_proc(pred_id::in, proc_id::in, module_info::in,
	proc_info::in, proc_info::out, io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__prog_data.
:- import_module hlds__hlds_goal, hlds__hlds_llds.
:- import_module hlds__hlds_data.
:- import_module check_hlds__goal_path.
:- import_module ll_backend__llds, ll_backend__stack_opt.
:- import_module ll_backend__liveness, ll_backend__live_vars.
:- import_module ll_backend__trace.
:- import_module backend_libs__code_model.
:- import_module libs__globals, libs__options.
:- import_module libs__trace_params, libs__graph_colour.

:- import_module bool, int, list, assoc_list, map, set, std_util, require.

%-----------------------------------------------------------------------------%

allocate_stack_slots_in_proc(PredId, _ProcId, ModuleInfo, ProcInfo0, ProcInfo,
		IO, IO) :-
	initial_liveness(ProcInfo0, PredId, ModuleInfo, Liveness0),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	module_info_globals(ModuleInfo, Globals),
	globals__get_trace_level(Globals, TraceLevel),
	(
		eff_trace_level_needs_input_vars(PredInfo, ProcInfo0,
			TraceLevel) = yes
	->
		trace__fail_vars(ModuleInfo, ProcInfo0, FailVars)
	;
		set__init(FailVars)
	),
	body_should_use_typeinfo_liveness(PredInfo, Globals, TypeInfoLiveness),
	globals__lookup_bool_option(Globals, opt_no_return_calls,
		OptNoReturnCalls),
	AllocData = alloc_data(ModuleInfo, ProcInfo0, TypeInfoLiveness,
		OptNoReturnCalls),
	set__init(NondetLiveness0),
	SimpleStackAlloc0 = stack_alloc(set__make_singleton_set(FailVars)),
	proc_info_goal(ProcInfo0, Goal0),
	build_live_sets_in_goal(Goal0, SimpleStackAlloc0,
		Liveness0, NondetLiveness0, FailVars, AllocData,
		Goal, SimpleStackAlloc, _Liveness, _NondetLiveness),
	proc_info_set_goal(ProcInfo0, Goal, ProcInfo3),
	SimpleStackAlloc = stack_alloc(LiveSets0),

	trace__do_we_need_maxfr_slot(Globals, PredInfo, ProcInfo3, ProcInfo4),
	trace__reserved_slots(ModuleInfo, PredInfo, ProcInfo4, Globals,
		NumReservedSlots, MaybeReservedVarInfo),
	(
		MaybeReservedVarInfo = yes(ResVar - _),
		set__singleton_set(ResVarSet, ResVar),
		set__insert(LiveSets0, ResVarSet, LiveSets)
	;
		MaybeReservedVarInfo = no,
		LiveSets = LiveSets0
	),
	graph_colour__group_elements(LiveSets, ColourSets),
	set__to_sorted_list(ColourSets, ColourList),
	proc_info_interface_code_model(ProcInfo4, CodeModel),
	allocate_stack_slots(ColourList, CodeModel, NumReservedSlots,
		MaybeReservedVarInfo, StackSlots),

	proc_info_set_stack_slots(ProcInfo4, StackSlots, ProcInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type stack_alloc
	--->	stack_alloc(
			set(set(prog_var))	% The sets of vars that need to
						% be on the stack at the same
						% time.
		).

:- instance stack_alloc_info(stack_alloc) where [
	pred(at_call_site/4) is alloc_at_call_site,
	pred(at_resume_site/4) is alloc_at_resume_site,
	pred(at_par_conj/4) is alloc_at_par_conj
].

:- pred alloc_at_call_site(need_across_call::in, hlds_goal_info::in,
	stack_alloc::in, stack_alloc::out) is det.

alloc_at_call_site(NeedAtCall, _GoalInfo, StackAlloc0, StackAlloc) :-
	NeedAtCall = need_across_call(ForwardVars, ResumeVars, NondetLiveVars),
	LiveSet = set__union_list([ForwardVars, ResumeVars, NondetLiveVars]),
	StackAlloc0 = stack_alloc(LiveSets0),
	LiveSets = set__insert(LiveSets0, LiveSet),
	StackAlloc = stack_alloc(LiveSets).

:- pred alloc_at_resume_site(need_in_resume::in, hlds_goal_info::in,
	stack_alloc::in, stack_alloc::out) is det.

alloc_at_resume_site(NeedAtResume, _GoalInfo, StackAlloc0, StackAlloc) :-
	NeedAtResume = need_in_resume(ResumeOnStack, ResumeVars,
		NondetLiveVars),
	(
		ResumeOnStack = no,
		StackAlloc = StackAlloc0
	;
		ResumeOnStack = yes,
		LiveSet = set__union(ResumeVars, NondetLiveVars),
		StackAlloc0 = stack_alloc(LiveSets0),
		LiveSets = set__insert(LiveSets0, LiveSet),
		StackAlloc = stack_alloc(LiveSets)
	).

:- pred alloc_at_par_conj(need_in_par_conj::in, hlds_goal_info::in,
	stack_alloc::in, stack_alloc::out) is det.

alloc_at_par_conj(NeedParConj, _GoalInfo, StackAlloc0, StackAlloc) :-
	NeedParConj = need_in_par_conj(StackVars),
	StackAlloc0 = stack_alloc(LiveSets0),
	LiveSets = set__insert(LiveSets0, StackVars),
	StackAlloc = stack_alloc(LiveSets).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred allocate_stack_slots(list(set(prog_var))::in, code_model::in, int::in,
	maybe(pair(prog_var, int))::in, stack_slots::out) is det.

allocate_stack_slots(ColourList, CodeModel, NumReservedSlots,
		MaybeReservedVarInfo, StackSlots) :-
	map__init(StackSlots0),
		% The reserved slots are referred to by fixed number
		% (e.g. framevar(1)) in trace__setup.
	FirstVarSlot = NumReservedSlots + 1,
	allocate_stack_slots_2(ColourList, CodeModel, FirstVarSlot,
		MaybeReservedVarInfo, StackSlots0, StackSlots).

:- pred allocate_stack_slots_2(list(set(prog_var))::in, code_model::in,
	int::in, maybe(pair(prog_var, int))::in,
	stack_slots::in, stack_slots::out) is det.

allocate_stack_slots_2([], _, _, _, StackSlots, StackSlots).
allocate_stack_slots_2([Vars | VarSets], CodeModel, N0, MaybeReservedVarInfo,
		StackSlots0, StackSlots) :-
	(
		MaybeReservedVarInfo = yes(ResVar - ResSlotNum),
		set__member(ResVar, Vars)
	->
		SlotNum = ResSlotNum,
		N1 = N0
	;
		SlotNum = N0,
		N1 = N0 + 1
	),
	( CodeModel = model_non ->
		Slot = framevar(SlotNum)
	;
		Slot = stackvar(SlotNum)
	),
	set__to_sorted_list(Vars, VarList),
	allocate_same_stack_slot(VarList, Slot, StackSlots0, StackSlots1),
	allocate_stack_slots_2(VarSets, CodeModel, N1, MaybeReservedVarInfo,
		StackSlots1, StackSlots).

:- pred allocate_same_stack_slot(list(prog_var)::in, lval::in, stack_slots::in,
	stack_slots::out) is det.

allocate_same_stack_slot([], _Slot, StackSlots, StackSlots).
allocate_same_stack_slot([Var | Vars], Slot, StackSlots0, StackSlots) :-
	map__det_insert(StackSlots0, Var, Slot, StackSlots1),
	allocate_same_stack_slot(Vars, Slot, StackSlots1, StackSlots).

%-----------------------------------------------------------------------------%

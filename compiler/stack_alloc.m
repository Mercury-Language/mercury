%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2004 The University of Melbourne.
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

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.

:- import_module io.

:- pred allocate_stack_slots_in_proc(pred_id::in, proc_id::in, module_info::in,
	proc_info::in, proc_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__goal_path.
:- import_module check_hlds__type_util.
:- import_module hlds__code_model.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_llds.
:- import_module libs__globals.
:- import_module libs__graph_colour.
:- import_module libs__options.
:- import_module libs__trace_params.
:- import_module ll_backend__live_vars.
:- import_module ll_backend__liveness.
:- import_module ll_backend__llds.
:- import_module ll_backend__stack_opt.
:- import_module ll_backend__trace.
:- import_module parse_tree__prog_data.

:- import_module bool, int, list, assoc_list, map, set, std_util, require.

%-----------------------------------------------------------------------------%

allocate_stack_slots_in_proc(PredId, _ProcId, ModuleInfo, !ProcInfo, !IO) :-
	initial_liveness(!.ProcInfo, PredId, ModuleInfo, Liveness0),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	module_info_globals(ModuleInfo, Globals),
	globals__get_trace_level(Globals, TraceLevel),
	(
		eff_trace_level_needs_input_vars(PredInfo, !.ProcInfo,
			TraceLevel) = yes
	->
		trace__fail_vars(ModuleInfo, !.ProcInfo, FailVars)
	;
		set__init(FailVars)
	),
	body_should_use_typeinfo_liveness(PredInfo, Globals, TypeInfoLiveness),
	globals__lookup_bool_option(Globals, opt_no_return_calls,
		OptNoReturnCalls),
	AllocData = alloc_data(ModuleInfo, !.ProcInfo, TypeInfoLiveness,
		OptNoReturnCalls),
	set__init(NondetLiveness0),
	SimpleStackAlloc0 = stack_alloc(set__make_singleton_set(FailVars)),
	proc_info_goal(!.ProcInfo, Goal0),
	build_live_sets_in_goal(Goal0, Goal, FailVars, AllocData,
		SimpleStackAlloc0, SimpleStackAlloc, Liveness0, _Liveness,
		NondetLiveness0, _NondetLiveness),
	proc_info_set_goal(Goal, !ProcInfo),
	SimpleStackAlloc = stack_alloc(LiveSets0),

	trace__do_we_need_maxfr_slot(Globals, PredInfo, !ProcInfo),
	trace__reserved_slots(ModuleInfo, PredInfo, !.ProcInfo, Globals,
		NumReservedSlots, MaybeReservedVarInfo),
	(
		MaybeReservedVarInfo = yes(ResVar - _),
		set__singleton_set(ResVarSet, ResVar),
		set__insert(LiveSets0, ResVarSet, LiveSets1)
	;
		MaybeReservedVarInfo = no,
		LiveSets1 = LiveSets0
	),
	proc_info_vartypes(!.ProcInfo, VarTypes),
	filter_out_dummy_values(VarTypes, LiveSets1, LiveSets, DummyVars),
	graph_colour__group_elements(LiveSets, ColourSets),
	set__to_sorted_list(ColourSets, ColourList),
	proc_info_interface_code_model(!.ProcInfo, CodeModel),
	allocate_stack_slots(ColourList, CodeModel, NumReservedSlots,
		MaybeReservedVarInfo, StackSlots1),
	allocate_dummy_stack_slots(DummyVars, CodeModel, -1,
		StackSlots1, StackSlots),
	proc_info_set_stack_slots(StackSlots, !ProcInfo).

:- pred filter_out_dummy_values(vartypes::in,
	set(set(prog_var))::in, set(set(prog_var))::out,
	list(prog_var)::out) is det.

filter_out_dummy_values(VarTypes, LiveSet0, LiveSet, DummyVars) :-
	set__to_sorted_list(LiveSet0, LiveList0),
	filter_out_dummy_values_2(VarTypes, LiveList0, LiveList,
		set__init, Dummies),
	set__list_to_set(LiveList, LiveSet),
	set__to_sorted_list(Dummies, DummyVars).

:- pred filter_out_dummy_values_2(vartypes::in,
	list(set(prog_var))::in, list(set(prog_var))::out,
	set(prog_var)::in, set(prog_var)::out) is det.

filter_out_dummy_values_2(_VarTypes, [], [], !Dummies).
filter_out_dummy_values_2(VarTypes, [LiveSet0 | LiveSets0], LiveSets,
		!Dummies) :-
	filter_out_dummy_values_2(VarTypes, LiveSets0, LiveSets1, !Dummies),
	set__to_sorted_list(LiveSet0, LiveList0),
	list__filter(var_is_of_dummy_type(VarTypes), LiveList0,
		DummyVars, NonDummyVars),
	set__insert_list(!.Dummies, DummyVars, !:Dummies),
	(
		NonDummyVars = [],
		LiveSets = LiveSets1
	;
		NonDummyVars = [_ | _],
		LiveSets = [list_to_set(NonDummyVars) | LiveSets1]
	).

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
		% The reserved slots are referred to by fixed number
		% (e.g. framevar(1)) in trace__setup.
	FirstVarSlot = NumReservedSlots + 1,
	allocate_stack_slots_2(ColourList, CodeModel, FirstVarSlot,
		MaybeReservedVarInfo, map__init, StackSlots).

:- pred allocate_stack_slots_2(list(set(prog_var))::in, code_model::in,
	int::in, maybe(pair(prog_var, int))::in,
	stack_slots::in, stack_slots::out) is det.

allocate_stack_slots_2([], _, _, _, !StackSlots).
allocate_stack_slots_2([Vars | VarSets], CodeModel, N0, MaybeReservedVarInfo,
		!StackSlots) :-
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
	set__to_sorted_list(Vars, VarList),
	allocate_same_stack_slot(VarList, CodeModel, SlotNum, !StackSlots),
	allocate_stack_slots_2(VarSets, CodeModel, N1, MaybeReservedVarInfo,
		!StackSlots).

:- pred allocate_same_stack_slot(list(prog_var)::in, code_model::in, int::in,
	stack_slots::in, stack_slots::out) is det.

allocate_same_stack_slot([], _CodeModel, _Slot, !StackSlots).
allocate_same_stack_slot([Var | Vars], CodeModel, Slot, !StackSlots) :-
	( CodeModel = model_non ->
		Locn = nondet_slot(Slot)
	;
		Locn = det_slot(Slot)
	),
	map__det_insert(!.StackSlots, Var, Locn, !:StackSlots),
	allocate_same_stack_slot(Vars, CodeModel, Slot, !StackSlots).

% We must not allocate the same stack slot to dummy variables. If we do,
% then the code that saves variables on the stack at calls will get confused.
% After saving one dummy variable on the stack, it will try to save the next
% in the same stack slot; believing the first variable to still be live, it
% will move it away.
%
% In ordinary grades, it is possible to have one value of type io__state
% and another of type store__store live at the same time; in debugging grades,
% due to our policy of extending variable lifetimes, more than one io__state
% may be live at the same time.

:- pred allocate_dummy_stack_slots(list(prog_var)::in, code_model::in,
	int::in, stack_slots::in, stack_slots::out) is det.

allocate_dummy_stack_slots([], _, _, !StackSlots).
allocate_dummy_stack_slots([Var | Vars], CodeModel, N0, !StackSlots) :-
	allocate_same_stack_slot([Var], CodeModel, N0, !StackSlots),
	allocate_dummy_stack_slots(Vars, CodeModel, N0 - 1, !StackSlots).

%-----------------------------------------------------------------------------%

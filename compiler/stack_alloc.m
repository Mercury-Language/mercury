%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2007, 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File stack_alloc.m.
% Authors: zs, conway.
%
% This module allocates stack slots to the variables that need to be saved
% across a call, across a goal that may fail, or in a parallel conjunction.
%
% The jobs is done in two steps. First we traverse the predicate definition
% looking for sets of variables that must be saved on the stack at the same
% time. If --optimize-stack-slots is set, then this phase is done by
% stack_opt.m; if --optimize-stack-slots is not set, then it is done by this
% module. Then we use a graph colouring algorithm to find an allocation of
% stack slots (colours) to variables such that in each set of variables that
% must be saved at the same time, each variable has a different colour.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.stack_alloc.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

%-----------------------------------------------------------------------------%

:- pred allocate_stack_slots_in_proc(module_info::in, pred_proc_id::in,
    proc_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.code_model.
:- import_module hlds.hlds_llds.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module ll_backend.live_vars.
:- import_module ll_backend.liveness.
:- import_module ll_backend.llds.
:- import_module ll_backend.trace_gen.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.

%-----------------------------------------------------------------------------%

allocate_stack_slots_in_proc(ModuleInfo, proc(PredId, ProcId), !ProcInfo) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    initial_liveness(ModuleInfo, PredInfo, !.ProcInfo, Liveness0),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_trace_level(Globals, TraceLevel),
    EffTraceLevel =
        eff_trace_level_for_proc(ModuleInfo, PredInfo, !.ProcInfo, TraceLevel),
    NeedFailVars = eff_trace_level_needs_fail_vars(EffTraceLevel),
    (
        NeedFailVars = yes,
        trace_fail_vars(ModuleInfo, !.ProcInfo, FailVars)
    ;
        NeedFailVars = no,
        FailVars = set_of_var.init
    ),
    body_should_use_typeinfo_liveness(PredInfo, Globals, TypeInfoLiveness),
    globals.lookup_bool_option(Globals, opt_no_return_calls,
        OptNoReturnCalls),
    proc_info_get_var_table(!.ProcInfo, VarTable),
    AllocData = alloc_data(ModuleInfo, !.ProcInfo, proc(PredId, ProcId),
        some_var_may_be_dummy(VarTable), TypeInfoLiveness, OptNoReturnCalls),
    NondetLiveness0 = set_of_var.init,
    SimpleStackAlloc0 = stack_alloc(set.make_singleton_set(FailVars)),
    proc_info_get_goal(!.ProcInfo, Goal0),
    build_live_sets_in_goal_no_par_stack(AllocData, FailVars, Goal0, Goal,
        SimpleStackAlloc0, SimpleStackAlloc, Liveness0, _Liveness,
        NondetLiveness0, _NondetLiveness),
    proc_info_set_goal(Goal, !ProcInfo),
    SimpleStackAlloc = stack_alloc(LiveSets0),

    do_we_need_maxfr_slot(EffTraceLevel, !ProcInfo),
    trace_reserved_slots(Globals, !.ProcInfo, EffTraceLevel,
        NumReservedSlots, MaybeReservedVarInfo),
    (
        MaybeReservedVarInfo = yes(ResVar - _),
        ResVarSet = set_of_var.make_singleton(ResVar),
        set.insert(ResVarSet, LiveSets0, LiveSets)
    ;
        MaybeReservedVarInfo = no,
        LiveSets = LiveSets0
    ),
    graph_colour_group_elements(LiveSets, ColourSets),
    set.to_sorted_list(ColourSets, ColourList),

    CodeModel = proc_info_interface_code_model(!.ProcInfo),
    MainStack = code_model_to_main_stack(CodeModel),
    allocate_stack_slots(Globals, VarTable, MainStack, NumReservedSlots,
        MaybeReservedVarInfo, ColourList, StackSlots1),
    var_table_to_sorted_assoc_list(VarTable, VarTableAL),
    allocate_stack_slots_for_dummy_vars(MainStack, VarTableAL, -1,
        StackSlots1, StackSlots),
    proc_info_set_stack_slots(StackSlots, !ProcInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type stack_alloc
    --->    stack_alloc(
                % Each element of this set is a set of variables
                % that need to be on the stack at the same time.
                set(set_of_progvar)
            ).

:- instance stack_alloc_info(stack_alloc) where [
    pred(at_call_site/4) is alloc_at_call_site,
    pred(at_resume_site/4) is alloc_at_resume_site,
    pred(at_par_conj/4) is alloc_at_par_conj,
    pred(at_recursive_call_for_loop_control/4) is
        alloc_at_recursive_call_for_loop_control
].

:- pred alloc_at_call_site(need_across_call::in, alloc_data::in,
    stack_alloc::in, stack_alloc::out) is det.

alloc_at_call_site(NeedAtCall, AllocData, !StackAlloc) :-
    NeedAtCall = need_across_call(ForwardVars, ResumeVars, NondetLiveVars),
    LiveSet0 = set_of_var.union_list([ForwardVars, ResumeVars,
        NondetLiveVars]),
    filter_out_dummy_vars(AllocData, LiveSet0, LiveSet),

    !.StackAlloc = stack_alloc(LiveSets0),
    LiveSets = set.insert(LiveSets0, LiveSet),
    !:StackAlloc = stack_alloc(LiveSets).

:- pred alloc_at_resume_site(need_in_resume::in, alloc_data::in,
    stack_alloc::in, stack_alloc::out) is det.

alloc_at_resume_site(NeedAtResume, AllocData, !StackAlloc) :-
    NeedAtResume = need_in_resume(ResumeOnStack, ResumeVars, NondetLiveVars),
    (
        ResumeOnStack = no
    ;
        ResumeOnStack = yes,
        LiveSet0 = set_of_var.union(ResumeVars, NondetLiveVars),
        filter_out_dummy_vars(AllocData, LiveSet0, LiveSet),

        !.StackAlloc = stack_alloc(LiveSets0),
        LiveSets = set.insert(LiveSets0, LiveSet),
        !:StackAlloc = stack_alloc(LiveSets)
    ).

:- pred alloc_at_par_conj(need_in_par_conj::in, alloc_data::in,
    stack_alloc::in, stack_alloc::out) is det.

alloc_at_par_conj(NeedParConj, AllocData, !StackAlloc) :-
    NeedParConj = need_in_par_conj(StackVars0),
    filter_out_dummy_vars(AllocData, StackVars0, StackVars),

    !.StackAlloc = stack_alloc(LiveSets0),
    LiveSets = set.insert(LiveSets0, StackVars),
    !:StackAlloc = stack_alloc(LiveSets).

:- pred alloc_at_recursive_call_for_loop_control(need_for_loop_control::in,
    alloc_data::in, stack_alloc::in, stack_alloc::out) is det.

alloc_at_recursive_call_for_loop_control(NeedLC, AllocData, !StackAlloc) :-
    NeedLC = need_for_loop_control(StackVarsSets),
    list.foldl(set_for_loop_control(AllocData), StackVarsSets, !StackAlloc).

:- pred set_for_loop_control(alloc_data::in, set_of_progvar::in,
    stack_alloc::in, stack_alloc::out) is det.

set_for_loop_control(AllocData, Set0, !StackAlloc) :-
    !.StackAlloc = stack_alloc(LiveSets0),
    filter_out_dummy_vars(AllocData, Set0, Set),
    LiveSets = set.insert(LiveSets0, Set),
    !:StackAlloc = stack_alloc(LiveSets).

:- pred filter_out_dummy_vars(alloc_data::in,
    set_of_progvar::in, set_of_progvar::out) is det.

filter_out_dummy_vars(AllocData, Vars, NonDummyVars) :-
    DummyVarInfo = AllocData ^ ad_dummy_var_info,
    (
        DummyVarInfo = no_var_is_dummy,
        NonDummyVars = Vars
    ;
        DummyVarInfo = some_var_may_be_dummy(VarTable),
        set_of_var.filter(var_is_not_dummy(VarTable), Vars, NonDummyVars)
    ).

:- pred var_is_not_dummy(var_table::in, prog_var::in) is semidet.

var_is_not_dummy(VarTable, Var) :-
    lookup_var_entry(VarTable, Var, VarEntry),
    VarEntry ^ vte_is_dummy = is_not_dummy_type.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred allocate_stack_slots(globals::in, var_table::in,
    main_stack::in, int::in, maybe(pair(prog_var, int))::in,
    list(set_of_progvar)::in, stack_slots::out) is det.

allocate_stack_slots(Globals, VarTable, MainStack, NumReservedSlots,
        MaybeReservedVarInfo, Colours, StackSlots) :-
    ( if float_width_on_stack(Globals, MainStack) = double_width then
        MaybeDoubleWidthFloats = yes(VarTable)
    else
        MaybeDoubleWidthFloats = no
    ),
    % The reserved slots are referred to by fixed number
    % (e.g. framevar(1)) in trace.setup.
    FirstFreeSlot = NumReservedSlots + 1,
    allocate_stack_slots_to_colours(MainStack, MaybeDoubleWidthFloats,
        MaybeReservedVarInfo, Colours, FirstFreeSlot, map.init, StackSlots).

:- pred allocate_stack_slots_to_colours(main_stack::in, maybe(var_table)::in,
    maybe(pair(prog_var, int))::in, list(set_of_progvar)::in,
    int::in, stack_slots::in, stack_slots::out) is det.

allocate_stack_slots_to_colours(_, _, _, [], _, !StackSlots).
allocate_stack_slots_to_colours(MainStack, MaybeDoubleWidthFloats,
        MaybeReservedVarInfo, [FirstColour | LaterColours],
        !.FirstFreeSlot, !StackSlots) :-
    allocate_stack_slots_to_colour(MainStack, MaybeDoubleWidthFloats,
        MaybeReservedVarInfo, FirstColour, !FirstFreeSlot, !StackSlots),
    allocate_stack_slots_to_colours(MainStack, MaybeDoubleWidthFloats,
        MaybeReservedVarInfo, LaterColours, !.FirstFreeSlot, !StackSlots).

:- pred allocate_stack_slots_to_colour(main_stack::in, maybe(var_table)::in,
    maybe(pair(prog_var, int))::in, set_of_progvar::in,
    int::in, int::out, stack_slots::in, stack_slots::out) is det.

allocate_stack_slots_to_colour(MainStack, MaybeDoubleWidthFloats,
        MaybeReservedVarInfo, ColourVars, !FirstFreeSlot, !StackSlots) :-
    (
        MaybeDoubleWidthFloats = no,
        % Treat all variables in ColourVars as single_width.
        allocate_next_stack_slot(MainStack, single_width,
            MaybeReservedVarInfo, ColourVars, !FirstFreeSlot, !StackSlots)
    ;
        MaybeDoubleWidthFloats = yes(VarTable),
        % XXX We do NOT currently allow single-width vars to overlap with
        % double-width vars, because the code generator does not understand
        % that clobbering one slot in a pair destroys any double_width values
        % in that slot pair, even if its officially-recorded slot number
        % is the number of the *other* slot.
        set_of_var.divide(var_is_float(VarTable), ColourVars,
            DoubleWidthVars, SingleWidthVars),
        ( if set_of_var.is_non_empty(SingleWidthVars) then
            allocate_next_stack_slot(MainStack, single_width,
                MaybeReservedVarInfo, SingleWidthVars,
                !FirstFreeSlot, !StackSlots)
        else
            true
        ),
        ( if set_of_var.is_non_empty(DoubleWidthVars) then
            align_double_width_slots(!FirstFreeSlot),
            allocate_next_stack_slot(MainStack, double_width,
                MaybeReservedVarInfo, DoubleWidthVars,
                !FirstFreeSlot, !StackSlots)
        else
            true
        )
    ).

:- pred allocate_next_stack_slot(main_stack::in, stack_slot_width::in,
    maybe(pair(prog_var, int))::in, set_of_progvar::in, int::in, int::out,
    stack_slots::in, stack_slots::out) is det.

allocate_next_stack_slot(MainStack, StackSlotWidth, MaybeReservedVarInfo, Vars,
        !FirstFreeSlot, !StackSlots) :-
    ( if
        MaybeReservedVarInfo = yes(ResVar - ResSlotNum),
        set_of_var.member(Vars, ResVar)
    then
        expect(unify(StackSlotWidth, single_width), $pred,
            "reserved multiple stack slots"),
        SlotNum = ResSlotNum
    else
        SlotNum = !.FirstFreeSlot,
        next_slot(StackSlotWidth, !FirstFreeSlot)
    ),
    (
        MainStack = det_stack,
        Locn = det_slot(SlotNum, StackSlotWidth)
    ;
        MainStack = nondet_stack,
        Locn = nondet_slot(SlotNum)
    ),
    VarList = set_of_var.to_sorted_list(Vars),
    allocate_given_stack_slot(Locn, VarList, !StackSlots).

:- pred allocate_given_stack_slot(stack_slot::in, list(prog_var)::in,
    stack_slots::in, stack_slots::out) is det.

allocate_given_stack_slot(_Slot, [], !StackSlots).
allocate_given_stack_slot(Slot, [Var | Vars], !StackSlots) :-
    map.det_insert(Var, Slot, !StackSlots),
    allocate_given_stack_slot(Slot, Vars, !StackSlots).

%-----------------------------------------------------------------------------%

    % We must not allocate the same stack slot to different dummy variables.
    % If we do, then the code that saves variables on the stack at calls
    % will get confused. After saving one dummy variable on the stack,
    % it will try to save the next in the same stack slot; believing
    % the first variable to still be live, it will move it away.
    %
    % In ordinary grades, it is possible to have one value of type io.state
    % and another of type store.store live at the same time; in debugging
    % grades, due to our policy of extending variable lifetimes, more than
    % one io.state may be live at the same time.
    %
:- pred allocate_stack_slots_for_dummy_vars(main_stack::in,
    assoc_list(prog_var, var_table_entry)::in,
    int::in, stack_slots::in, stack_slots::out) is det.

allocate_stack_slots_for_dummy_vars(_, [], _, !StackSlots).
allocate_stack_slots_for_dummy_vars(MainStack, [Var - Entry | VarsEntries],
        N0, !StackSlots) :-
    Entry = vte(_N, _T, IsDummy),
    (
        IsDummy = is_not_dummy_type,
        N1 = N0
    ;
        IsDummy = is_dummy_type,
        (
            MainStack = det_stack,
            Locn = det_slot(N0, single_width)
        ;
            MainStack = nondet_stack,
            Locn = nondet_slot(N0)
        ),
        allocate_given_stack_slot(Locn, [Var], !StackSlots),
        N1 = N0 - 1
    ),
    allocate_stack_slots_for_dummy_vars(MainStack, VarsEntries,
        N1, !StackSlots).

%-----------------------------------------------------------------------------%

:- func float_width_on_stack(globals, main_stack) = stack_slot_width.

float_width_on_stack(Globals, Stack) = FloatWidth :-
    % We only store unboxed double-width floats on the det stack.
    % It would be possible to do on the nondet stack but we would probably
    % need to pad the frame allocation at run time to ensure that any double
    % variables in the frame will be at aligned memory addresses.
    ( if
        Stack = det_stack,
        double_width_floats_on_det_stack(Globals, yes)
    then
        FloatWidth = double_width
    else
        FloatWidth = single_width
    ).

:- pred var_is_float(var_table::in, prog_var::in) is semidet.

var_is_float(VarTable, Var) :-
    lookup_var_type(VarTable, Var, float_type).

%-----------------------------------------------------------------------------%

    % Conform to memory alignment requirements for double-word values.
    % We maintain the invariant that the stack pointer is double-aligned.
    % The first stack variable is numbered 1 so all odd-numbered stack slots
    % are aligned. In a downwards-growing stack a higher slot number has a
    % lower address. When allocating two consecutive slots, we therefore want
    % the highered-numbered slot to be ODD.
    %
    % [!:FirstFreeSlot, !:FirstFreeSlot+1] shall be the next slots to be
    % allocated, therefore !:FirstFreeSlot must be EVEN and !:FirstFreeSlot+1
    % must be ODD.
    %
:- pred align_double_width_slots(int::in, int::out) is det.

align_double_width_slots(!FirstFreeSlot) :-
    ( if int.even(!.FirstFreeSlot) then
        true
    else
        !:FirstFreeSlot= !.FirstFreeSlot + 1
    ).

:- pred next_slot(stack_slot_width::in, int::in, int::out) is det.

next_slot(single_width, FirstFreeSlot, FirstFreeSlot + 1).
next_slot(double_width, FirstFreeSlot, FirstFreeSlot + 2).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.stack_alloc.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000,2002-2011 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: par_conj.m.
% Main authors: conway, wangp.
%
% The predicates of this module generate code for parallel conjunctions.
%
%---------------------------------------------------------------------------%
%
% Notes on parallel conjunction:
%
% A parallel conjunction (A & B) denotes that the goals `A' and `B' should
% be executed concurrently. Parallel conjunction has exactly the same
% declarative semantics as normal conjunction, but it has different (stricter)
% rules for mode-correctness and determinism-correctness, and it has different
% operational semantics.
%
%   [Operational semantics]
%   - `,'/2 gives some operational guarantees that `&'/2 does not:
%     if `--no-reorder-conj' is set, there is an implied ordering
%     in the code:  conjunctions must not be reordered beyond the
%     minimum necessary for mode correctness.
%     This is justified for reasons performance modeling and ensuring
%     predictable termination properties.
%     Parallel conjunction does not of itself suggest any information
%     about which order two goals should be executed, however if
%     coroutining is being used, then the data dependencies between
%     the two goals will constrain the order of execution at runtime.
%
%   [Mode correctness]
%   - `,'/2 has a *sequential* behaviour `A, B' proves `A' *then*
%     proves `B'. Mode analysis only allows unidirectional data-
%     dependencies for conjunction. In independent and-parallelism,
%     for the goal `A & B', mode analysis requires that `A' and `B'
%     bind disjoint sets of free variables (or when mode analysis
%     supports it properly, disjoint sets of type-nodes), and that
%     `A' does not require any bindings made in `B' and vice versa.
%     In dependant and-parallelism, mode analysis requires that each
%     variable (or type-node) have a unique producer (as in independent
%     and-parallelism), but an and-parallel goal may use bindings made
%     in conjoined goals which may lead to coroutining.
%
% The current implementation mainly independent and-parallelism and
% a subset of dependent and-parallelism (see dep_par_conj.m).
% The syntax for parallel conjunction is `&'/2 which behaves like `,'/2
% in that sequences get flattened (ie A & (B & C) <=> (A & B) & C).
%
% Type checking and mode analysis work exactly the same for parallel
% conjunction as for sequential conjunction.
%
% In principle, the determinism of a parallel conjunction is derived from
% its conjuncts in the same way as the determinism of a conjunction but
% because the current runtime implementation only allows model_det parallel
% conjunction, determinism analysis works by inferring the determinism of
% each conjunct and reporting an error if it is not a model_det determinism.
%
% The code generated for a parallel conjunction consists of a piece of
% initialization code which creates a term on the heap to be used for
% controlling the synchronization of the conjuncts and the code for the
% conjuncts.  The synchronization terms are referred to in the code as
% 'sync_term's.  Conjuncts are executed "left to right".  At the start of
% the i'th conjunct is a command to "spark" the i+1'th conjunct, i.e.
% record enough information to begin executing the next conjunct either
% in parallel, or to return to it after the current conjunct ends.
%
% At the end of each conjunct is a call to an join_and_continue instruction.
% It executes the next parallel conjunct or, if the parallel conjunction is
% finished, causes the code following the parallel conjunction to execute in
% the context that originated the parallel conjunction.  If the originating
% context can't execute the next conjunct and the parallel conjunction isn't
% finished, it must suspend.  When a non-originating context later finds that
% the parallel conjunction _is_ finished, it will then cause the originating
% context to resume execution at the join point.  Please see the
% implementation of MR_join_and_continue() for the details.
%
% The runtime support for parallel conjunction is documented in the runtime
% directory in mercury_context.{c,h}.
%
%---------------------------------------------------------------------------%

:- module ll_backend.par_conj_gen.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.llds.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred generate_par_conj(list(hlds_goal)::in, hlds_goal_info::in,
    code_model::in, llds_code::out, code_info::in, code_info::out) is det.

:- pred generate_loop_control(hlds_goal::in, prog_var::in, prog_var::in,
    llds_code::out, code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.instmap.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.code_info.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.exprn_aux.
:- import_module ll_backend.var_locn.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%

generate_par_conj(Goals, GoalInfo, CodeModel, Code, !CI) :-
    (
        CodeModel = model_det
    ;
        CodeModel = model_semi,
        sorry($module, $pred, "semidet parallel conjunction not implemented")
    ;
        CodeModel = model_non,
        sorry($module, $pred, "nondet parallel conjunction not implemented")
    ),

    get_globals(!.CI, Globals),
    globals.lookup_int_option(Globals, sync_term_size, STSize),

    % When entering a parallel conjunctions at the shallowest level in
    % the procedure, we have to set the parent_sp register to the value
    % of the sp register, and restore it when the parallel conjunction
    % finishes.
    get_par_conj_depth(!.CI, Depth),
    ( Depth = 0 ->
        acquire_temp_slot(slot_lval(parent_sp),
            non_persistent_temp_slot, ParentSpSlot, !CI),
        MaybeSetParentSpCode = from_list([
            llds_instr(assign(ParentSpSlot, lval(parent_sp)),
                "save the old parent stack pointer"),
            llds_instr(assign(parent_sp, lval(sp)),
                "set the parent stack pointer")
        ]),
        MaybeRestoreParentSpCode = singleton(
            llds_instr(assign(parent_sp, lval(ParentSpSlot)),
                "restore old parent stack pointer")
        ),
        MaybeReleaseParentSpSlot = yes(ParentSpSlot)
    ;
        MaybeSetParentSpCode = empty,
        MaybeRestoreParentSpCode = empty,
        MaybeReleaseParentSpSlot = no
    ),

    get_known_variables(!.CI, Vars),
    save_variables_on_stack(Vars, SaveCode, !CI),

    Nonlocals = goal_info_get_code_gen_nonlocals(GoalInfo),
    set_of_var.to_sorted_list(Nonlocals, Variables),
    get_instmap(!.CI, Initial),
    Delta = goal_info_get_instmap_delta(GoalInfo),
    instmap.apply_instmap_delta(Initial, Delta, Final),
    get_module_info(!.CI, ModuleInfo),
    find_outputs(Variables, Initial, Final, ModuleInfo, [], Outputs),

    % Reserve a contiguous block on the stack to hold the synchronisation term.
    Contents = list.duplicate(STSize, slot_sync_term),
    acquire_several_temp_slots(Contents, persistent_temp_slot, SyncTermSlots,
        StackId, _N, _M, !CI),
    (
        % The highest numbered slot has the lowest address.
        list.last(SyncTermSlots, SyncTermBaseSlotPrime),
        SyncTermBaseSlotPrime = stackvar(SlotNumPrime),
        StackId = det_stack
    ->
        SlotNum = SlotNumPrime,
        SyncTermBaseSlot = SyncTermBaseSlotPrime,
        ParentSyncTermBaseSlot = parent_stackvar(SlotNum)
    ;
        unexpected($module, $pred, "cannot find stack slot")
    ),

    NumGoals = list.length(Goals),
    create_static_conj_id(GoalInfo, StaticConjId, !CI),
    MakeSyncTermCode = singleton(
        llds_instr(init_sync_term(SyncTermBaseSlot, NumGoals, StaticConjId),
            "initialize sync term")
    ),

    set_par_conj_depth(Depth+1, !CI),
    get_next_label(EndLabel, !CI),
    clear_all_registers(no, !CI),
    generate_det_par_conj_2(Goals, ParentSyncTermBaseSlot, EndLabel, Initial,
        no, GoalCode, !CI),
    set_par_conj_depth(Depth, !CI),

    EndLabelCode = from_list([
        llds_instr(label(EndLabel), "end of parallel conjunction"),
        llds_instr(ts_finish_par_conj_instr(SlotNum, SyncTermBaseSlot),
            "finish parallel conjunction (ThreadScope instrumentation")
    ]),
    Code =
        MaybeSetParentSpCode ++
        SaveCode ++
        MakeSyncTermCode ++
        GoalCode ++
        EndLabelCode ++
        MaybeRestoreParentSpCode,

    % We can't release the sync slot right now, in case we are in a
    % nested parallel conjunction.  Consider:
    %
    %   (
    %       (A & B)   % inner1
    %   &
    %       (C & D)   % inner2
    %   )
    %
    % If inner1 released its sync slot now then it might end up being reused
    % by inner2.  But inner1 and inner2 could be executing simultaneously.
    % In general we can't release the sync slot of any parallel conjunction
    % until we leave the shallowest parallel conjunction, i.e. at depth 0.
    % For now we only release the sync slots of parallel conjunctions at the
    % top level.
    %
    % XXX release sync slots of nested parallel conjunctions

    ( Depth = 0 ->
        release_several_temp_slots(SyncTermSlots, persistent_temp_slot, !CI)
    ;
        true
    ),
    (
        MaybeReleaseParentSpSlot = yes(ParentSpSlot1),
        release_temp_slot(ParentSpSlot1, non_persistent_temp_slot, !CI)
    ;
        MaybeReleaseParentSpSlot = no
    ),
    clear_all_registers(no, !CI),
    place_all_outputs(Outputs, !CI).

:- pred generate_det_par_conj_2(list(hlds_goal)::in,
    lval::in, label::in, instmap::in, branch_end::in, llds_code::out,
    code_info::in, code_info::out) is det.

generate_det_par_conj_2([], _ParentSyncTermBaseSlot, _EndLabel,
        _Initial, _, empty, !CI).
generate_det_par_conj_2([Goal | Goals], ParentSyncTermBaseSlot, EndLabel,
        Initial, MaybeEnd0, Code, !CI) :-
    remember_position(!.CI, StartPos),
    code_gen.generate_goal(model_det, Goal, ThisGoalCode0, !CI),
    replace_stack_vars_by_parent_sv(ThisGoalCode0, ThisGoalCode),

    get_stack_slots(!.CI, AllSlots),
    get_known_variables(!.CI, Variables),
    set.list_to_set(Variables, LiveVars),
    map.select(AllSlots, LiveVars, StoreMap0),
    StoreMap = map.map_values_only(stack_slot_to_abs_locn, StoreMap0),
    generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd, SaveCode0, !CI),
    replace_stack_vars_by_parent_sv(SaveCode0, SaveCode),

    (
        Goals = [_ | _],
        get_next_label(NextConjunct, !CI),
        reset_to_position(StartPos, !CI),
        ForkCode = singleton(
            llds_instr(fork_new_child(ParentSyncTermBaseSlot, NextConjunct),
                "fork off a child")
        ),
        JoinCode = from_list([
            llds_instr(join_and_continue(ParentSyncTermBaseSlot, EndLabel),
                "finish"),
            llds_instr(label(NextConjunct),
                "start of the next conjunct")
        ])
    ;
        Goals = [],
        ForkCode = empty,
        JoinCode = singleton(
            llds_instr(join_and_continue(ParentSyncTermBaseSlot, EndLabel),
                "finish")
        )
    ),
    ThisCode = ForkCode ++ ThisGoalCode ++ SaveCode ++ JoinCode,
    generate_det_par_conj_2(Goals, ParentSyncTermBaseSlot, EndLabel, Initial,
        MaybeEnd, RestCode, !CI),
    Code = ThisCode ++ RestCode.

:- func ts_finish_par_conj_instr(int, lval) = instr.

ts_finish_par_conj_instr(SyncTermBaseSlot, SyncTermBaseSlotLval) =
        foreign_proc_code([], Components, proc_will_not_call_mercury, no, no,
            no, no, no, yes, proc_may_duplicate) :-
    Components = [foreign_proc_raw_code(cannot_branch_away,
        proc_does_not_affect_liveness,
        live_lvals_info(set([SyncTermBaseSlotLval])),
        format(Code, [i(SyncTermBaseSlot)]))],
    Code = "#ifdef MR_THREADSCOPE
MR_threadscope_post_end_par_conj(&MR_sv(%d));
#endif
".

%-----------------------------------------------------------------------------%

generate_loop_control(Goal, LCVar, LCSVar, Code, !CI) :-
    % We don't need to save the parent stack pointer, we do not use it in the
    % main context and all the worker contexts will never have some data that
    % we shouldn't clobber there.
    % We also expect the runtime code to setup the parent stack pointer for us.

    get_known_variables(!.CI, KnownVars),
    NonLocals = goal_info_get_nonlocals(Goal ^ hlds_goal_info),
    InputVars = set_of_var.intersect(NonLocals, list_to_set(KnownVars)),
    save_variables_on_stack(to_sorted_list(InputVars), SaveCode, !CI),

    % Create the call to spawn_off.
    remember_position(!.CI, PositionBeforeSpawnOff),

    get_next_label(SpawnOffLabel, !CI),
    best_variable_location_det(!.CI, LCVar, LCVarLocn),
    best_variable_location_det(!.CI, LCSVar, LCSVarLocn),
    SpawnOffCallCode =
        singleton(llds_instr(lc_spawn_off(lval(LCVarLocn), lval(LCSVarLocn),
                SpawnOffLabel),
            "Spawn off job for worker using loop control")),
    SpawnOffCode = SpawnOffCallCode,
    remember_position(!.CI, PositionAfterSpawnOff),

    % Code to spawn off.
    LabelCode = singleton(llds_instr(label(SpawnOffLabel),
        "Label for spawned off code")),
    reset_to_position(PositionBeforeSpawnOff, !CI),

    % We don't need to clear all the registers, all the variables except for
    % LC and LCS are considered to be on the stack.
    % mark only the registers used by LC and LCS as clobbered.
    clobber_regs([LCVarLocn, LCSVarLocn], !CI),

    generate_goal(model_det, Goal, GoalCode, !CI),
    % We expect that the join_and_terminate call is already in Goal.
    SpawnedOffCode0 = LabelCode ++ GoalCode,
    % Note: Zoltan, Peter and I (Paul) have discussed compressing the stack
    % frame of the spawned off computation.  This would be _instead of_ using
    % the parent stack pointer.  TODO: Before we can do this we need to
    % determine in which loop controls we should use the parent's stack frame,
    % and then perform this selectively.  The primitives in the runtime system
    % also need to support this.
    replace_stack_vars_by_parent_sv(SpawnedOffCode0, SpawnedOffCode),

    reset_to_position(PositionAfterSpawnOff, !CI),

    % The spawned off code is written into the procedure seperatly.
    add_out_of_line_code(SpawnedOffCode, !CI),

    % Concatentate the inline code.
    Code = SaveCode ++ SpawnOffCode.

%----------------------------------------------------------------------------%

:- pred best_variable_location_det(code_info::in, prog_var::in, lval::out)
    is det.

best_variable_location_det(CI, Var, Locn) :-
    promise_equivalent_solutions [Locn] (
        ( best_variable_location(CI, Var, LocnPrime) ->
            Locn = LocnPrime
        ;
            unexpected($module, $pred, "Could not find location for variable")
        )
    ).

:- pred best_variable_location(code_info::in, prog_var::in, lval::out)
    is nondet.

best_variable_location(CI, Var, Locn) :-
    variable_locations(CI, Map),
    map.search(Map, Var, AllLocns),
    filter(lval_is_reg, AllLocns, RegLocns),
    ( member(LocnPrime, RegLocns) ->
        % Commit to register locations before trying any location.
        LocnPrime = Locn
    ;
        member(Locn, AllLocns)
    ).

:- pred lval_is_reg(lval::in) is semidet.

lval_is_reg(reg(_, _)).

%-----------------------------------------------------------------------------%

    % In the code of parallel conjuncts we have to refer to stack slots in
    % the procedure's stack frame via the `parent_sp' register instead of the
    % usual `sp' register, as the conjunct could be running in a different
    % context.
    %
:- pred replace_stack_vars_by_parent_sv(llds_code::in, llds_code::out) is det.

replace_stack_vars_by_parent_sv(!Code) :-
    cord.map_pred(replace_stack_vars_by_parent_sv_instr, !Code).

:- pred replace_stack_vars_by_parent_sv_instr(instruction::in,
    instruction::out) is det.

replace_stack_vars_by_parent_sv_instr(!Instr) :-
    transform_lval_in_instr(replace_stack_vars_by_parent_sv_lval,
        !Instr, unit, _).

:- pred replace_stack_vars_by_parent_sv_lval(lval::in, lval::out,
    unit::in, unit::out) is det.

replace_stack_vars_by_parent_sv_lval(Lval0, Lval, !Acc) :-
    TransformRval = replace_stack_vars_by_parent_sv_lval,
    (
        ( Lval0 = stackvar(SlotNum)
        ; Lval0 = parent_stackvar(SlotNum)
        ),
        Lval = parent_stackvar(SlotNum)
    ;
        Lval0 = double_stackvar(Type, SlotNum),
        (
            Type = double_stackvar,
            Lval = double_stackvar(double_parent_stackvar, SlotNum)
        ;
            Type = double_parent_stackvar,
            Lval = Lval0
        ;
            Type = double_framevar,
            Lval = Lval0
        )
    ;
        ( Lval0 = reg(_Type, _RegNum)
        ; Lval0 = succip
        ; Lval0 = maxfr
        ; Lval0 = curfr
        ; Lval0 = hp
        ; Lval0 = sp
        ; Lval0 = parent_sp
        ; Lval0 = temp(_Type, _TmpNum)
        ; Lval0 = framevar(_SlotNum)
        ; Lval0 = lvar(_Var)
        ; Lval0 = global_var_ref(_GlobalVarName)
        ),
        Lval = Lval0
    ;
        Lval0 = succip_slot(Rval0),
        transform_lval_in_rval(TransformRval, Rval0, Rval, !Acc),
        Lval = succip_slot(Rval)
    ;
        Lval0 = redoip_slot(Rval0),
        transform_lval_in_rval(TransformRval, Rval0, Rval, !Acc),
        Lval = redoip_slot(Rval)
    ;
        Lval0 = redofr_slot(Rval0),
        transform_lval_in_rval(TransformRval, Rval0, Rval, !Acc),
        Lval = redofr_slot(Rval)
    ;
        Lval0 = succfr_slot(Rval0),
        transform_lval_in_rval(TransformRval, Rval0, Rval, !Acc),
        Lval = succfr_slot(Rval)
    ;
        Lval0 = prevfr_slot(Rval0),
        transform_lval_in_rval(TransformRval, Rval0, Rval, !Acc),
        Lval = prevfr_slot(Rval)
    ;
        Lval0 = field(Tag, Rval1, Rval2),
        transform_lval_in_rval(TransformRval, Rval1, Rval3, !Acc),
        transform_lval_in_rval(TransformRval, Rval2, Rval4, !Acc),
        Lval = field(Tag, Rval3, Rval4)
    ;
        Lval0 = mem_ref(Rval0),
        transform_lval_in_rval(TransformRval, Rval0, Rval, !Acc),
        Lval = mem_ref(Rval)
    ).

%-----------------------------------------------------------------------------%

:- pred find_outputs(list(prog_var)::in, instmap::in, instmap::in,
    module_info::in, list(prog_var)::in, list(prog_var)::out) is det.

find_outputs([], _Initial, _Final, _ModuleInfo, !Outputs).
find_outputs([Var | Vars],  Initial, Final, ModuleInfo, !Outputs) :-
    instmap_lookup_var(Initial, Var, InitialInst),
    instmap_lookup_var(Final, Var, FinalInst),
    ( mode_is_output(ModuleInfo, (InitialInst -> FinalInst)) ->
        !:Outputs = [Var | !.Outputs]
    ;
        !:Outputs = !.Outputs
    ),
    find_outputs(Vars, Initial, Final, ModuleInfo, !Outputs).

:- pred place_all_outputs(list(prog_var)::in, code_info::in, code_info::out)
    is det.

place_all_outputs([], !CI).
place_all_outputs([Var | Vars], !CI) :-
    variable_locations(!.CI, VarLocations),
    get_variable_slot(!.CI, Var, Slot),
    (
        map.search(VarLocations, Var, Locations),
        set.member(Slot, Locations)
    ->
        true
    ;
        set_var_location(Var, Slot, !CI)
    ),
    place_all_outputs(Vars, !CI).

%----------------------------------------------------------------------------%

:- pred create_static_conj_id(hlds_goal_info::in, int::out,
    code_info::in, code_info::out) is det.

create_static_conj_id(GoalInfo, SlotNum, !CI) :-
    get_pred_id(!.CI, PredId),
    get_proc_id(!.CI, ProcId),
    get_module_info(!.CI, ModuleInfo),
    ProcString = pred_proc_id_pair_to_string(ModuleInfo, PredId, ProcId),

    get_containing_goal_map_det(!.CI, ContainingGoalMap),
    GoalId = goal_info_get_goal_id(GoalInfo),
    GoalPath = goal_id_to_forward_path(ContainingGoalMap, GoalId),
    GoalPathString = goal_path_to_string(GoalPath),

    String = format("%s: %s", [s(ProcString), s(GoalPathString)]),
    add_threadscope_string(String, SlotNum, !CI).

%----------------------------------------------------------------------------%
:- end_module ll_backend.par_conj_gen.
%----------------------------------------------------------------------------%

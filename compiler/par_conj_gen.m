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
% Operational semantics:
%
%   `,'/2 gives some operational guarantees that `&'/2 does not.
%   If `--no-reorder-conj' is set, sequential conjunction provides
%   an implied ordering to the code: the conjuncts must not be reordered
%   beyond the minimum that is necessary for mode correctness.
%   The reason for this is to allow programmers to model the performance
%   of the code more simply, which also includes (in the extreme)
%   simpler reasoning about termination properties.
%
%   Parallel conjunctions do not specify the order in which their conjuncts
%   will be executed. However, any data dependencies between conjuncts
%   will constrain the order of those conjuncts' execution at runtime.
%   If the conjunction is executed with coroutining, a data dependency
%   constrains the order of the start times of the conjuncts involved;
%   if the conjunction is executed in parallel, the constraint does not
%   extend that far, and imposes only a requirement for the standard
%   reader-writer synchronization.
%
% Mode correctness:
%   `,'/2 has a *sequential* behaviour: `A, B' proves *first* `A' and
%   *then* proves `B'. Mode analysis only allows unidirectional data-
%   dependencies for conjunction. Applying independent and-parallelism
%   to `A & B', mode analysis would require that `A' and `B' bind
%   disjoint sets of free variables (or when mode analysis supports
%   it properly, disjoint sets of type-nodes), and that `A' does not
%   require any bindings made in `B' and vice versa.
%   With dependant and-parallelism, mode analysis requires that each
%   variable (or type-node) have a unique producer (as in independent
%   and-parallelism), but an and-parallel goal may use bindings made
%   in conjoined goals to its left which may lead to coroutining.
%   (Allowing it to use conjoined goals to its right would, in general,
%   allow circular data dependencies, which would lead to deadlock.)
%
% The current system implements mainly independent and-parallelism and
% a subset of dependent and-parallelism (see dep_par_conj.m).
%
% Type checking and mode analysis work exactly the same for parallel
% conjunction as for sequential conjunction.
%
% In principle, the determinism of a parallel conjunction is derived from
% its conjuncts in the same way as the determinism of a conjunction but
% because the current runtime implementation only allows model_det parallel
% conjunction, determinism analysis works by inferring the determinism of
% each conjunct and reporting an error if it is not model_det.
%
% The code we generate for a parallel conjunction consists of
%
%   - a piece of initialization code, which creates a term on the heap
%     that we use for controlling the synchronization of the conjuncts
%     (we call them `sync_term's), and
%   - the code for the conjuncts themselves.
%
% We execute conjuncts left to right. Before the code of the i'th conjunct,
% we put code to "spark" the i+1'th conjunct (if there is one). Sparking
% a conjunct means either assigning its execution to a spare thread
% (if there is one), or recording the information needed to begin executing it
% when a thread becomes available. A thread *should* become available
% when the ith conjunct ends.
%
% At the end of each conjunct we put a join_and_continue instruction.
% This executes the next parallel conjunct if there is one, or if there isn't,
% waits until all the conjuncts are finished, and then branches to the code
% *following* the parallel conjunction. We take care to ensure that the
% code following the parallel conjunction executes in the context
% that *originated* the parallel conjunction. If the originating context
% can't execute the next conjunct and the parallel conjunction isn't finished,
% it must suspend. When a non-originating context later finds that
% the parallel conjunction _is_ finished, it will then cause the originating
% context to resume execution at the join point. Please see the
% implementation of MR_join_and_continue() for the details.
%
% The runtime support for parallel conjunction is documented in the runtime
% directory in mercury_context.{c,h}.
%
%---------------------------------------------------------------------------%

:- module ll_backend.par_conj_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.llds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred generate_par_conj(list(hlds_goal)::in, hlds_goal_info::in,
    code_model::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred generate_lc_spawn_off(hlds_goal::in, prog_var::in, prog_var::in,
    lc_use_parent_stack::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_test.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.code_util.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.exprn_aux.
:- import_module ll_backend.llds_out.
:- import_module ll_backend.llds_out.llds_out_data.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%

generate_par_conj(Conjuncts, GoalInfo, CodeModel, Code, !CI, !CLD) :-
    % Some sanity checks.
    (
        CodeModel = model_det
    ;
        CodeModel = model_semi,
        sorry($pred, "semidet parallel conjunction not implemented")
    ;
        CodeModel = model_non,
        sorry($pred, "nondet parallel conjunction not implemented")
    ),

    get_globals(!.CI, Globals),
    globals.lookup_int_option(Globals, sync_term_size, STSize),

    % When entering a parallel conjunctions at the shallowest level in
    % the procedure, we have to set the parent_sp register to the value
    % of the sp register, and restore it when the parallel conjunction
    % finishes.
    get_par_conj_depth(!.CLD, Depth),
    ( if Depth = 0 then
        acquire_temp_slot(slot_lval(parent_sp),
            non_persistent_temp_slot, ParentSpSlot, !CI, !CLD),
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
    else
        MaybeSetParentSpCode = empty,
        MaybeRestoreParentSpCode = empty,
        MaybeReleaseParentSpSlot = no
    ),

    get_known_variables(!.CLD, Vars),
    save_variables_on_stack(Vars, SaveCode, !.CI, !CLD),

    Nonlocals = goal_info_get_code_gen_nonlocals(GoalInfo),
    set_of_var.to_sorted_list(Nonlocals, Variables),
    get_instmap(!.CLD, InitialInstMap),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    apply_instmap_delta(InstMapDelta, InitialInstMap, FinalInstMap),
    get_module_info(!.CI, ModuleInfo),
    find_outputs(Variables, InitialInstMap, FinalInstMap, ModuleInfo,
        [], Outputs),

    % Reserve a contiguous block on the stack to hold the synchronisation term.
    Contents = list.duplicate(STSize, slot_sync_term),
    acquire_several_temp_slots(Contents, persistent_temp_slot, SyncTermSlots,
        StackId, _N, _M, !CI, !CLD),
    ( if
        % The highest numbered slot has the lowest address.
        list.last(SyncTermSlots, SyncTermBaseSlotPrime),
        SyncTermBaseSlotPrime = stackvar(SlotNumPrime),
        StackId = det_stack
    then
        SlotNum = SlotNumPrime,
        SyncTermBaseSlot = SyncTermBaseSlotPrime,
        ParentSyncTermBaseSlot = parent_stackvar(SlotNum)
    else
        unexpected($pred, "cannot find stack slot")
    ),

    NumConjuncts = list.length(Conjuncts),
    create_static_conj_id(GoalInfo, StaticConjId, !CI),
    MakeSyncTermCode = singleton(
        llds_instr(
            init_sync_term(SyncTermBaseSlot, NumConjuncts, StaticConjId),
            "initialize sync term")
    ),

    get_next_label(EndLabel, !CI),
    remember_position(!.CLD, BeforeConjunctionPos),

    some [!InConjunctionCLD] (
        !:InConjunctionCLD = !.CLD,
        set_par_conj_depth(Depth+1, !InConjunctionCLD),
        clear_all_registers(no, !InConjunctionCLD),
        remember_position(!.InConjunctionCLD, ConjunctStartPos)
    ),
    generate_det_par_conjuncts(ConjunctStartPos, InitialInstMap,
        EndLabel, ParentSyncTermBaseSlot, Conjuncts, no, GoalCode, !CI),

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
    % nested parallel conjunction. Consider:
    %
    %   (
    %       (A & B)   % inner1
    %   &
    %       (C & D)   % inner2
    %   )
    %
    % If inner1 released its sync slot now then it might end up being reused
    % by inner2. But inner1 and inner2 could be executing simultaneously.
    % In general we can't release the sync slot of any parallel conjunction
    % until we leave the shallowest parallel conjunction, i.e. at depth 0.
    % For now we only release the sync slots of parallel conjunctions at the
    % top level.
    %
    % XXX Release the sync slots of nested parallel conjunctions.

    reset_to_position(BeforeConjunctionPos, !.CI, !:CLD),
    ( if Depth = 0 then
        release_several_temp_slots(SyncTermSlots, persistent_temp_slot,
            !CI, !CLD)
    else
        true
    ),
    (
        MaybeReleaseParentSpSlot = yes(ParentSpSlot1),
        release_temp_slot(ParentSpSlot1, non_persistent_temp_slot, !CI, !CLD)
    ;
        MaybeReleaseParentSpSlot = no
    ),
    clear_all_registers(no, !CLD),
    place_all_outputs(Outputs, !.CI, !CLD).

:- pred generate_det_par_conjuncts(position_info::in, instmap::in, label::in,
    lval::in, list(hlds_goal)::in, branch_end::in, llds_code::out,
    code_info::in, code_info::out) is det.

generate_det_par_conjuncts(_ConjunctStartPos, _InitialInstMap, _EndLabel,
        _ParentSyncTermBaseSlot, [], _, empty, !CI).
generate_det_par_conjuncts(ConjunctStartPos, InitialInstMap, EndLabel,
        ParentSyncTermBaseSlot, [Conjunct | Conjuncts], MaybeEnd0, Code,
        !CI) :-
    some [!CLD] (
        reset_to_position(ConjunctStartPos, !.CI, !:CLD),
        code_gen.generate_goal(model_det, Conjunct, ThisConjunctCode0,
            !CI, !CLD),
        replace_stack_vars_by_parent_sv(ThisConjunctCode0, ThisConjunctCode),

        get_stack_slots(!.CI, AllSlots),
        get_known_variables(!.CLD, Variables),
        set.list_to_set(Variables, LiveVars),
        map.select(AllSlots, LiveVars, StoreMap0),
        StoreMap = map.map_values_only(stack_slot_to_abs_locn, StoreMap0),
        generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd, SaveCode0, !.CLD),
        replace_stack_vars_by_parent_sv(SaveCode0, SaveCode)
    ),

    (
        Conjuncts = [_ | _],
        get_next_label(NextConjunct, !CI),
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
        Conjuncts = [],
        ForkCode = empty,
        JoinCode = singleton(
            llds_instr(join_and_continue(ParentSyncTermBaseSlot, EndLabel),
                "finish")
        )
    ),
    ConjunctCode = ForkCode ++ ThisConjunctCode ++ SaveCode ++ JoinCode,
    generate_det_par_conjuncts(ConjunctStartPos, InitialInstMap, EndLabel,
        ParentSyncTermBaseSlot, Conjuncts, MaybeEnd, ConjunctsCode, !CI),
    Code = ConjunctCode ++ ConjunctsCode.

:- func ts_finish_par_conj_instr(int, lval) = instr.

ts_finish_par_conj_instr(SyncTermBaseSlot, SyncTermBaseSlotLval) = Instr :-
    CodeTemplate =
"#ifdef MR_THREADSCOPE
MR_threadscope_post_end_par_conj(&MR_sv(%d));
#endif
",
    Components = [foreign_proc_raw_code(cannot_branch_away,
        proc_does_not_affect_liveness,
        live_lvals_info(set.make_singleton_set(SyncTermBaseSlotLval)),
        string.format(CodeTemplate, [i(SyncTermBaseSlot)]))],
    Instr = foreign_proc_code([], Components, proc_will_not_call_mercury,
        no, no, no, no, no, refers_to_llds_stack, proc_may_duplicate).

%-----------------------------------------------------------------------------%

generate_lc_spawn_off(Goal, LCVar, LCSVar, UseParentStack, Code, !CI, !CLD) :-
    % We don't need to save the parent stack pointer, we do not use it in the
    % main context and all the worker contexts will never have some data that
    % we shouldn't clobber there.
    % We also expect the runtime code to setup the parent stack pointer for us.

    get_known_variables(!.CLD, KnownVars),
    KnownVarsSet = set_of_var.list_to_set(KnownVars),
    NonLocalsSet = goal_info_get_nonlocals(Goal ^ hg_info),
    InputVarsSet = set_of_var.intersect(NonLocalsSet, KnownVarsSet),
    InputVars = set_of_var.to_sorted_list(InputVarsSet),
    save_variables_on_stack(InputVars, SaveCode, !.CI, !CLD),

    best_variable_location_det(!.CLD, LCVar, LCVarLocn),
    best_variable_location_det(!.CLD, LCSVar, LCSVarLocn),

    get_next_label(SpawnOffLabel, !CI),
    SpawnUinstr = lc_spawn_off(lval(LCVarLocn), lval(LCSVarLocn),
        SpawnOffLabel),
    SpawnInstr = llds_instr(SpawnUinstr, ""),
    SpawnOffCode = singleton(SpawnInstr),
    remember_position(!.CLD, PositionAfterSpawnOff),

    % Code to spawn off.
    LabelUinstr = label(SpawnOffLabel),
    LabelInstr = llds_instr(LabelUinstr, "Label for spawned off code"),
    LabelCode = singleton(LabelInstr),

    % We don't need to clear all the registers, all the variables except for
    % LC and LCS are considered to be on the stack.
    % Mark only the registers used by LC and LCS as clobbered.
    clobber_regs([LCVarLocn, LCSVarLocn], !CLD),

    generate_goal(model_det, Goal, GoalCode, !CI, !.CLD, _CLDAfterGoal),
    % We expect that the join_and_terminate call is already in Goal.
    SpawnedOffCode0 = LabelCode ++ GoalCode,

    reset_to_position(PositionAfterSpawnOff, !.CI, !:CLD),

    (
        UseParentStack = lc_use_parent_stack_frame,
        replace_stack_vars_by_parent_sv(SpawnedOffCode0, SpawnedOffCode),
        CopyCode = cord.empty,

        % Mark the output values as available in registers, code inserted after
        % the recursive call expects to be able to read them. Because they are
        % guaranteed to be placed in distinct stack slots it's okay to produce
        % them a little early - really they could be produced from any point
        % after spawn_off until the barrier in the base case.

        % This module has a find_outputs predicate, but I can't see how set
        % difference wouldn't work.
        OutputVars = set_of_var.difference(NonLocalsSet, KnownVarsSet),
        place_all_outputs(set_of_var.to_sorted_list(OutputVars), !.CI, !CLD)
    ;
        UseParentStack = lc_create_frame_on_child_stack,
        list.map(get_variable_slot(!.CI), InputVars, InputStackSlots),

        % XXX: We could take this opportunity to remove gaps in the stack
        % frame as discussed in our meetings.
        instr_list_max_stack_ref(SpawnedOffCode0, MaxStackRef),
        SpawnedOffCode = SpawnedOffCode0,

        % We only know the size of the stack frame for certain after we have
        % finished generating code for the procedure.
        %
        % There are several different ways we can set the size of the first
        % stack frame on the stack of the child context.
        %
        % We have chosen to implement this by collecting all the stackvar
        % references in SpawnedOffCode0, and base the size of the child stack
        % frame on the highest numbered stackvar reference in there.
        %
        % We could also compress out any stack slots that are not used by
        % GoalCode. This would require remapping all the stackvar references in
        % GoalCode, as well as applying the compression map during the creation
        % of CopyStr.
        FrameSize = MaxStackRef,
        copy_slots_to_child_stack(FrameSize, LCVarLocn, LCSVarLocn,
            InputStackSlots, CopyStr),
        AffectsLiveness = proc_does_not_affect_liveness,
        LiveLvalsInfo = live_lvals_info(
            set.list_to_set([LCVarLocn, LCSVarLocn | InputStackSlots])),
        CopyUinstr = arbitrary_c_code(AffectsLiveness, LiveLvalsInfo,
            CopyStr),
        CopyInstr = llds_instr(CopyUinstr, ""),
        CopyCode = singleton(CopyInstr)
    ),

    % The spawned off code is written into the procedure separately.
    add_out_of_line_code(SpawnedOffCode, !CI),

    % Concatenate the inline code.
    Code = SaveCode ++ CopyCode ++ SpawnOffCode.

:- pred copy_slots_to_child_stack(int::in, lval::in, lval::in, list(lval)::in,
    string::out) is det.

copy_slots_to_child_stack(FrameSize, LCVarLocn, LCSVarLocn, StackSlots,
        CodeStr) :-
    ( if
        LCVarNamePrime = lval_to_string(LCVarLocn),
        LCSVarNamePrime = lval_to_string(LCSVarLocn)
    then
        LCVarName = LCVarNamePrime,
        LCSVarName = LCSVarNamePrime
    else
        unexpected($pred, "cannot convert to string")
    ),

    FirstLine = "{\n",
    IncrLine = string.format("\tMR_lc_inc_worker_sp(%s, %s, %d);\n",
        [s(LCVarName), s(LCSVarName), i(FrameSize)]),
    list.map(copy_one_slot_to_child_stack(LCVarName, LCSVarName),
        StackSlots, CopyStrs),
    string.append_list(CopyStrs, CopyLines),
    LastLine = "\t}\n",
    CodeStr = FirstLine ++ IncrLine ++ CopyLines ++ LastLine.

:- pred copy_one_slot_to_child_stack(string::in, string::in, lval::in,
    string::out) is det.

copy_one_slot_to_child_stack(LCVarName, LCSVarName, StackSlot, CopyStr) :-
    ( if StackSlotName = lval_to_string(StackSlot) then
        ( if StackSlot = stackvar(N) then
            CopyStr = string.format("\tMR_lc_worker_sv(%s, %s, %d) = %s;\n",
                [s(LCVarName), s(LCSVarName), i(N), s(StackSlotName)])
        else
            unexpected($pred, "not stack slot")
        )
    else
        unexpected($pred, "cannot convert to string")
    ).

%----------------------------------------------------------------------------%

:- pred best_variable_location_det(code_loc_dep::in, prog_var::in, lval::out)
    is det.

best_variable_location_det(CLD, Var, Locn) :-
    variable_locations(CLD, Map),
    map.lookup(Map, Var, AllLocnsSet),
    set.to_sorted_list(AllLocnsSet, AllLocns),
    list.filter(lval_is_reg, AllLocns, RegLocns),
    (
        RegLocns = [Locn | _]
    ;
        RegLocns = [],
        (
            AllLocns = [Locn | _]
        ;
            AllLocns = [],
            unexpected($pred, "Could not find location for variable")
        )
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

:- pred instr_list_max_stack_ref(cord(instruction)::in, int::out) is det.

instr_list_max_stack_ref(Instrs, MaxRef) :-
    instrs_rvals_and_lvals(cord.list(Instrs), RVals, LVals0),
    LValsInRvalsLists = list.map(lvals_in_rval, set.to_sorted_list(RVals)),
    LValsSets = list.map(set.list_to_set, LValsInRvalsLists),
    LVals = set.union_list(LValsSets) `set.union` LVals0,
    set.fold(max_stack_ref_acc, LVals, 0, MaxRef).

:- pred max_stack_ref_acc(lval::in, int::in, int::out) is det.

max_stack_ref_acc(LVal, Max0, Max) :-
    ( if
        LVal = stackvar(N),
        N > Max0
    then
        Max = N
    else
        Max = Max0
    ).

%-----------------------------------------------------------------------------%

:- pred find_outputs(list(prog_var)::in, instmap::in, instmap::in,
    module_info::in, list(prog_var)::in, list(prog_var)::out) is det.

find_outputs([], _Initial, _Final, _ModuleInfo, !Outputs).
find_outputs([Var | Vars],  Initial, Final, ModuleInfo, !Outputs) :-
    instmap_lookup_var(Initial, Var, InitialInst),
    instmap_lookup_var(Final, Var, FinalInst),
    ( if mode_is_output(ModuleInfo, from_to_mode(InitialInst, FinalInst)) then
        !:Outputs = [Var | !.Outputs]
    else
        !:Outputs = !.Outputs
    ),
    find_outputs(Vars, Initial, Final, ModuleInfo, !Outputs).

:- pred place_all_outputs(list(prog_var)::in,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

place_all_outputs([], _CI, !CLD).
place_all_outputs([Var | Vars], CI, !CLD) :-
    variable_locations(!.CLD, VarLocations),
    get_variable_slot(CI, Var, Slot),
    ( if
        map.search(VarLocations, Var, Locations),
        set.member(Slot, Locations)
    then
        true
    else
        set_var_location(Var, Slot, !CLD)
    ),
    place_all_outputs(Vars, CI, !CLD).

%----------------------------------------------------------------------------%

:- pred create_static_conj_id(hlds_goal_info::in, int::out,
    code_info::in, code_info::out) is det.

create_static_conj_id(GoalInfo, SlotNum, !CI) :-
    get_pred_id(!.CI, PredId),
    get_proc_id(!.CI, ProcId),
    get_module_info(!.CI, ModuleInfo),
    ProcString = pred_proc_id_pair_to_dev_string(ModuleInfo, PredId, ProcId),

    get_containing_goal_map(!.CI, ContainingGoalMap),
    GoalId = goal_info_get_goal_id(GoalInfo),
    GoalPath = goal_id_to_forward_path(ContainingGoalMap, GoalId),
    GoalPathString = goal_path_to_string(GoalPath),

    String = format("%s: %s", [s(ProcString), s(GoalPathString)]),
    add_threadscope_string(String, SlotNum, !CI).

%----------------------------------------------------------------------------%
:- end_module ll_backend.par_conj_gen.
%----------------------------------------------------------------------------%

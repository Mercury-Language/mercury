%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2000,2002-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: disj_gen.m.
% Main authors: conway, zs.
%
% The predicates of this module generate code for disjunctions.
%
% The outlines of the implementation of lookup disjunctions here is shared
% with similar code in ml_disj_gen.m that does the same thing for the MLDS
% backend. Any changes here may need to be reflected there as well.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.disj_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.llds.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred generate_disj(code_model::in, list(hlds_goal)::in, hlds_goal_info::in,
    llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_llds.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.global_data.
:- import_module ll_backend.lookup_util.
:- import_module ll_backend.trace_gen.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module term.

%-----------------------------------------------------------------------------%

generate_disj(CodeModel, Goals, DisjGoalInfo, Code, !CI, !CLD) :-
    (
        Goals = [],
        (
            CodeModel = model_semi,
            remember_position(!.CLD, BeforeFailure),
            generate_failure(Code, !CI, !.CLD),
            reset_to_position(BeforeFailure, !.CI, !:CLD)
        ;
            ( CodeModel = model_det
            ; CodeModel = model_non
            ),
            unexpected($pred, "empty disjunction")
        )
    ;
        Goals = [Goal | _],
        Goal = hlds_goal(_, GoalInfo),
        goal_info_get_resume_point(GoalInfo, Resume),
        (
            Resume = resume_point(ResumeVarsPrime, _),
            ResumeVars = ResumeVarsPrime
        ;
            Resume = no_resume_point,
            ResumeVars = set_of_var.init
        ),
        AddTrailOps = should_add_trail_ops(!.CI, GoalInfo),
        AddRegionOps = should_add_region_ops(!.CI, GoalInfo),
        ( if
            CodeModel = model_non,
            is_lookup_disj(AddTrailOps, AddRegionOps, ResumeVars, Goals,
                DisjGoalInfo, LookupDisjInfo, !CI, !.CLD, AfterPrepPos)
        then
            reset_to_position(AfterPrepPos, !.CI, !:CLD),
            generate_lookup_disj(ResumeVars, LookupDisjInfo, Code, !CI, !CLD)
        else
            generate_real_disj(AddTrailOps, AddRegionOps, CodeModel,
                ResumeVars, Goals, DisjGoalInfo, Code, !CI, !CLD)
        )
    ).

:- type lookup_disj_info
    --->    lookup_disj_info(
                ldi_variables           :: list(prog_var),
                                        % The output variables.

                ldi_store_map           :: abs_store_map,
                ldi_branch_end          :: branch_end,
                ldi_liveness            :: set_of_progvar,

                lds_cur_slot            :: lval,

                lds_resume_map          :: resume_map,
                lds_flush_code          :: llds_code,

                lds_save_ticket_code    :: llds_code,
                lds_maybe_ticket_slot   :: maybe(lval),

                lds_save_hp_code        :: llds_code,
                lds_maybe_hp_slot       :: maybe(lval),

                lds_hijack_info         :: disj_hijack_info,
                lds_prepare_hijack_code :: llds_code,

                ldi_solns               :: list(list(rval)),
                ldi_field_types         :: list(llds_type)
            ).

:- pred is_lookup_disj(add_trail_ops::in, add_region_ops::in,
    set_of_progvar::in, list(hlds_goal)::in, hlds_goal_info::in,
    lookup_disj_info::out,
    code_info::in, code_info::out, code_loc_dep::in, position_info::out)
    is semidet.

is_lookup_disj(AddTrailOps, AddRegionOps, ResumeVars, Disjuncts, DisjGoalInfo,
        LookupDisjInfo, !CI, !.CLD, AfterPrepPos) :-
    % Since we generate two code sequences, one for the first solution and one
    % for all the later solutions, we don't get any benefit over the code
    % sequence generated by generate_real_disj unless there are at least three
    % solutions.
    Disjuncts = [FirstDisjunct | LaterDisjuncts],
    LaterDisjuncts = [_, _ | _],

    DisjNonLocals = goal_info_get_nonlocals(DisjGoalInfo),
    goal_is_conj_of_unify(DisjNonLocals, FirstDisjunct),
    all_disjuncts_are_conj_of_unify(DisjNonLocals, LaterDisjuncts),

    get_maybe_trace_info(!.CI, MaybeTraceInfo),
    MaybeTraceInfo = no,

    % Lookup disjunctions rely on static ground terms to work.
    get_globals(!.CI, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    OptTuple ^ ot_use_static_ground_cells = use_static_ground_cells,

    % XXX The code to generate lookup disjunctions hasn't yet been updated
    % to handle region operations.
    AddRegionOps = do_not_add_region_ops,

    figure_out_output_vars(!.CI, !.CLD, DisjGoalInfo, OutVars),
    get_var_table(!.CI, VarTable),
    OutTypes = list.map(lookup_var_type_func(VarTable), OutVars),

    produce_vars(set_of_var.to_sorted_list(ResumeVars), ResumeMap,
        FlushCode, !CLD),

    % We cannot release this stack slot anywhere within the disjunction,
    % since it will be needed after backtracking to later disjuncts.
    % However, if we are inside an outer branched control structure
    % (disjunction, switch, if-then-else), it may be released (implicitly)
    % when we get into the next branch of that outer control structure.
    acquire_temp_slot(slot_lookup_disj_cur, non_persistent_temp_slot, CurSlot,
        !CI, !CLD),
    maybe_save_ticket(AddTrailOps, SaveTicketCode, MaybeTicketSlot, !CI, !CLD),
    get_globals(!.CI, Globals),
    globals.lookup_bool_option(Globals, reclaim_heap_on_nondet_failure,
        ReclaimHeap),
    maybe_save_hp(ReclaimHeap, SaveHpCode, MaybeHpSlot, !CI, !CLD),
    prepare_for_disj_hijack(model_non, HijackInfo, PrepareHijackCode,
        !CI, !CLD),

    % Every update to the code_info that needs to persist beyond the
    % disjunction as a whole (e.g. the reservations of the stack slots required
    % to implement the hijack) must be done before we remember this position.
    remember_position(!.CLD, AfterPrepPos),

    goal_info_get_store_map(DisjGoalInfo, StoreMap),
    remember_position(!.CLD, DisjStart),
    generate_constants_for_disjunct(DisjStart, FirstDisjunct, OutVars,
        StoreMap, FirstSoln, no, MaybeEnd1, Liveness, !CI),
    generate_constants_for_disjuncts(DisjStart, LaterDisjuncts, OutVars,
        StoreMap, LaterSolns, MaybeEnd1, MaybeEnd, !CI),
    Solns = [FirstSoln | LaterSolns],

    get_exprn_opts(!.CI, ExprnOpts),
    UnboxFloats = get_unboxed_floats(ExprnOpts),
    UnboxInt64s = get_unboxed_int64s(ExprnOpts),
    find_general_llds_types(UnboxFloats, UnboxInt64s, OutTypes, Solns,
        LLDSTypes),
    LookupDisjInfo = lookup_disj_info(OutVars, StoreMap, MaybeEnd, Liveness,
        CurSlot, ResumeMap, FlushCode, SaveTicketCode, MaybeTicketSlot,
        SaveHpCode, MaybeHpSlot, HijackInfo, PrepareHijackCode,
        Solns, LLDSTypes).

:- pred generate_lookup_disj(set_of_progvar::in, lookup_disj_info::in,
    llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_lookup_disj(ResumeVars, LookupDisjInfo, Code, !CI, !CLD) :-
    LookupDisjInfo = lookup_disj_info(OutVars, StoreMap, MaybeEnd0, Liveness,
        CurSlot, ResumeMap, FlushCode, SaveTicketCode, MaybeTicketSlot,
        SaveHpCode, MaybeHpSlot, HijackInfo, PrepareHijackCode,
        Solns, LLDSTypes),

    list.length(Solns, NumSolns),
    list.length(OutVars, NumOutVars),

    add_vector_static_cell(LLDSTypes, Solns, SolnVectorAddr, !CI),
    SolnVectorAddrRval = const(llconst_data_addr(SolnVectorAddr, no)),

    get_next_label(EndLabel, !CI),

    % Since we release BaseReg only after the calls to generate_branch_end
    % (invoked through set_liveness_and_end_branch) we must ensure that
    % generate_branch_end won't want to overwrite BaseReg.
    acquire_reg_not_in_storemap(StoreMap, reg_r, BaseReg, !CLD),

    BaseRegInitCode = cord.singleton(
        llds_instr(assign(BaseReg,
            mem_addr(heap_ref(SolnVectorAddrRval, yes(ptag(0u8)),
                const(llconst_int(0))))),
            "Compute base address for this case")
    ),
    SaveSlotCode = cord.singleton(
        llds_instr(assign(CurSlot, const(llconst_int(NumOutVars))),
            "Setup current slot in the solution array")
    ),

    remember_position(!.CLD, DisjEntry),

    make_resume_point(ResumeVars, resume_locs_stack_only, ResumeMap,
        ResumePoint, !CI),
    effect_resume_point(ResumePoint, model_non, UpdateRedoipCode, !CLD),
    generate_offset_assigns(OutVars, 0, BaseReg, !.CI, !CLD),
    flush_resume_vars_to_stack(FirstFlushResumeVarsCode, !CLD),

    % Forget the variables that are needed only at the resumption point at
    % the start of the next disjunct, so that we don't generate exceptions
    % when their storage is clobbered by the movement of the live variables
    % to the places indicated in the store map.
    pop_resume_point(!CLD),
    pickup_zombies(FirstZombies, !CLD),
    make_vars_forward_dead(FirstZombies, !CLD),

    set_liveness_and_end_branch(StoreMap, Liveness, MaybeEnd0, MaybeEnd1,
        FirstBranchEndCode, !.CLD),

    GotoEndCode = singleton(
        llds_instr(goto(code_label(EndLabel)), "goto end of lookup disj")
    ),

    reset_to_position(DisjEntry, !.CI, !:CLD),
    generate_resume_point(ResumePoint, ResumePointCode, !CI, !CLD),

    maybe_reset_ticket(MaybeTicketSlot, reset_reason_undo, RestoreTicketCode),
    maybe_restore_hp(MaybeHpSlot, RestoreHpCode),

    acquire_reg_not_in_storemap(StoreMap, reg_r, LaterBaseReg, !CLD),
    get_next_label(UndoLabel, !CI),
    get_next_label(AfterUndoLabel, !CI),
    MaxSlot = (NumSolns - 1) * NumOutVars,
    TestMoreSolnsCode = from_list([
        llds_instr(assign(LaterBaseReg, lval(CurSlot)),
            "Init later base register"),
        llds_instr(if_val(binop(int_ge(int_type_int), lval(LaterBaseReg),
            const(llconst_int(MaxSlot))),
            code_label(UndoLabel)),
            "Jump to undo hijack code if there are no more solutions"),
        llds_instr(assign(CurSlot,
            binop(int_add(int_type_int), lval(CurSlot),
            const(llconst_int(NumOutVars)))),
            "Update current slot"),
        llds_instr(goto(code_label(AfterUndoLabel)),
            "Jump around undo hijack code"),
        llds_instr(label(UndoLabel),
            "Undo hijack code")
    ]),
    undo_disj_hijack(HijackInfo, UndoHijackCode, !CLD),
    AfterUndoLabelCode = from_list([
        llds_instr(label(AfterUndoLabel),
            "Return later answer code"),
        llds_instr(assign(LaterBaseReg,
            mem_addr(heap_ref(SolnVectorAddrRval, yes(ptag(0u8)),
                lval(LaterBaseReg)))),
            "Compute base address in later array for this solution")
    ]),

    % We need to call effect_resume_point in order to push ResumePoint
    % onto the failure continuation stack, so pop_resume_point can pop
    % it off. However, since the redoip already points there, we don't need
    % to execute _LaterUpdateRedoipCode.
    effect_resume_point(ResumePoint, model_non, _LaterUpdateRedoipCode, !CLD),

    generate_offset_assigns(OutVars, 0, LaterBaseReg, !.CI, !CLD),
    flush_resume_vars_to_stack(LaterFlushResumeVarsCode, !CLD),

    % Forget the variables that are needed only at the resumption point at
    % the start of the next disjunct, so that we don't generate exceptions
    % when their storage is clobbered by the movement of the live variables
    % to the places indicated in the store map.
    pop_resume_point(!CLD),
    pickup_zombies(LaterZombies, !CLD),
    make_vars_forward_dead(LaterZombies, !CLD),

    set_liveness_and_end_branch(StoreMap, Liveness, MaybeEnd1, MaybeEnd,
        LaterBranchEndCode, !.CLD),

    after_all_branches(StoreMap, MaybeEnd, !.CI, !:CLD),

    EndLabelCode = singleton(
        llds_instr(label(EndLabel), "end of lookup disj")
    ),
    Comment = singleton(
        llds_instr(comment("lookup disj"), "")
    ),

    Code =
        Comment ++
        FlushCode ++
        BaseRegInitCode ++
        SaveSlotCode ++
        SaveTicketCode ++
        SaveHpCode ++
        PrepareHijackCode ++
        UpdateRedoipCode ++
        FirstFlushResumeVarsCode ++
        FirstBranchEndCode ++
        GotoEndCode ++
        ResumePointCode ++
        RestoreTicketCode ++
        RestoreHpCode ++
        TestMoreSolnsCode ++
        UndoHijackCode ++
        AfterUndoLabelCode ++
        LaterFlushResumeVarsCode ++
        LaterBranchEndCode ++
        EndLabelCode.

%---------------------------------------------------------------------------%

:- pred generate_real_disj(add_trail_ops::in, add_region_ops::in,
    code_model::in, set_of_progvar::in, list(hlds_goal)::in,
    hlds_goal_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_real_disj(AddTrailOps, AddRegionOps, CodeModel, ResumeVars, Goals,
        DisjGoalInfo, Code, !CI, !CLD)  :-
    % Make sure that the variables whose values will be needed on backtracking
    % to any disjunct are materialized into registers or stack slots. Their
    % locations are recorded in ResumeMap.
    produce_vars(set_of_var.to_sorted_list(ResumeVars), ResumeMap,
        FlushCode, !CLD),

    % If we are using a trail, save the current trail state before the
    % first disjunct.
    % XXX We should use a scheme such as the one we use for heap recovery
    % for semi and det disjunctions, and delay saving the ticket until
    % necessary.
    get_globals(!.CI, Globals),
    maybe_save_ticket(AddTrailOps, SaveTicketCode, MaybeTicketSlot, !CI, !CLD),

    % If we are using a grade in which we can recover memory by saving
    % and restoring the heap pointer, set up for doing so if necessary.
    (
        CodeModel = model_non,
        % With nondet disjunctions, we must recover memory across all
        % disjuncts, even disjuncts that cannot themselves allocate memory,
        % since we can backtrack to disjunct N after control leaves
        % disjunct N-1.
        globals.lookup_bool_option(Globals, reclaim_heap_on_nondet_failure,
            ReclaimHeap),
        maybe_save_hp(ReclaimHeap, SaveHpCode, MaybeHpSlot, !CI, !CLD),

        maybe_create_disj_region_frame_nondet(AddRegionOps, DisjGoalInfo,
            BeforeEnterRegionCode, LaterRegionCode, LastRegionCode, !CI, !CLD),
        % We can't release any of the stack slots holding the embedded stack
        % frame, since we can't let code to the right of the disjunction reuse
        % any of those slots.
        RegionStackVarsToRelease = [],
        RegionCommitDisjCleanup = no_commit_disj_region_cleanup
    ;
        ( CodeModel = model_det
        ; CodeModel = model_semi
        ),
        % With other disjunctions, we can backtrack to disjunct N only from
        % disjunct N-1, so if disjunct N-1 does not allocate memory, we need
        % not recover memory across it. Since it is possible (and common)
        % for no disjunct to allocate memory, we delay saving the heap pointer
        % and allocating a stack slot for the saved hp as long as possible.
        globals.lookup_bool_option(Globals, reclaim_heap_on_semidet_failure,
            ReclaimHeap),
        SaveHpCode = cord.empty,
        MaybeHpSlot = no,

        MaybeRbmmInfo = goal_info_get_maybe_rbmm(DisjGoalInfo),
        (
            MaybeRbmmInfo = no,
            BeforeEnterRegionCode = cord.empty,
            LaterRegionCode = cord.empty,
            LastRegionCode = cord.empty,
            RegionStackVarsToRelease = [],
            RegionCommitDisjCleanup = no_commit_disj_region_cleanup
        ;
            MaybeRbmmInfo = yes(RbmmInfo),
            RbmmInfo = rbmm_goal_info(DisjCreatedRegionVars,
                DisjRemovedRegionVars, _DisjCarriedRegionVars,
                DisjAllocRegionVars, _DisjUsedRegionVars),
            ( if
                set.is_empty(DisjCreatedRegionVars),
                set.is_empty(DisjRemovedRegionVars),
                set.is_empty(DisjAllocRegionVars)
            then
                BeforeEnterRegionCode = cord.empty,
                LaterRegionCode = cord.empty,
                LastRegionCode = cord.empty,
                RegionStackVarsToRelease = [],
                RegionCommitDisjCleanup = no_commit_disj_region_cleanup
            else
                % We only need region support for backtracking if some disjunct
                % performs some region operations (allocation or removal).
                maybe_create_disj_region_frame_semi(AddRegionOps,
                    set_of_var.set_to_bitset(DisjRemovedRegionVars),
                    set_of_var.set_to_bitset(DisjAllocRegionVars),
                    BeforeEnterRegionCode, LaterRegionCode, LastRegionCode,
                    RegionStackVars, RegionCommitDisjCleanup, !CI, !CLD),
                RegionStackVarsToRelease = RegionStackVars
            )
        )
    ),

    % Save the values of any stack slots we may hijack, and if necessary,
    % set the redofr slot of the top frame to point to this frame.
    prepare_for_disj_hijack(CodeModel, HijackInfo, PrepareHijackCode,
        !CI, !CLD),

    get_next_label(EndLabel, !CI),

    remember_position(!.CLD, BranchStart),
    generate_disjuncts(Goals, CodeModel, ResumeMap, no, HijackInfo,
        DisjGoalInfo, RegionCommitDisjCleanup, EndLabel, ReclaimHeap,
        MaybeHpSlot, MaybeTicketSlot, LaterRegionCode, LastRegionCode,
        BranchStart, no, MaybeEnd, GoalsCode, !CI),

    goal_info_get_store_map(DisjGoalInfo, StoreMap),
    after_all_branches(StoreMap, MaybeEnd, !.CI, !:CLD),
    release_several_temp_slots(RegionStackVarsToRelease,
        non_persistent_temp_slot, !CI, !CLD),
    (
        CodeModel = model_non,
        set_resume_point_to_unknown(!CLD)
    ;
        ( CodeModel = model_det
        ; CodeModel = model_semi
        )
        % The resume point is unchanged.
    ),

    Code = FlushCode ++ SaveTicketCode ++ SaveHpCode ++
        BeforeEnterRegionCode ++ PrepareHijackCode ++ GoalsCode.

:- pred generate_disjuncts(list(hlds_goal)::in, code_model::in,
    resume_map::in, maybe(resume_point_info)::in, disj_hijack_info::in,
    hlds_goal_info::in, commit_disj_region_cleanup::in, label::in, bool::in,
    maybe(lval)::in, maybe(lval)::in, llds_code::in, llds_code::in,
    position_info::in, maybe(branch_end_info)::in, maybe(branch_end_info)::out,
    llds_code::out, code_info::in, code_info::out) is det.

generate_disjuncts([], _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, !CI) :-
    unexpected($pred, "empty disjunction").
generate_disjuncts([Goal0 | Goals], CodeModel, FullResumeMap,
        MaybeEntryResumePoint, HijackInfo, DisjGoalInfo,
        RegionCommitDisjCleanup, EndLabel, ReclaimHeap,
        MaybeHpSlot0, MaybeTicketSlot, LaterRegionCode, LastRegionCode,
        BranchStart0, MaybeEnd0, MaybeEnd, Code, !CI) :-
    some [!CLD] (
        reset_to_position(BranchStart0, !.CI, !:CLD),

        % If this is not the first disjunct, generate the resume point by which
        % we arrive at this disjunct.
        (
            MaybeEntryResumePoint = yes(EntryResumePoint),
            generate_resume_point(EntryResumePoint, EntryResumePointCode,
                !CI, !CLD)
        ;
            MaybeEntryResumePoint = no,
            EntryResumePointCode = cord.empty
        ),

        Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
        goal_info_get_resume_point(GoalInfo0, Resume),
        (
            Resume = resume_point(ResumeVars, ResumeLocs),
            % Emit code for a non-last disjunct, including setting things
            % up for the execution of the next disjunct.

            (
                MaybeEntryResumePoint = yes(_),
                % Reset the heap pointer to recover memory allocated by the
                % previous disjunct(s), if necessary.
                maybe_restore_hp(MaybeHpSlot0, RestoreHpCode),

                % Reset the solver state if necessary.
                maybe_reset_ticket(MaybeTicketSlot, reset_reason_undo,
                    RestoreTicketCode),
                ThisDisjunctRegionCode = LaterRegionCode
            ;
                MaybeEntryResumePoint = no,
                RestoreHpCode = cord.empty,
                RestoreTicketCode = cord.empty,
                ThisDisjunctRegionCode = cord.empty
            ),

            % The pre_goal_update sanity check insists on no_resume_point,
            % to make sure that all resume points have been handled
            % by surrounding code.
            goal_info_set_resume_point(no_resume_point, GoalInfo0, GoalInfo),
            Goal = hlds_goal(GoalExpr0, GoalInfo),

            % Save hp if it needs to be saved and hasn't been saved previously.
            ( if
                ReclaimHeap = yes,
                goal_may_allocate_heap(Goal),
                MaybeHpSlot0 = no
            then
                save_hp(SaveHpCode, HpSlot, !CI, !CLD),
                MaybeHpSlot = yes(HpSlot),

                % This method of updating BranchStart0 is ugly. The best
                % alternative would be to arrange things so that a
                % remember_position here could get BranchStart, but doing so is
                % iffy because we have already created the resumption point for
                % entry into this disjunction, which overwrites part of the
                % location-dependent state originally in BranchStart0.

                save_hp_in_branch(BranchSaveHpCode, BranchHpSlot,
                    BranchStart0, BranchStart, !CI),
                HpCodeInstrs = cord.list(SaveHpCode),
                BranchHpCodeInstrs = cord.list(BranchSaveHpCode),
                expect(unify(HpCodeInstrs, BranchHpCodeInstrs), $pred,
                    "cannot use same code for saving hp"),
                expect(unify(HpSlot, BranchHpSlot), $pred,
                    "cannot allocate same slot for saved hp")
            else
                SaveHpCode = cord.empty,
                MaybeHpSlot = MaybeHpSlot0,
                BranchStart = BranchStart0
            ),

            make_resume_point(ResumeVars, ResumeLocs, FullResumeMap,
                NextResumePoint, !CI),
            effect_resume_point(NextResumePoint, CodeModel, ModContCode, !CLD),

            maybe_generate_internal_event_code(Goal, DisjGoalInfo, TraceCode,
                !CI, !CLD),
            GoalCodeModel = goal_info_get_code_model(GoalInfo),
            (
                CodeModel = model_non,

                % We can backtrack to the next disjunct from outside,
                % so we make sure every variable in the resume set
                % is in its stack slot.
                flush_resume_vars_to_stack(FlushResumeVarsCode, !CLD),
                code_gen.generate_goal(GoalCodeModel, Goal, GoalCode,
                    !CI, !CLD),

                % We hang onto any temporary slots holding saved heap pointers
                % and/or tickets, thus ensuring that they will still be
                % reserved after the disjunction.
                PruneTicketCode = cord.empty
            ;
                ( CodeModel = model_det
                ; CodeModel = model_semi
                ),

                FlushResumeVarsCode = cord.empty,
                code_gen.generate_goal(GoalCodeModel, Goal, GoalCode,
                    !CI, !CLD),

                maybe_release_hp(MaybeHpSlot, !CI, !CLD),
                % We are committing to this disjunct if it succeeds.
                maybe_reset_prune_and_release_ticket(MaybeTicketSlot,
                    reset_reason_commit, PruneTicketCode, !CI, !CLD),

                reset_resume_known(BranchStart, !CLD)
            ),

            % Forget the variables that are needed only at the resumption point
            % at the start of the next disjunct, so that we don't generate
            % exceptions when their storage is clobbered by the movement
            % of the live variables to the places indicated in the store map.
            pop_resume_point(!CLD),
            pickup_zombies(Zombies, !CLD),
            make_vars_forward_dead(Zombies, !CLD),

            % Put every variable whose value is needed after the disjunction to
            % the place indicated by StoreMap, and accumulate information about
            % the code_info state at the ends of the branches so far.
            goal_info_get_store_map(DisjGoalInfo, StoreMap),
            generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd1, SaveCode,
                !.CLD),

            (
                RegionCommitDisjCleanup = no_commit_disj_region_cleanup,
                BranchCode = singleton(
                    llds_instr(goto(code_label(EndLabel)),
                        "skip to end of disjunction")
                )
            ;
                RegionCommitDisjCleanup =
                    commit_disj_region_cleanup(CleanupLabel, _CleanupCode),
                BranchCode = singleton(
                    llds_instr(goto(code_label(CleanupLabel)),
                        "skip to end of disjunction with region cleanup")
                )
            ),

            generate_disjuncts(Goals, CodeModel, FullResumeMap,
                yes(NextResumePoint), HijackInfo, DisjGoalInfo,
                RegionCommitDisjCleanup, EndLabel,
                ReclaimHeap, MaybeHpSlot, MaybeTicketSlot,
                LaterRegionCode, LastRegionCode, BranchStart,
                MaybeEnd1, MaybeEnd, RestCode, !CI),

            Code =
                EntryResumePointCode ++
                RestoreHpCode ++
                RestoreTicketCode ++
                SaveHpCode ++
                ThisDisjunctRegionCode ++
                ModContCode ++
                TraceCode ++
                FlushResumeVarsCode ++
                GoalCode ++
                PruneTicketCode ++
                SaveCode ++
                BranchCode ++
                RestCode
        ;
            Resume = no_resume_point,
            % Emit code for the last disjunct.

            % Restore the heap pointer and solver state if necessary.
            maybe_restore_and_release_hp(MaybeHpSlot0, RestoreHpCode,
                !CI, !CLD),
            maybe_reset_discard_and_release_ticket(MaybeTicketSlot,
                reset_reason_undo, RestoreTicketCode, !CI, !CLD),

            undo_disj_hijack(HijackInfo, UndoCode, !CLD),

            maybe_generate_internal_event_code(Goal0, DisjGoalInfo, TraceCode,
                !CI, !CLD),
            code_gen.generate_goal(CodeModel, Goal0, GoalCode, !CI, !CLD),
            goal_info_get_store_map(DisjGoalInfo, StoreMap),
            generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd, SaveCode,
                !.CLD),

            (
                RegionCommitDisjCleanup = no_commit_disj_region_cleanup,
                RegionCleanupCode = cord.empty
            ;
                RegionCommitDisjCleanup =
                    commit_disj_region_cleanup(CleanupLabel, CleanupCode),
                RegionCleanupStartCode = from_list([
                    llds_instr(goto(code_label(EndLabel)),
                        "Skip over cleanup code at end of disjunction"),
                    llds_instr(label(CleanupLabel),
                        "Cleanup at end of disjunction")
                ]),
                RegionCleanupCode = RegionCleanupStartCode ++ CleanupCode
            ),

            EndLabelCode = singleton(
                llds_instr(label(EndLabel),
                    "End of disjunction")
            ),

            Code =
                EntryResumePointCode ++
                TraceCode ++      % XXX Should this be after LastRegionCode?
                RestoreHpCode ++
                RestoreTicketCode ++
                LastRegionCode ++
                UndoCode ++
                GoalCode ++
                SaveCode ++
                RegionCleanupCode ++
                EndLabelCode
        )
    ).

%-----------------------------------------------------------------------------%

:- type commit_disj_region_cleanup
    --->    no_commit_disj_region_cleanup
    ;       commit_disj_region_cleanup(
                cleanup_label       :: label,
                cleanup_code        :: llds_code
            ).

:- pred maybe_create_disj_region_frame_nondet(add_region_ops::in,
    hlds_goal_info::in, llds_code::out, llds_code::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

maybe_create_disj_region_frame_nondet(DisjRegionOps, _DisjGoalInfo,
        BeforeEnterCode, LaterCode, LastCode, !CI, !CLD) :-
    (
        DisjRegionOps = do_not_add_region_ops,
        BeforeEnterCode = cord.empty,
        LaterCode = cord.empty,
        LastCode = cord.empty
    ;
        DisjRegionOps = add_region_ops,
        get_forward_live_vars(!.CLD, ForwardLiveVars),
        LiveRegionVars = filter_region_vars(!.CI, ForwardLiveVars),

        % Protection of backward live regions for nondet disjunction is by
        % saving the sequence number to the disj frame, therefore we do not
        % need to save any protected regions.

        % XXX In computing SnapshotRegionVars,
        % we should intersect LiveRegionVars with the set of region variables
        % whose regions (the regions themselves, not their variables) are live
        % at the starts of some later disjuncts (i.e. aren't used only in the
        % first disjunct). We don't yet gather this information.
        SnapshotRegionVars = LiveRegionVars,
        SnapshotRegionVarList = set_of_var.to_sorted_list(SnapshotRegionVars),
        list.length(SnapshotRegionVarList, NumSnapshotRegionVars),

        get_globals(!.CI, Globals),
        globals.lookup_int_option(Globals, size_region_disj_fixed,
            FixedSize),
        globals.lookup_int_option(Globals, size_region_disj_snapshot,
            SnapshotSize),
        FrameSize = FixedSize + SnapshotSize * NumSnapshotRegionVars,

        Items = list.duplicate(FrameSize, slot_region_disj),
        acquire_several_temp_slots(Items, non_persistent_temp_slot,
            _StackVars, MainStackId, FirstSlotNum, LastSlotNum, !CI, !CLD),
        EmbeddedStackFrame = embedded_stack_frame_id(MainStackId,
            FirstSlotNum, LastSlotNum),
        FirstNonFixedAddr =
            first_nonfixed_embedded_slot_addr(EmbeddedStackFrame, FixedSize),
        acquire_reg(reg_r, SnapshotNumRegLval, !CLD),
        acquire_reg(reg_r, AddrRegLval, !CLD),
        PushInitCode = from_list([
            llds_instr(
                push_region_frame(region_stack_disj, EmbeddedStackFrame),
                "Save stack pointer of embedded region nondet stack"),
            llds_instr(
                assign(SnapshotNumRegLval, const(llconst_int(0))),
                "Initialize number of snapshot_infos"),
            llds_instr(
                assign(AddrRegLval, FirstNonFixedAddr),
                "Initialize pointer to nonfixed part of embedded frame")
        ]),
        disj_alloc_snapshot_regions(SnapshotNumRegLval, AddrRegLval,
            EmbeddedStackFrame, SnapshotRegionVarList, SnapshotRegionCode,
            !CLD),
        SetCode = singleton(
            llds_instr(
                region_set_fixed_slot(region_set_disj_num_snapshots,
                    EmbeddedStackFrame, lval(SnapshotNumRegLval)),
                "Store the number of snapshot_infos")
        ),
        release_reg(SnapshotNumRegLval, !CLD),
        release_reg(AddrRegLval, !CLD),

        BeforeEnterCode = PushInitCode ++ SnapshotRegionCode ++ SetCode,
        LaterCode = singleton(
            llds_instr(
                use_and_maybe_pop_region_frame(region_disj_later,
                    EmbeddedStackFrame),
                "region enter later disjunct")
        ),
        LastCode = singleton(
            llds_instr(
                use_and_maybe_pop_region_frame(region_disj_last,
                    EmbeddedStackFrame),
                "region enter last disjunct")
        )
    ).

:- pred maybe_create_disj_region_frame_semi(add_region_ops::in,
    set_of_progvar::in, set_of_progvar::in, llds_code::out, llds_code::out,
    llds_code::out, list(lval)::out, commit_disj_region_cleanup::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

maybe_create_disj_region_frame_semi(DisjRegionOps, DisjRemovedRegionVars,
        DisjAllocRegionVars, BeforeEnterCode, LaterCode, LastCode, StackVars,
        RegionCommitDisjCleanup, !CI, !CLD) :-
    (
        DisjRegionOps = do_not_add_region_ops,
        BeforeEnterCode = cord.empty,
        LaterCode = cord.empty,
        LastCode = cord.empty,
        StackVars = [],
        RegionCommitDisjCleanup = no_commit_disj_region_cleanup
    ;
        DisjRegionOps = add_region_ops,

        % For a semidet disjunction, we need to save the protected regions,
        % i.e., those removed in the scope of the semidet disjunction,
        % into the disj frame so that if a non-last disjunct succeeds,
        % we can reclaim such regions. We will only save ones which
        % are currently not protected (this is checked at runtime).
        ProtectRegionVars = DisjRemovedRegionVars,

        % XXX In computing SnapshotRegionVars,
        % we should intersect DisjAllocRegionVars with the set of region
        % variables whose regions (the regions themselves, not their variables)
        % are live at the starts of some later disjuncts (i.e. aren't used only
        % in the first disjunct). We don't yet gather this information.
        %
        SnapshotRegionVars = DisjAllocRegionVars,

        ProtectRegionVarList = set_of_var.to_sorted_list(ProtectRegionVars),
        SnapshotRegionVarList = set_of_var.to_sorted_list(SnapshotRegionVars),

        list.length(ProtectRegionVarList, NumProtectRegionVars),
        list.length(SnapshotRegionVarList, NumSnapshotRegionVars),

        get_globals(!.CI, Globals),
        globals.lookup_int_option(Globals, size_region_disj_fixed,
            FixedSize),
        globals.lookup_int_option(Globals, size_region_semi_disj_protect,
            ProtectSize),
        globals.lookup_int_option(Globals, size_region_disj_snapshot,
            SnapshotSize),
        FrameSize = FixedSize
            + ProtectSize * NumProtectRegionVars
            + SnapshotSize * NumSnapshotRegionVars,

        Items = list.duplicate(FrameSize, slot_region_disj),
        acquire_several_temp_slots(Items, non_persistent_temp_slot,
            StackVars, MainStackId, FirstSlotNum, LastSlotNum, !CI, !CLD),
        EmbeddedStackFrame = embedded_stack_frame_id(MainStackId,
            FirstSlotNum, LastSlotNum),
        FirstNonFixedAddr =
            first_nonfixed_embedded_slot_addr(EmbeddedStackFrame, FixedSize),
        acquire_reg(reg_r, ProtectNumRegLval, !CLD),
        acquire_reg(reg_r, SnapshotNumRegLval, !CLD),
        acquire_reg(reg_r, AddrRegLval, !CLD),
        PushInitCode = from_list([
            llds_instr(
                push_region_frame(region_stack_disj, EmbeddedStackFrame),
                "Save stack pointer of embedded region nondet stack"),
            llds_instr(
                assign(ProtectNumRegLval, const(llconst_int(0))),
                "Initialize number of protect_infos"),
            llds_instr(
                assign(SnapshotNumRegLval, const(llconst_int(0))),
                "Initialize number of snapshot_infos"),
            llds_instr(
                assign(AddrRegLval, FirstNonFixedAddr),
                "Initialize pointer to nonfixed part of embedded frame")
        ]),
        disj_protect_regions(ProtectNumRegLval, AddrRegLval,
            EmbeddedStackFrame, ProtectRegionVarList, ProtectRegionCode,
            !CLD),
        disj_alloc_snapshot_regions(SnapshotNumRegLval, AddrRegLval,
            EmbeddedStackFrame, SnapshotRegionVarList, SnapshotRegionCode,
            !CLD),
        SetCode = from_list([
            llds_instr(
                region_set_fixed_slot(region_set_disj_num_protects,
                    EmbeddedStackFrame, lval(ProtectNumRegLval)),
                "Store the number of protect_infos"),
            llds_instr(
                region_set_fixed_slot(region_set_disj_num_snapshots,
                    EmbeddedStackFrame, lval(SnapshotNumRegLval)),
                "Store the number of snapshot_infos")
        ]),
        release_reg(ProtectNumRegLval, !CLD),
        release_reg(SnapshotNumRegLval, !CLD),
        release_reg(AddrRegLval, !CLD),

        BeforeEnterCode = PushInitCode ++ ProtectRegionCode ++
            SnapshotRegionCode ++ SetCode,
        LaterCode = singleton(
            llds_instr(
                use_and_maybe_pop_region_frame(region_disj_later,
                    EmbeddedStackFrame),
                "region enter later disjunct")
        ),
        LastCode = singleton(
            llds_instr(
                use_and_maybe_pop_region_frame(region_disj_last,
                    EmbeddedStackFrame),
                "region enter last disjunct")
        ),

        get_next_label(CleanupLabel, !CI),
        CleanupCode = singleton(
            llds_instr(
                use_and_maybe_pop_region_frame(
                    region_disj_nonlast_semi_commit, EmbeddedStackFrame),
                "region cleanup commit for nonlast disjunct")
        ),
        RegionCommitDisjCleanup = commit_disj_region_cleanup(CleanupLabel,
            CleanupCode)
    ).

:- pred disj_protect_regions(lval::in, lval::in, embedded_stack_frame_id::in,
    list(prog_var)::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

disj_protect_regions(_, _, _, [], cord.empty, !CLD).
disj_protect_regions(NumLval, AddrLval, EmbeddedStackFrame,
        [RegionVar | RegionVars], Code ++ Codes, !CLD) :-
    produce_variable(RegionVar, ProduceVarCode, RegionVarRval, !CLD),
    SaveCode = singleton(
        llds_instr(
            region_fill_frame(region_fill_semi_disj_protect,
                EmbeddedStackFrame, RegionVarRval, NumLval, AddrLval),
            "disj protect the region if needed")
    ),
    Code = ProduceVarCode ++ SaveCode,
    disj_protect_regions(NumLval, AddrLval, EmbeddedStackFrame,
        RegionVars, Codes, !CLD).

:- pred disj_alloc_snapshot_regions(lval::in, lval::in,
    embedded_stack_frame_id::in, list(prog_var)::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

disj_alloc_snapshot_regions(_, _, _, [], cord.empty, !CLD).
disj_alloc_snapshot_regions(NumLval, AddrLval, EmbeddedStackFrame,
        [RegionVar | RegionVars], Code ++ Codes, !CLD) :-
    produce_variable(RegionVar, ProduceVarCode, RegionVarRval, !CLD),
    SaveCode = singleton(
        llds_instr(
            region_fill_frame(region_fill_disj_snapshot,
                EmbeddedStackFrame, RegionVarRval, NumLval, AddrLval),
            "take alloc snapshot of the region")
    ),
    Code = ProduceVarCode ++ SaveCode,
    disj_alloc_snapshot_regions(NumLval, AddrLval, EmbeddedStackFrame,
        RegionVars, Codes, !CLD).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.disj_gen.
%-----------------------------------------------------------------------------%

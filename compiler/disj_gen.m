%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2000,2002-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: disj_gen.m.
% Main authors: conway, zs.
%
% The predicates of this module generate code for disjunctions.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.disj_gen.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.llds.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred generate_disj(code_model::in, list(hlds_goal)::in, hlds_goal_info::in,
    code_tree::out, code_info::in, code_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_llds.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.tree.
:- import_module libs.tree.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.global_data.
:- import_module ll_backend.lookup_util.
:- import_module ll_backend.trace_gen.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module term.

%-----------------------------------------------------------------------------%

generate_disj(CodeModel, Goals, DisjGoalInfo, Code, !CI) :-
    (
        Goals = [],
        (
            CodeModel = model_semi,
            generate_failure(Code, !CI)
        ;
            ( CodeModel = model_det
            ; CodeModel = model_non
            ),
            unexpected(this_file, "generate_disj: empty disjunction.")
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
            set.init(ResumeVars)
        ),
        AddTrailOps = should_add_trail_ops(!.CI, GoalInfo),
        AddRegionOps = should_add_region_ops(!.CI, GoalInfo),
        (
            CodeModel = model_non,
            is_lookup_disj(AddTrailOps, AddRegionOps, ResumeVars, Goals,
                DisjGoalInfo, LookupDisjInfo, !CI)
        ->
            generate_lookup_disj(ResumeVars, LookupDisjInfo, Code, !CI)
        ;
            generate_real_disj(AddTrailOps, AddRegionOps, CodeModel,
                ResumeVars, Goals, DisjGoalInfo, Code, !CI)
        )
    ).

:- type lookup_disj_info
    --->    lookup_disj_info(
                ldi_variables           :: list(prog_var),
                                        % The output variables.

                ldi_store_map           :: abs_store_map,
                ldi_branch_end          :: branch_end,
                ldi_liveness            :: set(prog_var),

                lds_cur_slot            :: lval,

                lds_resume_map          :: resume_map,
                lds_flush_code          :: code_tree,

                lds_save_ticket_code    :: code_tree,
                lds_maybe_ticket_slot   :: maybe(lval),

                lds_save_hp_code        :: code_tree,
                lds_maybe_hp_slot       :: maybe(lval),

                lds_hijack_info         :: disj_hijack_info,
                lds_prepare_hijack_code :: code_tree,

                ldi_solns               :: list(list(rval)),
                ldi_field_types         :: list(llds_type)
            ).

:- pred is_lookup_disj(add_trail_ops::in, add_region_ops::in,
    set(prog_var)::in, list(hlds_goal)::in, hlds_goal_info::in,
    lookup_disj_info::out, code_info::in, code_info::out) is semidet.

is_lookup_disj(AddTrailOps, AddRegionOps, ResumeVars, Goals, DisjGoalInfo,
        LookupDisjInfo, !CI) :-
    get_maybe_trace_info(!.CI, MaybeTraceInfo),
    MaybeTraceInfo = no,

    % Lookup disjunctions rely on static ground terms to work.
    get_globals(!.CI, Globals),
    globals.lookup_bool_option(Globals, static_ground_terms, yes),

    % XXX The code to generate lookup disjunctions hasn't yet been updated
    % to handle region operations.
    AddRegionOps = do_not_add_region_ops,

    % Since we generate two code sequences, one for the first solution and one
    % for all the later solutions, we don't get any benefit over the code
    % sequence generated by generate_real_disj unless there are at least three
    % solutions.
    Goals = [_, _, _ | _],
    all_disjuncts_are_conj_of_unify(Goals),

    figure_out_output_vars(!.CI, DisjGoalInfo, OutVars),
    VarTypes = get_var_types(!.CI),
    list.map(map.lookup(VarTypes), OutVars, OutTypes),

    produce_vars(ResumeVars, ResumeMap, FlushCode, !CI),

    % We cannot release this stack slot anywhere within the disjunction,
    % since it will be needed after backtracking to later disjuncts.
    % However, if we are inside an outer branched control structure
    % (disjunction, switch, if-then-else), it may be released (implicitly)
    % when we get into the next branch of that outer control structure.
    acquire_temp_slot(slot_lookup_disj_cur, non_persistent_temp_slot, CurSlot,
        !CI),
    maybe_save_ticket(AddTrailOps, SaveTicketCode, MaybeTicketSlot, !CI),
    get_globals(!.CI, Globals),
    globals.lookup_bool_option(Globals, reclaim_heap_on_nondet_failure,
        ReclaimHeap),
    maybe_save_hp(ReclaimHeap, SaveHpCode, MaybeHpSlot, !CI),
    prepare_for_disj_hijack(model_non, HijackInfo, PrepareHijackCode, !CI),

    % Every update to the code_info that needs to persist beyond the
    % disjunction as a whole (e.g. the reservations of the stack slots required
    % to implement the hijack) must be done before we remember this position.
    remember_position(!.CI, CurPos),

    goal_info_get_store_map(DisjGoalInfo, StoreMap),
    generate_constants_for_disjuncts(Goals, OutVars, StoreMap, Solns,
        no, MaybeEnd, MaybeLiveness, !CI),
    (
        MaybeLiveness = yes(Liveness)
    ;
        MaybeLiveness = no,
        unexpected(this_file, "is_lookup_disj: no liveness")
    ),
    reset_to_position(CurPos, !CI),

    get_globals(!.CI, Globals),
    globals.lookup_bool_option(Globals, unboxed_float, UnboxFloat),
    find_general_llds_types(UnboxFloat, OutTypes, Solns, LLDSTypes),
    LookupDisjInfo = lookup_disj_info(OutVars, StoreMap, MaybeEnd, Liveness,
        CurSlot, ResumeMap, FlushCode, SaveTicketCode, MaybeTicketSlot,
        SaveHpCode, MaybeHpSlot, HijackInfo, PrepareHijackCode,
        Solns, LLDSTypes).

:- pred generate_lookup_disj(set(prog_var)::in, lookup_disj_info::in,
    code_tree::out, code_info::in, code_info::out) is det.

generate_lookup_disj(ResumeVars, LookupDisjInfo, Code, !CI) :-
    LookupDisjInfo = lookup_disj_info(OutVars, StoreMap, MaybeEnd, Liveness,
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
    acquire_reg_not_in_storemap(StoreMap, BaseReg, !CI),

    BaseRegInitCode = node([
        llds_instr(assign(BaseReg,
            mem_addr(heap_ref(SolnVectorAddrRval, 0, const(llconst_int(0))))),
            "Compute base address for this case")
    ]),
    SaveSlotCode = node([
        llds_instr(assign(CurSlot, const(llconst_int(NumOutVars))),
            "Setup current slot in the solution array")
    ]),

    remember_position(!.CI, DisjEntry),

    make_resume_point(ResumeVars, resume_locs_stack_only,
        ResumeMap, ResumePoint, !CI),
    effect_resume_point(ResumePoint, model_non, UpdateRedoipCode, !CI),
    generate_offset_assigns(OutVars, 0, BaseReg, !CI),
    flush_resume_vars_to_stack(FirstFlushResumeVarsCode, !CI),

    % Forget the variables that are needed only at the resumption point at
    % the start of the next disjunct, so that we don't generate exceptions
    % when their storage is clobbered by the movement of the live variables
    % to the places indicated in the store map.
    pop_resume_point(!CI),
    pickup_zombies(FirstZombies, !CI),
    make_vars_forward_dead(FirstZombies, !CI),

    set_liveness_and_end_branch(StoreMap, MaybeEnd, Liveness,
        FirstBranchEndCode, !CI),
    release_reg(BaseReg, !CI),

    GotoEndCode = node([
        llds_instr(goto(code_label(EndLabel)), "goto end of lookup disj")
    ]),

    reset_to_position(DisjEntry, !CI),
    generate_resume_point(ResumePoint, ResumePointCode, !CI),

    maybe_reset_ticket(MaybeTicketSlot, reset_reason_undo, RestoreTicketCode),
    maybe_restore_hp(MaybeHpSlot, RestoreHpCode),

    acquire_reg_not_in_storemap(StoreMap, LaterBaseReg, !CI),
    get_next_label(UndoLabel, !CI),
    get_next_label(AfterUndoLabel, !CI),
    MaxSlot = (NumSolns - 1) * NumOutVars,
    TestMoreSolnsCode = node([
        llds_instr(assign(LaterBaseReg, lval(CurSlot)),
            "Init later base register"),
        llds_instr(if_val(binop(int_ge, lval(LaterBaseReg),
            const(llconst_int(MaxSlot))),
            code_label(UndoLabel)),
            "Jump to undo hijack code if there are no more solutions"),
        llds_instr(assign(CurSlot,
            binop(int_add, lval(CurSlot), const(llconst_int(NumOutVars)))),
            "Update current slot"),
        llds_instr(goto(code_label(AfterUndoLabel)),
            "Jump around undo hijack code"),
        llds_instr(label(UndoLabel),
            "Undo hijack code")
    ]),
    undo_disj_hijack(HijackInfo, UndoHijackCode, !CI),
    AfterUndoLabelCode = node([
        llds_instr(label(AfterUndoLabel),
            "Return later answer code"),
        llds_instr(assign(LaterBaseReg,
            mem_addr(heap_ref(SolnVectorAddrRval, 0, lval(LaterBaseReg)))),
            "Compute base address in later array for this solution")
    ]),

    % We need to call effect_resume_point in order to push ResumePoint
    % onto the failure continuation stack, so pop_resume_point can pop
    % it off. However, since the redoip already points there, we don't need
    % to execute _LaterUpdateRedoipCode.
    effect_resume_point(ResumePoint, model_non, _LaterUpdateRedoipCode, !CI),

    generate_offset_assigns(OutVars, 0, LaterBaseReg, !CI),
    flush_resume_vars_to_stack(LaterFlushResumeVarsCode, !CI),

    % Forget the variables that are needed only at the resumption point at
    % the start of the next disjunct, so that we don't generate exceptions
    % when their storage is clobbered by the movement of the live variables
    % to the places indicated in the store map.
    pop_resume_point(!CI),
    pickup_zombies(LaterZombies, !CI),
    make_vars_forward_dead(LaterZombies, !CI),

    set_liveness_and_end_branch(StoreMap, MaybeEnd, Liveness,
        LaterBranchEndCode, !CI),

    after_all_branches(StoreMap, MaybeEnd, !CI),

    EndLabelCode = node([llds_instr(label(EndLabel), "end of lookup disj")]),
    Comment = node([llds_instr(comment("lookup disj"), "")]),

    Code = tree_list([
        Comment,
        FlushCode,
        BaseRegInitCode,
        SaveSlotCode,
        SaveTicketCode,
        SaveHpCode,
        PrepareHijackCode,
        UpdateRedoipCode,
        FirstFlushResumeVarsCode,
        FirstBranchEndCode,
        GotoEndCode,
        ResumePointCode,
        RestoreTicketCode,
        RestoreHpCode,
        TestMoreSolnsCode,
        UndoHijackCode,
        AfterUndoLabelCode,
        LaterFlushResumeVarsCode,
        LaterBranchEndCode,
        EndLabelCode
    ]).

%---------------------------------------------------------------------------%

:- pred generate_real_disj(add_trail_ops::in, add_region_ops::in,
    code_model::in, set(prog_var)::in, list(hlds_goal)::in, hlds_goal_info::in,
    code_tree::out, code_info::in, code_info::out) is det.

generate_real_disj(AddTrailOps, AddRegionOps, CodeModel, ResumeVars, Goals,
        DisjGoalInfo, Code, !CI)  :-
    % Make sure that the variables whose values will be needed on backtracking
    % to any disjunct are materialized into registers or stack slots. Their
    % locations are recorded in ResumeMap.
    produce_vars(ResumeVars, ResumeMap, FlushCode, !CI),

    % If we are using a trail, save the current trail state before the
    % first disjunct.
    % XXX We should use a scheme such as the one we use for heap recovery
    % for semi and det disjunctions, and delay saving the ticket until
    % necessary.
    get_globals(!.CI, Globals),
    maybe_save_ticket(AddTrailOps, SaveTicketCode, MaybeTicketSlot,
        !CI),

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
        maybe_save_hp(ReclaimHeap, SaveHpCode, MaybeHpSlot, !CI),

        maybe_create_disj_region_frame(AddRegionOps, DisjGoalInfo,
            FirstRegionCode, LaterRegionCode, LastRegionCode,
            _RegionStackVars, !CI),
        % We can't release any of the stack slots holding the embedded stack
        % frame, since we can't let code to the right of the disjunction reuse
        % any of those slots.
        RegionStackVarsToRelease = []
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
        SaveHpCode = empty,
        MaybeHpSlot = no,

        % XXX The condition should succeed only if some disjunct performs
        % some region operations (allocation or removal). The HLDS does not yet
        % contain the information we need to decide whether this is the case.
        ( semidet_succeed ->
            maybe_create_disj_region_frame(AddRegionOps, DisjGoalInfo,
                FirstRegionCode, LaterRegionCode, LastRegionCode,
                RegionStackVars, !CI),
            RegionStackVarsToRelease = RegionStackVars
        ;
            FirstRegionCode = empty,
            LaterRegionCode = empty,
            LastRegionCode = empty,
            RegionStackVarsToRelease = []
        )
    ),

    % Save the values of any stack slots we may hijack, and if necessary,
    % set the redofr slot of the top frame to point to this frame.
    prepare_for_disj_hijack(CodeModel, HijackInfo, PrepareHijackCode, !CI),

    get_next_label(EndLabel, !CI),

    remember_position(!.CI, BranchStart),
    generate_disjuncts(Goals, CodeModel, ResumeMap, no, HijackInfo,
        DisjGoalInfo, EndLabel, ReclaimHeap, MaybeHpSlot, MaybeTicketSlot,
        LaterRegionCode, LastRegionCode, BranchStart, no, MaybeEnd, GoalsCode,
        !CI),

    goal_info_get_store_map(DisjGoalInfo, StoreMap),
    after_all_branches(StoreMap, MaybeEnd, !CI),
    release_several_temp_slots(RegionStackVarsToRelease,
        non_persistent_temp_slot, !CI),
    (
        CodeModel = model_non,
        set_resume_point_to_unknown(!CI)
    ;
        ( CodeModel = model_det
        ; CodeModel = model_semi
        )
        % The resume point is unchanged.
    ),

    Code = tree_list([
        FlushCode,
        SaveTicketCode,
        SaveHpCode,
        FirstRegionCode,
        PrepareHijackCode,
        GoalsCode
    ]).

:- pred generate_disjuncts(list(hlds_goal)::in, code_model::in,
    resume_map::in, maybe(resume_point_info)::in, disj_hijack_info::in,
    hlds_goal_info::in, label::in, bool::in, maybe(lval)::in,
    maybe(lval)::in, code_tree::in, code_tree::in,
    position_info::in, maybe(branch_end_info)::in, maybe(branch_end_info)::out,
    code_tree::out, code_info::in, code_info::out) is det.

generate_disjuncts([], _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, !CI) :-
    unexpected(this_file, "generate_disjuncts: empty disjunction!").
generate_disjuncts([Goal0 | Goals], CodeModel, FullResumeMap,
        MaybeEntryResumePoint, HijackInfo, DisjGoalInfo, EndLabel, ReclaimHeap,
        MaybeHpSlot0, MaybeTicketSlot, LaterRegionCode, LastRegionCode,
        BranchStart0, MaybeEnd0, MaybeEnd, Code, !CI) :-

    reset_to_position(BranchStart0, !CI),

    % If this is not the first disjunct, generate the resume point by which
    % we arrive at this disjunct.
    (
        MaybeEntryResumePoint = yes(EntryResumePoint),
        generate_resume_point(EntryResumePoint, EntryResumePointCode, !CI)
    ;
        MaybeEntryResumePoint = no,
        EntryResumePointCode = empty
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
                RestoreTicketCode)
        ;
            MaybeEntryResumePoint = no,
            RestoreHpCode = empty,
            RestoreTicketCode = empty
        ),

        % The pre_goal_update sanity check insists on no_resume_point, to make
        % sure that all resume points have been handled by surrounding code.
        goal_info_set_resume_point(no_resume_point, GoalInfo0, GoalInfo),
        Goal = hlds_goal(GoalExpr0, GoalInfo),

        % Save hp if it needs to be saved and hasn't been saved previously.
        (
            ReclaimHeap = yes,
            goal_may_allocate_heap(Goal),
            MaybeHpSlot0 = no
        ->
            save_hp(SaveHpCode, HpSlot, !CI),
            MaybeHpSlot = yes(HpSlot),

            % This method of updating BranchStart0 is ugly. The best
            % alternative would be to arrange things so that a
            % remember_position here could get BranchStart, but doing so is
            % iffy because we have already created the resumption point for
            % entry into this disjunction, which overwrites part of the
            % location-dependent state originally in BranchStart0.

            save_hp_in_branch(BranchSaveHpCode, BranchHpSlot,
                BranchStart0, BranchStart),
            tree.flatten(SaveHpCode, HpCodeList),
            tree.flatten(BranchSaveHpCode, BranchHpCodeList),
            expect(unify(HpCodeList, BranchHpCodeList), this_file,
                "cannot use same code for saving hp"),
            expect(unify(HpSlot, BranchHpSlot), this_file,
                "cannot allocate same slot for saved hp")
        ;
            SaveHpCode = empty,
            MaybeHpSlot = MaybeHpSlot0,
            BranchStart = BranchStart0
        ),

        make_resume_point(ResumeVars, ResumeLocs, FullResumeMap,
            NextResumePoint, !CI),
        effect_resume_point(NextResumePoint, CodeModel, ModContCode, !CI),

        maybe_generate_internal_event_code(Goal, DisjGoalInfo, TraceCode, !CI),
        GoalCodeModel = goal_info_get_code_model(GoalInfo),
        code_gen.generate_goal(GoalCodeModel, Goal, GoalCode, !CI),

        (
            CodeModel = model_non,

            % We can backtrack to the next disjunct from outside, so we make
            % sure every variable in the resume set is in its stack slot.
            flush_resume_vars_to_stack(ResumeVarsCode, !CI),

            % We hang onto any temporary slots holding saved heap pointers
            % and/or tickets, thus ensuring that they will still be reserved
            % after the disjunction.
            PruneTicketCode = empty
        ;
            ( CodeModel = model_det
            ; CodeModel = model_semi
            ),

            ResumeVarsCode = empty,

            maybe_release_hp(MaybeHpSlot, !CI),
            % We're committing to this disjunct if it succeeds.
            maybe_reset_prune_and_release_ticket(MaybeTicketSlot,
                reset_reason_commit, PruneTicketCode, !CI),

            reset_resume_known(BranchStart, !CI)
        ),

        % Forget the variables that are needed only at the resumption point at
        % the start of the next disjunct, so that we don't generate exceptions
        % when their storage is clobbered by the movement of the live
        % variables to the places indicated in the store map.
        pop_resume_point(!CI),
        pickup_zombies(Zombies, !CI),
        make_vars_forward_dead(Zombies, !CI),

        % Put every variable whose value is needed after the disjunction to
        % the place indicated by StoreMap, and accumulate information about
        % the code_info state at the ends of the branches so far.
        goal_info_get_store_map(DisjGoalInfo, StoreMap),
        generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd1, SaveCode, !CI),

        BranchCode = node([
            llds_instr(goto(code_label(EndLabel)),
                "skip to end of nondet disj")
        ]),

        generate_disjuncts(Goals, CodeModel, FullResumeMap,
            yes(NextResumePoint), HijackInfo, DisjGoalInfo,
            EndLabel, ReclaimHeap, MaybeHpSlot, MaybeTicketSlot,
            LaterRegionCode, LastRegionCode, BranchStart,
            MaybeEnd1, MaybeEnd, RestCode, !CI),

        Code = tree_list([
            EntryResumePointCode,
            RestoreHpCode,
            RestoreTicketCode,
            SaveHpCode,
            LaterRegionCode,
            ModContCode,
            TraceCode,
            GoalCode,
            ResumeVarsCode,
            PruneTicketCode,
            SaveCode,
            BranchCode,
            RestCode
        ])
    ;
        Resume = no_resume_point,

        % Emit code for the last disjunct.

        % Restore the heap pointer and solver state if necessary.
        maybe_restore_and_release_hp(MaybeHpSlot0, RestoreHpCode, !CI),
        maybe_reset_discard_and_release_ticket(MaybeTicketSlot,
            reset_reason_undo, RestoreTicketCode, !CI),

        undo_disj_hijack(HijackInfo, UndoCode, !CI),

        maybe_generate_internal_event_code(Goal0, DisjGoalInfo, TraceCode,
            !CI),
        code_gen.generate_goal(CodeModel, Goal0, GoalCode, !CI),
        goal_info_get_store_map(DisjGoalInfo, StoreMap),
        generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd, SaveCode, !CI),

        EndCode = node([
            llds_instr(label(EndLabel), "End of nondet disj")
        ]),
        Code = tree_list([
            EntryResumePointCode,
            TraceCode,      % XXX Should this be after LastRegionCode?
            RestoreHpCode,
            RestoreTicketCode,
            LastRegionCode,
            UndoCode,
            GoalCode,
            SaveCode,
            EndCode
        ])
    ).

%-----------------------------------------------------------------------------%

:- pred maybe_create_disj_region_frame(add_region_ops::in, hlds_goal_info::in,
    code_tree::out, code_tree::out, code_tree::out, list(lval)::out,
    code_info::in, code_info::out) is det.

maybe_create_disj_region_frame(DisjRegionOps, _DisjGoalInfo,
        FirstCode, LaterCode, LastCode, StackVars, !CI) :-
    (
        DisjRegionOps = do_not_add_region_ops,
        FirstCode = empty,
        LaterCode = empty,
        LastCode = empty,
        StackVars = []
    ;
        DisjRegionOps = add_region_ops,
        get_forward_live_vars(!.CI, ForwardLiveVars),
        LiveRegionVars = filter_region_vars(!.CI, ForwardLiveVars),

        % XXX In computing both ProtectRegionVars and SnapshotRegionVars,
        % we should intersect LiveRegionVars with the set of region variables
        % whose regions (the regions themselves, not their variables) are live
        % at the starts of some later disjuncts (i.e. aren't used only in the
        % first disjunct). We don't yet gather this information.
        %
        % XXX In computing ProtectRegionVars, we should also delete any
        % variables that are statically known to be already protected by
        % an outer disjunction, but we don't yet have the program analysis
        % required to gather such information.
        ProtectRegionVars = LiveRegionVars,
        SnapshotRegionVars = LiveRegionVars,

        ProtectRegionVarList = set.to_sorted_list(ProtectRegionVars),
        SnapshotRegionVarList = set.to_sorted_list(SnapshotRegionVars),

        list.length(ProtectRegionVarList, NumProtectRegionVars),
        list.length(SnapshotRegionVarList, NumSnapshotRegionVars),

        get_globals(!.CI, Globals),
        globals.lookup_int_option(Globals, size_region_disj_fixed,
            FixedSize),
        globals.lookup_int_option(Globals, size_region_disj_protect,
            ProtectSize),
        globals.lookup_int_option(Globals, size_region_disj_snapshot,
            SnapshotSize),
        FrameSize = FixedSize
            + ProtectSize * NumProtectRegionVars
            + SnapshotSize * NumSnapshotRegionVars,

        Items = list.duplicate(FrameSize, slot_region_disj),
        acquire_several_temp_slots(Items, non_persistent_temp_slot,
            StackVars, MainStackId, FirstSlotNum, LastSlotNum, !CI),
        EmbeddedStackFrame = embedded_stack_frame_id(MainStackId,
            FirstSlotNum, LastSlotNum),
        FirstNonFixedAddr =
            first_nonfixed_embedded_slot_addr(EmbeddedStackFrame, FixedSize),
        acquire_reg(reg_r, ProtectNumRegLval, !CI),
        acquire_reg(reg_r, SnapshotNumRegLval, !CI),
        acquire_reg(reg_r, AddrRegLval, !CI),
        PushInitCode = node([
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
            !CI),
        disj_alloc_snapshot_regions(SnapshotNumRegLval, AddrRegLval,
            EmbeddedStackFrame, SnapshotRegionVarList, SnapshotRegionCode,
            !CI),
        SetCode = node([
            llds_instr(
                region_set_fixed_slot(region_set_disj_num_protects,
                    EmbeddedStackFrame, lval(ProtectNumRegLval)),
                "Store the number of protect_infos"),
            llds_instr(
                region_set_fixed_slot(region_set_disj_num_snapshots,
                    EmbeddedStackFrame, lval(SnapshotNumRegLval)),
                "Store the number of snapshot_infos")
        ]),
        release_reg(ProtectNumRegLval, !CI),
        release_reg(SnapshotNumRegLval, !CI),
        release_reg(AddrRegLval, !CI),

        FirstCode = tree_list([
            PushInitCode,
            ProtectRegionCode,
            SnapshotRegionCode,
            SetCode
        ]),
        LaterCode = node([
            llds_instr(
                use_and_maybe_pop_region_frame(region_disj_later,
                    EmbeddedStackFrame),
                "region enter later disjunct")
        ]),
        LastCode = node([
            llds_instr(
                use_and_maybe_pop_region_frame(region_disj_last,
                    EmbeddedStackFrame),
                "region enter last disjunct")
        ])
    ).

:- pred disj_protect_regions(lval::in, lval::in, embedded_stack_frame_id::in,
    list(prog_var)::in, code_tree::out, code_info::in, code_info::out) is det.

disj_protect_regions(_, _, _, [], empty, !CI).
disj_protect_regions(NumLval, AddrLval, EmbeddedStackFrame,
        [RegionVar | RegionVars], tree(Code, Codes), !CI) :-
    produce_variable(RegionVar, ProduceVarCode, RegionVarRval, !CI),
    SaveCode = node([
        llds_instr(
            region_fill_frame(region_fill_disj_protect,
                EmbeddedStackFrame, RegionVarRval, NumLval, AddrLval),
            "disj protect the region if needed")
    ]),
    Code = tree(ProduceVarCode, SaveCode),
    disj_protect_regions(NumLval, AddrLval, EmbeddedStackFrame,
        RegionVars, Codes, !CI).

:- pred disj_alloc_snapshot_regions(lval::in, lval::in,
    embedded_stack_frame_id::in, list(prog_var)::in, code_tree::out,
    code_info::in, code_info::out) is det.

disj_alloc_snapshot_regions(_, _, _, [], empty, !CI).
disj_alloc_snapshot_regions(NumLval, AddrLval, EmbeddedStackFrame,
        [RegionVar | RegionVars], tree(Code, Codes), !CI) :-
    produce_variable(RegionVar, ProduceVarCode, RegionVarRval, !CI),
    SaveCode = node([
        llds_instr(
            region_fill_frame(region_fill_disj_snapshot,
                EmbeddedStackFrame, RegionVarRval, NumLval, AddrLval),
            "take alloc snapshot of the region")
    ]),
    Code = tree(ProduceVarCode, SaveCode),
    disj_alloc_snapshot_regions(NumLval, AddrLval, EmbeddedStackFrame,
        RegionVars, Codes, !CI).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "disj_gen.m".

%-----------------------------------------------------------------------------%
:- end_module disj_gen.
%-----------------------------------------------------------------------------%

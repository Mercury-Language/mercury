%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ite_gen.m.
% Main authors: conway, fjh, zs.
%
% The predicates of this module generate code for if-then-elses, and for
% negations (which are cut-down versions of if-then-elses, since "not(G)"
% is equivalent to "if G then fail else true").
%
%---------------------------------------------------------------------------%

:- module ll_backend.ite_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.llds.

%---------------------------------------------------------------------------%

:- pred generate_ite(code_model::in,
    hlds_goal::in, hlds_goal::in, hlds_goal::in, hlds_goal_info::in,
    llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred generate_negation(code_model::in, hlds_goal::in, hlds_goal_info::in,
    llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.opt_debug.
:- import_module ll_backend.trace_gen.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.
:- import_module transform_hlds.rbmm.
:- import_module transform_hlds.rbmm.region_transformation.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%

generate_ite(CodeModel, CondGoal0, ThenGoal, ElseGoal, IteGoalInfo, Code,
        !CI, !CLD) :-
    CondGoal0 = hlds_goal(CondExpr, CondInfo0),
    CondCodeModel = goal_info_get_code_model(CondInfo0),
    ( if
        CodeModel = model_non,
        CondCodeModel \= model_non
    then
        EffCodeModel = model_semi
    else
        EffCodeModel = CodeModel
    ),

    read_and_erase_resume_point("condition of an if-then-else",
        ResumeVars, ResumeLocs, CondInfo0, CondInfo),
    CondGoal = hlds_goal(CondExpr, CondInfo),

    % Make sure that the variables whose values will be needed on backtracking
    % to the else part are materialized into registers or stack slots.
    % Their locations are recorded in ResumeMap.
    produce_vars(set_of_var.to_sorted_list(ResumeVars), ResumeMap,
        FlushCode, !CLD),

    % Maybe save the heap state current before the condition.
    % This is after produce_vars, since code that flushes the cache
    % may allocate memory we must not "recover".
    % XXX We specify reclaim_heap_on_semidet_failure even if EffCodeModel
    % is *not* model_semi, which looks to be a bug, though one that affects
    % only non-gc grades, which haven't been used in a long time.
    ite_maybe_save_hp(reclaim_heap_on_semidet_failure, CondGoal,
        SaveHpCode, MaybeHpSlot, !CI, !CLD),

    % Maybe save the current trail state before the condition.
    % NOTE: This code should be kept up-to-date with the corresponding code
    % for the MLDS backend in add_trail_ops.m.
    AddTrailOps = should_add_trail_ops(!.CI, IteGoalInfo),
    (
        AddTrailOps = do_not_add_trail_ops,
        SaveTicketCode = cord.empty,
        MaybeTicketSlot = no
    ;
        AddTrailOps = add_trail_ops,
        ( if
            % This test is much more likely to fail ...
            goal_cannot_modify_trail(CondInfo0) = yes,
            % ... than these two.
            CondCodeModel \= model_non,
            get_opt_trail_ops(!.CI, yes)
        then
            SaveTicketCode = cord.empty,
            MaybeTicketSlot = no
        else
            save_ticket(SaveTicketCode, TicketSlot, !CI, !CLD),
            MaybeTicketSlot = yes(TicketSlot)
        )
    ),

    goal_to_conj_list(ElseGoal, ElseGoals),
    maybe_create_ite_region_frame(IteGoalInfo, CondGoal, ElseGoals,
        RegionCondCode, RegionThenCode, RegionElseCode, RegionStackVars,
        MaybeEmbeddedStackFrameId, !CI, !CLD),

    remember_position(!.CLD, BranchStart),

    prepare_for_ite_hijack(CondCodeModel, MaybeEmbeddedStackFrameId,
        HijackInfo, PrepareHijackCode, !CI, !CLD),

    make_resume_point(ResumeVars, ResumeLocs, ResumeMap, ResumePoint, !CI),
    effect_resume_point(ResumePoint, EffCodeModel, EffectResumeCode, !CLD),

    trace [compiletime(flag("codegen_goal")), io(!IO)] (
        ( if should_trace_code_gen(!.CI) then
            get_module_info(!.CI, ModuleInfo),
            get_debug_output_stream(ModuleInfo, DebugStream, !IO),
            EffectResumeInstrs = cord.list(EffectResumeCode),
            io.write_string(DebugStream, "\nEFFECT RESUME INSTRS:\n", !IO),
            MaybeProcLabel = no,
            write_instrs(DebugStream, EffectResumeInstrs, MaybeProcLabel,
                auto_comments, !IO),
            io.flush_output(DebugStream, !IO)
        else
            true
        )
    ),

    % Generate the condition.
    maybe_generate_internal_event_code(CondGoal, IteGoalInfo, CondTraceCode,
        !CI, !CLD),
    generate_goal(CondCodeModel, CondGoal, CondCode, !CI, !CLD),

    ite_enter_then(HijackInfo, ResumePoint, ThenNeckCode, ElseNeckCode,
        !CI, !CLD),

    % Kill again any variables that have become zombies.
    pickup_zombies(Zombies, !CLD),
    make_vars_forward_dead(Zombies, !CLD),

    % Discard hp and prune trail ticket if the condition succeeded.
    (
        CondCodeModel = model_non,
        % We cannot release the stack slots used for the heap pointer
        % and the trail ticket if the condition can be backtracked
        % into.  Nor can we prune the trail ticket that we allocated,
        % since the condition may have allocated other trail tickets
        % since then which have not yet been pruned.
        %
        % We also cannot release RegionStackVars.
        maybe_reset_ticket(MaybeTicketSlot, reset_reason_solve,
            ResetTicketCode)
    ;
        ( CondCodeModel = model_det
        ; CondCodeModel = model_semi
        ),
        maybe_release_hp(MaybeHpSlot, !CI, !CLD),
        maybe_reset_prune_and_release_ticket(MaybeTicketSlot,
            reset_reason_commit, ResetTicketCode, !CI, !CLD),

        release_several_temp_slots(RegionStackVars, non_persistent_temp_slot,
            !CI, !CLD)
    ),

    goal_info_get_store_map(IteGoalInfo, StoreMap),
    get_instmap(!.CLD, EndCondInstMap),
    ( if instmap_is_unreachable(EndCondInstMap) then
        % If the instmap indicates we cannot reach the then part,
        % do not attempt to generate it (may cause aborts).
        ThenTraceCode = cord.empty,
        ThenCode = cord.empty,
        map.init(BranchEndStoreMap)
    else
        % Generate the then branch.
        maybe_generate_internal_event_code(ThenGoal, IteGoalInfo,
            ThenTraceCode, !CI, !CLD),
        code_gen.generate_goal(CodeModel, ThenGoal, ThenCode, !CI, !CLD),
        BranchEndStoreMap = StoreMap
    ),
    generate_branch_end(BranchEndStoreMap, no, MaybeEnd0, ThenSaveCode, !.CLD),

    % Generate the entry to the else branch.
    reset_to_position(BranchStart, !.CI, !:CLD),
    generate_resume_point(ResumePoint, ResumeCode, !CI, !CLD),

    trace [compiletime(flag("codegen_goal")), io(!IO)] (
        ( if should_trace_code_gen(!.CI) then
            get_module_info(!.CI, ModuleInfo),
            get_debug_output_stream(ModuleInfo, DebugStream, !IO),
            ResumeInstrs = cord.list(ResumeCode),
            io.write_string(DebugStream, "\nRESUME INSTRS:\n", !IO),
            write_instrs(DebugStream, ResumeInstrs, no, auto_comments, !IO),
            io.flush_output(DebugStream, !IO)
        else
            true
        )
    ),

    % Restore the heap pointer and solver state if necessary.
    maybe_restore_and_release_hp(MaybeHpSlot, RestoreHpCode, !CI, !CLD),
    maybe_reset_discard_and_release_ticket(MaybeTicketSlot,
        reset_reason_undo, RestoreTicketCode, !CI, !CLD),

    % Generate the else branch.
    maybe_generate_internal_event_code(ElseGoal, IteGoalInfo, ElseTraceCode,
        !CI, !CLD),
    code_gen.generate_goal(CodeModel, ElseGoal, ElseCode, !CI, !CLD),
    generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd, ElseSaveCode, !.CLD),

    trace [compiletime(flag("codegen_goal")), io(!IO)] (
        ( if should_trace_code_gen(!.CI) then
            get_module_info(!.CI, ModuleInfo),
            get_debug_output_stream(ModuleInfo, DebugStream, !IO),
            ElseSaveInstrs = cord.list(ElseSaveCode),
            io.write_string(DebugStream, "\nBRANCH END INSTRS:\n", !IO),
            write_instrs(DebugStream, ElseSaveInstrs, no, auto_comments, !IO),
            io.flush_output(DebugStream, !IO)
        else
            true
        )
    ),

    get_next_label(EndLabel, !CI),
    JumpToEndCode = singleton(
        llds_instr(goto(code_label(EndLabel)),
            "Jump to the end of if-then-else")
    ),
    EndLabelCode = singleton(
        llds_instr(label(EndLabel), "end of if-then-else")
    ),
    get_globals(!.CI, Globals),
    make_pneg_context_wrappers(Globals, CondInfo, PNegCondCode, PNegThenCode,
        PNegElseCode),
    Code =
        FlushCode ++
        SaveHpCode ++
        SaveTicketCode ++
        RegionCondCode ++
        PrepareHijackCode ++
        EffectResumeCode ++
        CondTraceCode ++
        PNegCondCode ++
        CondCode ++
        ThenNeckCode ++
        ResetTicketCode ++
        RegionThenCode ++
        ThenTraceCode ++
        PNegThenCode ++
        ThenCode ++
        ThenSaveCode ++
        JumpToEndCode ++
        ResumeCode ++
        ElseNeckCode ++
        RestoreHpCode ++
        RestoreTicketCode ++
        RegionElseCode ++
        ElseTraceCode ++
        PNegElseCode ++
        ElseCode ++
        ElseSaveCode ++
        EndLabelCode,
    after_all_branches(StoreMap, MaybeEnd, !.CI, !:CLD).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

generate_negation(CodeModel, Goal0, NotGoalInfo, Code, !CI, !CLD) :-
    (
        CodeModel = model_non,
        unexpected($pred, "nondet negation")
    ;
        ( CodeModel = model_det
        ; CodeModel = model_semi
        )
    ),

    Goal0 = hlds_goal(GoalExpr, GoalInfo0),
    read_and_erase_resume_point("negated goal",
        ResumeVars, ResumeLocs, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo),

    % For a negated simple test, we can generate better code than the general
    % mechanism, because we don't have to flush the cache.
    ( if
        CodeModel = model_semi,
        GoalExpr = unify(_, _, _, simple_test(L, R), _),
        failure_is_direct_branch(!.CLD, CodeAddr),
        get_globals(!.CI, Globals),
        globals.get_opt_tuple(Globals, OptTuple),
        OptTuple ^ ot_opt_simple_neg = opt_simple_neg
    then
        % Because we are generating the negated goal ourselves, we need to
        % apply the pre- and post-goal updates that would normally be applied
        % by code_gen.generate_goal.

        enter_simple_neg(set_of_var.to_sorted_list(ResumeVars), GoalInfo,
            SimpleNeg, !CLD),
        produce_variable(L, CodeL, ValL, !CLD),
        produce_variable(R, CodeR, ValR, !CLD),
        Type = variable_type(!.CI, L),
        ( if Type = builtin_type(BuiltinType) then
            ( if BuiltinType = builtin_type_string then
                Op = str_eq
            else if BuiltinType = builtin_type_float then
                Op = float_eq
            else if BuiltinType = builtin_type_int(IntType) then
                Op = eq(IntType)
            else
                Op = eq(int_type_int)
            )
        else
            Op = eq(int_type_int)
        ),
        TestCode = singleton(
            llds_instr(if_val(binop(Op, ValL, ValR), CodeAddr),
                "test inequality")
        ),
        leave_simple_neg(GoalInfo, SimpleNeg, !.CI, !CLD),
        Code = CodeL ++ CodeR ++ TestCode
    else
        generate_negation_general(CodeModel, Goal, NotGoalInfo,
            ResumeVars, ResumeLocs, Code, !CI, !CLD)
    ).

    % The code of generate_negation_general is a cut-down version
    % of the code for if-then-elses.
    %
:- pred generate_negation_general(code_model::in,
    hlds_goal::in, hlds_goal_info::in, set_of_progvar::in,
    resume_locs::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_negation_general(CodeModel, Goal, NotGoalInfo, ResumeVars, ResumeLocs,
        Code, !CI, !CLD) :-
    produce_vars(set_of_var.to_sorted_list(ResumeVars), ResumeMap,
        FlushCode, !CLD),

    % Maybe save the heap state current before the condition. This ought to be
    % after we make the failure continuation, because that causes the cache to
    % get flushed.
    ite_maybe_save_hp(reclaim_heap_on_semidet_failure, Goal,
        SaveHpCode, MaybeHpSlot, !CI, !CLD),

    % XXX Consider optimizing AddTrailOps as we do above for if-then-elses.
    AddTrailOps = should_add_trail_ops(!.CI, NotGoalInfo),
    maybe_save_ticket(AddTrailOps, SaveTicketCode, MaybeTicketSlot, !CI, !CLD),

    Goal = hlds_goal(_, GoalInfo),
    CondGoal = Goal,
    ElseGoals = [],
    maybe_create_ite_region_frame(GoalInfo, CondGoal, ElseGoals,
        RegionCondCode, RegionThenCode, RegionElseCode, RegionStackVars,
        MaybeEmbeddedStackFrameId, !CI, !CLD),
    % MaybeEmbeddedStackFrameId should be yes only for nondet conditions,
    % and a negated goal can't be nondet.
    expect(unify(MaybeEmbeddedStackFrameId, no), $pred,
        "MaybeEmbeddedStackFrameId = yes(_)"),

    prepare_for_ite_hijack(CodeModel, MaybeEmbeddedStackFrameId, HijackInfo,
        PrepareHijackCode, !CI, !CLD),

    make_resume_point(ResumeVars, ResumeLocs, ResumeMap, ResumePoint, !CI),
    effect_resume_point(ResumePoint, CodeModel, EffectResumeCode, !CLD),

    % Generate the negated goal as a semi-deterministic goal; it cannot be
    % nondet, since mode correctness requires it to have no output vars.
    maybe_generate_internal_event_code(Goal, NotGoalInfo, EnterTraceCode,
        !CI, !CLD),
    code_gen.generate_goal(model_semi, Goal, GoalCode, !CI, !CLD),

    ite_enter_then(HijackInfo, ResumePoint, ThenNeckCode, ElseNeckCode,
        !CI, !CLD),

    % Kill again any variables that have become zombies.
    pickup_zombies(Zombies, !CLD),
    make_vars_forward_dead(Zombies, !CLD),

    get_forward_live_vars(!.CLD, LiveVars),

    (
        CodeModel = model_det,
        % The then branch will never be reached.
        PruneTicketCode = cord.empty,
        FailTraceCode = cord.empty,
        FailCode = cord.empty
    ;
        ( CodeModel = model_semi
        ; CodeModel = model_non
        ),
        remember_position(!.CLD, AfterNegatedGoal),
        % The call to reset_ticket(..., commit) here is necessary
        % in order to properly detect floundering.
        maybe_release_hp(MaybeHpSlot, !CI, !CLD),
        maybe_reset_prune_and_release_ticket(MaybeTicketSlot,
            reset_reason_commit, PruneTicketCode, !CI, !CLD),
        maybe_generate_negated_event_code(Goal, NotGoalInfo, neg_failure,
            FailTraceCode, !CI, !CLD),
        generate_failure(FailCode, !CI, !.CLD),
        % We want liveness after not(G) to be the same as after G.
        % Information about what variables are where will be set
        % by generate_resume_point.
        reset_to_position(AfterNegatedGoal, !.CI, !:CLD)
    ),

    % Generate the entry to the else branch.
    generate_resume_point(ResumePoint, ResumeCode, !CI, !CLD),

    set_forward_live_vars(LiveVars, !CLD),

    % Restore the heap pointer and solver state if necessary.
    maybe_restore_and_release_hp(MaybeHpSlot, RestoreHpCode, !CI, !CLD),
    maybe_reset_discard_and_release_ticket(MaybeTicketSlot,
        reset_reason_undo, RestoreTicketCode, !CI, !CLD),
    release_several_temp_slots(RegionStackVars,
        non_persistent_temp_slot, !CI, !CLD),
    maybe_generate_negated_event_code(Goal, NotGoalInfo, neg_success,
        SuccessTraceCode, !CI, !CLD),

    code_info.get_globals(!.CI, Globals),
    make_pneg_context_wrappers(Globals, NotGoalInfo, PNegCondCode,
        PNegThenCode, PNegElseCode),
    Code =
        FlushCode ++
        PrepareHijackCode ++
        EffectResumeCode ++
        SaveHpCode ++
        SaveTicketCode ++
        RegionCondCode ++
        EnterTraceCode ++
        PNegCondCode ++
        GoalCode ++
        ThenNeckCode ++
        PruneTicketCode ++
        RegionThenCode ++
        FailTraceCode ++
        PNegThenCode ++
        FailCode ++
        ResumeCode ++
        ElseNeckCode ++
        RestoreTicketCode ++
        RestoreHpCode ++
        RegionElseCode ++
        SuccessTraceCode ++
        PNegElseCode.

%---------------------------------------------------------------------------%

:- pred read_and_erase_resume_point(string::in,
    set_of_progvar::out, resume_locs::out,
    hlds_goal_info::in, hlds_goal_info::out) is det.

read_and_erase_resume_point(CondStr, ResumeVars, ResumeLocs,
        CondInfo0, CondInfo) :-
    goal_info_get_resume_point(CondInfo0, Resume),
    (
        Resume = resume_point(ResumeVars, ResumeLocs),
        % The pre_goal_update sanity check insists on no_resume_point,
        % to make sure that all resume points have been handled by
        % surrounding code.
        goal_info_set_resume_point(no_resume_point, CondInfo0, CondInfo)
    ;
        Resume = no_resume_point,
        Msg = CondStr ++ " has no resume point",
        unexpected($pred, Msg)
    ).

%---------------------------------------------------------------------------%

    % If the code in the condition depends on a consumer of a generator
    % that is not complete by the time we finish executing the condition,
    % then failure out of the condition does not necessarily mean that
    % the condition has no solution; it may mean simply that the condition's
    % solution depends on a generator solution that hasn't been produced yet
    % and thus hasn't been given to the consumer yet.
    %
    % Detecting such situations requires knowing whether tabled subgoals
    % (both generators and consumers) are started inside possibly negated
    % contexts or not, which is why we wrap the condition inside
    % MR_pneg_enter_{cond,then,exit}.
    %
:- pred make_pneg_context_wrappers(globals::in, hlds_goal_info::in,
    llds_code::out, llds_code::out, llds_code::out) is det.

make_pneg_context_wrappers(Globals, GoalInfo, PNegCondCode, PNegThenCode,
        PNegElseCode) :-
    globals.lookup_bool_option(Globals, use_minimal_model_stack_copy_pneg,
        UseMinimalModelStackCopyPNeg),
    ( if
        UseMinimalModelStackCopyPNeg = yes,
        not goal_info_has_feature(GoalInfo, feature_will_not_call_mm_tabled)
    then
        Context = goal_info_get_context(GoalInfo),
        ( if is_dummy_context(Context) then
            CtxtStr = "NULL"
        else
            File = term_context.context_file(Context),
            Line = term_context.context_line(Context),
            CtxtStr = "\"" ++ File ++ ":" ++ int_to_string(Line) ++ "\""
        ),

        PNegCondComponents = [
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, live_lvals_info(set.init),
                wrap_transient("\t\tMR_pneg_enter_cond();\n"))
        ],
        PNegThenComponents = [
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, live_lvals_info(set.init),
                wrap_transient("\t\tMR_pneg_enter_then();\n"))
        ],
        PNegElseComponents = [
            foreign_proc_raw_code(cannot_branch_away,
                proc_does_not_affect_liveness, live_lvals_info(set.init),
                wrap_transient("\t\tMR_pneg_enter_else(" ++ CtxtStr ++ ");\n"))
        ],
        MD = proc_may_duplicate,
        PNegCondCode = singleton(
            llds_instr(foreign_proc_code([], PNegCondComponents,
                proc_will_not_call_mercury, no, no, no, no, no, yes, MD), "")
        ),
        PNegThenCode = singleton(
            llds_instr(foreign_proc_code([], PNegThenComponents,
                proc_will_not_call_mercury, no, no, no, no, no, yes, MD), "")
        ),
        PNegElseCode = singleton(
            llds_instr(foreign_proc_code([], PNegElseComponents,
                proc_will_not_call_mercury, no, no, no, no, no, yes, MD), "")
        )
    else
        PNegCondCode = cord.empty,
        PNegThenCode = cord.empty,
        PNegElseCode = cord.empty
    ).

:- func wrap_transient(string) = string.

wrap_transient(Code) =
    string.append_list([
        "\t\tMR_save_transient_registers();\n",
        Code,
        "\t\tMR_restore_transient_registers();\n"]).

%-----------------------------------------------------------------------------%

:- pred maybe_create_ite_region_frame(hlds_goal_info::in, hlds_goal::in,
    list(hlds_goal)::in,
    llds_code::out, llds_code::out, llds_code::out, list(lval)::out,
    maybe(embedded_stack_frame_id)::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

maybe_create_ite_region_frame(WholeGoalInfo, CondGoal, ElseGoals,
        RegionCondCode, RegionThenCode, RegionElseCode, RegionStackVars,
        MaybeEmbeddedStackFrameId, !CI, !CLD) :-
    AddRegionOps = should_add_region_ops(!.CI, WholeGoalInfo),
    (
        AddRegionOps = do_not_add_region_ops,
        RegionCondCode = cord.empty,
        RegionThenCode = cord.empty,
        RegionElseCode = cord.empty,
        RegionStackVars = [],
        MaybeEmbeddedStackFrameId = no
    ;
        AddRegionOps = add_region_ops,
        create_ite_region_frame(CondGoal, ElseGoals,
            RegionCondCode, RegionThenCode, RegionElseCode, RegionStackVars,
            MaybeEmbeddedStackFrameId, !CI, !CLD)
    ).

:- pred create_ite_region_frame(hlds_goal::in, list(hlds_goal)::in,
    llds_code::out, llds_code::out, llds_code::out, list(lval)::out,
    maybe(embedded_stack_frame_id)::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

create_ite_region_frame(CondGoal, ElseGoals, CondCode, ThenCode, ElseCode,
        StackVars, MaybeEmbeddedStackFrameId, !CI, !CLD) :-
    get_forward_live_vars(!.CLD, ForwardLiveVars),
    LiveRegionVars = filter_region_vars(!.CI, ForwardLiveVars),

    CondGoal = hlds_goal(_CondExpr, CondGoalInfo),
    MaybeRbmmInfo = goal_info_get_maybe_rbmm(CondGoalInfo),
    (
        MaybeRbmmInfo = no,
        CondCode = cord.empty,
        ThenCode = cord.empty,
        ElseCode = cord.empty,
        StackVars = [],
        MaybeEmbeddedStackFrameId = no
    ;
        MaybeRbmmInfo = yes(RbmmInfo),
        goal_to_conj_list(CondGoal, CondGoals),

        RbmmInfo = rbmm_goal_info(CondCreatedRegionVars,
            CondRemovedRegionVars, CondCarriedRegionVars,
            CondAllocRegionVars, _CondUsedRegionVars),
        list.reverse(CondGoals, ReversedCondGoals),
        code_info.get_module_info(!.CI, ModuleInfo),
        find_regions_removed_at_start_of_goals(ReversedCondGoals,
            ModuleInfo, set.init, RemovedAtEndOfThen),
        set.difference(CondRemovedRegionVars, RemovedAtEndOfThen,
            NeedToBeProtectedRegionVars),
        ( if
            set.is_empty(CondCreatedRegionVars),
            set.is_empty(NeedToBeProtectedRegionVars),
            set.is_empty(CondAllocRegionVars)
        then
            % When no region-related operations occur in the condition,
            % we do not need the backtracking support code.
            CondCode = cord.empty,
            ThenCode = cord.empty,
            ElseCode = cord.empty,
            StackVars = [],
            MaybeEmbeddedStackFrameId = no
        else
            find_regions_removed_at_start_of_goals(ElseGoals, ModuleInfo,
                set.init, RemovedAtStartOfElse),

            % The UnprotectedRemovedAtStartOfElse is the intersection of
            % RemovedAtStartOfElse and the set of region variables
            % whose regions are statically known to be unprotected
            % at this point in the code. These are actually carried regions
            % because carried region are statically known to be
            % not protected by the condition.
            set.intersect(RemovedAtStartOfElse, CondCarriedRegionVars,
                UnprotectedRemovedAtStartOfElse),

            set_of_var.intersect(LiveRegionVars,
                set_to_bitset(NeedToBeProtectedRegionVars),
                ProtectRegionVars),
            set_of_var.intersect(LiveRegionVars,
                set_to_bitset(CondAllocRegionVars),
                SnapshotRegionVars0),
            set_of_var.difference(SnapshotRegionVars0,
                set_to_bitset(UnprotectedRemovedAtStartOfElse),
                SnapshotRegionVars),

            ProtectRegionVarList =
                set_of_var.to_sorted_list(ProtectRegionVars),
            SnapshotRegionVarList =
                set_of_var.to_sorted_list(SnapshotRegionVars),

            list.length(ProtectRegionVarList, NumProtectRegionVars),
            list.length(SnapshotRegionVarList, NumSnapshotRegionVars),

            code_info.get_globals(!.CI, Globals),
            globals.lookup_int_option(Globals, size_region_ite_fixed,
                FixedSize),
            globals.lookup_int_option(Globals, size_region_ite_protect,
                ProtectSize),
            globals.lookup_int_option(Globals, size_region_ite_snapshot,
                SnapshotSize),
            FrameSize = FixedSize
                + ProtectSize * NumProtectRegionVars
                + SnapshotSize * NumSnapshotRegionVars,

            Items = list.duplicate(FrameSize, slot_region_ite),
            acquire_several_temp_slots(Items, non_persistent_temp_slot,
                StackVars, MainStackId, FirstSlotNum, LastSlotNum, !CI, !CLD),
            EmbeddedStackFrameId = embedded_stack_frame_id(MainStackId,
                FirstSlotNum, LastSlotNum),
            FirstNonFixedAddr = first_nonfixed_embedded_slot_addr(
                EmbeddedStackFrameId, FixedSize),
            acquire_reg(reg_r, ProtectNumRegLval, !CLD),
            acquire_reg(reg_r, SnapshotNumRegLval, !CLD),
            acquire_reg(reg_r, AddrRegLval, !CLD),
            PushInitCode = from_list([
                llds_instr(
                    push_region_frame(region_stack_ite,
                        EmbeddedStackFrameId),
                    "Save stack pointer of embedded region ite stack"),
                llds_instr(
                    assign(ProtectNumRegLval, const(llconst_int(0))),
                    "Initialize number of protect_infos"),
                llds_instr(
                    assign(SnapshotNumRegLval, const(llconst_int(0))),
                    "Initialize number of snapshot_infos"),
                llds_instr(
                    assign(AddrRegLval, FirstNonFixedAddr),
                    "Initialize pointer to nonfixed part of" ++
                    " embedded frame")
            ]),
            ite_protect_regions(ProtectNumRegLval, AddrRegLval,
                EmbeddedStackFrameId, ProtectRegionVarList,
                ProtectRegionCode, !CLD),
            ite_alloc_snapshot_regions(SnapshotNumRegLval, AddrRegLval,
                EmbeddedStackFrameId, RemovedAtStartOfElse,
                SnapshotRegionVarList, SnapshotRegionCode, !CLD),
            SetCode = from_list([
                llds_instr(
                    region_set_fixed_slot(region_set_ite_num_protects,
                        EmbeddedStackFrameId, lval(ProtectNumRegLval)),
                    "Store the number of protect_infos"),
                llds_instr(
                    region_set_fixed_slot(region_set_ite_num_snapshots,
                        EmbeddedStackFrameId, lval(SnapshotNumRegLval)),
                    "Store the number of snapshot_infos")
            ]),
            release_reg(ProtectNumRegLval, !CLD),
            release_reg(SnapshotNumRegLval, !CLD),
            release_reg(AddrRegLval, !CLD),

            CondCodeModel = goal_info_get_code_model(CondGoalInfo),
            (
                CondCodeModel = model_non,
                CondKind = region_ite_nondet_cond,
                MaybeEmbeddedStackFrameId = yes(EmbeddedStackFrameId)
            ;
                CondCodeModel = model_semi,
                CondKind = region_ite_semidet_cond,
                MaybeEmbeddedStackFrameId = no
            ;
                CondCodeModel = model_det,
                unexpected($pred, "det cond")
            ),

            CondCode = PushInitCode ++ ProtectRegionCode ++
                SnapshotRegionCode ++ SetCode,
            ThenCode = singleton(
                llds_instr(
                    use_and_maybe_pop_region_frame(
                        region_ite_then(CondKind),
                        EmbeddedStackFrameId),
                    "region enter then")
            ),
            ElseCode = singleton(
                llds_instr(
                    use_and_maybe_pop_region_frame(
                        region_ite_else(CondKind),
                        EmbeddedStackFrameId),
                    "region enter else")
            )

            % XXX A model_non condition can succeed more than once,
            % so the region_ite_then(region_ite_nondet_cond) operation
            % cannot pop the ite stack frame. We need to pop this frame
            % when the condition fails *after* succeeding at least once.
            % This requires modifying the failure continuation and/or
            % the resume point. This has not yet been implemented.
        )
    ).

    % Given the list of goals in the else branch, accumulate the region
    % variables whose regions are removed at the start of that list.
    %
:- pred find_regions_removed_at_start_of_goals(list(hlds_goal)::in,
    module_info::in, set(prog_var)::in, set(prog_var)::out) is det.

find_regions_removed_at_start_of_goals([], _, !Removed).
find_regions_removed_at_start_of_goals([Goal | Goals], ModuleInfo, !Removed) :-
    Goal = hlds_goal(GoalExpr, _),
    ( if
        GoalExpr = plain_call(PredId, _ProcId, Args, _Builtin, _UC, _SymName),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_module(PredInfo) = mercury_region_builtin_module,
        pred_info_name(PredInfo) = remove_region_pred_name,
        Args = [RegionVar]
    then
        set.insert(RegionVar, !Removed),
        find_regions_removed_at_start_of_goals(Goals, ModuleInfo, !Removed)
    else
        true
    ).

:- pred ite_protect_regions(lval::in, lval::in, embedded_stack_frame_id::in,
    list(prog_var)::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

ite_protect_regions(_, _, _, [], cord.empty, !CLD).
ite_protect_regions(NumLval, AddrLval, EmbeddedStackFrameId,
        [RegionVar | RegionVars], Code ++ Codes, !CLD) :-
    produce_variable(RegionVar, ProduceVarCode, RegionVarRval, !CLD),
    SaveCode = singleton(
        llds_instr(
            region_fill_frame(region_fill_ite_protect,
                EmbeddedStackFrameId, RegionVarRval, NumLval, AddrLval),
            "ite protect the region if needed")
    ),
    Code = ProduceVarCode ++ SaveCode,
    ite_protect_regions(NumLval, AddrLval, EmbeddedStackFrameId,
        RegionVars, Codes, !CLD).

:- pred ite_alloc_snapshot_regions(lval::in, lval::in,
    embedded_stack_frame_id::in, set(prog_var)::in,
    list(prog_var)::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

ite_alloc_snapshot_regions(_, _, _, _, [], cord.empty, !CLD).
ite_alloc_snapshot_regions(NumLval, AddrLval, EmbeddedStackFrameId,
        RemovedVars, [RegionVar | RegionVars], Code ++ Codes, !CLD) :-
    produce_variable(RegionVar, ProduceVarCode, RegionVarRval, !CLD),
    ( if set.member(RegionVar, RemovedVars) then
        RemovedAtStartOfElse = removed_at_start_of_else
    else
        RemovedAtStartOfElse = not_removed_at_start_of_else
    ),
    SaveCode = singleton(
        llds_instr(
            region_fill_frame(region_fill_ite_snapshot(RemovedAtStartOfElse),
                EmbeddedStackFrameId, RegionVarRval, NumLval, AddrLval),
            "take alloc snapshot of the region")
    ),
    Code = ProduceVarCode ++ SaveCode,
    ite_alloc_snapshot_regions(NumLval, AddrLval, EmbeddedStackFrameId,
        RemovedVars, RegionVars, Codes, !CLD).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.ite_gen.
%-----------------------------------------------------------------------------%

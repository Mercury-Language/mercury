%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ite_gen.m.
% Main authors: conway, fjh, zs.
%
% The predicates of this module generate code for if-then-elses, and for
% negations (which are cut-down versions of if-then-elses, since not(G)
% is equivalent to (G -> fail ; true)).
%
%---------------------------------------------------------------------------%

:- module ll_backend.ite_gen.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.llds.

%---------------------------------------------------------------------------%

:- pred ite_gen.generate_ite(add_trail_ops::in, code_model::in,
    hlds_goal::in, hlds_goal::in, hlds_goal::in, hlds_goal_info::in,
    code_tree::out, code_info::in, code_info::out) is det.

:- pred ite_gen.generate_negation(add_trail_ops::in, code_model::in,
    hlds_goal::in, hlds_goal_info::in, code_tree::out,
    code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_llds.
:- import_module hlds.instmap.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.tree.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.trace_gen.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

generate_ite(AddTrailOps, CodeModel, CondGoal0, ThenGoal, ElseGoal,
        IteGoalInfo, Code, !CI) :-
    CondGoal0 = hlds_goal(CondExpr, CondInfo0),
    goal_info_get_code_model(CondInfo0, CondCodeModel),
    (
        CodeModel = model_non,
        CondCodeModel \= model_non
    ->
        EffCodeModel = model_semi
    ;
        EffCodeModel = CodeModel
    ),

    goal_info_get_resume_point(CondInfo0, Resume),
    (
        Resume = resume_point(ResumeVarsPrime, ResumeLocsPrime)
    ->
        ResumeVars = ResumeVarsPrime,
        ResumeLocs = ResumeLocsPrime,
        % The pre_goal_update sanity check insists on no_resume_point,
        % to make sure that all resume points have been handled by
        % surrounding code.
        goal_info_set_resume_point(no_resume_point, CondInfo0, CondInfo),
        CondGoal = hlds_goal(CondExpr, CondInfo)
    ;
        unexpected(this_file,
            "condition of an if-then-else has no resume point")
    ),

    % Make sure that the variables whose values will be needed on backtracking
    % to the else part are materialized into registers or stack slots.
    % Their locations are recorded in ResumeMap.
    code_info.produce_vars(ResumeVars, ResumeMap, FlushCode, !CI),

    % Maybe save the heap state current before the condition.
    % This is after code_info.produce_vars since code that
    % flushes the cache may allocate memory we must not "recover".
    code_info.get_globals(!.CI, Globals),
    (
        globals.lookup_bool_option(Globals,
            reclaim_heap_on_semidet_failure, yes),
        goal_may_allocate_heap(CondGoal)
    ->
        ReclaimHeap = yes
    ;
        ReclaimHeap = no
    ),
    code_info.maybe_save_hp(ReclaimHeap, SaveHpCode, MaybeHpSlot, !CI),

    % Maybe save the current trail state before the condition.
    % NOTE: this code should be kept up-to-date with the corresponding
    %       code for the MLDS backend in add_trail_ops.m.
    (
        AddTrailOps = no,
        IteTrailOps = no
    ;
        AddTrailOps = yes,
        get_opt_trail_ops(!.CI, OptTrailOps),
        (
            OptTrailOps = yes,
            goal_cannot_modify_trail(CondInfo0) = yes,
            CondCodeModel \= model_non
        ->
            IteTrailOps = no
        ;
            IteTrailOps = yes
        )
    ), 
    
    code_info.maybe_save_ticket(IteTrailOps, SaveTicketCode, MaybeTicketSlot,
        !CI),

    code_info.remember_position(!.CI, BranchStart),

    code_info.prepare_for_ite_hijack(EffCodeModel, HijackInfo,
        PrepareHijackCode, !CI),

    code_info.make_resume_point(ResumeVars, ResumeLocs, ResumeMap,
        ResumePoint, !CI),
    code_info.effect_resume_point(ResumePoint, EffCodeModel, EffectResumeCode,
        !CI),

    % Generate the condition.
    maybe_generate_internal_event_code(CondGoal, IteGoalInfo, CondTraceCode,
        !CI),
    code_gen.generate_goal(CondCodeModel, CondGoal, CondCode, !CI),

    code_info.ite_enter_then(HijackInfo, ThenNeckCode, ElseNeckCode, !CI),

    % Kill again any variables that have become zombies.
    code_info.pickup_zombies(Zombies, !CI),
    code_info.make_vars_forward_dead(Zombies, !CI),

    % Discard hp and prune trail ticket if the condition succeeded.
    ( CondCodeModel = model_non ->
        % We cannot release the stack slots used for the heap pointer
        % and the trail ticket if the condition can be backtracked
        % into.  Nor can we prune the trail ticket that we allocated,
        % since the condition may have allocated other trail tickets
        % since then which have not yet been pruned.
        code_info.maybe_reset_ticket(MaybeTicketSlot, reset_reason_solve,
            ResetTicketCode)
    ;
        code_info.maybe_release_hp(MaybeHpSlot, !CI),
        code_info.maybe_reset_prune_and_release_ticket(MaybeTicketSlot,
            reset_reason_commit, ResetTicketCode, !CI)
    ),

    goal_info_get_store_map(IteGoalInfo, StoreMap),
    code_info.get_instmap(!.CI, EndCondInstMap),
    ( instmap.is_unreachable(EndCondInstMap) ->
        % If the instmap indicates we cannot reach the then part,
        % do not attempt to generate it (may cause aborts).
        ThenTraceCode = empty,
        ThenCode = empty,
        map.init(EmptyStoreMap),
        code_info.generate_branch_end(EmptyStoreMap, no,
            MaybeEnd0, ThenSaveCode, !CI)
    ;
        % Generate the then branch.
        maybe_generate_internal_event_code(ThenGoal, IteGoalInfo,
            ThenTraceCode, !CI),
        code_gen.generate_goal(CodeModel, ThenGoal, ThenCode, !CI),
        code_info.generate_branch_end(StoreMap, no,
            MaybeEnd0, ThenSaveCode, !CI)
    ),

    % Generate the entry to the else branch.
    code_info.reset_to_position(BranchStart, !CI),
    code_info.generate_resume_point(ResumePoint, ResumeCode, !CI),

    % Restore the heap pointer and solver state if necessary.
    code_info.maybe_restore_and_release_hp(MaybeHpSlot, RestoreHpCode, !CI),
    code_info.maybe_reset_discard_and_release_ticket(MaybeTicketSlot,
        reset_reason_undo, RestoreTicketCode, !CI),

    % Generate the else branch.
    maybe_generate_internal_event_code(ElseGoal, IteGoalInfo, ElseTraceCode,
        !CI),
    code_gen.generate_goal(CodeModel, ElseGoal, ElseCode, !CI),
    code_info.generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd,
        ElseSaveCode, !CI),

    code_info.get_next_label(EndLabel, !CI),
    JumpToEndCode = node([
        llds_instr(goto(code_label(EndLabel)),
            "Jump to the end of if-then-else")
    ]),
    EndLabelCode = node([
        llds_instr(label(EndLabel), "end of if-then-else")
    ]),
    make_pneg_context_wrappers(Globals, CondInfo, PNegCondCode, PNegThenCode,
        PNegElseCode),
    Code = tree_list([
        FlushCode,
        SaveHpCode,
        SaveTicketCode,
        PrepareHijackCode,
        EffectResumeCode,
        CondTraceCode,
        PNegCondCode,
        CondCode,
        ThenNeckCode,
        ResetTicketCode,
        ThenTraceCode,
        PNegThenCode,
        ThenCode,
        ThenSaveCode,
        JumpToEndCode,
        ResumeCode,
        ElseNeckCode,
        RestoreHpCode,
        RestoreTicketCode,
        ElseTraceCode,
        PNegElseCode,
        ElseCode,
        ElseSaveCode,
        EndLabelCode]),
    code_info.after_all_branches(StoreMap, MaybeEnd, !CI).

%-----------------------------------------------------------------------------%

generate_negation(AddTrailOps, CodeModel, Goal0, NotGoalInfo, Code, !CI) :-
    ( CodeModel = model_non ->
        unexpected(this_file, "generate_negation: nondet negation.")
    ;
        true
    ),

    Goal0 = hlds_goal(GoalExpr, GoalInfo0),
    goal_info_get_resume_point(GoalInfo0, Resume),
    ( Resume = resume_point(ResumeVarsPrime, ResumeLocsPrime) ->
        ResumeVars = ResumeVarsPrime,
        ResumeLocs = ResumeLocsPrime,
        goal_info_set_resume_point(no_resume_point, GoalInfo0, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        unexpected(this_file,
            "generate_negation: negated goal has no resume point.")
    ),
    %
    % For a negated simple test, we can generate better code than the general
    % mechanism, because we don't have to flush the cache.
    %
    (
        CodeModel = model_semi,
        GoalExpr = unify(_, _, _, simple_test(L, R), _),
        code_info.failure_is_direct_branch(!.CI, CodeAddr),
        code_info.get_globals(!.CI, Globals),
        globals.lookup_bool_option(Globals, simple_neg, yes)
    ->
        % Because we are generating the negated goal ourselves, we need to
        % apply the pre- and post-goal updates that would normally be applied
        % by code_gen.generate_goal.

        code_info.enter_simple_neg(ResumeVars, GoalInfo, SimpleNeg, !CI),
        code_info.produce_variable(L, CodeL, ValL, !CI),
        code_info.produce_variable(R, CodeR, ValR, !CI),
        Type = code_info.variable_type(!.CI, L),
        ( Type = builtin_type(builtin_type_string) ->
            Op = str_eq
        ; Type = builtin_type(builtin_type_float) ->
            Op = float_eq
        ;
            Op = eq
        ),
        TestCode = node([
            llds_instr(if_val(binop(Op, ValL, ValR), CodeAddr),
                "test inequality")
        ]),
        code_info.leave_simple_neg(GoalInfo, SimpleNeg, !CI),
        Code = tree(tree(CodeL, CodeR), TestCode)
    ;
        generate_negation_general(AddTrailOps, CodeModel, Goal, NotGoalInfo,
            ResumeVars, ResumeLocs, Code, !CI)
    ).

    % The code of generate_negation_general is a cut-down version
    % of the code for if-then-elses.
    %
:- pred generate_negation_general(add_trail_ops::in, code_model::in,
    hlds_goal::in, hlds_goal_info::in, set(prog_var)::in, resume_locs::in,
    code_tree::out, code_info::in, code_info::out) is det.

generate_negation_general(AddTrailOps, CodeModel, Goal, NotGoalInfo,
        ResumeVars, ResumeLocs, Code, !CI) :-
    code_info.produce_vars(ResumeVars, ResumeMap, FlushCode, !CI),

    % Maybe save the heap state current before the condition; this ought to be
    % after we make the failure continuation because that causes the cache to
    % get flushed.

    code_info.get_globals(!.CI, Globals),
    (
        globals.lookup_bool_option(Globals,
            reclaim_heap_on_semidet_failure, yes),
        goal_may_allocate_heap(Goal)
    ->
        ReclaimHeap = yes
    ;
        ReclaimHeap = no
    ),
    code_info.maybe_save_hp(ReclaimHeap, SaveHpCode, MaybeHpSlot, !CI),

    code_info.maybe_save_ticket(AddTrailOps, SaveTicketCode,
        MaybeTicketSlot, !CI),

    code_info.prepare_for_ite_hijack(CodeModel, HijackInfo,
        PrepareHijackCode, !CI),

    code_info.make_resume_point(ResumeVars, ResumeLocs, ResumeMap,
        ResumePoint, !CI),
    code_info.effect_resume_point(ResumePoint, CodeModel,
        EffectResumeCode, !CI),

    % Generate the negated goal as a semi-deterministic goal; it cannot be
    % nondet, since mode correctness requires it to have no output vars.
    maybe_generate_internal_event_code(Goal, NotGoalInfo, EnterTraceCode, !CI),
    code_gen.generate_goal(model_semi, Goal, GoalCode, !CI),

    code_info.ite_enter_then(HijackInfo, ThenNeckCode, ElseNeckCode, !CI),

    % Kill again any variables that have become zombies.
    code_info.pickup_zombies(Zombies, !CI),
    code_info.make_vars_forward_dead(Zombies, !CI),

    code_info.get_forward_live_vars(!.CI, LiveVars),

    ( CodeModel = model_det ->
        % The then branch will never be reached.
        PruneTicketCode = empty,
        FailTraceCode = empty,
        FailCode = empty
    ;
        code_info.remember_position(!.CI, AfterNegatedGoal),
        % The call to reset_ticket(..., commit) here is necessary
        % in order to properly detect floundering.
        code_info.maybe_release_hp(MaybeHpSlot, !CI),
        code_info.maybe_reset_prune_and_release_ticket(MaybeTicketSlot,
            reset_reason_commit, PruneTicketCode, !CI),
        maybe_generate_negated_event_code(Goal, NotGoalInfo, neg_failure,
            FailTraceCode, !CI),
        code_info.generate_failure(FailCode, !CI),
        % We want liveness after not(G) to be the same as after G.
        % Information about what variables are where will be set
        % by code_info.generate_resume_point.
        code_info.reset_to_position(AfterNegatedGoal, !CI)
    ),

    % Generate the entry to the else branch.
    code_info.generate_resume_point(ResumePoint, ResumeCode, !CI),

    code_info.set_forward_live_vars(LiveVars, !CI),

    % Restore the heap pointer and solver state if necessary.
    code_info.maybe_restore_and_release_hp(MaybeHpSlot, RestoreHpCode, !CI),
    code_info.maybe_reset_discard_and_release_ticket(MaybeTicketSlot,
        reset_reason_undo, RestoreTicketCode, !CI),
    maybe_generate_negated_event_code(Goal, NotGoalInfo, neg_success,
        SuccessTraceCode, !CI),

    make_pneg_context_wrappers(Globals, NotGoalInfo, PNegCondCode,
        PNegThenCode, PNegElseCode),
    Code = tree_list([
        FlushCode,
        PrepareHijackCode,
        EffectResumeCode,
        SaveHpCode,
        SaveTicketCode,
        EnterTraceCode,
        PNegCondCode,
        GoalCode,
        ThenNeckCode,
        PruneTicketCode,
        FailTraceCode,
        PNegThenCode,
        FailCode,
        ResumeCode,
        ElseNeckCode,
        RestoreTicketCode,
        RestoreHpCode,
        SuccessTraceCode,
        PNegElseCode]).

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
    code_tree::out, code_tree::out, code_tree::out) is det.

make_pneg_context_wrappers(Globals, GoalInfo, PNegCondCode, PNegThenCode,
        PNegElseCode) :-
    globals.lookup_bool_option(Globals, use_minimal_model_stack_copy_pneg,
        UseMinimalModelStackCopyPNeg),
    (
        UseMinimalModelStackCopyPNeg = yes,
        not goal_info_has_feature(GoalInfo, feature_will_not_call_mm_tabled)
    ->      
        goal_info_get_context(GoalInfo, Context),
        term.context_file(Context, File),
        term.context_line(Context, Line),
        (
            File \= "",
            Line > 0
        ->
            CtxtStr = "\"" ++ File ++ ":" ++ int_to_string(Line) ++ "\""
        ;
            CtxtStr = "NULL"
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
        PNegCondCode = node([
            llds_instr(foreign_proc_code([], PNegCondComponents,
                proc_will_not_call_mercury, no, no, no, no, yes, MD), "")
        ]),
        PNegThenCode = node([
            llds_instr(foreign_proc_code([], PNegThenComponents,
                proc_will_not_call_mercury, no, no, no, no, yes, MD), "")
        ]),
        PNegElseCode = node([
            llds_instr(foreign_proc_code([], PNegElseComponents,
                proc_will_not_call_mercury, no, no, no, no, yes, MD), "")
        ])
    ;
        PNegCondCode = empty,
        PNegThenCode = empty,
        PNegElseCode = empty
    ).

:- func wrap_transient(string) = string.

wrap_transient(Code) =
    string.append_list([
        "\t\tMR_save_transient_registers();\n",
        Code,
        "\t\tMR_restore_transient_registers();\n"]).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "ite_gen".

%-----------------------------------------------------------------------------%
:- end_module ite_gen.
%-----------------------------------------------------------------------------%

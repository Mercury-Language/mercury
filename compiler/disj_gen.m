%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2000,2002-2006 The University of Melbourne.
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

:- pred generate_disj(add_trail_ops::in, code_model::in, list(hlds_goal)::in,
    hlds_goal_info::in, code_tree::out, code_info::in, code_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_form.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_llds.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.tree.
:- import_module libs.tree.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.trace_gen.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module term.

%-----------------------------------------------------------------------------%

generate_disj(AddTrailOps, CodeModel, Goals, DisjGoalInfo, Code, !CI) :-
    (
        Goals = [],
        ( CodeModel = model_semi ->
            code_info.generate_failure(Code, !CI)
        ;
            unexpected(this_file, "generate_disj: empty disjunction.")
        )
    ;
        Goals = [Goal | _],
        Goal = _ - GoalInfo,
        goal_info_get_resume_point(GoalInfo, Resume),
        ( Resume = resume_point(ResumeVarsPrime, _) ->
            ResumeVars = ResumeVarsPrime
        ;
            set.init(ResumeVars)
        ),
        generate_real_disj(AddTrailOps, CodeModel, ResumeVars, Goals,
            DisjGoalInfo, Code, !CI)
    ).

%---------------------------------------------------------------------------%

:- pred disj_gen.generate_real_disj(bool::in, code_model::in,
    set(prog_var)::in, list(hlds_goal)::in, hlds_goal_info::in,
    code_tree::out, code_info::in, code_info::out) is det.

generate_real_disj(AddTrailOps, CodeModel, ResumeVars, Goals, DisjGoalInfo,
        Code, !CI)  :-
    % Make sure that the variables whose values will be needed on backtracking
    % to any disjunct are materialized into registers or stack slots. Their
    % locations are recorded in ResumeMap.
    code_info.produce_vars(ResumeVars, ResumeMap, FlushCode, !CI),

    % If we are using a trail, save the current trail state before the
    % first disjunct.
    % XXX We should use a scheme such as the one we use for heap recovery
    % for semi and det disjunctions, and delay saving the ticket until
    % necessary.
    code_info.get_globals(!.CI, Globals),
    code_info.maybe_save_ticket(AddTrailOps, SaveTicketCode, MaybeTicketSlot,
        !CI),

    % If we are using a grade in which we can recover memory by saving
    % and restoring the heap pointer, set up for doing so if necessary.
    ( CodeModel = model_non ->
        % With nondet disjunctions, we must recover memory across all
        % disjuncts, even disjuncts that cannot themselves allocate memory,
        % since we can backtrack to disjunct N after control leaves
        % disjunct N-1.
        globals.lookup_bool_option(Globals, reclaim_heap_on_nondet_failure,
            ReclaimHeap),
        code_info.maybe_save_hp(ReclaimHeap, SaveHpCode, MaybeHpSlot, !CI)
    ;
        % With other disjunctions, we can backtrack to disjunct N only from
        % disjunct N-1, so if disjunct N-1 does not allocate memory, we need
        % not recover memory across it. Since it is possible (and common)
        % for no disjunct to allocate memory, we delay saving the heap pointer
        % and allocating a stack slot for the saved hp as long as possible.
        globals.lookup_bool_option(Globals, reclaim_heap_on_semidet_failure,
            ReclaimHeap),
        SaveHpCode = empty,
        MaybeHpSlot = no
    ),

    % Save the values of any stack slots we may hijack, and if necessary,
    % set the redofr slot of the top frame to point to this frame.
    code_info.prepare_for_disj_hijack(CodeModel, HijackInfo,
        PrepareHijackCode, !CI),

    code_info.get_next_label(EndLabel, !CI),

    code_info.remember_position(!.CI, BranchStart),
    generate_disjuncts(Goals, CodeModel, ResumeMap, no, HijackInfo,
        DisjGoalInfo, EndLabel, ReclaimHeap, MaybeHpSlot, MaybeTicketSlot,
        BranchStart, no, MaybeEnd, GoalsCode, !CI),

    goal_info_get_store_map(DisjGoalInfo, StoreMap),
    code_info.after_all_branches(StoreMap, MaybeEnd, !CI),
    ( CodeModel = model_non ->
        code_info.set_resume_point_to_unknown(!CI)
    ;
        true
    ),
    Code = tree_list([FlushCode, SaveTicketCode, SaveHpCode, PrepareHijackCode,
         GoalsCode]).

%---------------------------------------------------------------------------%

:- pred generate_disjuncts(list(hlds_goal)::in, code_model::in,
    resume_map::in, maybe(resume_point_info)::in, disj_hijack_info::in,
    hlds_goal_info::in, label::in, bool::in, maybe(lval)::in,
    maybe(lval)::in, position_info::in, maybe(branch_end_info)::in,
    maybe(branch_end_info)::out, code_tree::out, code_info::in,
    code_info::out) is det.

generate_disjuncts([], _, _, _, _, _, _, _, _, _, _, _, _, _, !CI) :-
    unexpected(this_file, "generate_disjuncts: empty disjunction!").
generate_disjuncts([Goal0 | Goals], CodeModel, FullResumeMap,
        MaybeEntryResumePoint, HijackInfo, DisjGoalInfo, EndLabel, ReclaimHeap,
        MaybeHpSlot0, MaybeTicketSlot, BranchStart0, MaybeEnd0, MaybeEnd,
        Code, !CI) :-

    code_info.reset_to_position(BranchStart0, !CI),

    % If this is not the first disjunct, generate the resume point by which
    % we arrive at this disjunct.
    (
        MaybeEntryResumePoint = yes(EntryResumePoint),
        code_info.generate_resume_point(EntryResumePoint,
            EntryResumePointCode, !CI)
    ;
        MaybeEntryResumePoint = no,
        EntryResumePointCode = empty
    ),

    Goal0 = GoalExpr0 - GoalInfo0,
    goal_info_get_resume_point(GoalInfo0, Resume),
    ( Resume = resume_point(ResumeVars, ResumeLocs) ->
        % Emit code for a non-last disjunct, including setting things
        % up for the execution of the next disjunct.

        (
            MaybeEntryResumePoint = yes(_),
            % Reset the heap pointer to recover memory allocated by the
            % previous disjunct(s), if necessary.
            code_info.maybe_restore_hp(MaybeHpSlot0, RestoreHpCode),

            % Reset the solver state if necessary.
            code_info.maybe_reset_ticket(MaybeTicketSlot,
                reset_reason_undo, RestoreTicketCode)
        ;
            MaybeEntryResumePoint = no,
            RestoreHpCode = empty,
            RestoreTicketCode = empty
        ),

        % The pre_goal_update sanity check insists on no_resume_point, to make
        % sure that all resume points have been handled by surrounding code.
        goal_info_set_resume_point(no_resume_point, GoalInfo0, GoalInfo),
        Goal = GoalExpr0 - GoalInfo,

        % Save hp if it needs to be saved and hasn't been saved previously.
        (
            ReclaimHeap = yes,
            goal_may_allocate_heap(Goal),
            MaybeHpSlot0 = no
        ->
            code_info.save_hp(SaveHpCode, HpSlot, !CI),
            MaybeHpSlot = yes(HpSlot),

            % This method of updating BranchStart0 is ugly. The best
            % alternative would be to arrange things so that a
            % remember_position here could get BranchStart, but doing so is
            % iffy because we have already created the resumption point for
            % entry into this disjunction, which overwrites part of the
            % location-dependent state originally in BranchStart0.
            %
            code_info.save_hp_in_branch(BranchSaveHpCode, BranchHpSlot,
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

        code_info.make_resume_point(ResumeVars, ResumeLocs, FullResumeMap,
            NextResumePoint, !CI),
        code_info.effect_resume_point(NextResumePoint, CodeModel, ModContCode,
            !CI),

        maybe_generate_internal_event_code(Goal, DisjGoalInfo, TraceCode, !CI),
        goal_info_get_code_model(GoalInfo, GoalCodeModel),
        code_gen.generate_goal(GoalCodeModel, Goal, GoalCode, !CI),

        ( CodeModel = model_non ->
            % We can backtrack to the next disjunct from outside, so we make
            % sure every variable in the resume set is in its stack slot.
            code_info.flush_resume_vars_to_stack(ResumeVarsCode, !CI),

            % We hang onto any temporary slots holding saved heap pointers
            % and/or tickets, thus ensuring that they will still be reserved
            % after the disjunction.
            PruneTicketCode = empty
        ;
            ResumeVarsCode = empty,

            code_info.maybe_release_hp(MaybeHpSlot, !CI),
            % We're committing to this disjunct if it succeeds.
            code_info.maybe_reset_prune_and_release_ticket(MaybeTicketSlot,
                reset_reason_commit, PruneTicketCode, !CI),

            code_info.reset_resume_known(BranchStart, !CI)
        ),

        % Forget the variables that are needed only at the resumption point at
        % the start of the next disjunct, so that we don't generate exceptions
        % when their storage is clobbered by the movement of the live
        % variables to the places indicated in the store map.
        code_info.pop_resume_point(!CI),
        code_info.pickup_zombies(Zombies, !CI),
        code_info.make_vars_forward_dead(Zombies, !CI),

        % Put every variable whose value is needed after the disjunction to
        % the place indicated by StoreMap, and accumulate information about
        % the code_info state at the ends of the branches so far.
        goal_info_get_store_map(DisjGoalInfo, StoreMap),
        code_info.generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd1,
            SaveCode, !CI),

        BranchCode = node([
            goto(code_label(EndLabel)) - "skip to end of nondet disj"
        ]),

        disj_gen.generate_disjuncts(Goals, CodeModel, FullResumeMap,
            yes(NextResumePoint), HijackInfo, DisjGoalInfo,
            EndLabel, ReclaimHeap, MaybeHpSlot, MaybeTicketSlot, BranchStart,
            MaybeEnd1, MaybeEnd, RestCode, !CI),

        Code = tree_list([EntryResumePointCode, RestoreHpCode,
            RestoreTicketCode, SaveHpCode, ModContCode, TraceCode,
            GoalCode, ResumeVarsCode, PruneTicketCode, SaveCode,
            BranchCode, RestCode])
    ;
        % Emit code for the last disjunct.

        % Restore the heap pointer and solver state if necessary.
        code_info.maybe_restore_and_release_hp(MaybeHpSlot0, RestoreHpCode,
            !CI),
        code_info.maybe_reset_discard_and_release_ticket(MaybeTicketSlot,
            reset_reason_undo, RestoreTicketCode, !CI),

        code_info.undo_disj_hijack(HijackInfo, UndoCode, !CI),

        maybe_generate_internal_event_code(Goal0, DisjGoalInfo, TraceCode,
            !CI),
        code_gen.generate_goal(CodeModel, Goal0, GoalCode, !CI),
        goal_info_get_store_map(DisjGoalInfo, StoreMap),
        code_info.generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd, SaveCode,
            !CI),

        EndCode = node([
            label(EndLabel) - "End of nondet disj"
        ]),
        Code = tree_list([EntryResumePointCode, TraceCode, RestoreHpCode,
            RestoreTicketCode, UndoCode, GoalCode, SaveCode, EndCode])
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "disj_gen.m".

%-----------------------------------------------------------------------------%
:- end_module disj_gen.
%-----------------------------------------------------------------------------%

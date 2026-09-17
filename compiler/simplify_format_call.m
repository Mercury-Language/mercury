%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: simplify_format_call.m.
%
% This module manages the simplification's pass interaction with
% opt_format_call.m. Specifically, its main job is that if opt_format_call.m
% cannot do its assigned job due to some format string not being available
% in one piece. it tries again *after* transforming the body of the procedure
% in an attempt to make it available in one piece.
%
%---------------------------------------------------------------------------%

:- module check_hlds.simplify.simplify_format_call.
:- interface.

:- import_module check_hlds.simplify.opt_format_call.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_proc.
:- import_module hlds.pred_proc_id.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred simplify_proc_analyze_and_format_calls(io.text_output_stream::in,
    maybe_generate_implicit_stream_warnings::in,
    module_info::in, module_info::out, pred_id::in, pred_info::in,
    proc_id::in, proc_info::in, proc_info::out, list(diag_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.recompute_instmap_deltas.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.options.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module cord.
:- import_module maybe.
:- import_module require.

%---------------------------------------------------------------------------%

simplify_proc_analyze_and_format_calls(ProgressStream, ImplicitStreamWarnings,
        !ModuleInfo, PredId, PredInfo0, ProcId, !ProcInfo, FormatSpecs) :-
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_var_table(!.ProcInfo, VarTable0),
    analyze_and_optimize_format_calls(ProgressStream,
        ImplicitStreamWarnings, !.ModuleInfo, PredInfo0, !.ProcInfo,
        Goal0, MaybeGoal1, FormatSpecs1, VarTable0, VarTable1),
    ( if
        had_some_unknown_format_calls(FormatSpecs1) = yes,
        % We found some format calls that we couldn't optimize because
        % we don't know either the format string or the list of values
        % to be printed. Try to fix this by moving copies of the format call
        % into the tail ends of the immediately previous branched control
        % structure, since this will fix the problem *if* the missing info
        % is available in each branch. If it is not, then the transformation
        % does not help, but it does not hurt either. (Even if we get one copy
        % per branch of e.g. a warning about the format string being unknown,
        % write_error_spec.m will print only one copy.)
        %
        % Note that we do *not* test the value of warn_unknown_format_calls.
        % Even if the warning is not enabled, knowing the format string
        % allows us to generate better code.
        push_format_calls_into_branches_in_goal(!.ModuleInfo, Goal0, Goal1),
        % Don't call analyze_and_optimize_format_calls again with the same
        % input goal; the results won't change.
        not Goal0 = Goal1
    then
        % Repeat the call to analyze_and_optimize_format_calls on the
        % transformed procedure body, once we have fixed up the goal_infos.
        % The code here does most of the same things as the
        % "MaybeGoal = yes(Goal)" case below, though the reasons for
        % e.g. the nonlocals fields needing recomputation are different.
        proc_info_set_goal(Goal1, !ProcInfo),
        proc_info_set_var_table(VarTable1, !ProcInfo),
        requantify_proc_general(ord_nl_maybe_lambda, !ProcInfo),
        recompute_instmap_delta_proc(no_recomp_atomics,
            !ProcInfo, !ModuleInfo),
        pred_info_set_proc_info(ProcId, !.ProcInfo, PredInfo0, PredInfo1),
        analyze_and_optimize_format_calls(ProgressStream,
            ImplicitStreamWarnings, !.ModuleInfo, PredInfo1, !.ProcInfo,
            Goal1, MaybeGoal, FormatSpecs, VarTable0, VarTable)
    else
        MaybeGoal = MaybeGoal1,
        FormatSpecs = FormatSpecs1,
        VarTable = VarTable1,
        PredInfo1 = PredInfo0
    ),
    (
        MaybeGoal = yes(Goal),
        proc_info_set_goal(Goal, !ProcInfo),
        proc_info_set_var_table(VarTable, !ProcInfo),

        % The goals we replace format calls with are created with the
        % correct nonlocals, but analyze_and_optimize_format_calls can
        % take code for building a list of string.poly_types out of one
        % scope (e.g. the condition of an if-then-else) and replace it
        % with code to build the string directly in another scope
        % (such as the then part of that if-then-else, if that is where
        % the format call is). This can leave variables missing from
        % the nonlocal fields of the original scopes. And since
        % instmap_deltas are restricted to the goal's nonlocals,
        % they need to be recomputed as well.
        requantify_proc_general(ord_nl_maybe_lambda, !ProcInfo),
        recompute_instmap_delta_proc(no_recomp_atomics,
            !ProcInfo, !ModuleInfo),

        % Put the new proc_info back into !ModuleInfo, since some of the
        % following code could otherwise find obsolete information in there.
        pred_info_set_proc_info(ProcId, !.ProcInfo, PredInfo1, PredInfo2),

        % Remove the has_format_call marker from the pred_info before
        % putting it back, since any optimizable format calls will already
        % have been optimized. Since currently there is no program
        % transformation that inserts calls to these predicates,
        % there is no point in trying to optimize format_calls again later.
        pred_info_get_markers(PredInfo2, Markers2),
        remove_marker(marker_has_format_call, Markers2, Markers),
        pred_info_set_markers(Markers, PredInfo2, PredInfo),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    ;
        MaybeGoal = no
        % There should not be any updates to the var_table,
        % but even if there are, throw them away, since they apply to a version
        % of the goal that we will not be using.
    ).

:- func had_some_unknown_format_calls(list(diag_spec)) = bool.

had_some_unknown_format_calls([]) = no.
had_some_unknown_format_calls([Spec | Specs]) = SomeUnknown :-
    extract_spec_severity(Spec, Severity),
    ( if Severity = severity_warning(warn_unknown_format_calls) then
        SomeUnknown = yes
    else
        SomeUnknown = had_some_unknown_format_calls(Specs)
    ).

:- pred push_format_calls_into_branches_in_goal(module_info::in,
    hlds_goal::in, hlds_goal::out) is det.

push_format_calls_into_branches_in_goal(ModuleInfo, Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = conj(ConjType0, Conjuncts0),
        list.map(push_format_calls_into_branches_in_goal(ModuleInfo),
            Conjuncts0, Conjuncts1),
        (
            ConjType0 = plain_conj,
            % It is simpler to have the list.map above process
            % each conjunct, before this call does the pushing,
            % Separation-of-concerns works.
            push_format_calls_into_branches_in_conjunction(ModuleInfo,
                Conjuncts1, Conjuncts)
        ;
            ConjType0 = parallel_conj,
            Conjuncts = Conjuncts1
        ),
        GoalExpr = conj(ConjType0, Conjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Disjuncts0),
        list.map(push_format_calls_into_branches_in_goal(ModuleInfo),
            Disjuncts0, Disjuncts),
        GoalExpr = disj(Disjuncts),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var0, CanFail0, Cases0),
        list.map(push_format_calls_into_branches_in_case(ModuleInfo),
            Cases0, Cases),
        GoalExpr = switch(Var0, CanFail0, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars0, Cond0, Then0, Else0),
        push_format_calls_into_branches_in_goal(ModuleInfo, Cond0, Cond),
        push_format_calls_into_branches_in_goal(ModuleInfo, Then0, Then),
        push_format_calls_into_branches_in_goal(ModuleInfo, Else0, Else),
        GoalExpr = if_then_else(Vars0, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(SubGoal0),
        push_format_calls_into_branches_in_goal(ModuleInfo, SubGoal0, SubGoal),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason0, SubGoal0),
        (
            Reason0 = from_ground_term(_, _),
            Goal = Goal0
        ;
            ( Reason0 = exist_quant(_, _)
            ; Reason0 = disable_warnings(_, _)
            ; Reason0 = promise_solutions(_, _)
            ; Reason0 = promise_purity(_)
            ; Reason0 = require_detism(_)
            ; Reason0 = commit(_)
            ; Reason0 = barrier(_)
            ; Reason0 = trace_goal(_, _, _, _, _)
            ; Reason0 = loop_control(_, _, _)
            ; Reason0 = require_complete_switch(_)
            ; Reason0 = require_switch_arms_detism(_, _)
            ),
            push_format_calls_into_branches_in_goal(ModuleInfo,
                SubGoal0, SubGoal),
            GoalExpr = scope(Reason0, SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType0, Outer0, Inner0,
                MaybeOutputVars0, MainGoal0, OrElseGoals0, OrElseInners0),
            push_format_calls_into_branches_in_goal(ModuleInfo,
                MainGoal0, MainGoal),
            list.map(push_format_calls_into_branches_in_goal(ModuleInfo),
                OrElseGoals0, OrElseGoals),
            ShortHand = atomic_goal(GoalType0, Outer0, Inner0,
                MaybeOutputVars0, MainGoal, OrElseGoals, OrElseInners0)
        ;
            ShortHand0 = try_goal(MaybeIO0, ResultVar0, SubGoal0),
            push_format_calls_into_branches_in_goal(ModuleInfo,
                SubGoal0, SubGoal),
            ShortHand = try_goal(MaybeIO0, ResultVar0, SubGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        ),
        GoalExpr = shorthand(ShortHand),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ).

:- pred push_format_calls_into_branches_in_case(module_info::in,
    case::in, case::out) is det.

push_format_calls_into_branches_in_case(ModuleInfo, Case0, Case) :-
    Case0 = case(MainConsId0, OtherConsIds0, Goal0),
    push_format_calls_into_branches_in_goal(ModuleInfo, Goal0, Goal),
    Case = case(MainConsId0, OtherConsIds0, Goal).

%---------------------%

    % This predicate pushes format calls into the tail ends
    % of the last branched goal that preceded it in a conjunction.
    %
    % The algorithm has two stages.
    %
    % The first stage partitions the conjuncts into segments, where
    %
    % - each segment consists of a contiguous sequence of the conjuncts
    %   of the original conjunctions,
    %
    % - concatenating the contents of the segments together would yield back
    %   the original conjunction, and
    %
    % - conjuncts that are either branched goals or format calls can occur
    %   only as the distinguished last conjunct in a segment.
    %
    % The result is a sequence of segments that each either in a branched goal
    % or in a format call, with the last segment ending with the end of
    % the original conjunction.
    %
    % The second stage then looks for situations where a segment that ends with
    % a branched goal is followed by a segment that ends with a format call.
    % When it finds one, it copies the latter segment into each branch
    % of the branched goal in the former segment, calls the result an updated
    % segment that ends with a branched goal, and then keeps looking for
    % more such situations.
    %
:- pred push_format_calls_into_branches_in_conjunction(module_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

push_format_calls_into_branches_in_conjunction(ModuleInfo,
        Conjuncts0, Conjuncts) :-
    segment_conjunction(ModuleInfo, Conjuncts0, cord.init, SegmentsCord,
        cord.init, LeftOverCord),
    Segments = cord.list(SegmentsCord),
    (
        Segments = [],
        Conjuncts = Conjuncts0
    ;
        Segments = [HeadSegment | TailSegments],
        push_format_segments_into_branched_goals(cord.init,
            HeadSegment, TailSegments, SegmentGoalsCord),
        Conjuncts = cord.list(SegmentGoalsCord ++ LeftOverCord)
    ).

%---------------------%

    % The data structure that the first stage computes and
    % the second stage processes. Its semantics are explained by
    % the big comment on push_format_calls_into_branches_in_conjunction.

:- type conjunction_segment
    --->    segment_branched(segment_ends_with_branched)
    ;       segment_format(segment_ends_with_format).

:- type segment_ends_with_branched
    --->    segment_ends_with_branched(cord(hlds_goal), branched_goal).
:- type segment_ends_with_format
    --->    segment_ends_with_format(cord(hlds_goal), hlds_goal).

:- type branched_goal_expr =< hlds_goal_expr
    --->    disj(list(hlds_goal))
    ;       switch(prog_var, can_fail, list(case))
    ;       if_then_else(list(prog_var), hlds_goal, hlds_goal, hlds_goal).
:- type branched_goal =< hlds_goal
    --->    hlds_goal(branched_goal_expr, hlds_goal_info).

%---------------------%

    % Partition the given list of conjuncts into a cord of segments,
    % followed by a cord of leftover goals.
    %
:- pred segment_conjunction(module_info::in, list(hlds_goal)::in,
    cord(conjunction_segment)::in, cord(conjunction_segment)::out,
    cord(hlds_goal)::in, cord(hlds_goal)::out) is det.

segment_conjunction(_, [], !SegmentsCord,
        !.AfterLastSegmentCord, LeftOverCord) :-
    LeftOverCord = !.AfterLastSegmentCord.
segment_conjunction(ModuleInfo, [HeadConjunct | TailConjuncts], !SegmentsCord,
        !.AfterLastSegmentCord, LeftOverCord) :-
    HeadConjunct = hlds_goal(GoalExpr, GoalInfo),
    (
        ( GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr = negation(_)
        ; GoalExpr = scope(_, _)
        ; GoalExpr = shorthand(_)
        ),
        cord.snoc(HeadConjunct, !AfterLastSegmentCord),
        NextConjuncts = TailConjuncts
    ;
        GoalExpr = plain_call(CalleePredId, _, ArgVars, _, _, _),
        module_info_pred_info(ModuleInfo, CalleePredId, CalleePredInfo),
        ( if is_format_call(CalleePredInfo, ArgVars) then
            SegmentFormat = segment_ends_with_format(!.AfterLastSegmentCord,
                HeadConjunct),
            cord.snoc(segment_format(SegmentFormat), !SegmentsCord),
            !:AfterLastSegmentCord = cord.init
        else
            cord.snoc(HeadConjunct, !AfterLastSegmentCord)
        ),
        NextConjuncts = TailConjuncts
    ;
        GoalExpr = conj(ConjType, SubConjuncts),
        (
            ConjType = plain_conj,
            NextConjuncts = SubConjuncts ++ TailConjuncts
        ;
            ConjType = parallel_conj,
            cord.snoc(HeadConjunct, !AfterLastSegmentCord),
            NextConjuncts = TailConjuncts
        )
    ;
        ( GoalExpr = disj(_)
        ; GoalExpr = switch(_, _, _)
        ; GoalExpr = if_then_else(_, _, _, _)
        ),
        SegmentBranched = segment_ends_with_branched(!.AfterLastSegmentCord,
            coerce(hlds_goal(GoalExpr, GoalInfo))),
        cord.snoc(segment_branched(SegmentBranched), !SegmentsCord),
        !:AfterLastSegmentCord = cord.init,
        NextConjuncts = TailConjuncts
    ),
    segment_conjunction(ModuleInfo, NextConjuncts, !SegmentsCord,
        !.AfterLastSegmentCord, LeftOverCord).

    % Look for a segment ending in a branched goal followed immediately
    % by a segment ending in a format call, and then push the latter segment
    % into each branch of the branched goal ending the former segment.
    % Keep doing this until there are no such segment pairs are left.
    %
:- pred push_format_segments_into_branched_goals(cord(hlds_goal)::in,
    conjunction_segment::in, list(conjunction_segment)::in,
    cord(hlds_goal)::out) is det.

push_format_segments_into_branched_goals(!.DoneCord, HeadSegment, TailSegments,
        AllCord) :-
    (
        HeadSegment = segment_format(SegmentFormat),
        SegmentFormat = segment_ends_with_format(FormatStartCord, FormatGoal),
        !:DoneCord = !.DoneCord ++ FormatStartCord,
        cord.snoc(FormatGoal, !DoneCord),
        (
            TailSegments = [],
            AllCord = !.DoneCord
        ;
            TailSegments = [HeadTailSegment | TailTailSegments],
            push_format_segments_into_branched_goals(!.DoneCord,
                HeadTailSegment, TailTailSegments, AllCord)
        )
    ;
        HeadSegment = segment_branched(SegmentBranched0),
        SegmentBranched0 =
            segment_ends_with_branched(BranchedStartCord, BranchedGoal0),
        (
            TailSegments = [],
            !:DoneCord = !.DoneCord ++ BranchedStartCord,
            cord.snoc(coerce(BranchedGoal0), !DoneCord),
            AllCord = !.DoneCord
        ;
            TailSegments = [HeadTailSegment | TailTailSegments],
            (
                HeadTailSegment = segment_branched(_),
                % HeadSegment and HeadTailSegment both end in branched goals.
                % We only ever push format calls segments into the last
                % branched goal, so we now consider HeadSegment to be all done.
                !:DoneCord = !.DoneCord ++ BranchedStartCord,
                cord.snoc(coerce(BranchedGoal0), !DoneCord),
                push_format_segments_into_branched_goals(!.DoneCord,
                    HeadTailSegment, TailTailSegments, AllCord)
            ;
                HeadTailSegment = segment_format(SegmentFormat),
                SegmentFormat =
                    segment_ends_with_format(FormatStartCord, FormatGoal),
                % XXX Consider doing the following merge only if
                % the nonlocals sets of BranchedGoal0 and FormatGoal overlap.
                cord.snoc(FormatGoal, FormatStartCord, GoalsToAppendCord),
                GoalsToAppend = cord.list(GoalsToAppendCord),
                BranchedGoal0 = hlds_goal(BranchedGoalExpr0, BranchedGoalInfo),
                (
                    BranchedGoalExpr0 = disj(Disjuncts0),
                    list.map(append_goals_to_goal(GoalsToAppend),
                        Disjuncts0, Disjuncts),
                    BranchedGoalExpr = disj(Disjuncts)
                ;
                    BranchedGoalExpr0 = switch(Var, CanFail, Cases0),
                    list.map(append_goals_to_case(GoalsToAppend),
                        Cases0, Cases),
                    BranchedGoalExpr = switch(Var, CanFail, Cases)
                ;
                    BranchedGoalExpr0 =
                        if_then_else(Vars0, Cond0, Then0, Else0),
                    append_goals_to_goal(GoalsToAppend, Then0, Then),
                    append_goals_to_goal(GoalsToAppend, Else0, Else),
                    BranchedGoalExpr =
                        if_then_else(Vars0, Cond0, Then, Else)
                ),
                BranchedGoal = hlds_goal(BranchedGoalExpr, BranchedGoalInfo),
                SegmentBranched = segment_ends_with_branched(BranchedStartCord,
                    BranchedGoal),
                UpdatedHeadSegment = segment_branched(SegmentBranched),
                push_format_segments_into_branched_goals(!.DoneCord,
                    UpdatedHeadSegment, TailTailSegments, AllCord)
            )
        )
    ).

:- pred append_goals_to_goal(list(hlds_goal)::in,
    hlds_goal::in, hlds_goal::out) is det.

append_goals_to_goal(GoalsToAppend, Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    ( if GoalExpr0 = conj(plain_conj, Conjuncts0) then
        GoalExpr = conj(plain_conj, Conjuncts0 ++ GoalsToAppend)
    else
        GoalExpr = conj(plain_conj, [Goal0 | GoalsToAppend])
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred append_goals_to_case(list(hlds_goal)::in, case::in, case::out) is det.

append_goals_to_case(GoalsToAppend, Case0, Case) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    append_goals_to_goal(GoalsToAppend, Goal0, Goal),
    Case = case(MainConsId, OtherConsIds, Goal).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_format_call.
%---------------------------------------------------------------------------%

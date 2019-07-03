%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: autopar_search_goals.m
% Authors: pbone, zs.
%
% This module contains the code for searching a goal for conjunctions worth
% parallelising.
%
%---------------------------------------------------------------------------%

:- module mdprof_fb.automatic_parallelism.autopar_search_goals.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.feedback.
:- import_module mdbcomp.feedback.automatic_parallelism.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.
:- import_module mdprof_fb.automatic_parallelism.autopar_types.
:- import_module message.

:- import_module cord.

%---------------------------------------------------------------------------%

:- pred goal_get_conjunctions_worth_parallelising(
    implicit_parallelism_info::in, reverse_goal_path::in,
    pard_goal_detail::in, pard_goal_detail::out,
    cord(candidate_par_conjunction(pard_goal_detail))::out,
    cord(push_goal)::out, cord(pard_goal_detail)::out, cord(message)::out)
    is det.

    % Transform a goal in a conjunction into a pard_goal.
    %
:- pred goal_to_pard_goal(implicit_parallelism_info::in,
    reverse_goal_path::in, goal_rep(goal_id)::in,
    pard_goal_detail::out, cord(message)::in, cord(message)::out) is det.

    % Check if it is appropriate to parallelise this call. The call must be
    % model_det, and must have a cost above the call site cost threshold.
    % XXX probable bug: the cost criterion is implemented elsewhere.
    %
:- pred can_parallelise_goal(goal_rep(T)::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis_utils.
:- import_module coverage.
:- import_module mdprof_fb.automatic_parallelism.autopar_costs.
:- import_module mdprof_fb.automatic_parallelism.autopar_find_best_par.
:- import_module mdprof_fb.automatic_parallelism.autopar_reports.
:- import_module measurements.
:- import_module program_representation_utils.
:- import_module report.
:- import_module var_use_analysis.

:- import_module assoc_list.
:- import_module float.
:- import_module int.
:- import_module io.
:- import_module lazy.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

goal_get_conjunctions_worth_parallelising(Info, RevGoalPath,
        !Goal, Candidates, Pushes, Singles, Messages) :-
    !.Goal = goal_rep(GoalExpr0, DetismRep, Annotation0),
    Coverage = Annotation0 ^ pgd_coverage,
    get_coverage_before_det(Coverage, Calls),
    (
        (
            GoalExpr0 = conj_rep(Conjs0),
            conj_get_conjunctions_worth_parallelising(Info, RevGoalPath,
                Conjs0, Conjs, 1, [], SinglesSoFar, [], RevSingleCands,
                cord.empty, CandidatesBelow, cord.empty, PushesBelow,
                cord.empty, MessagesBelow),
            GoalExpr = conj_rep(Conjs),
            list.reverse(RevSingleCands, SingleCands),
            (
                SingleCands = [],
                Candidates = CandidatesBelow,
                Pushes = PushesBelow,
                Singles = cord.from_list(SinglesSoFar),
                Messages = MessagesBelow,
                Cost = Annotation0 ^ pgd_cost
            ;
                SingleCands = [CostlyIndex - SinglesBefore],
                push_and_build_candidate_conjunctions(Info, RevGoalPath,
                    Conjs, CostlyIndex, SinglesBefore,
                    MessagesThisLevel, CandidatesThisLevel),
                (
                    CandidatesThisLevel = [],
                    Candidates = CandidatesBelow,
                    Pushes = PushesBelow,
                    % No candidate was built, pass our singles to our caller.
                    Singles = cord.from_list(SinglesSoFar)
                ;
                    CandidatesThisLevel = [FirstCandidate | LaterCandidates],
                    merge_same_level_pushes(FirstCandidate, LaterCandidates,
                        PushThisLevel),
                    Candidates = CandidatesBelow ++
                        cord.from_list(CandidatesThisLevel),
                    Pushes = cord.snoc(PushesBelow, PushThisLevel),
                    % Any single expensive goals inside this conjunction
                    % cannot have later expensive goals pushed next to them
                    % without reanalysis of the whole goal, which we do not do.
                    Singles = cord.empty
                ),
                Messages = MessagesBelow ++ MessagesThisLevel,
                % XXX We should update the cost for CandidatesThisLevel.
                Cost = Annotation0 ^ pgd_cost
            ;
                SingleCands = [_, _ | _],
                assoc_list.keys(SingleCands, CostlyIndexes),
                build_candidate_conjunction(Info, RevGoalPath,
                    Conjs, CostlyIndexes, MessagesThisLevel, MaybeCandidate),
                Pushes = PushesBelow,
                Messages = MessagesBelow ++ MessagesThisLevel,
                (
                    MaybeCandidate = yes(Candidate),
                    Candidates = cord.cons(Candidate, CandidatesBelow),
                    ExecMetrics = Candidate ^ cpc_par_exec_metrics,
                    Cost = call_goal_cost(ExecMetrics ^ pem_num_calls,
                        ExecMetrics ^ pem_par_time),
                    % We parallelized this conjunction. Trying to push a goal
                    % after it next to a costly goal inside it would require
                    % pushing that following goal into a conjunct of this
                    % parallel conjunction. Due to our current prohibition
                    % on reordering, that costly goal would have to be inside
                    % the last parallel conjunct. That would require replacing
                    % Candidate with another candidate that includes the
                    % following goal. While that is in theory doable, it is not
                    % doable *here*, since at this point yet know whether
                    % a following costly goal even exists. On the other hand,
                    % any later part of this algorithm that does discover
                    % a later costly goal won't know how to redo this overlap
                    % calculation. We avoid the problem by pretending that this
                    % conjunction contains no expensive goals.
                    Singles = cord.empty
                ;
                    MaybeCandidate = no,
                    Candidates = CandidatesBelow,
                    Singles = cord.from_list(SinglesSoFar),
                    Cost = Annotation0 ^ pgd_cost
                )
            )
        ;
            GoalExpr0 = disj_rep(Disjs0),
            list.map_foldl5(
                disj_get_conjunctions_worth_parallelising(Info, RevGoalPath),
                Disjs0, Disjs, 1, _, cord.empty, Candidates,
                cord.empty, Pushes, cord.empty, Singles,
                cord.empty, Messages),
            disj_calc_cost(DetismRep, Disjs, Calls, Cost),
            GoalExpr = disj_rep(Disjs)
        ;
            GoalExpr0 = switch_rep(Var, CanFail, Cases0),
            list.map_foldl5(
                switch_case_get_conjunctions_worth_parallelising(Info,
                    RevGoalPath),
                Cases0, Cases, 1, _, cord.empty, Candidates,
                cord.empty, Pushes, cord.empty, Singles,
                cord.empty, Messages),
            switch_calc_cost(Cases, Calls, Cost),
            GoalExpr = switch_rep(Var, CanFail, Cases)
        ;
            GoalExpr0 = ite_rep(Cond0, Then0, Else0),
            ite_get_conjunctions_worth_parallelising(Info, RevGoalPath,
                Cond0, Cond, Then0, Then, Else0, Else,
                Candidates, Pushes, Singles, Messages),
            ite_calc_cost(Cond, Then, Else, Cost),
            GoalExpr = ite_rep(Cond, Then, Else)
        ;
            GoalExpr0 = scope_rep(SubGoal0, MaybeCut),
            RevSubGoalPath = rgp_cons(RevGoalPath, step_scope(MaybeCut)),
            goal_get_conjunctions_worth_parallelising(Info,
                RevSubGoalPath, SubGoal0, SubGoal,
                Candidates, Pushes, Singles, Messages),
            Cost = SubGoal ^ goal_annotation ^ pgd_cost,
            GoalExpr = scope_rep(SubGoal, MaybeCut)
        ;
            GoalExpr0 = negation_rep(SubGoal0),
            RevSubGoalPath = rgp_cons(RevGoalPath, step_neg),
            % We ignore _Singles here because you cannot push goals
            % after a negation into the negation.
            goal_get_conjunctions_worth_parallelising(Info,
                RevSubGoalPath, SubGoal0, SubGoal,
                Candidates, Pushes, _Singles, Messages),
            Singles = cord.empty,
            Cost = SubGoal ^ goal_annotation ^ pgd_cost,
            GoalExpr = negation_rep(SubGoal)
        ),
        Annotation = Annotation0 ^ pgd_cost := Cost
    ;
        GoalExpr0 = atomic_goal_rep(_, _, _, _),
        identify_costly_goal(Annotation0, Costly),
        (
            Costly = is_costly_goal,
            Singles = cord.singleton(!.Goal)
        ;
            Costly = is_not_costly_goal,
            Singles = cord.empty
        ),
        Candidates = cord.empty,
        Pushes = cord.empty,
        Messages = cord.empty,
        GoalExpr = GoalExpr0,
        Annotation = Annotation0
    ),
    !:Goal = goal_rep(GoalExpr, DetismRep, Annotation).

:- pred disj_get_conjunctions_worth_parallelising(
    implicit_parallelism_info::in, reverse_goal_path::in,
    pard_goal_detail::in, pard_goal_detail::out,
    int::in, int::out,
    cord(candidate_par_conjunction(pard_goal_detail))::in,
    cord(candidate_par_conjunction(pard_goal_detail))::out,
    cord(push_goal)::in, cord(push_goal)::out,
    cord(pard_goal_detail)::in, cord(pard_goal_detail)::out,
    cord(message)::in, cord(message)::out) is det.

disj_get_conjunctions_worth_parallelising(Info, RevGoalPath,
        !Disj, !DisjNum, !Candidates, !Pushes, !Singles, !Messages) :-
    RevDisjGoalPath= rgp_cons(RevGoalPath, step_disj(!.DisjNum)),
    goal_get_conjunctions_worth_parallelising(Info, RevDisjGoalPath,
        !Disj, Candidates, Pushes, Singles, Messages),
    !:Candidates = !.Candidates ++ Candidates,
    !:Pushes = !.Pushes ++ Pushes,
    !:Singles = !.Singles ++ Singles,
    !:Messages = !.Messages ++ Messages,
    !:DisjNum = !.DisjNum + 1.

:- pred switch_case_get_conjunctions_worth_parallelising(
    implicit_parallelism_info::in, reverse_goal_path::in,
    case_rep(pard_goal_detail_annotation)::in,
    case_rep(pard_goal_detail_annotation)::out,
    int::in, int::out,
    cord(candidate_par_conjunction(pard_goal_detail))::in,
    cord(candidate_par_conjunction(pard_goal_detail))::out,
    cord(push_goal)::in, cord(push_goal)::out,
    cord(pard_goal_detail)::in, cord(pard_goal_detail)::out,
    cord(message)::in, cord(message)::out) is det.

switch_case_get_conjunctions_worth_parallelising(Info, RevGoalPath, !Case,
        !CaseNum, !Candidates, !Pushes, !Singles, !Messages) :-
    !.Case = case_rep(MainConsIdRep, OtherConsIdReps, Goal0),
    RevArmPath = rgp_cons(RevGoalPath,
        step_switch(!.CaseNum, unknown_num_functors_in_type)),
    goal_get_conjunctions_worth_parallelising(Info, RevArmPath,
        Goal0, Goal, Candidates, Pushes, Singles, Messages),
    !:Case = case_rep(MainConsIdRep, OtherConsIdReps, Goal),
    !:Candidates = !.Candidates ++ Candidates,
    !:Pushes = !.Pushes ++ Pushes,
    !:Singles = !.Singles ++ Singles,
    !:Messages = !.Messages ++ Messages,
    !:CaseNum = !.CaseNum + 1.

:- pred ite_get_conjunctions_worth_parallelising(
    implicit_parallelism_info::in, reverse_goal_path::in,
    pard_goal_detail::in, pard_goal_detail::out,
    pard_goal_detail::in, pard_goal_detail::out,
    pard_goal_detail::in, pard_goal_detail::out,
    cord(candidate_par_conjunction(pard_goal_detail))::out,
    cord(push_goal)::out, cord(pard_goal_detail)::out,
    cord(message)::out) is det.

ite_get_conjunctions_worth_parallelising(Info, RevGoalPath,
        !Cond, !Then, !Else, Candidates, Pushes, Singles, Messages) :-
    RevCondGoalPath = rgp_cons(RevGoalPath, step_ite_cond),
    RevThenGoalPath = rgp_cons(RevGoalPath, step_ite_then),
    RevElseGoalPath = rgp_cons(RevGoalPath, step_ite_else),
    % We ignore _CondSingles here because you cannot push goals
    % following an if-then-else into the condition.
    goal_get_conjunctions_worth_parallelising(Info, RevCondGoalPath,
        !Cond, CondCandidates, CondPushes, _CondSingles, CondMessages),
    goal_get_conjunctions_worth_parallelising(Info, RevThenGoalPath,
        !Then, ThenCandidates, ThenPushes, ThenSingles, ThenMessages),
    goal_get_conjunctions_worth_parallelising(Info, RevElseGoalPath,
        !Else, ElseCandidates, ElsePushes, ElseSingles, ElseMessages),
    Candidates = CondCandidates ++ ThenCandidates ++ ElseCandidates,
    Pushes = CondPushes ++ ThenPushes ++ ElsePushes,
    Singles = ThenSingles ++ ElseSingles,
    Messages = CondMessages ++ ThenMessages ++ ElseMessages.

:- pred conj_get_conjunctions_worth_parallelising(
    implicit_parallelism_info::in, reverse_goal_path::in,
    list(pard_goal_detail)::in, list(pard_goal_detail)::out, int::in,
    list(pard_goal_detail)::in, list(pard_goal_detail)::out,
    assoc_list(int, list(pard_goal_detail))::in,
    assoc_list(int, list(pard_goal_detail))::out,
    cord(candidate_par_conjunction(pard_goal_detail))::in,
    cord(candidate_par_conjunction(pard_goal_detail))::out,
    cord(push_goal)::in, cord(push_goal)::out,
    cord(message)::in, cord(message)::out) is det.

conj_get_conjunctions_worth_parallelising(_Info, _RevGoalPath,
        [], [], _ConjNum, !SinglesSoFar, !RevSingleCands,
        !CandidatesBelow, !Pushes, !MessagesBelow).
conj_get_conjunctions_worth_parallelising(Info, RevGoalPath,
        [Conj0 | Conjs0], [Conj | Conjs], ConjNum, SinglesSoFar0, SinglesSoFar,
        !RevSingleCands, !CandidatesBelow, !Pushes, !MessagesBelow) :-
    RevConjGoalPath = rgp_cons(RevGoalPath, step_conj(ConjNum)),
    goal_get_conjunctions_worth_parallelising(Info, RevConjGoalPath,
        Conj0, Conj, Candidates, Pushes, SinglesCord, Messages),
    Singles = cord.list(SinglesCord),
    !:CandidatesBelow = !.CandidatesBelow ++ Candidates,
    !:Pushes = !.Pushes ++ Pushes,
    !:MessagesBelow = !.MessagesBelow ++ Messages,
    identify_costly_goal(Conj ^ goal_annotation, Costly),
    (
        Costly = is_costly_goal,
        !:RevSingleCands = [ConjNum - SinglesSoFar0 | !.RevSingleCands],
        SinglesSoFar1 = Singles
    ;
        Costly = is_not_costly_goal,
        % This goal might be costly if it is pushed into the context
        % of one of SinglesSoFar. This is common for recursive goals.
        list.filter(single_context_makes_goal_costly(Info, Conj),
            SinglesSoFar0, SinglesSoFarMakeConjCostly),
        (
            SinglesSoFarMakeConjCostly = []
        ;
            SinglesSoFarMakeConjCostly = [_ | _],
            !:RevSingleCands = [ConjNum - SinglesSoFarMakeConjCostly |
                !.RevSingleCands]
        ),

        (
            SinglesSoFar0 = [],
            Singles = [],
            SinglesSoFar1 = []
        ;
            SinglesSoFar0 = [_ | _],
            Singles = [],
            SinglesSoFar1 = SinglesSoFar0
        ;
            SinglesSoFar0 = [],
            Singles = [_ | _],
            SinglesSoFar1 = Singles
        ;
            SinglesSoFar0 = [_ | _],
            Singles = [_ | _],
            % XXX choose between SinglesSoFar0 and Singles, either on max cost
            % or total cost.
            SinglesSoFar1 = Singles
        )
    ),
    conj_get_conjunctions_worth_parallelising(Info, RevGoalPath,
        Conjs0, Conjs, ConjNum + 1, SinglesSoFar1, SinglesSoFar,
        !RevSingleCands, !CandidatesBelow, !Pushes, !MessagesBelow).

:- pred single_context_makes_goal_costly(implicit_parallelism_info::in,
    pard_goal_detail::in, pard_goal_detail::in) is semidet.

single_context_makes_goal_costly(Info, Goal, Single) :-
    SingleCost = Single ^ goal_annotation ^ pgd_cost,
    SingleCount = goal_cost_get_calls(SingleCost),
    fix_goal_counts(Info, SingleCount, Goal, ConjNewCounts),
    identify_costly_goal(ConjNewCounts ^ goal_annotation, is_costly_goal).

    % Given a conjunction with two or more costly goals (identified by
    % CostlyGoalsIndexes), check whether executing the conjunction in parallel
    % can yield a speedup.
    %
:- pred build_candidate_conjunction(implicit_parallelism_info::in,
    reverse_goal_path::in, list(pard_goal_detail)::in, list(int)::in,
    cord(message)::out,
    maybe(candidate_par_conjunction(pard_goal_detail))::out) is det.

build_candidate_conjunction(Info, RevGoalPath, Conjs, CostlyGoalsIndexes,
        !:Messages, MaybeCandidate) :-
    ProcLabel = Info ^ ipi_proc_label,
    !:Messages = cord.empty,
    NumCostlyGoals = list.length(CostlyGoalsIndexes),
    Location = pl_goal(ProcLabel, RevGoalPath),
    append_message(Location,
        info_found_conjs_above_callsite_threshold(NumCostlyGoals),
        !Messages),

    pardgoals_build_candidate_conjunction(Info, Location, RevGoalPath, no,
        Conjs, MaybeCandidate, !Messages),
    (
        MaybeCandidate = yes(_Candidate),
        append_message(Location,
            info_found_n_conjunctions_with_positive_speedup(1),
            !Messages)
    ;
        MaybeCandidate = no
    ).

    % Given a conjunction one costly goal (identified by CostlyIndex) directly
    % in it and other costly goals SinglesBefore inside alternate execution
    % paths in an earlier conjunct (conjunct i < CostlyIndex), check whether
    % pushing CostlyIndex next to SinglesBefore and executing those
    % conjunctions in parallel can yield a speedup.
    %
:- pred push_and_build_candidate_conjunctions(implicit_parallelism_info::in,
    reverse_goal_path::in, list(pard_goal_detail)::in, int::in,
    list(pard_goal_detail)::in, cord(message)::out,
    list(candidate_par_conjunction(pard_goal_detail))::out) is det.

push_and_build_candidate_conjunctions(_Info, _RevGoalPath, _Conjs,
        _CostlyIndex, [], cord.empty, []).
push_and_build_candidate_conjunctions(Info, RevGoalPath, Conjs,
        CostlyIndex, [Single | Singles], Messages, Candidates) :-
    push_and_build_candidate_conjunction(Info, RevGoalPath, Conjs,
        CostlyIndex, Single, HeadMessages, MaybeHeadCandidate),
    push_and_build_candidate_conjunctions(Info, RevGoalPath, Conjs,
        CostlyIndex, Singles, TailMessages, TailCandidates),
    Messages = HeadMessages ++ TailMessages,
    (
        MaybeHeadCandidate = yes(HeadCandidate),
        Candidates = [HeadCandidate | TailCandidates]
    ;
        MaybeHeadCandidate = no,
        Candidates = TailCandidates
    ).

:- pred push_and_build_candidate_conjunction(implicit_parallelism_info::in,
    reverse_goal_path::in, list(pard_goal_detail)::in, int::in,
    pard_goal_detail::in, cord(message)::out,
    maybe(candidate_par_conjunction(pard_goal_detail))::out) is det.

push_and_build_candidate_conjunction(Info, RevGoalPath, Conjs,
        CostlyIndex, Single, !:Messages, MaybeCandidate) :-
    SingleRevPath = Single ^ goal_annotation ^ pgd_original_path,
    rgp_to_fgp(SingleRevPath, SinglePath),
    rgp_to_fgp(RevGoalPath, GoalPath),
    ( if
        goal_path_inside_relative(GoalPath, SinglePath, RelativePath),
        RelativePath = fgp_cons(step_conj(RelConjStep), TailRelativePath),
        RelConjStep < CostlyIndex,
        list.take(CostlyIndex, Conjs, ConjsUptoCostly),
        list.drop(RelConjStep - 1, ConjsUptoCostly,
            [GoalToPushInto | GoalsToPush])
    then
        RevPushGoalPath = rgp_cons(RevGoalPath, step_conj(RelConjStep)),
        push_goals_create_candidate(Info, RevPushGoalPath,
            TailRelativePath, GoalToPushInto, GoalsToPush,
            RevCandidateGoalPath, CandidateConjs),

        ProcLabel = Info ^ ipi_proc_label,
        !:Messages = cord.empty,
        % XXX Location is a lie
        Location = pl_goal(ProcLabel, RevGoalPath),
        append_message(Location,
            info_found_pushed_conjs_above_callsite_threshold,
            !Messages),

        ( if
            goal_path_remove_last(RelativePath, MostRelativePath,
                LastRelativeStep),
            LastRelativeStep = step_conj(_)
        then
            % We push GoalsToPush into the existing conjunction
            % containing Single.
            PushGoalPath = MostRelativePath
        else
            % We push GoalsToPush next to Single in a newly created
            % conjunction.
            PushGoalPath = RelativePath
        ),

        PushGoal = push_goal(rev_goal_path_to_string(RevGoalPath),
            RelConjStep + 1, CostlyIndex,
            [goal_path_to_string(PushGoalPath)]),
        pardgoals_build_candidate_conjunction(Info, Location,
            RevCandidateGoalPath, yes(PushGoal), CandidateConjs,
            MaybeCandidate, !Messages),
        (
            MaybeCandidate = yes(_),
            append_message(Location,
                info_found_n_conjunctions_with_positive_speedup(1),
                !Messages)
        ;
            MaybeCandidate = no
        )
    else
        unexpected($pred, "bad goal path for Single")
    ).

:- pred merge_same_level_pushes(
    candidate_par_conjunction(pard_goal_detail)::in,
    list(candidate_par_conjunction(pard_goal_detail))::in,
    push_goal::out) is det.

merge_same_level_pushes(MainCandidate, [], MainPush) :-
    MaybeMainPush = MainCandidate ^ cpc_maybe_push_goal,
    (
        MaybeMainPush = yes(MainPush)
    ;
        MaybeMainPush = no,
        unexpected($pred, "no push")
    ).
merge_same_level_pushes(MainCandidate, [HeadCandidate | TailCandidates],
        Push) :-
    merge_same_level_pushes(HeadCandidate, TailCandidates, RestPush),
    MaybeMainPush = MainCandidate ^ cpc_maybe_push_goal,
    (
        MaybeMainPush = yes(MainPush)
    ;
        MaybeMainPush = no,
        unexpected($pred, "no push")
    ),
    ( if
        MainPush = push_goal(GoalPathStr, Lo, Hi, [MainPushInto]),
        RestPush = push_goal(GoalPathStr, Lo, Hi, RestPushInto)
    then
        Push = push_goal(GoalPathStr, Lo, Hi, [MainPushInto | RestPushInto])
    else
        unexpected($pred, "mismatch on pushed goals")
    ).

:- pred push_goals_create_candidate(implicit_parallelism_info::in,
    reverse_goal_path::in, forward_goal_path::in,
    pard_goal_detail::in, list(pard_goal_detail)::in,
    reverse_goal_path::out, list(pard_goal_detail)::out) is det.

push_goals_create_candidate(Info, RevCurPath, ForwardGoalPath,
        GoalToPushInto, GoalsToPush0, RevCandidateGoalPath, CandidateConjs) :-
    (
        ForwardGoalPath = fgp_nil,
        RevCandidateGoalPath = RevCurPath,
        % The pushed goals will have different costs in this context,
        % in particular the number of times they're called varies. This affects
        % the per-call costs of recursive calls.
        Calls = goal_cost_get_calls(GoalToPushInto ^ goal_annotation
            ^ pgd_cost),
        map(fix_goal_counts(Info, Calls), GoalsToPush0, GoalsToPush),
            CandidateConjs = [GoalToPushInto | GoalsToPush]
    ;
        ForwardGoalPath = fgp_cons(FirstRelStep, TailRelPath),
        GoalToPushInto = goal_rep(GoalExpr, _, _),
        (
            FirstRelStep = step_conj(N),
            ( if GoalExpr = conj_rep(Goals) then
                (
                    TailRelPath = fgp_nil,
                    % Conjoin GoalsToPush not with just the expensive goal,
                    % but with the whole conjunction containing it.
                    RevCandidateGoalPath = RevCurPath,
                    % The pushed goals will have different costs in this
                    % context, in particular the number of times they're called
                    % varies. This affects the per-call costs of recursive
                    % calls.
                    Cost = GoalToPushInto ^ goal_annotation ^ pgd_cost,
                    Calls = goal_cost_get_calls(Cost),
                    list.map(fix_goal_counts(Info, Calls),
                        GoalsToPush0, GoalsToPush),
                    CandidateConjs = Goals ++ GoalsToPush
                ;
                    TailRelPath = fgp_cons(_, _),
                    list.det_drop(N - 1, Goals, Tail),
                    ( if Tail = [SubGoal] then
                        push_goals_create_candidate(Info,
                            rgp_cons(RevCurPath, FirstRelStep),
                            TailRelPath, SubGoal, GoalsToPush0,
                            RevCandidateGoalPath, CandidateConjs)
                    else
                        % We can't push goals into a non-last conjunct without
                        % reordering, which is currently not supported.
                        % By building a conjunction here, we may still be able
                        % to create a worthwhile parallelisation. However,
                        % there is a trade-off to explore between this
                        % and not generating the single expensive goal
                        % from within the conjunction, and therefore possibly
                        % finding other single expensive goals later in this
                        % conjunction.
                        RevCandidateGoalPath = RevCurPath,
                        Cost = GoalToPushInto ^ goal_annotation ^ pgd_cost,
                        Calls = goal_cost_get_calls(Cost),
                            list.map(fix_goal_counts(Info, Calls),
                                GoalsToPush0, GoalsToPush),
                        CandidateConjs = Goals ++ GoalsToPush
                    )
                )
            else
                unexpected($pred, "not conj")
            )
        ;
            FirstRelStep = step_disj(N),
            ( if GoalExpr = disj_rep(Goals) then
                list.det_index1(Goals, N, SubGoal),
                push_goals_create_candidate(Info,
                    rgp_cons(RevCurPath, FirstRelStep),
                    TailRelPath, SubGoal, GoalsToPush0,
                    RevCandidateGoalPath, CandidateConjs)
            else
                unexpected($pred, "not disj")
            )
        ;
            FirstRelStep = step_switch(N, _),
            ( if GoalExpr = switch_rep(_, _, Cases) then
                list.det_index1(Cases, N, Case),
                Case = case_rep(_, _, SubGoal),
                push_goals_create_candidate(Info,
                    rgp_cons(RevCurPath, FirstRelStep),
                    TailRelPath, SubGoal, GoalsToPush0,
                    RevCandidateGoalPath, CandidateConjs)
            else
                unexpected($pred, "not switch")
            )
        ;
            FirstRelStep = step_ite_then,
            ( if GoalExpr = ite_rep(_, Then, _) then
                push_goals_create_candidate(Info,
                    rgp_cons(RevCurPath, FirstRelStep),
                    TailRelPath, Then, GoalsToPush0,
                    RevCandidateGoalPath, CandidateConjs)
            else
                unexpected($pred, "not ite_then")
            )
        ;
            FirstRelStep = step_ite_else,
            ( if GoalExpr = ite_rep(_, _, Else) then
                push_goals_create_candidate(Info,
                    rgp_cons(RevCurPath, FirstRelStep),
                    TailRelPath, Else, GoalsToPush0,
                    RevCandidateGoalPath, CandidateConjs)
            else
                unexpected($pred, "not ite_else")
            )
        ;
            FirstRelStep = step_ite_cond,
            % We cannot push into a condition.
            unexpected($pred, "ite_cond")
        ;
            FirstRelStep = step_neg,
            % We cannot push into a negated goal.
            unexpected($pred, "neg")
        ;
            FirstRelStep = step_scope(_),
            ( if GoalExpr = scope_rep(SubGoal, _) then
                push_goals_create_candidate(Info,
                    rgp_cons(RevCurPath, FirstRelStep),
                    TailRelPath, SubGoal, GoalsToPush0,
                    RevCandidateGoalPath, CandidateConjs)
            else
                unexpected($pred, "not scope")
            )
        ;
            FirstRelStep = step_lambda,
            % These should not exist in a profiled program.
            unexpected($pred, "lambda")
        ;
            FirstRelStep = step_try,
            % These should not exist in a profiled program.
            unexpected($pred, "try")
        ;
            FirstRelStep = step_atomic_main,
            % These should not exist in a profiled program.
            unexpected($pred, "atomic_main")
        ;
            FirstRelStep = step_atomic_orelse(_),
            % These should not exist in a profiled program.
            unexpected($pred, "atomic_orelse")
        )
    ).

:- pred fix_goal_counts(implicit_parallelism_info::in, int::in,
    pard_goal_detail::in, pard_goal_detail::out) is det.

fix_goal_counts(Info, Count, !Goal) :-
    Annotation0 = !.Goal ^ goal_annotation,
    Cost0 = Annotation0 ^ pgd_cost,
    Cost = goal_cost_change_calls(Cost0, Count),
    !Goal ^ goal_annotation ^ pgd_cost := Cost,
    ( if goal_cost_above_par_threshold(Info, Cost) then
        AboveThreshold = cost_above_par_threshold
    else
        AboveThreshold = cost_not_above_par_threshold
    ),
    !Goal ^ goal_annotation ^ pgd_cost_above_threshold := AboveThreshold.

:- pred pardgoals_build_candidate_conjunction(implicit_parallelism_info::in,
    program_location::in, reverse_goal_path::in,
    maybe(push_goal)::in, list(pard_goal_detail)::in,
    maybe(candidate_par_conjunction(pard_goal_detail))::out,
    cord(message)::in, cord(message)::out) is det.

pardgoals_build_candidate_conjunction(Info, Location, RevGoalPath,
        MaybePushGoal, Goals, MaybeCandidate, !Messages) :-
    % Setting up the first parallel conjunct is a different algorithm to the
    % latter ones, at this point we have the option of moving goals from before
    % the first costly call to either before or during the parallel
    % conjunction.  Executing them during the parallel conjunction can be more
    % efficient.  However if goals within other parallel conjuncts depend on
    % them and don't depend upon the first costly call then this would make the
    % conjunction dependent when it could be independent.
    find_best_parallelisation(Info, Location, Goals, MaybeBestParallelisation,
        !Messages),
    (
        MaybeBestParallelisation = yes(BestParallelisation),
        FirstConjNum = 1,
        ParalleliseDepConjs = Info ^ ipi_opts ^ cpcp_parallelise_dep_conjs,
        SpeedupThreshold = Info ^ ipi_opts ^ cpcp_speedup_threshold,
        BestParallelisation = fp_parallel_execution(GoalsBefore, ParConjs,
            GoalsAfter, IsDependent, Metrics),
        Speedup = parallel_exec_metrics_get_speedup(Metrics),
        Calls = Metrics ^ pem_num_calls,
        conj_calc_cost(GoalsBefore, Calls, GoalsBeforeCost0),
        GoalsBeforeCost = goal_cost_get_percall(GoalsBeforeCost0),
        conj_calc_cost(GoalsAfter, Calls, GoalsAfterCost0),
        GoalsAfterCost = goal_cost_get_percall(GoalsAfterCost0),
        RevGoalPathString = rev_goal_path_to_string(RevGoalPath),
        Candidate = candidate_par_conjunction(RevGoalPathString, MaybePushGoal,
            FirstConjNum, IsDependent, GoalsBefore, GoalsBeforeCost, ParConjs,
            GoalsAfter, GoalsAfterCost, Metrics),
        ( if
            Speedup > SpeedupThreshold,
            (
                ParalleliseDepConjs = do_not_parallelise_dep_conjs
            =>
                IsDependent = conjuncts_are_independent
            )
        then
            MaybeCandidate = yes(Candidate)
        else
            MaybeCandidate = no,
            trace [
                compile_time(flag("debug_parallel_conjunction_speedup")),
                io(!IO)
            ]
            (
                (
                    ( Location = pl_proc(ProcLabel)
                    ; Location = pl_goal(ProcLabel, _)
                    )
                ;
                    Location = pl_clique(_),
                    unexpected($pred, "location is a clique")
                ;
                    Location = pl_csd(_),
                    unexpected($pred, "location is a csd")
                ),

                convert_candidate_par_conjunction(
                    pard_goal_detail_to_pard_goal, Candidate, FBCandidate),
                VarTable = Info ^ ipi_var_name_table,
                create_candidate_parallel_conj_report(VarTable,
                    FBCandidate, Report),
                print_proc_label_to_string(ProcLabel, ProcLabelString),
                io.format("Not parallelising conjunction in %s, " ++
                    "insufficient speedup or too dependent:\n",
                    [s(ProcLabelString)], !IO),
                io.write_string(append_list(cord.list(Report)), !IO),
                io.flush_output(!IO)
            )
        )
    ;
        MaybeBestParallelisation = no,
        MaybeCandidate = no
    ).

%---------------------------------------------------------------------------%

goal_to_pard_goal(Info, RevGoalPath, Goal, DetailGoal, !Messages) :-
    Goal = goal_rep(GoalExpr, Detism, GoalId),
    InstMapInfo = get_goal_attribute_det(Info ^ ipi_inst_map_array, GoalId),
    Coverage = get_goal_attribute_det(Info ^ ipi_coverage_array, GoalId),
    get_coverage_before_det(Coverage, Before),
    (
        (
            GoalExpr = conj_rep(Conjs),
            list.map_foldl2(conj_to_pard_goals(Info, RevGoalPath),
                Conjs, DetailConjs, 1, _, !Messages),
            conj_calc_cost(DetailConjs, Before, Cost),
            DetailGoalExpr = conj_rep(DetailConjs)
        ;
            GoalExpr = disj_rep(Disjs),
            list.map_foldl2(disj_to_pard_goals(Info, RevGoalPath),
                Disjs, DetailDisjs, 1, _, !Messages),
            disj_calc_cost(Detism, DetailDisjs, Before, Cost),
            DetailGoalExpr = disj_rep(DetailDisjs)
        ;
            GoalExpr = switch_rep(Var, CanFail, Cases),
            list.map_foldl2(case_to_pard_goal(Info, RevGoalPath),
                Cases, DetailCases, 1, _, !Messages),
            switch_calc_cost(DetailCases, Before, Cost),
            DetailGoalExpr = switch_rep(Var, CanFail, DetailCases)
        ;
            GoalExpr = ite_rep(Cond, Then, Else),
            CondRevGoalPath = rgp_cons(RevGoalPath, step_ite_cond),
            ThenRevGoalPath = rgp_cons(RevGoalPath, step_ite_then),
            ElseRevGoalPath = rgp_cons(RevGoalPath, step_ite_else),
            goal_to_pard_goal(Info, CondRevGoalPath, Cond, DetailCond,
                !Messages),
            goal_to_pard_goal(Info, ThenRevGoalPath, Then, DetailThen,
                !Messages),
            goal_to_pard_goal(Info, ElseRevGoalPath, Else, DetailElse,
                !Messages),
            ite_calc_cost(DetailCond, DetailThen, DetailElse, Cost),
            DetailGoalExpr = ite_rep(DetailCond, DetailThen, DetailElse)
        ;
            GoalExpr = negation_rep(SubGoal),
            SubRevGoalPath = rgp_cons(RevGoalPath, step_neg),
            goal_to_pard_goal(Info, SubRevGoalPath, SubGoal, DetailSubGoal,
                !Messages),
            Cost = DetailSubGoal ^ goal_annotation ^ pgd_cost,
            DetailGoalExpr = negation_rep(DetailSubGoal)
        ;
            GoalExpr = scope_rep(SubGoal, MaybeCut),
            SubRevGoalPath = rgp_cons(RevGoalPath, step_scope(MaybeCut)),
            goal_to_pard_goal(Info, SubRevGoalPath, SubGoal, DetailSubGoal,
                !Messages),
            Cost = DetailSubGoal ^ goal_annotation ^ pgd_cost,
            DetailGoalExpr = scope_rep(DetailSubGoal, MaybeCut)
        ),
        PardGoalType = pgt_non_atomic_goal,

        BoundVars = to_sorted_list(InstMapInfo ^ im_bound_vars),
        list.foldl(
            goal_build_use_map(Goal, RevGoalPath, Cost, Info,
                var_use_production),
            BoundVars, map.init, ProductionUseMap),
        ConsumedVars = to_sorted_list(InstMapInfo ^ im_consumed_vars),
        list.foldl(
            goal_build_use_map(Goal, RevGoalPath, Cost, Info,
                var_use_consumption),
            ConsumedVars, map.init, ConsumptionUseMap)
    ;
        GoalExpr = atomic_goal_rep(Context, Line, BoundVars, AtomicGoal),
        DetailGoalExpr = atomic_goal_rep(Context, Line, BoundVars, AtomicGoal),
        atomic_pard_goal_type(Info, RevGoalPath, AtomicGoal, InstMapInfo,
            PardGoalType, Messages),
        atomic_pard_goal_cost(Info, RevGoalPath, Coverage, AtomicGoal, Cost),

        list.foldl(
            atomic_goal_build_use_map(AtomicGoal, RevGoalPath, Info,
                var_use_production),
            BoundVars, map.init, ProductionUseMap),
        ConsumedVars = to_sorted_list(InstMapInfo ^ im_consumed_vars),
        list.foldl(
            atomic_goal_build_use_map(AtomicGoal, RevGoalPath, Info,
                var_use_consumption),
            ConsumedVars, map.init, ConsumptionUseMap),

        !:Messages = !.Messages ++ Messages
    ),
    % XXX: The goal annotations cannot represent reasons why a goal cannot be
    % parallelised, for example it could be nondet, semidet or impure.
    ( if
        can_parallelise_goal(Goal),
        goal_cost_above_par_threshold(Info, Cost)
    then
        CostAboveThreshold = cost_above_par_threshold
    else
        CostAboveThreshold = cost_not_above_par_threshold
    ),
    PardGoalAnnotation = pard_goal_detail(PardGoalType, InstMapInfo,
        RevGoalPath, Coverage, Cost, CostAboveThreshold,
        ProductionUseMap, ConsumptionUseMap),
    DetailGoal = goal_rep(DetailGoalExpr, Detism, PardGoalAnnotation).

:- pred goal_build_use_map(goal_rep(goal_id)::in,
    reverse_goal_path::in, goal_cost_csq::in, implicit_parallelism_info::in,
    var_use_type::in, var_rep::in,
    map(var_rep, lazy(var_use_info))::in,
    map(var_rep, lazy(var_use_info))::out) is det.

goal_build_use_map(Goal, RevGoalPath, Cost, Info, VarUseType, Var, !Map) :-
    LazyUse = delay((func) = compute_goal_var_use_lazy(Goal, RevGoalPath,
        Cost, Info, VarUseType, Var)),
    map.det_insert(Var, LazyUse, !Map).

:- func compute_goal_var_use_lazy(goal_rep(goal_id), reverse_goal_path,
    goal_cost_csq, implicit_parallelism_info, var_use_type, var_rep)
    = var_use_info.

compute_goal_var_use_lazy(Goal, RevGoalPath, Cost, Info, VarUseType, Var)
        = Use :-
    Info = implicit_parallelism_info(Deep, _ProgRep, _Params, CliquePtr,
        CallSiteMap, RecursiveCallSiteMap, ContainingGoalMap, CoverageArray,
        _InstMapArray, RecursionType, _VarTable, _ProcLabel),
    CostPercall = goal_cost_get_percall(Cost),
    (
        ( RecursionType = rt_not_recursive
        ; RecursionType = rt_single(_, _, _, _, _)
        ),
        recursion_type_get_interesting_parallelisation_depth(RecursionType,
            yes(RecDepth)),
        implicit_par_info_intermodule_var_use(Info, FollowCallsAcrossModules),
        VarUseOptions = var_use_options(Deep, FollowCallsAcrossModules,
            VarUseType),
        var_first_use(CliquePtr, CallSiteMap, RecursiveCallSiteMap,
            ContainingGoalMap, CoverageArray, RecursionType, RecDepth, Goal,
            RevGoalPath, CostPercall, Var, VarUseOptions, Use)
    ;
        ( RecursionType = rt_divide_and_conquer(_, _)
        ; RecursionType = rt_mutual_recursion(_)
        ; RecursionType = rt_other(_)
        ; RecursionType = rt_errors(_)
        ),
        % var_first_use doesn't work for these recursion types.
        pessimistic_var_use_info(VarUseType, CostPercall, Use),
        append_message(pl_clique(CliquePtr),
            warning_cannot_compute_first_use_time(
                "Recursion type unknown for var_first_use/12"),
            empty, Messages),
        trace [io(!IO)] (
            io.stderr_stream(Stderr, !IO),
            write_out_messages(Stderr, Messages, !IO)
        )
    ).

:- pred conj_to_pard_goals(implicit_parallelism_info::in,
    reverse_goal_path::in, goal_rep(goal_id)::in, pard_goal_detail::out,
    int::in, int::out, cord(message)::in, cord(message)::out) is det.

conj_to_pard_goals(Info, RevGoalPath, !Goal, !ConjNum, !Messages) :-
    ConjRevGoalPath = rgp_cons(RevGoalPath, step_conj(!.ConjNum)),
    goal_to_pard_goal(Info, ConjRevGoalPath, !Goal, !Messages),
    !:ConjNum = !.ConjNum + 1.

:- pred disj_to_pard_goals(implicit_parallelism_info::in,
    reverse_goal_path::in, goal_rep(goal_id)::in, pard_goal_detail::out,
    int::in, int::out, cord(message)::in, cord(message)::out) is det.

disj_to_pard_goals(Info, RevGoalPath, !Goal, !DisjNum, !Messages) :-
    DisjRevGoalPath = rgp_cons(RevGoalPath, step_disj(!.DisjNum)),
    goal_to_pard_goal(Info, DisjRevGoalPath, !Goal, !Messages),
    !:DisjNum = !.DisjNum + 1.

:- pred case_to_pard_goal(implicit_parallelism_info::in,
    reverse_goal_path::in,
    case_rep(goal_id)::in, case_rep(pard_goal_detail_annotation)::out,
    int::in, int::out, cord(message)::in, cord(message)::out) is det.

case_to_pard_goal(Info, RevGoalPath, !Case, !CaseNum, !Messages) :-
    !.Case = case_rep(ConsId, OtherConsId, Goal0),
    RevArmPath = rgp_cons(RevGoalPath,
        step_switch(!.CaseNum, unknown_num_functors_in_type)),
    goal_to_pard_goal(Info, RevArmPath, Goal0, Goal, !Messages),
    !:CaseNum = !.CaseNum + 1,
    !:Case = case_rep(ConsId, OtherConsId, Goal).

%---------------------------------------------------------------------------%

:- pred atomic_pard_goal_type(implicit_parallelism_info::in,
    reverse_goal_path::in, atomic_goal_rep::in, inst_map_info::in,
    pard_goal_type::out, cord(message)::out) is det.

atomic_pard_goal_type(Info, RevGoalPath, AtomicGoal, InstMapInfo,
        GoalType, !:Messages) :-
    !:Messages = cord.empty,
    InstMapBefore = InstMapInfo ^ im_before,
    InstMapAfter = InstMapInfo ^ im_after,
    atomic_goal_is_call(AtomicGoal, IsCall),
    (
        IsCall = atomic_goal_is_trivial,
        GoalType = pgt_other_atomic_goal
    ;
        IsCall = atomic_goal_is_call(Args),
        % Lookup var use information.
        map.lookup(Info ^ ipi_call_sites, RevGoalPath, CallSite),
        list.map_foldl(compute_var_modes(InstMapBefore, InstMapAfter),
            Args, VarsAndModes, 0, _),
        GoalType = pgt_call(VarsAndModes, CallSite)
    ).

:- pred atomic_pard_goal_cost(implicit_parallelism_info::in,
    reverse_goal_path::in, coverage_info::in, atomic_goal_rep::in,
    goal_cost_csq::out) is det.

atomic_pard_goal_cost(Info, RevGoalPath, Coverage, AtomicGoal, Cost) :-
    atomic_goal_is_call(AtomicGoal, IsCall),
    (
        IsCall = atomic_goal_is_trivial,
        get_coverage_before_det(Coverage, Calls),
        Cost = atomic_goal_cost(Calls)
    ;
        IsCall = atomic_goal_is_call(_),
        map.lookup(Info ^ ipi_call_sites, RevGoalPath, CallSite),
        ( if
            cost_and_callees_is_recursive(Info ^ ipi_clique, CallSite),
            map.search(Info ^ ipi_rec_call_sites, RevGoalPath, RecCost)
        then
            CSCost = RecCost
        else
            CSCost = CallSite ^ cac_cost
        ),
        Cost = call_goal_cost(CSCost)
    ).

:- pred compute_var_modes(inst_map::in, inst_map::in,
    var_rep::in, var_and_mode::out, int::in, int::out) is det.

compute_var_modes(InstMapBefore, InstMapAfter, Arg, VarAndMode, !ArgNum) :-
    var_get_mode(InstMapBefore, InstMapAfter, Arg, Mode),
    VarAndMode = var_and_mode(Arg, Mode),
    !:ArgNum = !.ArgNum + 1.

:- pred var_get_mode(inst_map::in, inst_map::in, var_rep::in,
    var_mode_rep::out) is det.

var_get_mode(InstMapBefore, InstMapAfter, VarRep, VarModeRep) :-
    inst_map_get(InstMapBefore, VarRep, InstBefore, _),
    inst_map_get(InstMapAfter, VarRep, InstAfter, _),
    VarModeRep = var_mode_rep(InstBefore, InstAfter).

%---------------------------------------------------------------------------%

can_parallelise_goal(Goal) :-
    Detism = Goal ^ goal_detism_rep,
    ( Detism = det_rep
    ; Detism = cc_multidet_rep
    ).
    % XXX We would check purity here except that purity information is not
    % present in the bytecode.

:- pred goal_cost_above_par_threshold(implicit_parallelism_info::in,
    goal_cost_csq::in) is semidet.

goal_cost_above_par_threshold(Info, Cost) :-
    goal_cost_get_calls(Cost) > 0,
    PercallCost = goal_cost_get_percall(Cost),
    PercallCost > float(Info ^ ipi_opts ^ cpcp_call_site_threshold).

%---------------------------------------------------------------------------%

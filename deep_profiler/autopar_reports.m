%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: autopar_reports.m
% Author: pbone.
%
% This module contains code for creating reports for debugging.
%
%---------------------------------------------------------------------------%

:- module mdprof_fb.automatic_parallelism.autopar_reports.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.feedback.
:- import_module mdbcomp.feedback.automatic_parallelism.
:- import_module mdbcomp.program_representation.

:- import_module cord.
:- import_module io.

:- pred print_feedback_report(io.text_output_stream::in, feedback_info::in,
    io::di, io::uo) is det.

:- pred create_candidate_parallel_conj_report(var_name_table::in,
    candidate_par_conjunction(pard_goal)::in, cord(string)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.goal_path.
:- import_module measurement_units.
:- import_module message.
:- import_module program_representation_utils.

:- import_module assoc_list.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.

%---------------------------------------------------------------------------%

print_feedback_report(OutputStream, FeedbackInfo, !IO) :-
    get_all_feedback_info(FeedbackInfo, ProfiledProgramName,
        MaybeCandidateParConjs),
    % This code is structured like this to make it easy to add
    % new feedback components.
    some [!RevReports] (
        !:RevReports = [],
        (
            MaybeCandidateParConjs = no
        ;
            MaybeCandidateParConjs = yes(CandidateParConjs),
            create_feedback_autopar_report(CandidateParConjs,
                CandidateParConjsReport),
            !:RevReports = [CandidateParConjsReport | !.RevReports]
        ),
        list.reverse(!.RevReports, Reports)
    ),
    (
        Reports = [],
        Report = "no report available.\n"
    ;
        Reports = [_ | _],
        string.append_list(Reports, Report)
    ),
    io.format(OutputStream, "Feedback report for %s:\n\n%s",
        [s(ProfiledProgramName), s(Report)], !IO).

%---------------------------------------------------------------------------%

:- pred create_feedback_autopar_report(
    feedback_info_candidate_parallel_conjunctions::in, string::out) is det.

create_feedback_autopar_report(CandidateParConjs, Report) :-
    CandidateParConjs =
        feedback_info_candidate_parallel_conjunctions(Parameters, ProcConjs),
    NumProcConjs = length(ProcConjs),
    foldl(count_conjunctions_in_procs, ProcConjs, 0, NumConjs),
    Parameters = candidate_par_conjunctions_params(DesiredParallelism,
        IntermoduleVarUse, SparkingCost, SparkingDelay, BarrierCost,
        SignalCost, WaitCost, ContextWakeupDelay, CliqueThreshold,
        CallSiteThreshold, SpeedupThreshold,
        ParalleliseDepConjs, AlgForFindingBestPar),
    AlgForFindingBestParStr =
        alg_for_finding_best_par_to_string(AlgForFindingBestPar),
    ReportHeader = singleton(format(
        "  Candidate parallel conjunctions:\n" ++
        "    Desired parallelism:       %f\n" ++
        "    Intermodule var use:       %s\n" ++
        "    Sparking cost:             %d\n" ++
        "    Sparking delay:            %d\n" ++
        "    Barrier cost:              %d\n" ++
        "    Future signal cost:        %d\n" ++
        "    Future wait cost:          %d\n" ++
        "    Context wakeup delay:      %d\n" ++
        "    Clique threshold:          %d\n" ++
        "    Call site threshold:       %d\n" ++
        "    Speedup threshold:         %f\n" ++
        "    Dependent conjs:           %s\n" ++
        "    BestParAlgorithm:          %s\n" ++
        "    # of par procs with conjs: %d\n" ++
        "    # of par conjunctions:     %d\n" ++
        "    Parallel conjunctions:\n\n",
        [f(DesiredParallelism),
         s(string(IntermoduleVarUse)),
         i(SparkingCost),
         i(SparkingDelay),
         i(BarrierCost),
         i(SignalCost),
         i(WaitCost),
         i(ContextWakeupDelay),
         i(CliqueThreshold),
         i(CallSiteThreshold),
         f(SpeedupThreshold),
         s(ParalleliseDepConjsStr),
         s(AlgForFindingBestParStr),
         i(NumProcConjs),
         i(NumConjs)])),
    (
        ParalleliseDepConjs = parallelise_dep_conjs(SpeedupAlg),
        (
            SpeedupAlg = estimate_speedup_naively,
            ParalleliseDepConjsStr = "yes, pretend they're independent"
        ;
            SpeedupAlg = estimate_speedup_by_overlap,
            ParalleliseDepConjsStr = "yes, use overlap calculation"
        )
    ;
        ParalleliseDepConjs = do_not_parallelise_dep_conjs,
        ParalleliseDepConjsStr = "no"
    ),
    list.map(create_candidate_parallel_conj_proc_report, ProcConjs,
        ReportConjs),
    Report = append_list(list(ReportHeader ++ cord_list_to_cord(ReportConjs))).

:- pred count_conjunctions_in_procs(
    pair(T, candidate_par_conjunctions_proc)::in, int::in, int::out) is det.

count_conjunctions_in_procs(_ - Cands, !NumConjs) :-
    Cands = candidate_par_conjunctions_proc(_VarNameTable, _Pushes, Conjs),
    !:NumConjs = !.NumConjs + length(Conjs).

:- func alg_for_finding_best_par_to_string(alg_for_finding_best_par) = string.

alg_for_finding_best_par_to_string(Alg) = Str :-
    (
        Alg = affbp_greedy,
        Str = "greedy"
    ;
        Alg = affbp_complete_branches(N),
        Str = string.format("complete-branches(%d)", [i(N)])
    ;
        Alg = affbp_complete_size(N),
        Str = string.format("complete-size(%d)", [i(N)])
    ;
        Alg = affbp_complete,
        Str = "complete"
    ).

:- pred create_candidate_parallel_conj_proc_report(
    pair(string_proc_label, candidate_par_conjunctions_proc)::in,
    cord(string)::out) is det.

create_candidate_parallel_conj_proc_report(Proc - CandidateParConjunctionProc,
        Report) :-
    CandidateParConjunctionProc = candidate_par_conjunctions_proc(VarNameTable,
        PushGoals, CandidateParConjunctions),
    print_proc_label_to_string(Proc, ProcString),
    list.map(create_push_goal_report, PushGoals, PushGoalReports),
    list.map(create_candidate_parallel_conj_report(VarNameTable),
        CandidateParConjunctions, CandidateParConjunctionReports),
    Header = string.format("    %s\n", [s(ProcString)]),
    Report = cord.singleton(Header) ++
        cord_list_to_cord(PushGoalReports) ++
        cord.singleton("\n") ++
        cord_list_to_cord(CandidateParConjunctionReports).

:- pred create_push_goal_report(push_goal::in, cord(string)::out) is det.

create_push_goal_report(PushGoal, Report) :-
    PushGoal = push_goal(PushGoalPathStr, Lo, Hi, PushedGoalPathStrs),
    string.format("\n      PushGoal: %s, lo %d, hi %d\n",
        [s(PushGoalPathStr), i(Lo), i(Hi)], HeadPushGoalStr),
    FormatPushedGoals = (
        func(PushedGoalPathStr) =
            string.format("        %s\n", [s(PushedGoalPathStr)])
    ),
    TailPushGoalStrs = list.map(FormatPushedGoals, PushedGoalPathStrs),
    Report = cord.from_list([HeadPushGoalStr | TailPushGoalStrs]).

create_candidate_parallel_conj_report(VarNameTable, CandidateParConjunction,
        Report) :-
    CandidateParConjunction = candidate_par_conjunction(GoalPathString,
        MaybePushGoal, FirstConjNum, IsDependent, GoalsBefore, GoalsBeforeCost,
        Conjs, GoalsAfter, GoalsAfterCost, ParExecMetrics),
    ParExecMetrics = parallel_exec_metrics(NumCalls, SeqTime, ParTime,
        SparkCost, BarrierCost, SignalsCost, WaitsCost, FirstConjDeadTime,
        FutureDeadTime),
    ParOverheads = parallel_exec_metrics_get_overheads(ParExecMetrics),
    (
        IsDependent = conjuncts_are_independent,
        DependenceString = "no"
    ;
        IsDependent = conjuncts_are_dependent(Vars),
        map(lookup_var_name(VarNameTable), Vars, VarNames),
        VarsString = join_list(", ", to_sorted_list(VarNames)),
        DependenceString = format("on %s", [s(VarsString)])
    ),
    Speedup = parallel_exec_metrics_get_speedup(ParExecMetrics),
    TimeSaving = parallel_exec_metrics_get_time_saving(ParExecMetrics),
    TotalDeadTime = FirstConjDeadTime + FutureDeadTime,

    string.format("      Path: %s\n", [s(GoalPathString)], Header1Str),
    Header1 = cord.singleton(Header1Str),

    (
        MaybePushGoal = no,
        Header2 = cord.empty
    ;
        MaybePushGoal = yes(PushGoal),
        PushGoal = push_goal(PushGoalPathStr, Lo, Hi, PushedGoalPathStrs),
        string.format("      PushGoal: %s, lo %d, hi %d\n",
            [s(PushGoalPathStr), i(Lo), i(Hi)], HeadPushGoalStr),
        FormatPushedGoals = (
            func(PushedGoalPathStr) =
                string.format("                %s\n", [s(PushedGoalPathStr)])
        ),
        TailPushGoalStrs = list.map(FormatPushedGoals, PushedGoalPathStrs),
        Header2 = cord.from_list([HeadPushGoalStr | TailPushGoalStrs])
    ),

    string.format(
        "      Dependent: %s\n" ++
        "      NumCalls: %s\n" ++
        "      SeqTime: %s\n" ++
        "      ParTime: %s\n" ++
        "      SparkCost: %s\n" ++
        "      BarrierCost: %s\n" ++
        "      SignalsCost: %s\n" ++
        "      WaitsCost: %s\n" ++
        "      ParOverheads total: %s\n" ++
        "      Speedup: %s\n" ++
        "      Time saving: %s\n" ++
        "      First conj dead time: %s\n" ++
        "      Future dead time: %s\n" ++
        "      Total dead time: %s\n\n",
        [s(DependenceString),
         s(commas(NumCalls)),
         s(two_decimal_fraction(SeqTime)),
         s(two_decimal_fraction(ParTime)),
         s(two_decimal_fraction(SparkCost)),
         s(two_decimal_fraction(BarrierCost)),
         s(two_decimal_fraction(SignalsCost)),
         s(two_decimal_fraction(WaitsCost)),
         s(two_decimal_fraction(ParOverheads)),
         s(four_decimal_fraction(Speedup)),
         s(two_decimal_fraction(TimeSaving)),
         s(two_decimal_fraction(FirstConjDeadTime)),
         s(two_decimal_fraction(FutureDeadTime)),
         s(two_decimal_fraction(TotalDeadTime))],
        Header2Str),
    Header3 = cord.singleton(Header2Str),

    ( if rev_goal_path_from_string(GoalPathString, RevGoalPathPrime) then
        RevGoalPath = RevGoalPathPrime
    else
        unexpected($pred, "couldn't parse goal path")
    ),
    some [!ConjNum] (
        !:ConjNum = FirstConjNum,
        format_sequential_conjunction(VarNameTable, 4, RevGoalPath,
            GoalsBefore, GoalsBeforeCost, !.ConjNum, ReportGoalsBefore0),
        ReportGoalsBefore = indent(3) ++ singleton("Goals before:\n") ++
            ReportGoalsBefore0,

        !:ConjNum = !.ConjNum + length(GoalsBefore),
        format_parallel_conjunction(VarNameTable, 4, RevGoalPath,
            !.ConjNum, Conjs, ReportParConj0),
        ReportParConj = indent(3) ++ singleton("Parallel conjunction:\n") ++
            ReportParConj0,

        !:ConjNum = !.ConjNum + 1,
        format_sequential_conjunction(VarNameTable, 4, RevGoalPath,
            GoalsAfter, GoalsAfterCost, !.ConjNum, ReportGoalsAfter0),
        ReportGoalsAfter = indent(3) ++ singleton("Goals after:\n") ++
            ReportGoalsAfter0
    ),
    Report = Header1 ++ Header2 ++ Header3 ++ ReportGoalsBefore ++ nl
        ++ ReportParConj ++ nl ++ ReportGoalsAfter ++ nl.

:- pred format_parallel_conjunction(var_name_table::in, int::in,
    reverse_goal_path::in, int::in,
    list(seq_conj(pard_goal))::in, cord(string)::out) is det.

format_parallel_conjunction(VarNameTable, Indent, RevGoalPath, ConjNum, Conjs,
        !:Report) :-
    IndentStr = indent(Indent),
    !:Report = IndentStr ++ singleton("(\n"),
    format_parallel_conjuncts(VarNameTable, Indent,
        rgp_cons(RevGoalPath, step_conj(ConjNum)), 1, Conjs, !Report).

:- pred format_parallel_conjuncts(var_name_table::in, int::in,
    reverse_goal_path::in, int::in, list(seq_conj(pard_goal))::in,
    cord(string)::in, cord(string)::out) is det.

format_parallel_conjuncts(_VarNameTable, Indent, _RevGoalPath, _ConjNum0,
        [], !Report) :-
    IndentStr = indent(Indent),
    !:Report = snoc(!.Report ++ IndentStr, ")\n").
format_parallel_conjuncts(VarNameTable, Indent, RevGoalPath, ConjNum0,
        [Conj | Conjs], !Report) :-
    Conj = seq_conj(Goals),
    (
        Goals = [],
        unexpected($pred, "empty conjunct in parallel conjunction")
    ;
        Goals = [Goal | GoalsTail],
        RevInnerGoalPath = rgp_cons(RevGoalPath, step_conj(ConjNum0)),
        (
            GoalsTail = [],
            % A singleton conjunction gets printed as a single goal.
            print_goal_to_strings(print_goal_info(id, VarNameTable),
                Indent + 1, RevInnerGoalPath, Goal, ConjReport)
        ;
            GoalsTail = [_ | _],
            Cost = foldl(
                (func(GoalI, Acc) =
                    Acc + GoalI ^ goal_annotation ^ pga_cost_percall),
                Goals, 0.0),
            format_sequential_conjunction(VarNameTable,
                Indent + 1, RevInnerGoalPath, Goals, Cost, 1, ConjReport)
        )
    ),
    !:Report = !.Report ++ ConjReport,
    (
        Conjs = []
    ;
        Conjs = [_ | _],
        !:Report = cord.snoc(!.Report ++ indent(Indent), "&\n")
    ),
    ConjNum = ConjNum0 + 1,
    format_parallel_conjuncts(VarNameTable, Indent, RevGoalPath, ConjNum,
        Conjs, !Report).

:- pred format_sequential_conjunction(var_name_table::in, int::in,
    reverse_goal_path::in, list(pard_goal)::in, float::in, int::in,
    cord(string)::out) is det.

format_sequential_conjunction(VarNameTable, Indent, RevGoalPath, Goals, Cost,
        FirstConjNum, !:Report) :-
    !:Report = empty,
    ( if FirstConjNum = 1 then
        !:Report = !.Report ++
            indent(Indent) ++
            singleton(format("%% conjunction: %s",
                [s(rev_goal_path_to_string(RevGoalPath))])) ++
            nl_indent(Indent) ++
            singleton(format("%% Cost: %s",
                [s(two_decimal_fraction(Cost))])) ++
            nl ++ nl
    else
        true
    ),
    format_sequential_conjuncts(VarNameTable, Indent, RevGoalPath, Goals,
        FirstConjNum, _, !Report).

:- pred format_sequential_conjuncts(var_name_table::in, int::in,
    reverse_goal_path::in, list(pard_goal)::in, int::in, int::out,
    cord(string)::in, cord(string)::out) is det.

format_sequential_conjuncts(_, _, _, [], !ConjNum, !Report).
format_sequential_conjuncts(VarNameTable, Indent, RevGoalPath, [Conj | Conjs],
        !ConjNum, !Report) :-
    print_goal_to_strings(print_goal_info(id, VarNameTable), Indent,
        rgp_cons(RevGoalPath, step_conj(!.ConjNum)), Conj, ConjReport),
    !:Report = !.Report ++ ConjReport,
    !:ConjNum = !.ConjNum + 1,
    (
        Conjs = []
    ;
        Conjs = [_ | _],
        !:Report = !.Report ++ indent(Indent) ++ singleton(",\n"),
        format_sequential_conjuncts(VarNameTable, Indent, RevGoalPath, Conjs,
            !ConjNum, !Report)
    ).

:- instance goal_annotation(pard_goal_annotation) where [
    pred(print_goal_annotation_to_strings/3) is format_pard_goal_annotation
].

:- pred format_pard_goal_annotation(var_name_table::in,
    pard_goal_annotation::in, cord(cord(string))::out) is det.

format_pard_goal_annotation(VarNameTable, GoalAnnotation, Report) :-
    GoalAnnotation = pard_goal_annotation(CostPercall, CostAboveThreshold,
        Productions, Consumptions),
    (
        CostAboveThreshold = cost_above_par_threshold,
        CostAboveThresholdStr = "above threshold"
    ;
        CostAboveThreshold = cost_not_above_par_threshold,
        CostAboveThresholdStr = "not above threshold"
    ),
    CostLine = singleton(format("cost: %s (%s)",
        [s(two_decimal_fraction(CostPercall)), s(CostAboveThresholdStr)])),
    format_var_use_report(VarNameTable, productions, Productions,
        ProductionsReport),
    format_var_use_report(VarNameTable, consumptions, Consumptions,
        ConsumptionsReport),
    Report = singleton(CostLine) ++ ProductionsReport ++ ConsumptionsReport.

:- func productions = string.

productions = "Productions".

:- func consumptions = string.

consumptions = "Consumptions".

:- pred format_var_use_report(var_name_table::in, string::in,
    assoc_list(var_rep, float)::in, cord(cord(string))::out) is det.

format_var_use_report(VarNameTable, Label, List, Report) :-
    (
        List = [_ | _],
        list.map(format_var_use_line(VarNameTable), List, Lines),
        Report = singleton(singleton(Label ++ ":")) ++ cord.from_list(Lines)
    ;
        List = [],
        Report = empty
    ).

:- pred format_var_use_line(var_name_table::in, pair(var_rep, float)::in,
    cord(string)::out) is det.

format_var_use_line(VarNameTable, Var - Use, singleton(String)) :-
    string.format("    %s: %s", [s(VarName), s(two_decimal_fraction(Use))],
        String),
    lookup_var_name(VarNameTable, Var, VarName).

%---------------------------------------------------------------------------%
:- end_module autopar_reports.
%---------------------------------------------------------------------------%

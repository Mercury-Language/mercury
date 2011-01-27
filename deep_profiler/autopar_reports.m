%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: autopar_reports.m
% Author: pbone.
%
% This module contains code for creating reports for debugging.
%
%-----------------------------------------------------------------------------%

:- module mdprof_fb.automatic_parallelism.autopar_reports.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.feedback.
:- import_module mdbcomp.feedback.automatic_parallelism.
:- import_module mdbcomp.program_representation.

:- import_module cord.
:- import_module pair.

:- pred create_candidate_parallel_conj_proc_report(
    pair(string_proc_label, candidate_par_conjunctions_proc)::in,
    cord(string)::out) is det.

:- pred create_candidate_parallel_conj_report(var_table::in,
    candidate_par_conjunction(pard_goal)::in, cord(string)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.

%----------------------------------------------------------------------------%

create_candidate_parallel_conj_proc_report(Proc - CandidateParConjunctionProc,
        Report) :-
    CandidateParConjunctionProc = candidate_par_conjunctions_proc(VarTable,
        PushGoals, CandidateParConjunctions),
    print_proc_label_to_string(Proc, ProcString),
    list.map(create_push_goal_report, PushGoals, PushGoalReports),
    list.map(create_candidate_parallel_conj_report(VarTable),
        CandidateParConjunctions, CandidateParConjunctionReports),
    Header = string.format(
        "    %s\n",
        [s(ProcString)]),
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

create_candidate_parallel_conj_report(VarTable, CandidateParConjunction,
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
        DependanceString = "no"
    ;
        IsDependent = conjuncts_are_dependent(Vars),
        map(lookup_var_name(VarTable), Vars, VarNames),
        VarsString = join_list(", ", to_sorted_list(VarNames)),
        DependanceString = format("on %s", [s(VarsString)])
    ),
    Speedup = parallel_exec_metrics_get_speedup(ParExecMetrics),
    TimeSaving = parallel_exec_metrics_get_time_saving(ParExecMetrics),
    TotalDeadTime = FirstConjDeadTime + FutureDeadTime,

    string.format(
        "      Path: %s\n",
        [s(GoalPathString)], Header1Str),
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
        [s(DependanceString),
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

    ( rev_goal_path_from_string(GoalPathString, RevGoalPath) ->
        RevGoalPath = rgp(RevGoalPathSteps)
    ;
        unexpected($module, $pred, "couldn't parse goal path")
    ),
    some [!ConjNum] (
        !:ConjNum = FirstConjNum,
        format_sequential_conjunction(VarTable, 4, RevGoalPathSteps,
            GoalsBefore, GoalsBeforeCost, !.ConjNum, ReportGoalsBefore0),
        ReportGoalsBefore = indent(3) ++ singleton("Goals before:\n") ++
            ReportGoalsBefore0,

        !:ConjNum = !.ConjNum + length(GoalsBefore),
        format_parallel_conjunction(VarTable, 4, RevGoalPathSteps,
            !.ConjNum, Conjs, ReportParConj0),
        ReportParConj = indent(3) ++ singleton("Parallel conjunction:\n") ++
            ReportParConj0,

        !:ConjNum = !.ConjNum + 1,
        format_sequential_conjunction(VarTable, 4, RevGoalPathSteps,
            GoalsAfter, GoalsAfterCost, !.ConjNum, ReportGoalsAfter0),
        ReportGoalsAfter = indent(3) ++ singleton("Goals after:\n") ++
            ReportGoalsAfter0
    ),
    Report = Header1 ++ Header2 ++ Header3 ++ ReportGoalsBefore ++ nl
        ++ ReportParConj ++ nl ++ ReportGoalsAfter ++ nl.

:- pred format_parallel_conjunction(var_table::in, int::in,
    list(goal_path_step)::in, int::in,
    list(seq_conj(pard_goal))::in, cord(string)::out) is det.

format_parallel_conjunction(VarTable, Indent, RevGoalPathSteps, ConjNum, Conjs,
        !:Report) :-
    IndentStr = indent(Indent),
    !:Report = IndentStr ++ singleton("(\n"),
    format_parallel_conjuncts(VarTable, Indent,
        [step_conj(ConjNum) | RevGoalPathSteps], 1, Conjs, !Report).

:- pred format_parallel_conjuncts(var_table::in, int::in,
    list(goal_path_step)::in, int::in, list(seq_conj(pard_goal))::in,
    cord(string)::in, cord(string)::out) is det.

format_parallel_conjuncts(_VarTable, Indent, _RevGoalPathSteps, _ConjNum0,
        [], !Report) :-
    IndentStr = indent(Indent),
    !:Report = snoc(!.Report ++ IndentStr, ")\n").
format_parallel_conjuncts(VarTable, Indent, RevGoalPathSteps, ConjNum0,
        [Conj | Conjs], !Report) :-
    Conj = seq_conj(Goals),
    (
        Goals = [],
        unexpected($module, $pred, "empty conjunct in parallel conjunction")
    ;
        Goals = [Goal | GoalsTail],
        RevInnerGoalPathSteps = [step_conj(ConjNum0) | RevGoalPathSteps],
        (
            GoalsTail = [],
            % A singleton conjunction gets printed as a single goal.
            print_goal_to_strings(print_goal_info(id, VarTable), Indent + 1,
                RevInnerGoalPathSteps, Goal, ConjReport)
        ;
            GoalsTail = [_ | _],
            Cost = foldl(
                (func(GoalI, Acc) =
                    Acc + GoalI ^ goal_annotation ^ pga_cost_percall),
                Goals, 0.0),
            format_sequential_conjunction(VarTable, Indent + 1,
                RevInnerGoalPathSteps, Goals, Cost, 1, ConjReport)
        )
    ),
    !:Report = !.Report ++ ConjReport,
    (
        Conjs = []
    ;
        Conjs = [_ | _],
        !:Report = snoc(!.Report ++ indent(Indent), "&\n")
    ),
    ConjNum = ConjNum0 + 1,
    format_parallel_conjuncts(VarTable, Indent, RevGoalPathSteps, ConjNum,
        Conjs, !Report).

:- pred format_sequential_conjunction(var_table::in, int::in,
    list(goal_path_step)::in, list(pard_goal)::in, float::in, int::in,
    cord(string)::out) is det.

format_sequential_conjunction(VarTable, Indent, RevGoalPathSteps, Goals, Cost,
        FirstConjNum, !:Report) :-
    !:Report = empty,
    ( FirstConjNum = 1 ->
        !:Report = !.Report ++
            indent(Indent) ++
            singleton(format("%% conjunction: %s",
                [s(rev_goal_path_to_string(rgp(RevGoalPathSteps)))])) ++
            nl_indent(Indent) ++
            singleton(format("%% Cost: %s",
                [s(two_decimal_fraction(Cost))])) ++
            nl ++ nl
    ;
        true
    ),
    format_sequential_conjuncts(VarTable, Indent, RevGoalPathSteps, Goals,
        FirstConjNum, _, !Report).

:- pred format_sequential_conjuncts(var_table::in, int::in,
    list(goal_path_step)::in, list(pard_goal)::in, int::in, int::out,
    cord(string)::in, cord(string)::out) is det.

format_sequential_conjuncts(_, _, _, [], !ConjNum, !Report).
format_sequential_conjuncts(VarTable, Indent, RevGoalPathSteps, [Conj | Conjs],
        !ConjNum, !Report) :-
    print_goal_to_strings(print_goal_info(id, VarTable), Indent,
        [step_conj(!.ConjNum) | RevGoalPathSteps], Conj, ConjReport),
    !:Report = !.Report ++ ConjReport,
    !:ConjNum = !.ConjNum + 1,
    (
        Conjs = []
    ;
        Conjs = [_ | _],
        !:Report = !.Report ++ indent(Indent) ++ singleton(",\n"),
        format_sequential_conjuncts(VarTable, Indent, RevGoalPathSteps, Conjs,
            !ConjNum, !Report)
    ).

:- instance goal_annotation(pard_goal_annotation) where [
    pred(print_goal_annotation_to_strings/3) is format_pard_goal_annotation
].

:- pred format_pard_goal_annotation(var_table::in, pard_goal_annotation::in,
    cord(cord(string))::out) is det.

format_pard_goal_annotation(VarTable, GoalAnnotation, Report) :-
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
    format_var_use_report(VarTable, productions, Productions,
        ProductionsReport),
    format_var_use_report(VarTable, consumptions, Consumptions,
        ConsumptionsReport),
    Report = singleton(CostLine) ++ ProductionsReport ++ ConsumptionsReport.

:- func productions = string.

productions = "Productions".

:- func consumptions = string.

consumptions = "Consumptions".

:- pred format_var_use_report(var_table::in, string::in,
    assoc_list(var_rep, float)::in, cord(cord(string))::out) is det.

format_var_use_report(VarTable, Label, List, Report) :-
    (
        List = [_ | _],
        list.map(format_var_use_line(VarTable), List, Lines),
        Report = singleton(singleton(Label ++ ":")) ++ cord.from_list(Lines)
    ;
        List = [],
        Report = empty
    ).

:- pred format_var_use_line(var_table::in, pair(var_rep, float)::in,
    cord(string)::out) is det.

format_var_use_line(VarTable, Var - Use, singleton(String)) :-
    format("    %s: %s", [s(VarName), s(two_decimal_fraction(Use))], String),
    lookup_var_name(VarTable, Var, VarName).

%-----------------------------------------------------------------------------%

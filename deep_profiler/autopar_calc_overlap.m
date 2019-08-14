%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: autopar_calc_overlap.m
% Author: pbone.
%
% This module contains the code that calculates the likely overlap
% between conjuncts in a parallelized conjunction.
%
%---------------------------------------------------------------------------%

:- module mdprof_fb.automatic_parallelism.autopar_calc_overlap.
:- interface.

:- import_module mdprof_fb.automatic_parallelism.autopar_types.

    % calculate_parallel_cost(Info, !Parallelisation, CostData).
    %
    % Analyse the parallel conjuncts and determine their likely performance.
    %
    % This is the new parallel execution overlap algorithm, it is general and
    % therefore we also use it for independent conjunctions.
    %
:- pred calculate_parallel_cost(implicit_parallelism_info::in,
    incomplete_parallelisation::in, incomplete_parallelisation::out,
    parallelisation_cost_data::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.feedback.
:- import_module mdbcomp.feedback.automatic_parallelism.
:- import_module mdbcomp.program_representation.
:- import_module mdprof_fb.automatic_parallelism.autopar_costs.
:- import_module measurements.
:- import_module var_use_analysis.

:- import_module assoc_list.
:- import_module digraph.
:- import_module float.
:- import_module int.
:- import_module lazy.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

calculate_parallel_cost(Info, !Parallelisation, CostData) :-
    ParConj = ip_get_par_conjs(!.Parallelisation),
    NumCalls = !.Parallelisation ^ ip_num_calls,

    maybe_calc_sequential_cost(
        (func(P) = P ^ ip_maybe_goals_before_cost),
        (func(P0, MaybeCost) = P0 ^ ip_maybe_goals_before_cost := MaybeCost),
        ip_get_goals_before, NumCalls, CostBeforePercall, !Parallelisation),
    maybe_calc_sequential_cost(
        (func(P) = P ^ ip_maybe_goals_after_cost),
        (func(P0, MaybeCost) = P0 ^ ip_maybe_goals_after_cost := MaybeCost),
        ip_get_goals_after, NumCalls, CostAfterPercall, !Parallelisation),

    Opts = Info ^ ipi_opts,
    SparkCost = Opts ^ cpcp_sparking_cost,
    SparkDelay = Opts ^ cpcp_sparking_delay,
    BarrierCost = Opts ^ cpcp_barrier_cost,
    ContextWakeupDelay = Opts ^ cpcp_context_wakeup_delay,
    Metrics0 = init_empty_parallel_exec_metrics(CostBeforePercall,
        CostAfterPercall, NumCalls, float(SparkCost), float(SparkDelay),
        float(BarrierCost), float(ContextWakeupDelay)),
    Overlap0 = peo_empty_conjunct,

    SharedVars = ip_calc_sharedvars_set(!.Parallelisation),
    CostData0 = parallelisation_cost_data(SharedVars, Overlap0, Metrics0,
        init),
    NumMiddleGoals = ip_get_num_goals_middle(!.Parallelisation),
    list.foldl3(calculate_parallel_cost_step(Info, NumMiddleGoals), ParConj,
        1, _, 0, _, CostData0, CostData),
    !Parallelisation ^ ip_maybe_par_cost_data := yes(CostData).

:- pred maybe_calc_sequential_cost((func(T) = maybe(goal_cost_csq))::in,
    (func(T, maybe(goal_cost_csq)) = T)::in,
    (func(T) = list(pard_goal_detail))::in,
    int::in, float::out, T::in, T::out) is det.

maybe_calc_sequential_cost(GetMaybe, SetMaybe, GetGoals, Calls, CostPercall,
        !Acc) :-
    MaybeCost = GetMaybe(!.Acc),
    (
        MaybeCost = yes(Cost)
    ;
        MaybeCost = no,
        Goals = GetGoals(!.Acc),
        conj_calc_cost(Goals, Calls, Cost),
        !:Acc = SetMaybe(!.Acc, yes(Cost))
    ),
    CostPercall = goal_cost_get_percall(Cost).

:- type is_last_par_conjunct
    --->    is_last_par_conjunct
    ;       not_last_par_conjunct.

:- pred calculate_parallel_cost_step(implicit_parallelism_info::in,
    int::in, seq_conj(pard_goal_detail)::in, int::in, int::out,
    int::in, int::out,
    parallelisation_cost_data::in, parallelisation_cost_data::out) is det.

calculate_parallel_cost_step(Info, NumMiddleGoals, Conjunct, !ConjNum,
        !NumGoals, !CostData) :-
    !.CostData = parallelisation_cost_data(SharedVars, Overlap0, Metrics0,
        PM0),
    !:NumGoals = !.NumGoals + length(Conjuncts),
    ( if !.NumGoals = NumMiddleGoals then
        IsLastConjunct = is_last_par_conjunct
    else
        IsLastConjunct = not_last_par_conjunct
    ),
    Conjunct = seq_conj(Conjuncts),
    calculate_parallel_cost_step(Info, SharedVars, IsLastConjunct, Conjuncts,
        !ConjNum, PM0, PM, Overlap0, Overlap, Metrics0, Metrics),
    !:CostData = parallelisation_cost_data(SharedVars, Overlap, Metrics, PM).

:- pred calculate_parallel_cost_step(implicit_parallelism_info::in,
    set(var_rep)::in, is_last_par_conjunct::in, list(pard_goal_detail)::in,
    int::in, int::out, map(var_rep, float)::in, map(var_rep, float)::out,
    parallel_execution_overlap::in, parallel_execution_overlap::out,
    parallel_exec_metrics_incomplete::in,
    parallel_exec_metrics_incomplete::out) is det.

calculate_parallel_cost_step(Info, AllSharedVars, IsLastConjunct, Conjunct,
        !ConjNum, !ProductionsMap, !Overlap, !Metrics) :-
    Algorithm = Info ^ ipi_opts ^ cpcp_parallelise_dep_conjs,

    Calls = parallel_exec_metrics_get_num_calls(!.Metrics),
    conj_calc_cost(Conjunct, Calls, CostB0),
    CostB = goal_cost_get_percall(CostB0),
    list.foldl2(conj_produced_and_consumed_vars, Conjunct,
        set.init, RightProducedVars0, set.init, RightConsumedVars0),
    RightProducedVars = set.intersect(RightProducedVars0, AllSharedVars),
    RightConsumedVars = set.intersect(RightConsumedVars0, AllSharedVars),
    ProducedVars =
        set.from_sorted_list(map.sorted_keys(!.ProductionsMap)),
    Vars = set.intersect(ProducedVars, RightConsumedVars),

    % This conjunct will actually start after it has been sparked by
    % the previous conjunct, which in turn may have been sparked by an
    % earlier conjunct.
    SparkDelay = Info ^ ipi_opts ^ cpcp_sparking_delay,
    StartTime0 = float((!.ConjNum - 1) * SparkDelay),

    % If there are conjuncts after this conjunct, we will have
    % the additional cost of sparking them.
    (
        IsLastConjunct = not_last_par_conjunct,
        SparkCost = float(Info ^ ipi_opts ^ cpcp_sparking_cost)
    ;
        IsLastConjunct = is_last_par_conjunct,
        SparkCost = 0.0
    ),
    StartTime = StartTime0 + SparkCost,

    (
        Algorithm = parallelise_dep_conjs(estimate_speedup_by_overlap),

        % Get the list of variables consumed by this conjunct
        % that will be turned into futures.
        list.foldl4(get_consumptions_and_productions_list, Conjunct, Vars, _,
            RightProducedVars, _, 0.0, _,
            [], ConsumptionsAndProductionsList0),
        list.reverse(ConsumptionsAndProductionsList0,
            ConsumptionsAndProductionsList),

        % Determine how the parallel conjuncts overlap.
        list.foldl5(
            calculate_dependent_parallel_cost_2(Info, !.ProductionsMap),
            ConsumptionsAndProductionsList, 0.0, LastSeqConsumeTime,
            StartTime, LastParConsumeTime, StartTime, LastResumeTime,
            [], RevExecution0, map.init, ConsumptionsMap),

        % Calculate the point at which this conjunct finishes execution
        % and complete the RevExecutions structure.
        list.reverse(RevExecution, Execution),
        CostBParElapsed = LastParConsumeTime + (CostB - LastSeqConsumeTime),
        RevExecution = [ (LastResumeTime - CostBParElapsed) | RevExecution0 ],

        CostSignals = float(Info ^ ipi_opts ^ cpcp_future_signal_cost *
            count(RightProducedVars)),
        CostWaits = float(Info ^ ipi_opts ^ cpcp_future_wait_cost *
            count(Vars)),
        calc_cost_and_dead_time(Execution, CostBPar, DeadTime)
    ;
        ( Algorithm = do_not_parallelise_dep_conjs
        ; Algorithm = parallelise_dep_conjs(estimate_speedup_naively)
        ),

        CostBPar = CostB + SparkCost,
        Execution = [StartTime - (StartTime + CostB)],
        ConsumptionsMap = init,
        CostSignals = 0.0,
        CostWaits = 0.0,
        DeadTime = 0.0
    ),

    % CostB    - the cost of B if it were to be executed in sequence.
    % CostBPar - CostB plus the overheads of parallel execution (not including
    %            the dead time).
    % DeadTime - The time that B spends blocked, waiting on other computations.
    % XXX: Need to account for SparkDelay here,
    !:Metrics = init_parallel_exec_metrics_incomplete(!.Metrics, CostSignals,
        CostWaits, CostB, CostBPar, DeadTime),

    % Build the productions map for the next conjunct. This map contains
    % all the variables produced by this code, not just that are used for
    % dependent parallelisation.
    list.foldl3(get_productions_map(RightProducedVars), Conjunct, StartTime, _,
        Execution, _, !ProductionsMap),

    DepConjExec = dependent_conjunct_execution(Execution,
        !.ProductionsMap, ConsumptionsMap),
    !:Overlap = peo_conjunction(!.Overlap, DepConjExec, Vars),
    !:ConjNum = !.ConjNum + 1.

    % calculate_dependent_parallel_cost_2(Info, ProductionsMap,
    %   Var - SeqConsTime, !PrevSeqConsumeTime, !PrevParConsumeTime,
    %   !ResumeTime, !RevExecution, !ConsumptionsMap).
    %
    % The main loop of the parallel overlap analysis.
    %
    % * ProductionsMap: A map of variable productions to the left of this
    %   conjunct.
    %
    % * Var: The current variable under consideration.
    %
    % * SeqConsTime: The type of event for this variable in this conjunct and
    %   the time at which it occurs.  It is either consumed or produced by this
    %   conjunct.
    %
    % * !PrevSeqConsumeTime: Accumulates the time of the previous consumption
    %   during sequential execution, or if there is none it represents the
    %   beginning of sequential execution (0.0).
    %
    % * !PrevParConsumeTime: Accumulates the time of the previous consumption
    %   during parallel execution.  Or if there is none this represents the
    %   tame that the parallel conjunct first begun execution.
    %
    % * !ResumeTime: Accumulates the time that execution last resumed if it
    %   became blocked, or the beginning of the parallel conjunct's execution.
    %
    % * !RevExecution: Accumulates a list of pairs, each pair stores the time
    %   that execution begun and the time that it pasted.  This never includes
    %   the remaining execution after all variables have been consumed.  This
    %   is used by our caller to calculate the production times of this
    %   conjunct for later ones.
    %
    % * !ConsumptionsMap: Accumulates a map of variable consumptions.
    %
:- pred calculate_dependent_parallel_cost_2(implicit_parallelism_info::in,
    map(var_rep, float)::in, pair(var_rep, production_or_consumption)::in,
    float::in, float::out, float::in, float::out, float::in, float::out,
    assoc_list(float, float)::in, assoc_list(float, float)::out,
    map(var_rep, float)::in, map(var_rep, float)::out) is det.

calculate_dependent_parallel_cost_2(Info, ProductionsMap, Var - SeqEventTime,
        !PrevSeqConsumeTime, !PrevParConsumeTime, !ResumeTime,
        !RevExecution, !ConsumptionsMap) :-
    (
        SeqEventTime = consumption(SeqConsTime),
        calculate_dependent_parallel_cost_consumption(Info, ProductionsMap,
            Var - SeqConsTime, !PrevSeqConsumeTime, !PrevParConsumeTime,
            !ResumeTime, !RevExecution, !ConsumptionsMap)
    ;
        SeqEventTime = production(SeqProdTime),
        calculate_dependent_parallel_cost_production(Info, SeqProdTime,
            !PrevSeqConsumeTime, !PrevParConsumeTime, !ResumeTime,
            !RevExecution, !ConsumptionsMap)
    ).

:- pred calculate_dependent_parallel_cost_consumption(
    implicit_parallelism_info::in, map(var_rep, float)::in,
    pair(var_rep, float)::in, float::in, float::out,
    float::in, float::out, float::in, float::out,
    assoc_list(float, float)::in, assoc_list(float, float)::out,
    map(var_rep, float)::in, map(var_rep, float)::out) is det.

calculate_dependent_parallel_cost_consumption(Info, ProductionsMap,
        Var - SeqConsTime, !PrevSeqConsumeTime, !PrevParConsumeTime,
        !ResumeTime, !RevExecution, !ConsumptionsMap) :-
    map.lookup(ProductionsMap, Var, ProdTime),
    % Consider (P & Q):
    %
    % Q cannot consume the variable until P produces it. Also Q cannot consume
    % the variable until it is ready for it. These are the two parameters to
    % max/2.
    %
    % The second parameter can be explained further. Q may have waited on a
    % future previously, if so !.PrevParConsumeTime is when it finished
    % waiting, and SeqConsTime - !.PrevSeqConsumeTime is how long Q will take
    % between the two waits.
    %
    ParConsTimeBlocked = ProdTime,
    ParConsTimeNotBlocked = !.PrevParConsumeTime +
        (SeqConsTime - !.PrevSeqConsumeTime),
    ParConsTime0 = max(ParConsTimeBlocked, ParConsTimeNotBlocked) +
        float(Info ^ ipi_opts ^ cpcp_future_wait_cost),

    ( if
        % True if Q had to suspend waiting for P. Note that we don't include
        % FutureSyncTime here. This is true if Q has to block at all even if
        % it can be made runnable before the context switch is complete.
        ProdTime > ParConsTimeNotBlocked
    then
        % Include the time that it may take to resume this thread.
        ParConsTime = ParConsTime0 +
            float(Info ^ ipi_opts ^ cpcp_context_wakeup_delay),
        !:RevExecution =
            [(!.ResumeTime - ParConsTimeNotBlocked) | !.RevExecution],
        !:ResumeTime = ParConsTime
    else
        ParConsTime = ParConsTime0
    ),

    !:PrevSeqConsumeTime = SeqConsTime,
    !:PrevParConsumeTime = ParConsTime,

    map.det_insert(Var, ParConsTime, !ConsumptionsMap).

:- pred calculate_dependent_parallel_cost_production(
    implicit_parallelism_info::in, float::in, float::in, float::out,
    float::in, float::out, float::in, float::out,
    assoc_list(float, float)::in, assoc_list(float, float)::out,
    map(var_rep, float)::in, map(var_rep, float)::out) is det.

calculate_dependent_parallel_cost_production(Info,
        SeqProdTime, !PrevSeqConsumeTime, !PrevParConsumeTime,
        !ResumeTime, !RevExecution, !ConsumptionsMap) :-
    SignalCost = float(Info ^ ipi_opts ^ cpcp_future_signal_cost),

    ParProdTime = !.PrevParConsumeTime +
        (SeqProdTime - !.PrevSeqConsumeTime) + SignalCost,
    !:PrevSeqConsumeTime = SeqProdTime,
    !:PrevParConsumeTime = ParProdTime.

    % Abstract code for querying a graph for a goal dependency.
    %
:- pred graph_do_lookup(
    pred(digraph(int), digraph_key(int), set(digraph_key(int)))::
        in(pred(in, in, out) is det),
    digraph(int)::in, int::in, set(int)::out) is det.

graph_do_lookup(Lookup, Graph, GoalNum, Deps) :-
    Lookup(Graph, lookup_key(Graph, GoalNum), DepsKeys),
    Deps = set(map(lookup_vertex(Graph), set.to_sorted_list(DepsKeys))).

    % foldl(get_productions_map(Goals, 0,0, _, Vars, _, map.init, Map).
    %
    % If Goals is semidet this can produce incorrect values in the !Time
    % accumulator that lead to exceptions.  This is prevented by only
    % attempting to parallelise goals that are det or cc_multi.
    %
    % Build a map of variable productions in Goals.
    %
:- pred get_productions_map(set(var_rep)::in, pard_goal_detail::in,
    float::in, float::out,
    assoc_list(float, float)::in, assoc_list(float, float)::out,
    map(var_rep, float)::in, map(var_rep, float)::out) is det.

get_productions_map(Vars, Goal, !Time, !Executions, !Map) :-
    InstMapInfo = Goal ^ goal_annotation ^ pgd_inst_map_info,
    BoundVars0 = InstMapInfo ^ im_bound_vars,
    BoundVars = set.intersect(BoundVars0, Vars),
    adjust_time_for_waits(!Time, !Executions),
    set.fold(var_production_time_to_map(!.Time, Goal), BoundVars, !Map),
    !:Time = !.Time + goal_cost_get_percall(Goal ^ goal_annotation ^ pgd_cost).

:- pred adjust_time_for_waits(float::in, float::out,
    assoc_list(float, float)::in, assoc_list(float, float)::out) is det.

adjust_time_for_waits(!Time, !Executions) :-
    (
        !.Executions = [Execution | NextExecution],
        ( Start - End ) = Execution,
        ( if (!.Time + adjust_time_for_waits_epsilon) < Start then
            unexpected($pred, "Time occurs before the current execution")
        else if !.Time =< (End + adjust_time_for_waits_epsilon) then
            % The production is within the current execution, no adjustment is
            % necessary.
            true
        else
            % The time is after this execution.
            !:Executions = NextExecution,
            adjust_time_for_waits_2(End, !Time, !Executions)
        )
    ;
        !.Executions = [],
        unexpected($pred, "Time occurs after all executions")
    ).

:- pred adjust_time_for_waits_2(float::in, float::in, float::out,
    assoc_list(float, float)::in, assoc_list(float, float)::out) is det.

adjust_time_for_waits_2(LastEnd, !Time, !Executions) :-
    (
        !.Executions = [ Execution | NextExecution ],
        ( Start - End ) = Execution,

        % Do the adjustment.
        !:Time = !.Time + (Start - LastEnd),

        ( if (!.Time + adjust_time_for_waits_epsilon) < Start then
            unexpected($pred,
                string.format("Adjustment didn't work, " ++
                "time occurs before the current execution. " ++
                "Time: %f, Start: %f.", [f(!.Time), f(Start)]))
        else if !.Time =< (End + adjust_time_for_waits_epsilon) then
            % The adjustment worked.
            true
        else
            % Further adjustment is needed.
            !:Executions = NextExecution,
            adjust_time_for_waits_2(End, !Time, !Executions)
        )
    ;
        !.Executions = [],
        error("adjust_time_for_waits: Ran out of executions")
    ).

:- func adjust_time_for_waits_epsilon = float.

adjust_time_for_waits_epsilon = 0.0001.

    % Calculate the time spend during execution and the time spent between
    % executions (dead time).
    %
:- pred calc_cost_and_dead_time(assoc_list(float, float)::in, float::out,
    float::out) is det.

calc_cost_and_dead_time([], 0.0, 0.0).
calc_cost_and_dead_time([Start - Stop | Executions], !:Time, DeadTime) :-
    !:Time = Stop - Start,
    calc_cost_and_dead_time_2(Executions, Stop, !Time, 0.0, DeadTime).

:- pred calc_cost_and_dead_time_2(assoc_list(float, float)::in, float::in,
    float::in, float::out, float::in, float::out) is det.

calc_cost_and_dead_time_2([], _, !Time, !DeadTime).
calc_cost_and_dead_time_2([Start - Stop | Executions], LastStop,
        !Time, !DeadTime) :-
    !:Time = !.Time + Stop - Start,
    !:DeadTime = !.DeadTime + Start - LastStop,
    calc_cost_and_dead_time_2(Executions, Stop, !Time, !DeadTime).

    % var_production_time_to_map(TimeBefore, Goal, Var, !Map).
    %
    % Find the latest production time of Var in Goal, and add TimeBefore + the
    % production time to the map.  An exception is thrown if a duplicate map
    % entry is found, our caller must prevent this.
    %
:- pred var_production_time_to_map(float::in, pard_goal_detail::in,
    var_rep::in, map(var_rep, float)::in, map(var_rep, float)::out) is det.

var_production_time_to_map(TimeBefore, Goal, Var, !Map) :-
    var_first_use_time(find_production, TimeBefore, Goal, Var, Time),
    map.det_insert(Var, Time, !Map).

    % Either a production or consumption time.  Consumptions should sort before
    % productions.
    %
:- type production_or_consumption
    --->    consumption(float)
    ;       production(float).

    % foldl(get_consumptions_list(Vars), Goals, 0.0, _, [], RevConsumptions),
    %
    % Compute the order and time of variable consumptions in goals.
    %
:- pred get_consumptions_and_productions_list(pard_goal_detail::in,
    set(var_rep)::in, set(var_rep)::out,
    set(var_rep)::in, set(var_rep)::out, float::in, float::out,
    assoc_list(var_rep, production_or_consumption)::in,
    assoc_list(var_rep, production_or_consumption)::out) is det.

get_consumptions_and_productions_list(Goal, !ConsumedVars, !ProducedVars,
        !Time, !List) :-
    InstMapInfo = Goal ^ goal_annotation ^ pgd_inst_map_info,

    AllConsumptionVars = InstMapInfo ^ im_consumed_vars,
    ConsumptionVars = set.intersect(!.ConsumedVars, AllConsumptionVars),
    set.map(var_consumptions(!.Time, Goal),
        ConsumptionVars, ConsumptionTimesSet0),
    !:ConsumedVars = difference(!.ConsumedVars, ConsumptionVars),
    % Since we re-sort the list we don't need a sorted one to start with,
    % but the set module doesn't export a "to_list" predicate. (Getting
    % a sorted list has no cost since the set is a sorted list internally).
    set.to_sorted_list(ConsumptionTimesSet0, ConsumptionTimes0),
    list.sort(compare_times, ConsumptionTimes0, ConsumptionTimes),

    AllProductionVars = InstMapInfo ^ im_bound_vars,
    ProductionVars = set.intersect(!.ProducedVars, AllProductionVars),
    set.map(var_productions(!.Time, Goal),
        ProductionVars, ProductionTimesSet0),
    !:ProducedVars = difference(!.ProducedVars, ProductionVars),
    set.to_sorted_list(ProductionTimesSet0, ProductionTimes0),
    list.sort(compare_times, ProductionTimes0, ProductionTimes),

    merge_consumptions_and_productions(ConsumptionTimes, ProductionTimes,
        ConsumptionAndProductionTimes),
    !:List = ConsumptionAndProductionTimes ++ !.List,
    !:Time = !.Time + goal_cost_get_percall(Goal ^ goal_annotation ^ pgd_cost).

:- pred compare_times(pair(A, float)::in, pair(A, float)::in,
    comparison_result::out) is det.

compare_times(_ - TimeA, _ - TimeB, Result) :-
    % Note that the Time arguments are swapped, this list must be
    % produced in latest to earliest order.
    compare(Result, TimeB, TimeA).

:- pred merge_consumptions_and_productions(
    assoc_list(var_rep, float)::in, assoc_list(var_rep, float)::in,
    assoc_list(var_rep, production_or_consumption)::out) is det.

merge_consumptions_and_productions([], [], []).
merge_consumptions_and_productions([],
        [Var - Time | Prods0], [Var - production(Time) | Prods]) :-
    merge_consumptions_and_productions([], Prods0, Prods).
merge_consumptions_and_productions([Var - Time | Cons0], [],
        [Var - consumption(Time) | Cons]) :-
    merge_consumptions_and_productions(Cons0, [], Cons).
merge_consumptions_and_productions(Cons@[ConsVar - ConsTime | Cons0],
        Prods@[ProdVar - ProdTime | Prods0], [ProdOrCons | ProdsAndCons]) :-
    ( if ProdTime < ConsTime then
        % Order earlier events first,
        ProdOrCons = ProdVar - production(ProdTime),
        merge_consumptions_and_productions(Cons, Prods0, ProdsAndCons)
    else
        % In this branch either the consumption occurs first or the events
        % occur at the same time in which case we order consumptions first.
        ProdOrCons = ConsVar - consumption(ConsTime),
        merge_consumptions_and_productions(Cons0, Prods, ProdsAndCons)
    ).

:- pred var_consumptions(float::in, pard_goal_detail::in, var_rep::in,
    pair(var_rep, float)::out) is det.

var_consumptions(TimeBefore, Goal, Var, Var - Time) :-
    var_first_use_time(find_consumption, TimeBefore, Goal, Var, Time).

:- pred var_productions(float::in, pard_goal_detail::in, var_rep::in,
    pair(var_rep, float)::out) is det.

var_productions(TimeBefore, Goal, Var, Var - Time) :-
    var_first_use_time(find_production, TimeBefore, Goal, Var, Time).

:- type find_production_or_consumption
    --->    find_production
    ;       find_consumption.

    % var_first_use_time(FindProdOrCons, Time0, Goal, Var, Time).
    %
    % if FindProdOrCons = find_production:
    %   Time is Time0 + the time that Goal produces Var.
    % if FindProdOrCons = find_consumption:
    %   Time is Time0 + the time that Goal first consumes Var.
    %
:- pred var_first_use_time(find_production_or_consumption::in,
    float::in, pard_goal_detail::in, var_rep::in, float::out) is det.

var_first_use_time(FindProdOrCons, TimeBefore, Goal, Var, Time) :-
    (
        FindProdOrCons = find_production,
        Map = Goal ^ goal_annotation ^ pgd_var_production_map
    ;
        FindProdOrCons = find_consumption,
        Map = Goal ^ goal_annotation ^ pgd_var_consumption_map
    ),
    map.lookup(Map, Var, LazyUse),
    Use = force(LazyUse),
    UseType = Use ^ vui_use_type,
    (
        (
            UseType = var_use_production,
            (
                FindProdOrCons = find_production
            ;
                FindProdOrCons = find_consumption,
                unexpected($pred,
                    "Found production when looking for consumption")
            )
        ;
            UseType = var_use_consumption,
            (
                FindProdOrCons = find_production,
                unexpected($pred,
                    "Found consumption when looking for production")
            ;
                FindProdOrCons = find_consumption
            )
        ),
        UseTime = Use ^ vui_cost_until_use
    ;
        UseType = var_use_other,
        % The analysis didn't recognise the instantiation here,
        % so use a conservative default for the production time.
        % XXX: How often does this occur?
        (
            FindProdOrCons = find_production,
            UseTime = goal_cost_get_percall(Goal ^ goal_annotation ^ pgd_cost)
        ;
            FindProdOrCons = find_consumption,
            UseTime = 0.0
        )
    ),
    Time = TimeBefore + UseTime.

%---------------------------------------------------------------------------%

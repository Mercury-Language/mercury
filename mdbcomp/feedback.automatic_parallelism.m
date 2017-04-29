%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: feedback.automatic_parallelism.m.
% Main author: pbone.
%
% This module defines data structures for representing automatic parallelism
% feedback information and some procedures for working with these structures.
%
% NOTE: After modifying any of these structures please increment the
% feedback_version in feedback.m
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdbcomp.feedback.automatic_parallelism.

:- interface.

:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

:- type stat_measure
    --->    stat_mean
    ;       stat_median.

:- type candidate_par_conjunctions_params
    --->    candidate_par_conjunctions_params(
                % The number of desired busy sparks.
                cpcp_desired_parallelism    :: float,

                % Should we follow variable use across module boundaries?
                cpcp_intermodule_var_use    :: bool,

                % The cost of creating a spark and adding it to the local
                % work queue, measured in call sequence counts.
                cpcp_sparking_cost          :: int,

                % The time taken between the creation of the spark and when
                % it starts being executed, measured in call sequence counts.
                cpcp_sparking_delay         :: int,

                % The cost of barrier synchronisation for each conjunct at the
                % end of the parallel conjunction.
                cpcp_barrier_cost           :: int,

                % The costs of maintaining a lock on a single dependent
                % variable, measured in call sequence counts. The first number
                % gives the cost of the call to signal, and the second gives
                % the cost of the call to wait assuming that the value is
                % already available.
                cpcp_future_signal_cost     :: int,
                cpcp_future_wait_cost       :: int,

                % The time it takes for a context to resume execution once
                % it has been put on the runnable queue, assuming that an
                % engine is available to pick it up. Measured in call sequence
                % counts.
                %
                % We use this to calculate how soon a context can recover
                % after being blocked by a future. It is also used to determine
                % how quickly the context executing MR_join_and_continue after
                % completing the leftmost conjunct of a parallel conjunction
                % can recover after being blocked on the completion of
                % one of the other conjuncts.
                cpcp_context_wakeup_delay   :: int,

                % The cost threshold in call sequence counts of a clique
                % before we consider it for parallel execution.
                cpcp_clique_threshold       :: int,

                % The cost threshold in call sequence counts of a call site
                % before we consider it for parallel execution.
                cpcp_call_site_threshold    :: int,

                % The speedup we require before we allow a conjunction to be
                % automatically parallelised. Should be either exactly 1.0
                % or just above 1.0.
                cpcp_speedup_threshold      :: float,

                % Whether we will allow parallelisation to result in
                % dependent parallel conjunctions, and if so, how we estimate
                % the speedup we get for them.
                cpcp_parallelise_dep_conjs  :: parallelise_dep_conjs,

                cpcp_alg_for_best_par       :: alg_for_finding_best_par
            ).

:- type parallelise_dep_conjs
    --->    do_not_parallelise_dep_conjs
    ;       parallelise_dep_conjs(speedup_estimate_alg).

:- type speedup_estimate_alg
    --->    estimate_speedup_naively
            % Be naive to dependent parallelism, pretend it is independent.

    ;       estimate_speedup_by_overlap.
            % Use the overlap calculation for dependent parallelism.

    % This type is used to select which algorithm is used to find the most
    % profitable parallelisation of a particular conjunction.
    %
    % TODO: The type name could be improved to make it distinct from the
    % algorithm used to search through the clique graph.
    %
:- type alg_for_finding_best_par
    --->    affbp_complete_branches(
                % Use the complete algorithm until this many branches have been
                % created during the search, and then fall back to the greedy
                % algorithm. After such a fall back, all existing alternatives
                % will be explored, but no new ones will be generated.
                int
            )
    ;       affbp_complete_size(
                % Use the complete algorithm for conjunctions with fewer than
                % this many conjuncts, or a greedy algorithm. The recommended
                % value is 50.
                % XXX I (zs) think that 50 seems way too high.
                int
            )
    ;       affbp_complete
            % Use the complete brand-and-bound algorithm with no fallback.
    ;       affbp_greedy.
            % Use the linear greedy algorithm.

    % The set of candidate parallel conjunctions within a procedure.
    %
:- type candidate_par_conjunctions_proc(GoalType)
    --->    candidate_par_conjunctions_proc(
                % A variable name table for the variables that have
                % sensible names.
                cpcp_var_table  :: var_name_table,

                % Each push represents a program transformation.
                % Most of the time, we expect the list to be empty,
                % but if it isn't, then the list of candidate conjunctions
                % is valid only AFTER the transformations described
                % by this list have been applied. (The transformations
                % should be independent of one another, so it should be
                % OK to apply them in any order.)
                cpcp_push_goals :: list(push_goal),

                cpcp_par_conjs  :: list(candidate_par_conjunction(GoalType))
            ).

    % This goal describes 'push goal' transformations.
    %
    % This is where a goal may be pushed into the arms of a branching goal that
    % occurs before it in the same conjunction. It can allow the pushed goal
    % to be parallelised against goals in one or more branches without
    % parallelising the whole branch goal (whose per-call cost may be
    % too small).
    %
:- type push_goal
    --->    push_goal(
                % The goal path of the conjunction in which the push is done.
                pg_goal_path    :: goal_path_string,

                % The range of conjuncts to push (inclusive).
                pg_pushee_lo    :: int,
                pg_pushee_hi    :: int,

                % The set of expensive goals inside earlier conjuncts in that
                % conjunction "next" to which the pushee goals should be
                % pushed. By "next", we mean that the pushee goals should be
                % added to the end of whatever conjunction contains the
                % expensive goal, creating a containing conjunction if
                % there wasn't one there before.
                %
                % Each of these expensive goals should be on a different
                % execution path.
                %
                % This list should not be empty.
                pg_pushed_into  :: list(goal_path_string)
            ).

:- type candidate_par_conjunctions_proc ==
    candidate_par_conjunctions_proc(pard_goal).

    % A conjunction that is a candidate for parallelisation, it is identified
    % by a procedure label, goal path to the conjunction and the call sites
    % within the conjunction that are to be parallelised.
    %
    % TODO: In the future support more expressive candidate parallel
    % conjunctions, so that more opportunities for parallelism can be found.
    % Although it's probably not a good idea to parallelise three conjuncts or
    % more against one another without first having a good system for reaching
    % and maintaining the target amount of parallelism, this may involve
    % distance granularity.
    %
:- type candidate_par_conjunction(GoalType)
    --->    candidate_par_conjunction(
                % The path within the procedure to this conjunction.
                cpc_goal_path           :: goal_path_string,

                % If the candidate is dependent on a push being performed,
                % what is that push? Note that any push that specifies the same
                % goals being pushed and the same OR GREATER set of goals next
                % to which to push them is acceptable: if such a push is
                % performed, then this candidate is viable.
                cpc_maybe_push_goal     :: maybe(push_goal),

                % The position within the original conjunction that this
                % parallelisation starts.
                cpc_first_conj_num      :: int,

                cpc_is_dependent        :: conjuncts_are_dependent,

                cpc_goals_before        :: list(GoalType),
                cpc_goals_before_cost   :: float,

                % A list of parallel conjuncts, each is a sequential
                % conjunction of inner goals. All inner goals that are
                % seen in the program presentation must be stored here
                % unless they are to be scheduled before or after the
                % sequential conjunction. If these conjuncts are flattened,
                % the inner goals will appear in the same order as the
                % program representation. By maintaining these two rules
                % the compiler and analysis tools can use similar
                % algorithms to construct the same parallel conjunction
                % from the same program representation/HLDS structure.

                cpc_conjs               :: list(seq_conj(GoalType)),

                cpc_goals_after         :: list(GoalType),
                cpc_goals_after_cost    :: float,

                cpc_par_exec_metrics    :: parallel_exec_metrics
            ).

:- type seq_conj(GoalType)
    --->    seq_conj(
                sc_conjs            :: list(GoalType)
            ).

:- type callee_rep
    --->    unknown_callee
            % An unknown callee such as a higher order or method call.

    ;       named_callee(
                % A known callee. Note that arity and mode are not stored at
                % all. XXX why?

                nc_module_name  :: string,
                nc_proc_name    :: string
            ).

    % A parallelised goal (pard_goal), a goal within a parallel conjunction.
    % We don't yet have to represent many types of goals or details about them.
    %
:- type pard_goal == goal_rep(pard_goal_annotation).

:- type pard_goal_annotation
    --->    pard_goal_annotation(
                % The per-call cost of this call in call sequence counts.
                pga_cost_percall            :: float,

                pga_coat_above_threshold    :: cost_above_par_threshold,

                % Variable use information.
                pga_var_productions         :: assoc_list(var_rep, float),
                pga_var_consumptions        :: assoc_list(var_rep, float)
            ).

:- type cost_above_par_threshold
    --->    cost_above_par_threshold
            % The goal has a significant enough cost to be considered for
            % parallelisation.

    ;       cost_not_above_par_threshold.
            % The goal is too cheap to be considered for parallelisation.
            % We track it in the feedback information to help inform the
            % compiler about _how_ to parallelise calls around it.

:- type conjuncts_are_dependent
    --->    conjuncts_are_dependent(set(var_rep))
    ;       conjuncts_are_independent.

:- pred convert_candidate_par_conjunctions_proc(
    pred(candidate_par_conjunction(A), A, B)::in(pred(in, in, out) is det),
    candidate_par_conjunctions_proc(A)::in,
    candidate_par_conjunctions_proc(B)::out) is det.

:- pred convert_candidate_par_conjunction(
    pred(candidate_par_conjunction(A), A, B)::in(pred(in, in, out) is det),
    candidate_par_conjunction(A)::in, candidate_par_conjunction(B)::out)
    is det.

:- pred convert_seq_conj(
    pred(A, B)::in(pred(in, out) is det),
    seq_conj(A)::in, seq_conj(B)::out) is det.

%---------------------------------------------------------------------------%

    % Represent the metrics of a parallel execution.
    %
:- type parallel_exec_metrics
    --->    parallel_exec_metrics(
                % The number of calls into this parallelisation.
                pem_num_calls               :: int,

                % The elapsed time of the original sequential execution.
                pem_seq_time                :: float,

                % The estimated elapsed time of the parallel execution.
                pem_par_time                :: float,

                % The overheads of parallel execution. These are already
                % included in pem_par_time. Overheads are separated into
                % different causes.
                pem_par_overhead_spark_cost :: float,
                pem_par_overhead_barrier    :: float,
                pem_par_overhead_signals    :: float,
                pem_par_overhead_waits      :: float,

                % The amount of time the initial (left most) conjunct spends
                % waiting for the other conjuncts. During this time,
                % the context used by this conjunct must be kept alive
                % because it will resume executing sequential code after
                % the conjunct, however we know that it cannot be resumed
                % before its children have completed.
                pem_first_conj_dead_time    :: float,

                % The amount of time all conjuncts spend blocked on the
                % production of futures.
                pem_future_dead_time        :: float
            ).

    % The speedup per call: SeqTime / ParTime. For example, a value of 2.0
    % means that the goal is twice as fast when parallelised.
    %
:- func parallel_exec_metrics_get_speedup(parallel_exec_metrics) = float.

    % The amount of time saved per-call: SeqTime - ParTime.
    %
:- func parallel_exec_metrics_get_time_saving(parallel_exec_metrics) = float.

    % The amount of time spent 'on cpu', (seq time plus non-dead overheads).
    %
:- func parallel_exec_metrics_get_cpu_time(parallel_exec_metrics) = float.

    % The overheads of parallel execution.
    %
    % Add these to pem_seq_time to get the 'time on cpu' of this execution.
    %
:- func parallel_exec_metrics_get_overheads(parallel_exec_metrics) = float.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module float.
:- import_module require.
:- import_module unit.
:- import_module univ.

%---------------------------------------------------------------------------%
%
% Helper predicates for the candidate parallel conjunctions type.
%

convert_candidate_par_conjunctions_proc(Conv, CPCProcA, CPCProcB) :-
    CPCProcA = candidate_par_conjunctions_proc(VarTable, PushGoals, CPCA),
    list.map(convert_candidate_par_conjunction(Conv), CPCA, CPCB),
    CPCProcB = candidate_par_conjunctions_proc(VarTable, PushGoals, CPCB).

convert_candidate_par_conjunction(Conv0, CPC0, CPC) :-
    CPC0 = candidate_par_conjunction(GoalPath, MaybePushGoal, FirstGoalNum,
        IsDependent, GoalsBefore0, GoalsBeforeCost, Conjs0,
        GoalsAfter0, GoalsAfterCost, Metrics),
    Conv = (pred(A::in, B::out) is det :-
            Conv0(CPC0, A, B)
        ),
    list.map(convert_seq_conj(Conv), Conjs0, Conjs),
    list.map(Conv, GoalsBefore0, GoalsBefore),
    list.map(Conv, GoalsAfter0, GoalsAfter),
    CPC = candidate_par_conjunction(GoalPath, MaybePushGoal, FirstGoalNum,
        IsDependent, GoalsBefore, GoalsBeforeCost, Conjs,
        GoalsAfter, GoalsAfterCost, Metrics).

convert_seq_conj(Conv, seq_conj(Conjs0), seq_conj(Conjs)) :-
    list.map(Conv, Conjs0, Conjs).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

parallel_exec_metrics_get_speedup(PEM) = SeqTime / ParTime :-
    SeqTime = PEM ^ pem_seq_time,
    ParTime = PEM ^ pem_par_time.

parallel_exec_metrics_get_time_saving(PEM) = SeqTime - ParTime :-
    SeqTime = PEM ^ pem_seq_time,
    ParTime = PEM ^ pem_par_time.

parallel_exec_metrics_get_cpu_time(PEM) = SeqTime + Overheads :-
    SeqTime = PEM ^ pem_seq_time,
    Overheads = parallel_exec_metrics_get_overheads(PEM).

parallel_exec_metrics_get_overheads(PEM) =
        SparkCosts + BarrierCosts + SignalCosts + WaitCosts :-
    PEM = parallel_exec_metrics(_, _, _, SparkCosts, BarrierCosts,
        SignalCosts, WaitCosts, _, _).

%---------------------------------------------------------------------------%
:- end_module mdbcomp.feedback.automatic_parallelism.
%---------------------------------------------------------------------------%

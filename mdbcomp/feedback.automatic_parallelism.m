%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: feedback.automatic_parallelism.m.
% Main author: pbone.
%
% This module defines data structures for representing automatic parallelism
% feedback information and some procedures for working with these structures.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module mdbcomp.feedback.automatic_parallelism.

:- interface.

:- import_module mdbcomp.program_representation.

:- import_module int.
:- import_module list.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type stat_measure
    --->    stat_mean
    ;       stat_median.

:- type candidate_par_conjunctions_params
    --->    candidate_par_conjunctions_params(
                cpcp_desired_parallelism    :: float,
                    % The number of desired busy sparks.

                cpcp_sparking_cost          :: int,
                    % The cost of creating a spark and adding it to the local
                    % work queue in call sequence counts.

                cpcp_sparking_delay         :: int,
                    % The time taken between the creation of the spark and when
                    % it starts being executed in call sequence counts.

                cpcp_future_signal_cost     :: int,
                cpcp_future_wait_cost       :: int,
                    % The costs of maintaining a lock on a single dependent
                    % variable in call sequence counts.  The first gives the
                    % cost of the call to signal and the second gives the cost
                    % of the call to wait assuming that the value is already
                    % available.

                cpcp_context_wakeup_delay   :: int,
                    % The time it takes for a context to resume execution once
                    % it has been put on the runnable queue assuming that an
                    % engine is available to pick it up.  This is measured in
                    % call sequence counts.
                    %
                    % This is used to calculate how soon a context can recover
                    % after being blocked by a future.  It is also used to
                    % determine how quickly the context executing
                    % MR_join_and_continue after completing the leftmost
                    % conjunct of a parallel conjunction can recover after
                    % being blocked on the completion of one of the other
                    % conjuncts.

                cpcp_clique_threshold       :: int,
                    % The cost threshold in call sequence counts of a clique
                    % before it is considered for parallel execution.

                cpcp_call_site_threshold    :: int,
                    % The cost threshold in call sequence counts of a call site
                    % before it is considered for parallel execution.

                cpcp_parallelise_dep_conjs  :: parallelise_dep_conjs,
                    % Whether we will allow parallelisation to result in
                    % dependent parallel conjunctions.

                cpcp_best_par_alg           :: best_par_algorithm
            ).

:- type parallelise_dep_conjs
    --->    parallelise_dep_conjs_overlap
                % Use the overlap calculation for dependent parallelism.

    ;       parallelise_dep_conjs_num_vars
                % Use the num vars approximation for how much conjuncts
                % overlap.

    ;       parallelise_dep_conjs_naive
                % Be naive to dependent parallelism, pretend its independent.

    ;       do_not_parallelise_dep_conjs.

    % This type is used to select which algorithm is used to find the most
    % profitable parallelisation of a particular conjunction.
    %
    % TODO: The type name could be improved to make it distinct from the
    % algorithm use use to search through the clique graph.
    %
:- type best_par_algorithm
    --->    bpa_complete_bnb(
                % Use a complete but exponential algorithm.
                
                int
                    % If nonzero a conjunct with more than this many conjuncts
                    % will be solved with the greedy algorithm instead of this
                    % slower one.  (10 is the recommended value).
            )
    ;       bpa_greedy.
                % Use a greedy and linear algorithm.

    % The set of candidate parallel conjunctions within a procedure.
    %
:- type candidate_par_conjunctions_proc(GoalType)
    --->    candidate_par_conjunctions_proc(
                cpcp_var_table  :: var_table,
                    % A variable name table for the variables that have
                    % sensible names.
                                        
                cpcp_par_conjs  :: list(candidate_par_conjunction(GoalType))
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
                cpc_goal_path           :: goal_path_string,
                    % The path within the procedure to this conjunuction.
               
                cpc_partition_number    :: int,
                    % Used to locate the goals to be parallelised within the
                    % conjunction.  Partitions are separated by non atomic
                    % goals, the first partition has the number 1.

                cpc_first_conj_num      :: int,
                    % The first conjunct number in the partition.  This is only
                    % used for pretty-printing these reports with meaningful
                    % goal paths.

                cpc_is_dependent        :: conjuncts_are_dependent,

                cpc_goals_before        :: list(GoalType),

                cpc_conjs               :: list(seq_conj(GoalType)),
                    % A list of parallel conjuncts, each is a sequential
                    % conjunction of inner goals.  All inner goals that are
                    % seen in the program presentation must be stored here
                    % unless they are to be scheduled before or after the
                    % sequential conjunction.  If these conjuncts are flattened
                    % the inner goals will appear in the same order as the
                    % program representation.  By maintaining these two rules
                    % the compiler and analysis tools can use similar
                    % algorithms to construct the same parallel conjunction
                    % from the same program representation/HLDS structure.

                cpc_goals_after         :: list(GoalType),

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
                % A known callee.  note that arrity and mode are not stored at
                % all.
               
                nc_module_name  :: string,
                nc_proc_name    :: string
            ).

    % A parallelised goal (pard_goal), a goal within a parallel conjunction.
    % We don't yet have to represent many types of goals or details about them.
    %
:- type pard_goal == goal_rep(pard_goal_annotation).

:- type pard_goal_annotation
    --->    pard_goal_call(
                % A call goal,  These are the most interesting goals WRT
                % parallelisation.

                pgc_cost_percall            :: float,
                    % The per-call cost of this call in call sequence counts.

                pgc_coat_above_threshold    :: cost_above_par_threshold
            )
    ;       pard_goal_other_atomic
                % Some other (cheap) atomic goal.

    ;       pard_goal_non_atomic.
                % A non-atomic goal.

:- type cost_above_par_threshold
    --->    cost_above_par_threshold
                % The goal has a significant enough cost to be considered for
                % parallelisation.

    ;       cost_not_above_par_threshold.
                % The goal is to cheap to be considered for parallelisation,
                % we track it in the feedback information to help inform the
                % compiler about _how_ to parallelise calls around it.

:- type conjuncts_are_dependent
    --->    conjuncts_are_dependent(set(var_rep))
    ;       conjuncts_are_independent.

:- pred convert_candidate_par_conjunctions_proc(pred(A, B),
    candidate_par_conjunctions_proc(A), candidate_par_conjunctions_proc(B)).
:- mode convert_candidate_par_conjunctions_proc(pred(in, out) is det, 
    in, out) is det.

:- pred convert_candidate_par_conjunction(pred(A, B), 
    candidate_par_conjunction(A), candidate_par_conjunction(B)).
:- mode convert_candidate_par_conjunction(pred(in, out) is det, in, out) is det.

:- pred convert_seq_conj(pred(A, B), seq_conj(A), seq_conj(B)).
:- mode convert_seq_conj(pred(in, out) is det, in, out) is det.

%-----------------------------------------------------------------------------%

    % Represent the metrics of a parallel execution.
    %
:- type parallel_exec_metrics
    --->    parallel_exec_metrics(
                pem_num_calls               :: int,
                    % The number of calls into this parallelisation.

                pem_seq_time                :: float,
                    % The elapsed time of the original sequential execution.

                pem_par_time                :: float,
                    % The elapsed time of the parallel execution.

                pem_par_overheads           :: float,
                    % The overheads of parallel execution.  These are already
                    % included in pem_par_time.
                    % Add these to pem_seq_time to get the 'time on cpu' of
                    % this execution.

                pem_first_conj_dead_time    :: float,
                    % The amount of time the initial (left most) conjunct
                    % spends waiting for the other conjuncts.  During this time
                    % the context used by this conjunct must be kept alive
                    % because it will resume executing sequential code after
                    % the conjunct, however we know that it cannot be resumed
                    % before it's children have completed.
                    %

                pem_future_dead_time        :: float
                    % The amount of time all conjuncts spend blocked on the
                    % production of futures.
            ).

    % The speedup per call.  SeqTime / ParTime.  For example, a value of 2.0
    % means that this is twice as fast when parallelised.
    %
:- func parallel_exec_metrics_get_speedup(parallel_exec_metrics) = float.
    
    % The amount of time saved per-call. SeqTime - ParTime.
    %
:- func parallel_exec_metrics_get_time_saving(parallel_exec_metrics) = float.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module float.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module svmap.
:- import_module unit.
:- import_module univ.

%-----------------------------------------------------------------------------%

parallel_exec_metrics_get_speedup(PEM) = SeqTime / ParTime :-
    SeqTime = PEM ^ pem_seq_time,
    ParTime = PEM ^ pem_par_time.

parallel_exec_metrics_get_time_saving(PEM) = SeqTime - ParTime :-
    SeqTime = PEM ^ pem_seq_time,
    ParTime = PEM ^ pem_par_time.

%-----------------------------------------------------------------------------%
%
% Helper predicates for the candidate parallel conjunctions type.
%

convert_candidate_par_conjunctions_proc(Conv, CPCProcA, CPCProcB) :-
    CPCProcA = candidate_par_conjunctions_proc(VarTable, CPCA),
    map(convert_candidate_par_conjunction(Conv), CPCA, CPCB),
    CPCProcB = candidate_par_conjunctions_proc(VarTable, CPCB).

convert_candidate_par_conjunction(Conv, CPC0, CPC) :-
    CPC0 = candidate_par_conjunction(GoalPath, PartNum, FirstGoalNum,
        IsDependent, GoalsBefore0, Conjs0, GoalsAfter0, Metrics),
    map(convert_seq_conj(Conv), Conjs0, Conjs),
    map(Conv, GoalsBefore0, GoalsBefore),
    map(Conv, GoalsAfter0, GoalsAfter),
    CPC = candidate_par_conjunction(GoalPath, PartNum, FirstGoalNum,
        IsDependent, GoalsBefore, Conjs, GoalsAfter, Metrics).

convert_seq_conj(Conv, seq_conj(Conjs0), seq_conj(Conjs)) :-
    map(Conv, Conjs0, Conjs).

%-----------------------------------------------------------------------------%
:- end_module mdbcomp.feedback.automatic_parallelism.
%-----------------------------------------------------------------------------%

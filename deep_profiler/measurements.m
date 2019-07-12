%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001, 2004-2006, 2008-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This module defines the data structures that store deep profiling
% measurements and the operations on them.
%
%---------------------------------------------------------------------------%

:- module measurements.

:- interface.

:- import_module array.
:- import_module list.
:- import_module maybe.

:- import_module mdbcomp.
:- import_module mdbcomp.feedback.
:- import_module mdbcomp.feedback.automatic_parallelism.
:- import_module measurement_units.

%---------------------------------------------------------------------------%

:- type own_prof_info.
:- type inherit_prof_info.

:- func calls(own_prof_info) = int.
:- func exits(own_prof_info) = int.
:- func fails(own_prof_info) = int.
:- func redos(own_prof_info) = int.
:- func excps(own_prof_info) = int.
:- func quanta(own_prof_info) = int.
:- func callseqs(own_prof_info) = int.
:- func allocs(own_prof_info) = int.
:- func words(own_prof_info) = int.

:- func zero_own_prof_info = own_prof_info.

:- pred is_zero_own_prof_info(own_prof_info::in) is semidet.

:- func inherit_quanta(inherit_prof_info) = int.
:- func inherit_callseqs(inherit_prof_info) = int.
:- func inherit_allocs(inherit_prof_info) = int.
:- func inherit_words(inherit_prof_info) = int.

:- func zero_inherit_prof_info = inherit_prof_info.

:- pred is_zero_inherit_prof_info(inherit_prof_info::in) is semidet.

:- func add_inherit_to_inherit(inherit_prof_info, inherit_prof_info)
    = inherit_prof_info.
:- func add_own_to_inherit(own_prof_info, inherit_prof_info)
    = inherit_prof_info.
:- func subtract_own_from_inherit(own_prof_info, inherit_prof_info)
    = inherit_prof_info.
:- func subtract_inherit_from_inherit(inherit_prof_info, inherit_prof_info)
    = inherit_prof_info.
:- func add_inherit_to_own(inherit_prof_info, own_prof_info) = own_prof_info.
:- func add_own_to_own(own_prof_info, own_prof_info) = own_prof_info.

:- func sum_own_infos(list(own_prof_info)) = own_prof_info.
:- func sum_inherit_infos(list(inherit_prof_info)) = inherit_prof_info.

:- func compress_profile(int, int, int, int, int, int, int, int)
    = own_prof_info.
:- func compress_profile(own_prof_info) = own_prof_info.

:- pred decompress_profile(own_prof_info::in, int::out, int::out, int::out,
    int::out, int::out, int::out, int::out, int::out, int::out) is det.

:- func own_to_string(own_prof_info) = string.

:- type is_active
    --->    is_active
    ;       is_not_active.

    % Tests if this profiling information represents an entity in the program
    % that was inactive during the profiling run, e.g. a module or procedure
    % that has had no calls made to it.
    %
:- func compute_is_active(own_prof_info) = is_active.

%---------------------------------------------------------------------------%

:- type proc_cost_csq.

:- type cs_cost_csq.

    % build_proc_cost_csq(NRCalls, RCalls, TotalCost) = ParentCost.
    %
    % NRCalls: the number of non-recursive calls into this context.
    % RCalls: the number of recursive (inc mutually-recursive) calls into this
    % context.
    %
:- func build_proc_cost_csq(int, int, int) = proc_cost_csq.

    % build_cs_cost_csq(Calls, TotalCost) = CallSiteCost.
    %
:- func build_cs_cost_csq(int, float) = cs_cost_csq.

    % build_cs_cost_csq_percall(Calls, PercallCost) = CallSiteCost.
    %
:- func build_cs_cost_csq_percall(float, float) = cs_cost_csq.

    % Call site cost structure that has a zero cost and zero calls.
    %
:- func zero_cs_cost = cs_cost_csq.

    % Retrieve the total cost of this context.
    %
:- func proc_cost_get_total(proc_cost_csq) = float.

    % Retrieve the number of calls made to this procedure.
    %
:- func proc_cost_get_calls_total(proc_cost_csq) = int.
:- func proc_cost_get_calls_nonrec(proc_cost_csq) = int.
:- func proc_cost_get_calls_rec(proc_cost_csq) = int.

    % Retrieve the total cost of a call site.
    %
:- func cs_cost_get_total(cs_cost_csq) = float.

    % Retrieve the per-call cost of a call site.
    % Note that this may throw an exception if the number of calls is zero.
    %
:- func cs_cost_get_percall(cs_cost_csq) = float.

    % Retrieve the number of calls made from this call site.
    %
:- func cs_cost_get_calls(cs_cost_csq) = float.

    % Convert a call site cost to a proc cost.
    %
:- pred cs_cost_to_proc_cost(cs_cost_csq::in, int::in,
    proc_cost_csq::out) is det.

:- func cs_cost_per_proc_call(cs_cost_csq, proc_cost_csq) = cs_cost_csq.

%---------------------------------------------------------------------------%

    % The cost of a goal.
    %
:- type goal_cost_csq.

    % atomic_goal_cost(Calls) = Cost.
    %
    % Cost is the cost of an atomic goal called Calls times.
    %
:- func atomic_goal_cost(int) = goal_cost_csq.

    % dead_goal_cost = Cost.
    %
    % Cost is the cost of a goal that is never called.
    %
:- func dead_goal_cost = goal_cost_csq.

    % call_goal_cost(NumCalls, PerCallCost) = Cost
    %
:- func call_goal_cost(int, float) = goal_cost_csq.

:- func call_goal_cost(cs_cost_csq) = goal_cost_csq.

    % add_goal_costs_seq(EarlierGoalCost, LaterGoalCost) = Cost.
    %
    % Compute the total cost of two goals executed one after the other,
    % either because EarlierGoal and LaterGoal are in a conjunction,
    % or because execution can backtrack from EarlierGoal to LaterGoal.
    %
:- func add_goal_costs_seq(goal_cost_csq, goal_cost_csq) = goal_cost_csq.

    % add_goal_costs_branch(TotalCalls, EarlierBranchCost, LaterBranchCost)
    %   = Cost.
    %
    % Compute the total cost of two alternative branches of execution.
    %
:- func add_goal_costs_branch(int, goal_cost_csq, goal_cost_csq) =
    goal_cost_csq.

:- func goal_cost_get_percall(goal_cost_csq) = float.

:- func goal_cost_get_total(goal_cost_csq) = float.

:- func goal_cost_get_calls(goal_cost_csq) = int.

:- func goal_cost_change_calls(goal_cost_csq, int) = goal_cost_csq.

%---------------------------------------------------------------------------%

:- type recursion_depth.

:- func recursion_depth_from_float(float) = recursion_depth.

:- func recursion_depth_to_float(recursion_depth) = float.
:- func recursion_depth_to_int(recursion_depth) = int.

:- pred recursion_depth_descend(recursion_depth, recursion_depth).
:- mode recursion_depth_descend(in, out) is det.

:- pred recursion_depth_is_base_case(recursion_depth::in) is semidet.

%---------------------------------------------------------------------------%

:- type static_coverage_info.

:- func zero_static_coverage = static_coverage_info.

:- pred add_coverage_arrays(array(int)::in,
    static_coverage_info::in, static_coverage_info::out) is det.

:- pred array_to_static_coverage(array(int)::in, static_coverage_info::out)
    is det.

:- func static_coverage_maybe_get_coverage_points(static_coverage_info) =
    maybe(array(int)).

%---------------------------------------------------------------------------%

    % The amount of parallelism either available or exploited.
    %
:- type parallelism_amount.

:- func no_parallelism = parallelism_amount.

:- func some_parallelism(float) = parallelism_amount.

    % sub_computation_parallelism(ParentParallelism, ChildRunnableProb,
    %   ChildParallelism, Parallelism).
    % sub_computation_parallelism(ParentParallelism, ChildRunnableProb,
    %   Parallelism).
    %
    % Compute the total parallelism seen during the child's execution if the
    % parent is executed in a context with parallelism and the child it's self
    % has some amount of parallelism and there is a probability of
    % ChildRunnableProb that the child will be executed.
    %
    % In the three argument version we assume that the child has no
    % parallelism. In this version it is useful to think of ChildRunnableProb
    % as the chance a forked off child (of a pair of two) will be 'runnable'
    % during the execution of it's sibling.
    %
:- pred sub_computation_parallelism(parallelism_amount::in, probability::in,
    parallelism_amount::in, parallelism_amount::out) is det.
:- pred sub_computation_parallelism(parallelism_amount::in, probability::in,
    parallelism_amount::out) is det.

    % exceeded_desired_parallelism(DesiredParallelism, Parallelism)
    %
    % True iff Parallelism > DesiredParallelism
    %
:- pred exceeded_desired_parallelism(float::in, parallelism_amount::in)
    is semidet.

%---------------------------------------------------------------------------%

    % Represent the metrics of part of a parallel execution.
    %
:- type parallel_exec_metrics_incomplete.

    % ParMetrics = init_parallel_exec_metrics_incomplete(PartMetricsA,
    %   TimeSignal, TimeBSeq, TimeBPar)
    %
    % Use this function to build parallel execution metrics for a parallel
    % conjunction of any size greater than one.
    %
    % Although the parallel conjunction operator is operationally
    % right-associative, parallel overlap due in dependant parallel
    % conjunctions is easier to model if we consider it to be left associative.
    % That is the conjunction ( A & B & C ) should be modeled as two conjuncts
    % (A & B) (which is a conjunction itself and C. This is because how C
    % waits for variables in either A or B may depend on How B waits for
    % variables in A.
    %
:- func init_parallel_exec_metrics_incomplete(parallel_exec_metrics_incomplete,
    float, float, float, float, float) = parallel_exec_metrics_incomplete.

    % StartMetrics = init_empty_parallel_exec_metrics(CostBefore, CostAfter,
    %   NumCalls, SparkCost, SparkDelay, BarrierCost, ContextWakeupDelay).
    %
    % Use this function to start with an empty set of metrics for an empty
    % conjunction. Then use init_parallel_exec_metrics_incomplete to continue
    % adding conjuncts on the right.
    %
:- func init_empty_parallel_exec_metrics(float, float, int, float, float,
    float, float) = parallel_exec_metrics_incomplete.

    % Metrics = finalise_parallel_exec_metrics(IncompleteMetrics).
    %
    % Make the metrics structure complete.
    %
:- func finalise_parallel_exec_metrics(parallel_exec_metrics_incomplete)
    = parallel_exec_metrics.

    % Calls = parallel_exec_metrics_get_num_calls(IncompleteMetrics).
    %
    % Get the number of calls.
    %
:- func parallel_exec_metrics_get_num_calls(parallel_exec_metrics_incomplete)
    = int.

%---------------------------------------------------------------------------%

:- pred weighted_average(list(float)::in, list(float)::in, float::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module int.
:- import_module require.
:- import_module string.

:- import_module array_util.

%---------------------------------------------------------------------------%

:- type own_prof_info
    --->    own_prof_all(
                opa_exits               :: int,
                opa_fails               :: int,
                opa_redos               :: int,
                opa_excps               :: int,
                opa_quanta              :: int,
                opa_callseqs            :: int,
                opa_allocs              :: int,
                opa_words               :: int
            )
            % implicit calls = exits + fails + excps - redos

    ;       own_prof_det(
                opd_exits               :: int,
                opd_quanta              :: int,
                opd_callseqs            :: int,
                opd_allocs              :: int,
                opd_words               :: int
            )
            % implicit fails == redos == excps == 0
            % implicit calls == exits

    ;       own_prof_fast_det(
                opfd_exits              :: int,
                opfd_callseqs           :: int,
                opfd_allocs             :: int,
                opfd_words              :: int
            )
            % implicit fails == redos == excps == 0
            % implicit calls == exits
            % implicit quanta == 0

    ;       own_prof_fast_nomem_semi(
                opfns_exits             :: int,
                opfns_fails             :: int,
                opfns_callseqs          :: int
            ).
            % implicit redos == excps == 0
            % implicit calls == exits + fails
            % implicit quanta == 0
            % implicit allocs == words == 0

:- type inherit_prof_info
    --->    inherit_prof_info(
                ipo_quanta              :: int,
                ipo_callseqs            :: int,
                ipo_allocs              :: int,
                ipo_words               :: int
            ).

calls(own_prof_fast_nomem_semi(Exits, Fails, _)) = Exits + Fails.
calls(own_prof_fast_det(Exits, _, _, _)) = Exits.
calls(own_prof_det(Exits, _, _, _, _)) = Exits.
calls(own_prof_all(Exits, Fails, Redos, Excps, _, _, _, _)) =
    Exits + Fails + Excps - Redos.

exits(own_prof_fast_nomem_semi(Exits, _, _)) = Exits.
exits(own_prof_fast_det(Exits, _, _, _)) = Exits.
exits(own_prof_det(Exits, _, _, _, _)) = Exits.
exits(own_prof_all(Exits, _, _, _, _, _, _, _)) = Exits.

fails(own_prof_fast_nomem_semi(_, Fails, _)) = Fails.
fails(own_prof_fast_det(_, _, _, _)) = 0.
fails(own_prof_det(_, _, _, _, _)) = 0.
fails(own_prof_all(_, Fails, _, _, _, _, _, _)) = Fails.

redos(own_prof_fast_nomem_semi(_, _, _)) = 0.
redos(own_prof_fast_det(_, _, _, _)) = 0.
redos(own_prof_det(_, _, _, _, _)) = 0.
redos(own_prof_all(_, _, Redos, _, _, _, _, _)) = Redos.

excps(own_prof_fast_nomem_semi(_, _, _)) = 0.
excps(own_prof_fast_det(_, _, _, _)) = 0.
excps(own_prof_det(_, _, _, _, _)) = 0.
excps(own_prof_all(_, _, _, Excps, _, _, _, _)) = Excps.

quanta(own_prof_fast_nomem_semi(_, _, _)) = 0.
quanta(own_prof_fast_det(_, _, _, _)) = 0.
quanta(own_prof_det(_, Quanta, _, _, _)) = Quanta.
quanta(own_prof_all(_, _, _, _, Quanta, _, _, _)) = Quanta.

callseqs(own_prof_fast_nomem_semi(_, _, CallSeqs)) = CallSeqs.
callseqs(own_prof_fast_det(_, CallSeqs, _, _)) = CallSeqs.
callseqs(own_prof_det(_, _, CallSeqs, _, _)) = CallSeqs.
callseqs(own_prof_all(_, _, _, _, _, CallSeqs, _, _)) = CallSeqs.

allocs(own_prof_fast_nomem_semi(_, _, _)) = 0.
allocs(own_prof_fast_det(_, _, Allocs, _)) = Allocs.
allocs(own_prof_det(_, _, _, Allocs, _)) = Allocs.
allocs(own_prof_all(_, _, _, _, _, _, Allocs, _)) = Allocs.

words(own_prof_fast_nomem_semi(_, _, _)) = 0.
words(own_prof_fast_det(_, _, _, Words)) = Words.
words(own_prof_det(_, _, _, _, Words)) = Words.
words(own_prof_all(_, _, _, _, _, _, _, Words)) = Words.

zero_own_prof_info = own_prof_fast_nomem_semi(0, 0, 0).

is_zero_own_prof_info(own_prof_all(0, 0, 0, 0, 0, 0, 0, 0)).
is_zero_own_prof_info(own_prof_det(0, 0, 0, 0, 0)).
is_zero_own_prof_info(own_prof_fast_det(0, 0, 0, 0)).
is_zero_own_prof_info(own_prof_fast_nomem_semi(0, 0, 0)).

inherit_quanta(inherit_prof_info(Quanta, _, _, _)) = Quanta.
inherit_callseqs(inherit_prof_info(_, CallSeqs, _, _)) = CallSeqs.
inherit_allocs(inherit_prof_info(_, _, Allocs, _)) = Allocs.
inherit_words(inherit_prof_info(_, _, _, Words)) = Words.

zero_inherit_prof_info = inherit_prof_info(0, 0, 0, 0).

is_zero_inherit_prof_info(inherit_prof_info(0, 0, 0, 0)).

add_inherit_to_inherit(PI1, PI2) = SumPI :-
    Quanta = inherit_quanta(PI1) + inherit_quanta(PI2),
    CallSeqs = inherit_callseqs(PI1) + inherit_callseqs(PI2),
    Allocs = inherit_allocs(PI1) + inherit_allocs(PI2),
    Words = inherit_words(PI1) + inherit_words(PI2),
    SumPI = inherit_prof_info(Quanta, CallSeqs, Allocs, Words).

add_own_to_inherit(PI1, PI2) = SumPI :-
    Quanta = quanta(PI1) + inherit_quanta(PI2),
    CallSeqs = callseqs(PI1) + inherit_callseqs(PI2),
    Allocs = allocs(PI1) + inherit_allocs(PI2),
    Words = words(PI1) + inherit_words(PI2),
    SumPI = inherit_prof_info(Quanta, CallSeqs, Allocs, Words).

subtract_own_from_inherit(PI1, PI2) = SumPI :-
    Quanta = inherit_quanta(PI2) - quanta(PI1),
    CallSeqs = inherit_callseqs(PI2) - callseqs(PI1),
    Allocs = inherit_allocs(PI2) - allocs(PI1),
    Words = inherit_words(PI2) - words(PI1),
    SumPI = inherit_prof_info(Quanta, CallSeqs, Allocs, Words).

subtract_inherit_from_inherit(PI1, PI2) = SumPI :-
    Quanta = inherit_quanta(PI2) - inherit_quanta(PI1),
    CallSeqs = inherit_callseqs(PI2) - inherit_callseqs(PI1),
    Allocs = inherit_allocs(PI2) - inherit_allocs(PI1),
    Words = inherit_words(PI2) - inherit_words(PI1),
    SumPI = inherit_prof_info(Quanta, CallSeqs, Allocs, Words).

add_inherit_to_own(PI1, PI2) = SumPI :-
    Exits = exits(PI2),
    Fails = fails(PI2),
    Redos = redos(PI2),
    Excps = excps(PI2),
    Quanta = inherit_quanta(PI1) + quanta(PI2),
    CallSeqs = inherit_callseqs(PI1) + callseqs(PI2),
    Allocs = inherit_allocs(PI1) + allocs(PI2),
    Words = inherit_words(PI1) + words(PI2),
    SumPI = compress_profile(Exits, Fails, Redos, Excps,
        Quanta, CallSeqs, Allocs, Words).

add_own_to_own(PI1, PI2) = SumPI :-
    Exits = exits(PI1) + exits(PI2),
    Fails = fails(PI1) + fails(PI2),
    Redos = redos(PI1) + redos(PI2),
    Excps = excps(PI1) + excps(PI2),
    Quanta = quanta(PI1) + quanta(PI2),
    CallSeqs = callseqs(PI1) + callseqs(PI2),
    Allocs = allocs(PI1) + allocs(PI2),
    Words = words(PI1) + words(PI2),
    SumPI = compress_profile(Exits, Fails, Redos, Excps,
        Quanta, CallSeqs, Allocs, Words).

sum_own_infos(Owns) =
    list.foldl(add_own_to_own, Owns, zero_own_prof_info).

sum_inherit_infos(Inherits) =
    list.foldl(add_inherit_to_inherit, Inherits, zero_inherit_prof_info).

compress_profile(Exits, Fails, Redos, Excps, Quanta, CallSeqs, Allocs, Words)
        = PI :-
    ( if
        Redos = 0,
        Excps = 0,
        Quanta = 0,
        Allocs = 0,
        Words = 0
    then
        PI = own_prof_fast_nomem_semi(Exits, Fails, CallSeqs)
    else if
        Fails = 0,
        Redos = 0,
        Excps = 0
    then
        ( if Quanta = 0 then
            PI = own_prof_fast_det(Exits, CallSeqs, Allocs, Words)
        else
            PI = own_prof_det(Exits, Quanta, CallSeqs, Allocs, Words)
        )
    else
        PI = own_prof_all(Exits, Fails, Redos, Excps, Quanta, CallSeqs,
            Allocs, Words)
    ).

compress_profile(PI0) = PI :-
    (
        PI0 = own_prof_all(Exits, Fails, Redos, Excps, Quanta, CallSeqs,
            Allocs, Words),
        ( if
            Redos = 0,
            Excps = 0,
            Quanta = 0,
            Allocs = 0,
            Words = 0
        then
            PI = own_prof_fast_nomem_semi(Exits, Fails, CallSeqs)
        else if
            Fails = 0,
            Redos = 0,
            Excps = 0
        then
            ( if Quanta = 0 then
                PI = own_prof_fast_det(Exits, CallSeqs, Allocs, Words)
            else
                PI = own_prof_det(Exits, Quanta, CallSeqs, Allocs, Words)
            )
        else
            PI = PI0
        )
    ;
        PI0 = own_prof_det(Exits, Quanta, CallSeqs, Allocs, Words),
        ( if Allocs = 0, Words = 0 then
            PI = own_prof_fast_nomem_semi(Exits, 0, CallSeqs)
        else if Quanta = 0 then
            PI = own_prof_fast_det(Exits, CallSeqs, Allocs, Words)
        else
            PI = PI0
        )
    ;
        PI0 = own_prof_fast_det(Exits, CallSeqs, Allocs, Words),
        ( if Allocs = 0, Words = 0 then
            PI = own_prof_fast_nomem_semi(Exits, 0, CallSeqs)
        else
            PI = PI0
        )
    ;
        PI0 = own_prof_fast_nomem_semi(_, _, _),
        PI = PI0
    ).

%---------------------------------------------------------------------------%

decompress_profile(Own, Calls, Exits, Fails, Redos, Excps, Quanta, CallSeqs,
        Allocs, Words) :-
    (
        Own = own_prof_all(Exits, Fails, Redos, Excps, Quanta, CallSeqs,
            Allocs, Words),
        Calls = Exits + Fails - Redos
    ;
        Own = own_prof_det(Exits, Quanta, CallSeqs, Allocs, Words),
        Calls = Exits,
        Fails = 0,
        Redos = 0,
        Excps = 0
    ;
        Own = own_prof_fast_det(Exits, CallSeqs, Allocs, Words),
        Calls = Exits,
        Fails = 0,
        Redos = 0,
        Excps = 0,
        Quanta = 0
    ;
        Own = own_prof_fast_nomem_semi(Exits, Fails, CallSeqs),
        Calls = Exits + Fails,
        Redos = 0,
        Excps = 0,
        Quanta = 0,
        Allocs = 0,
        Words = 0
    ).

own_to_string(own_prof_all(Exits, Fails, Redos, Excps, Quanta, CallSeqs,
        Allocs, Words)) =
    "all(" ++
    string.int_to_string(Exits) ++ ", " ++
    string.int_to_string(Fails) ++ ", " ++
    string.int_to_string(Redos) ++ ", " ++
    string.int_to_string(Excps) ++ ", " ++
    string.int_to_string(Quanta) ++ ", " ++
    string.int_to_string(CallSeqs) ++ ", " ++
    string.int_to_string(Allocs) ++ ", " ++
    string.int_to_string(Words) ++
    ")".
own_to_string(own_prof_det(Exits, Quanta, CallSeqs, Allocs, Words)) =
    "det(" ++
    string.int_to_string(Exits) ++ ", " ++
    string.int_to_string(Quanta) ++ ", " ++
    string.int_to_string(CallSeqs) ++ ", " ++
    string.int_to_string(Allocs) ++ ", " ++
    string.int_to_string(Words) ++
    ")".
own_to_string(own_prof_fast_det(Exits, CallSeqs, Allocs, Words)) =
    "fast_det(" ++
    string.int_to_string(Exits) ++ ", " ++
    string.int_to_string(CallSeqs) ++ ", " ++
    string.int_to_string(Allocs) ++ ", " ++
    string.int_to_string(Words) ++
    ")".
own_to_string(own_prof_fast_nomem_semi(Exits, Fails, CallSeqs)) =
    "fast_det(" ++
    string.int_to_string(Exits) ++ ", " ++
    string.int_to_string(Fails) ++ ", " ++
    string.int_to_string(CallSeqs) ++
    ")".

compute_is_active(Own) = IsActive :-
    ( if
        ( Own = own_prof_all(0, 0, 0, 0, _, _, _, _)
        ; Own = own_prof_det(0, _, _, _, _)
        ; Own = own_prof_fast_det(0, _, _, _)
        ; Own = own_prof_fast_nomem_semi(0, 0, _)
        )
    then
        IsActive = is_not_active
    else
        IsActive = is_active
    ).

%---------------------------------------------------------------------------%

:- type proc_cost_csq
    --->    proc_cost_csq(
                % The number of non-recursive calls into this context.
                % For example if this is a clique this is the number of
                % calls made from the parent' clique to this one.
                pcc_nr_calls        :: int,

                % The number of recursive calls into this context.
                % This includes mutual recursion.
                pcc_r_calls         :: int,

                % The number of callseq counts per call,
                pcc_csq             :: cost
            ).

:- type cs_cost_csq
    --->    cs_cost_csq(
                % The number of calls (per parent invocation) through this
                % call site.
                cscc_calls          :: float,

                % The cost of the call site per call.
                cscc_csq_cost       :: cost
            ).

%---------------------------------------------------------------------------%

build_proc_cost_csq(NonRecursiveCalls, RecursiveCalls, TotalCost) =
    proc_cost_csq(NonRecursiveCalls, RecursiveCalls,
        cost_total(float(TotalCost))).

build_cs_cost_csq(Calls, TotalCost) =
    cs_cost_csq(float(Calls), cost_total(TotalCost)).

build_cs_cost_csq_percall(Calls, PercallCost) =
    cs_cost_csq(Calls, cost_per_call(PercallCost)).

zero_cs_cost =
    % Build this using the percall structure so that if a percall cost is ever
    % retrieved, we don't have to divide by zero. This is only a partial
    % solution.
    build_cs_cost_csq_percall(0.0, 0.0).

%---------------------------------------------------------------------------%

proc_cost_get_total(proc_cost_csq(NRCalls, RCalls, Cost)) =
    cost_get_total(float(NRCalls + RCalls), Cost).

proc_cost_get_calls_total(proc_cost_csq(NRCalls, RCalls, _)) =
    NRCalls + RCalls.

proc_cost_get_calls_nonrec(proc_cost_csq(NRCalls, _, _)) = NRCalls.

proc_cost_get_calls_rec(proc_cost_csq(_, RCalls, _)) = RCalls.

%---------------------------------------------------------------------------%

cs_cost_get_total(cs_cost_csq(Calls, Cost)) =
    cost_get_total(Calls, Cost).

cs_cost_get_percall(cs_cost_csq(Calls, Cost)) =
    cost_get_percall(Calls, Cost).

cs_cost_get_calls(cs_cost_csq(Calls, _)) = Calls.

%---------------------------------------------------------------------------%

cs_cost_to_proc_cost(cs_cost_csq(CSCalls, CSCost), TotalCalls,
        proc_cost_csq(NRCalls, RCalls, PCost)) :-
    NRCalls = round_to_int(CSCalls),
    RCalls = TotalCalls - round_to_int(CSCalls),
    % The negative one represents the cost of the callsite itself.
    PCost = cost_total(cost_get_total(CSCalls, CSCost) - 1.0 * CSCalls).

cs_cost_per_proc_call(cs_cost_csq(CSCalls0, CSCost0), ParentCost) =
        cs_cost_csq(CSCalls, CSCost) :-
    TotalParentCalls = proc_cost_get_calls_nonrec(ParentCost),
    CSCalls = CSCalls0 / float(TotalParentCalls),
    CSCost = CSCost0 / TotalParentCalls.

%---------------------------------------------------------------------------%

:- type goal_cost_csq
    --->    dead_goal
    ;       trivial_goal(
                tg_calls                :: int
            )
    ;       non_trivial_goal(
                ntg_avg_cost            :: cost,
                ntg_calls               :: int
            ).

atomic_goal_cost(Calls) = trivial_goal(Calls).

dead_goal_cost = dead_goal.

call_goal_cost(Calls, PercallCost) = non_trivial_goal(Cost, Calls) :-
    Cost = cost_per_call(PercallCost).

call_goal_cost(CSCost) = non_trivial_goal(Cost, Calls) :-
    Calls = round_to_int(cs_cost_get_calls(CSCost)),
    Cost = CSCost ^ cscc_csq_cost.

add_goal_costs_seq(dead_goal, dead_goal) = dead_goal.
add_goal_costs_seq(dead_goal, R@trivial_goal(_)) = R.
add_goal_costs_seq(dead_goal, R@non_trivial_goal(_, _)) = R.
add_goal_costs_seq(R@trivial_goal(_), dead_goal) = R.
add_goal_costs_seq(trivial_goal(CallsA), trivial_goal(_CallsB)) =
    trivial_goal(CallsA).
add_goal_costs_seq(trivial_goal(Calls), non_trivial_goal(CostB, CallsB)) =
        non_trivial_goal(Cost, Calls) :-
    CostTotal = cost_get_total(float(CallsB), CostB),
    Cost = cost_total(CostTotal).
add_goal_costs_seq(R@non_trivial_goal(_, _), dead_goal) = R.
add_goal_costs_seq(R@non_trivial_goal(_, _), trivial_goal(_)) = R.
add_goal_costs_seq(non_trivial_goal(CostA, CallsA),
        non_trivial_goal(CostB, CallsB))
        = non_trivial_goal(Cost, Calls) :-
    Calls = CallsA,
    CostTotal = cost_get_total(float(CallsA), CostA) +
        cost_get_total(float(CallsB), CostB),
    Cost = cost_total(CostTotal),
    ( if
        Calls = 0,
        CostTotal \= 0.0
    then
        unexpected($pred, "Calls = 0, Cost \\= 0")
    else
        true
    ).

add_goal_costs_branch(TotalCalls, A, B) = R :-
    ( if TotalCalls = 0 then
        R = dead_goal
    else
        (
            A = dead_goal,
            CallsA = 0,
            (
                B = dead_goal,
                unexpected($pred, "TotalCalls \\= 0 for a dead goal")
            ;
                B = trivial_goal(CallsB),
                R = trivial_goal(TotalCalls)
            ;
                B = non_trivial_goal(Cost, CallsB),
                R = non_trivial_goal(Cost, TotalCalls)
            )
        ;
            A = trivial_goal(CallsA),
            (
                B = dead_goal,
                CallsB = 0,
                R = trivial_goal(TotalCalls)
            ;
                B = trivial_goal(CallsB),
                R = trivial_goal(TotalCalls)
            ;
                B = non_trivial_goal(Cost, CallsB),
                R = non_trivial_goal(Cost, TotalCalls)
            )
        ;
            A = non_trivial_goal(CostA, CallsA),
            (
                B = dead_goal,
                CallsB = 0,
                R = non_trivial_goal(CostA, TotalCalls)
            ;
                B = trivial_goal(CallsB),
                R = non_trivial_goal(CostA, TotalCalls)
            ;
                B = non_trivial_goal(CostB, CallsB),
                Cost = sum_costs(float(CallsA), CostA, float(CallsB), CostB),
                R = non_trivial_goal(Cost, CallsA + CallsB)
            )
        ),
        check_total_calls(CallsA, CallsB, TotalCalls)
    ).

:- pred check_total_calls(int::in, int::in, int::in) is det.

check_total_calls(CallsA, CallsB, TotalCalls) :-
    Calls = CallsA + CallsB,
    ( if unify(Calls, TotalCalls) then
        true
    else
        unexpected($pred, "TotalCalls \\= CallsA + CallsB")
    ).

goal_cost_get_percall(dead_goal) = 0.0.
goal_cost_get_percall(trivial_goal(_)) = 0.0.
goal_cost_get_percall(non_trivial_goal(Cost, Calls)) =
    ( if Calls = 0 then
        0.0
    else
        cost_get_percall(float(Calls), Cost)
    ).

goal_cost_get_total(dead_goal) = 0.0.
goal_cost_get_total(trivial_goal(_)) = 0.0.
goal_cost_get_total(non_trivial_goal(Cost, Calls)) =
    cost_get_total(float(Calls), Cost).

goal_cost_get_calls(dead_goal) = 0.
goal_cost_get_calls(trivial_goal(Calls)) = Calls.
goal_cost_get_calls(non_trivial_goal(_, Calls)) = Calls.

goal_cost_change_calls(dead_goal, _) =
    unexpected($pred, "Cannot compute new cost").
goal_cost_change_calls(trivial_goal(_), Calls) = trivial_goal(Calls).
goal_cost_change_calls(non_trivial_goal(Cost0, Calls0), Calls) =
        non_trivial_goal(Cost, Calls) :-
    Cost = cost_per_call(cost_get_percall(float(Calls0), Cost0)).

%---------------------------------------------------------------------------%

:- type cost
    --->    cost_per_call(float)
    ;       cost_total(float).

:- func cost_get_total(float, cost) = float.

cost_get_total(_, cost_total(Total)) = Total.
cost_get_total(Calls, cost_per_call(Percall)) = Calls * Percall.

:- func cost_get_percall(float, cost) = float.

cost_get_percall(Calls, cost_total(Total)) = Total / Calls.
cost_get_percall(_, cost_per_call(Percall)) = Percall.

:- func (cost) / (int) = cost.

Cost0 / Denom = Cost :-
    (
        Cost0 = cost_total(Total),
        Cost = cost_total(Total / float(Denom))
    ;
        Cost0 = cost_per_call(Percall),
        Cost = cost_per_call(Percall / float(Denom))
    ).

:- func cost_by_weight(float, cost) = cost.

cost_by_weight(Weight, cost_total(Total)) =
    cost_total(Total * Weight).
cost_by_weight(Weight, cost_per_call(Percall)) =
    cost_per_call(Percall * Weight).

:- func sum_costs(float, cost, float, cost) = cost.

sum_costs(CallsA, CostA, CallsB, CostB) = cost_total(Sum) :-
    Sum = CostTotalA + CostTotalB,
    CostTotalA = cost_get_total(CallsA, CostA),
    CostTotalB = cost_get_total(CallsB, CostB).

%---------------------------------------------------------------------------%

:- type recursion_depth
    --->    recursion_depth(float).

recursion_depth_from_float(F) = recursion_depth(F).

recursion_depth_to_float(recursion_depth(F)) = F.
recursion_depth_to_int(D) =
    round_to_int(recursion_depth_to_float(D)).

recursion_depth_descend(recursion_depth(D), recursion_depth(D - 1.0)) :-
    ( if D >= 0.5 then
        true
    else
        unexpected($pred,
            format("Recursion depth will be less than zero: %f", [f(D - 1.0)]))
    ).

recursion_depth_is_base_case(recursion_depth(D)) :-
    D < 0.5,
    D >= -0.5.

%---------------------------------------------------------------------------%

:- type static_coverage_info == maybe(array(int)).

zero_static_coverage = no.

add_coverage_arrays(Array, no, yes(Array)).
add_coverage_arrays(NewArray, yes(!.Array), yes(!:Array)) :-
    ( if
        array.bounds(NewArray, Min, Max),
        array.bounds(!.Array, Min, Max)
    then
        !:Array = copy(!.Array),
        array_foldl_from_0(
            (pred(Index::in, E::in, A0::array_di, A::array_uo) is det :-
                array.lookup(A0, Index, Value),
                array.set(Index, Value + E, A0, A)
            ), NewArray, !Array)
    else
        unexpected($pred, "arrays' bounds do not match")
    ).

array_to_static_coverage(Array, yes(Array)).

static_coverage_maybe_get_coverage_points(MaybeCoverage) = MaybeCoverage.

%---------------------------------------------------------------------------%

    % This type can be represented in multiple ways. A single value expressing
    % the probable value, or a probable value and a measure of
    % deviation/variance. Or as below three values that can express the
    % skewedness of a distribution of values.
    %
    % For now represent it as a single scalar. Consider uncommenting the other
    % two fields in the future.
    %
:- type parallelism_amount
    --->    parallelism_amount(
%                pa_best         :: float,
%                    % An upper bound on the probable amount of parallelism.
%                    % IE: the most optimistic value.
%
%                pa_worst        :: float,
%                    % A lower bound on the probable amount of parallelism.
%
                pa_likely       :: float
                    % The likely amount of parallelism here.
            ).

no_parallelism = parallelism_amount(1.0).

some_parallelism(Num) = parallelism_amount(Num) :-
    ( if Num < 1.0 then
        unexpected($pred, "Parallelism amount cannot ever be less than 1.0")
    else
        true
    ).

sub_computation_parallelism(ParentParallelism, Prob, ChildParallelism,
        Parallelism) :-
    probability_to_float(Prob) = ProbFloat,
    ParentParallelism = parallelism_amount(ParLikely),
    ChildParallelism = parallelism_amount(ChildLikely),
    Likely = ParLikely + (ProbFloat * ChildLikely),
    Parallelism = parallelism_amount(Likely).

sub_computation_parallelism(ParentParallelism, Prob, Parallelism) :-
    sub_computation_parallelism(ParentParallelism, Prob, no_parallelism,
        Parallelism).

exceeded_desired_parallelism(DesiredParallelism, Parallelism) :-
    Parallelism = parallelism_amount(LikelyParallelism),
    DesiredParallelism < LikelyParallelism.

%---------------------------------------------------------------------------%

:- type parallel_exec_metrics_incomplete
    --->    pem_incomplete(
                pemi_time_before_conj       :: float,
                pemi_time_after_conj        :: float,

                pemi_num_calls              :: int,

                pemi_spark_cost             :: float,

                pemi_spark_delay            :: float,

                pemi_barrier_cost           :: float,

                pemi_context_wakeup_delay   :: float,

                % If there are no internal conjuncts then the parallel
                % conjunction is empty.
                pemi_internal               ::
                        maybe(parallel_exec_metrics_internal)
            ).

:- type parallel_exec_metrics_internal
    --->    pem_left_most(
                pemi_time_left_seq          :: float,
                pemi_time_left_par          :: float,
                pemi_time_left_signals      :: float

                % While the leftmost conjunct does have dead time it's not
                % possible to calculate this until we know the parallel
                % execution time of the whole conjunction, therefore it is not
                % included here and is in parallel_exec_metrics_incomplete.
            )
    ;       pem_additional(
                % The time of the left conjunct (that may be a conjunction).
                pemi_time_left              :: parallel_exec_metrics_internal,

                % The additional cost of calling signal during this conjunct.
                pemi_time_signals           :: float,

                % The additional cost of calling wait during this conjunct.
                pemi_time_waits             :: float,

                % The time of the right conjunct if it is running after
                % the left in normal sequential execution.
                pemi_time_right_seq         :: float,

                % The time of the right conjunct if it is running in parallel
                % with the left conjunct. Overheads are included in this value
                % so it will usually be larger than time_right_seq.
                pemi_time_right_par         :: float,

                % The dead time of this conjunct, This is the time that the
                % context will be blocked on futures. It does not include the
                % spark delay because the contact may not exist for most of
                % that time.
                pemi_time_right_dead        :: float
            ).

init_parallel_exec_metrics_incomplete(Metrics0, TimeSignals, TimeWaits,
        TimeBSeq, TimeBPar, TimeBDead) = Metrics :-
    MaybeInternal0 = Metrics0 ^ pemi_internal,
    (
        MaybeInternal0 = yes(Internal0),
        Internal = pem_additional(Internal0, TimeSignals, TimeWaits, TimeBSeq,
            TimeBPar, TimeBDead)
    ;
        MaybeInternal0 = no,
        Internal = pem_left_most(TimeBSeq, TimeBPar, TimeSignals),
        ( if
            TimeBDead = 0.0,
            TimeWaits = 0.0
        then
            true
        else
            unexpected($pred, "TimeWaits != 0 or TimeBDead != 0")
        )
    ),
    Metrics = Metrics0 ^ pemi_internal := yes(Internal).

init_empty_parallel_exec_metrics(TimeBefore, TimeAfter, NumCalls, SparkCost,
        SparkDelay, BarrierCost, ContextWakeupDelay) =
    pem_incomplete(TimeBefore, TimeAfter, NumCalls, SparkCost, SparkDelay,
        BarrierCost, ContextWakeupDelay, no).

finalise_parallel_exec_metrics(IncompleteMetrics) = Metrics :-
    IncompleteMetrics = pem_incomplete(TimeBefore, TimeAfter, NumCalls,
        SparkCost, SparkDelay, BarrierCost, ContextWakeupDelay, MaybeInternal),
    (
        MaybeInternal = yes(Internal)
    ;
        MaybeInternal = no,
        unexpected($pred, "cannot finalise empty parallel metrics")
    ),
    BeforeAndAfterTime = TimeBefore + TimeAfter,

    % Calculate par time.
    NumConjuncts = parallel_exec_metrics_internal_get_num_conjs(Internal),
    InnerParTime = parallel_exec_metrics_internal_get_par_time(Internal,
        SparkDelay, NumConjuncts),
    ( if FirstConjDeadTime > 0.0 then
        FirstConjWakeupPenalty = ContextWakeupDelay
    else
        FirstConjWakeupPenalty = 0.0
    ),
    ParTime = InnerParTime + BeforeAndAfterTime + FirstConjWakeupPenalty,

    % Calculate the sequential execution time.
    InnerSeqTime = parallel_exec_metrics_internal_get_seq_time(Internal),
    SeqTime = InnerSeqTime + BeforeAndAfterTime,

    % Calculate the amount of time that the first conjunct is blocked for.
    FirstConjParTime = pem_get_first_conj_par_time(Internal),
    FirstConjDeadTime = InnerParTime - FirstConjParTime,

    % Calculate the amount of time that the conjunction spends blocking on
    % futures.
    FutureDeadTime = pem_get_future_dead_time(Internal),

    % Calculate the overheads of parallelisation.
    %
    % These are already included in ParTime, we don't need to add them, just
    % calculate what they would be for reporting.
    SparkCosts = float(NumConjuncts - 1) * SparkCost,
    BarrierCosts = float(NumConjuncts) * BarrierCost,
    SignalCosts = pem_get_signal_costs(Internal),
    WaitCosts = pem_get_wait_costs(Internal),

    Metrics = parallel_exec_metrics(NumCalls, SeqTime, ParTime, SparkCosts,
        BarrierCosts, SignalCosts, WaitCosts, FirstConjDeadTime,
        FutureDeadTime).

parallel_exec_metrics_get_num_calls(Pem) =
    Pem ^ pemi_num_calls.

:- func parallel_exec_metrics_internal_get_num_conjs(
    parallel_exec_metrics_internal) = int.

parallel_exec_metrics_internal_get_num_conjs(pem_left_most(_, _, _)) = 1.
parallel_exec_metrics_internal_get_num_conjs(
        pem_additional(Left, _, _, _, _, _)) =
    1 + parallel_exec_metrics_internal_get_num_conjs(Left).

    % The expected parallel execution time, because this is the elapsed
    % execution time of the whole parallel conjunct it must include dead time.
    %
:- func parallel_exec_metrics_internal_get_par_time(
    parallel_exec_metrics_internal, float, int) = float.

parallel_exec_metrics_internal_get_par_time(PEM, SparkDelay, Depth) = Time :-
    (
        PEM = pem_left_most(_, ParTime, _),
        Time = ParTime
    ;
        PEM = pem_additional(MetricsLeft, _, _, _,
            TimeRightPar, TimeRightDead),
        TimeRight = TimeRightPar + TimeRightDead +
            SparkDelay * float(Depth - 1),
        TimeLeft = parallel_exec_metrics_internal_get_par_time(MetricsLeft,
            SparkDelay, Depth - 1),
        Time = max(TimeLeft, TimeRight)
    ).

    % The expected sequential execution time.
    %
:- func parallel_exec_metrics_internal_get_seq_time(
    parallel_exec_metrics_internal) = float.

parallel_exec_metrics_internal_get_seq_time(pem_left_most(Time, _, _)) = Time.
parallel_exec_metrics_internal_get_seq_time(pem_additional(MetricsLeft, _,
        _, TimeRight, _, _)) = Time :-
    TimeLeft = parallel_exec_metrics_internal_get_seq_time(MetricsLeft),
    Time = TimeLeft + TimeRight.

    % Get the parallel execution time of the first conjunct. This is used for
    % calculating the first conjunct's dead time (above).
    %
:- func pem_get_first_conj_par_time(parallel_exec_metrics_internal) = float.

pem_get_first_conj_par_time(pem_left_most(_, Time, _)) = Time.
pem_get_first_conj_par_time(pem_additional(Left, _, _, _, _, _)) =
        Time :-
    Time = pem_get_first_conj_par_time(Left).

:- func pem_get_future_dead_time(parallel_exec_metrics_internal) = float.

pem_get_future_dead_time(pem_left_most(_, _, _)) = 0.0.
pem_get_future_dead_time(pem_additional(Left, _, _, _, _, RightDeadTime)) =
        RightDeadTime + LeftDeadTime :-
    LeftDeadTime = pem_get_future_dead_time(Left).

    % Get the overheads of parallelisation.
    %
    % Remember that these are already represented within the parallel execution
    % time.
    %
:- func pem_get_par_overheads(parallel_exec_metrics_internal) = float.

pem_get_par_overheads(pem_left_most(Seq, Par, _)) = Par - Seq.
pem_get_par_overheads(pem_additional(Left, _, _, Seq, Par, _)) =
        Overheads :-
    Overheads = LeftOverheads + Par - Seq,
    pem_get_par_overheads(Left) = LeftOverheads.

:- func pem_get_signal_costs(parallel_exec_metrics_internal) = float.

pem_get_signal_costs(pem_left_most(_, _, SignalCosts)) = SignalCosts.
pem_get_signal_costs(pem_additional(Left, SignalsR, _, _, _, _)) = Signals :-
    Signals = SignalsR + SignalsL,
    SignalsL = pem_get_signal_costs(Left).

:- func pem_get_wait_costs(parallel_exec_metrics_internal) = float.

pem_get_wait_costs(pem_left_most(_, _, _)) = 0.0.
pem_get_wait_costs(pem_additional(Left, _, WaitsR, _, _, _)) = Waits :-
    Waits = WaitsR + WaitsL,
    WaitsL = pem_get_wait_costs(Left).

%---------------------------------------------------------------------------%

weighted_average(Weights, Values, Average) :-
    list.foldl2_corresponding(update_weighted_sum,
        Weights, Values, 0.0, WeightedSum, 0.0, TotalWeight),
    ( if abs(TotalWeight) < epsilon then
        Average = 0.0
    else
        Average = WeightedSum / TotalWeight
    ).

:- pred update_weighted_sum(float::in, float::in,
    float::in, float::out, float::in, float::out) is det.

update_weighted_sum(Weight, Value, !WeightedSum, !TotalWeight) :-
    !:WeightedSum = !.WeightedSum + (Weight * Value),
    !:TotalWeight = !.TotalWeight + Weight.

%---------------------------------------------------------------------------%
:- end_module measurements.
%---------------------------------------------------------------------------%

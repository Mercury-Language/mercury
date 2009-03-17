%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mdprof_fb.automatic_parallelism.m.
% Author: tannier, pbone.
%
% This module contains the code for analysing deep profiles of programs in
% order to determine how best to automatically parallelise the program.  This
% code is used by the mdprof_feedback tool.
%
%-----------------------------------------------------------------------------%

:- module mdprof_fb.automatic_parallelism.
:- interface.

:- import_module profile.
:- import_module message.
:- import_module mdbcomp.
:- import_module mdbcomp.feedback.

:- import_module cord.
:- import_module int.
:- import_module float.

%-----------------------------------------------------------------------------%

    % Build the candidate parallel conjunctions feedback information used for
    % implicit parallelism.
    %
:- pred candidate_parallel_conjunctions(
    candidate_parallel_conjunctions_opts::in, deep::in, cord(message)::out,
    feedback_info::in, feedback_info::out) is det.

:- type candidate_parallel_conjunctions_opts
    --->    candidate_parallel_conjunctions_opts(
                cpc_desired_parallelism     :: float,
                cpc_sparking_cost           :: int,
                cpc_locking_cost            :: int,
                cpc_clique_threshold        :: int,
                cpc_call_site_threshold     :: int
            ).
    
%-----------------------------------------------------------------------------%
    
    % Perform Jerome's analysis and update the feedback info structure.
    %
:- pred css_list_above_threshold(calls_above_threshold_sorted_opts::in,
    deep::in, feedback_info::in, feedback_info::out) is det.

:- type calls_above_threshold_sorted_opts
    --->    calls_above_threshold_sorted_opts(
                cats_measure                :: stat_measure,
                cats_threshold              :: int
            ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module create_report.
:- import_module mdbcomp.program_representation.
:- import_module measurements.
:- import_module program_representation_utils.
:- import_module report.
:- import_module var_use_analysis.

:- import_module array.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module pqueue.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module svset.

%----------------------------------------------------------------------------%
%
% The code in this section has some trace goals that can be enabled with:
%	--trace-flag=debug_cpc_search
%	  Debug the traversal through the clique tree.
%
%	--trace-flag=debug_recursive_costs 
%     Debug the calculation of the costs of recursive call sites.
%

candidate_parallel_conjunctions(Opts, Deep, Messages, !Feedback) :-
    Opts = candidate_parallel_conjunctions_opts(DesiredParallelism,
        SparkingCost, LockingCost, _CliqueThreshold, _CallSiteThreshold),

    % Find opertunities for parallelism by walking the clique tree.  Don't
    % Descened into cliques cheaper than the threshold.
    deep_lookup_clique_index(Deep, Deep ^ root, RootCliquePtr),
    TotalCallseqs = Deep ^ profile_stats ^ num_callseqs,
    % The +1 here accounts for the cost of the pseudo call into the mercury
    % runtime.
    RootCliqueCost = cost_info(1, TotalCallseqs + 1),
    candidate_parallel_conjunctions_clique(Opts, Deep, RootCliqueCost, 
        RootCliquePtr, ConjunctionsMultiMap, Messages),

    multi_map.to_flat_assoc_list(ConjunctionsMultiMap, ConjunctionsAssocList),
    CandidateParallelConjunctions =
        feedback_data_candidate_parallel_conjunctions(DesiredParallelism,
        SparkingCost, LockingCost, ConjunctionsAssocList),
    put_feedback_data(CandidateParallelConjunctions, !Feedback).

:- type implicit_parallelism_info
    --->    implicit_parallelism_info(
                ipi_deep            :: deep,
                ipi_progrep         :: prog_rep,
                ipi_opts            :: candidate_parallel_conjunctions_opts,
                ipi_call_sites      :: map(goal_path, clique_call_site_report),
                ipi_rec_call_sites  :: map(goal_path, cost_info),
                ipi_var_table       :: var_table
            ).

:- type cost_info
    --->    cost_info(
                cci_calls           :: int,
                cci_callseqs_total  :: int 
            ).

:- type candidate_par_conjunctions ==
    multi_map(string_proc_label, candidate_par_conjunction).

:- pred candidate_parallel_conjunctions_clique(
    candidate_parallel_conjunctions_opts::in, deep::in, cost_info::in,
    clique_ptr::in, candidate_par_conjunctions::out, cord(message)::out) 
    is det.

candidate_parallel_conjunctions_clique(Opts, Deep, ParentCostInfo, CliquePtr,
        Candidates, Messages) :-
    create_clique_report(Deep, CliquePtr, MaybeCliqueReport),
    (
        MaybeCliqueReport = ok(CliqueReport),
        CliqueProcs = CliqueReport ^ cr_clique_procs,
        % All cliques must contain at least one procedure.
        ( [ FirstCliqueProcPrime ] = CliqueProcs ->
            FirstCliqueProc = FirstCliqueProcPrime
        ;
            error(this_file ++ "A clique must have et least one procedure")
        ),    
        CliqueIsRecursive = is_clique_recursive(CliqueReport),
        make_clique_proc_map(CliqueProcs, CliqueProcMap),
        candidate_parallel_conjunctions_clique_proc(Opts, Deep,
            CliqueIsRecursive, ParentCostInfo, set.init, CliqueProcMap,
            CliquePtr, FirstCliqueProc, Candidates, Messages)
    ;
        MaybeCliqueReport = error(Error),
        error(this_file ++ Error),
        Messages = cord.empty
    ).

:- type clique_is_recursive
    --->    clique_is_recursive
    ;       clique_is_not_recursive.

:- func is_clique_recursive(clique_report) = clique_is_recursive.

is_clique_recursive(CliqueReport) = CliqueIsRecursive :-
    CliqueProcs = CliqueReport ^ cr_clique_procs,
    ( CliqueProcs = [_, _ | _] ->
        % If there is more than one procedure then the clique must be mutually
        % recursive.  This computation is trivial compared to the case below.
        CliqueIsRecursive = clique_is_recursive
    ; CliqueProcs = [CliqueProc] ->
        % Look for a self recursion in the single clique procedure.
        CliquePtr = CliqueReport ^ cr_clique_ptr,
        ( 
            % If at least one call site within the clique's proc makes a call
            % to this same clique then this is a recursive clique - this also
            % covers higher-order calls.
            some [CliqueProcDyanmic, CallSite, CalleePerf]
            (
                (
                    CliqueProcDynamic = CliqueProc ^ cpr_first_proc_dynamic
                ;
                    member(CliqueProcDynamic, 
                        CliqueProc ^ cpr_other_proc_dynamics)
                ),
                member(CallSite, CliqueProcDynamic ^ cpdr_call_sites), 
                member(CalleePerf, CallSite ^ ccsr_callee_perfs),
                CliquePtr = CalleePerf ^ perf_row_subject ^ cdesc_clique_ptr
            ) 
        ->
            CliqueIsRecursive = clique_is_recursive
        ;
            CliqueIsRecursive = clique_is_not_recursive
        )
    ;
        error(this_file ++ "Clique must have at least one procedure")
    ).

    % Construct a map of clique proc reports.
    %
:- pred make_clique_proc_map(list(clique_proc_report)::in,
    map(proc_desc, clique_proc_report)::out) is det.

make_clique_proc_map(CliqueProcs, CliqueProcMap) :-
    list.foldl((pred(CliqueProc::in, Map0::in, Map::out) is det :-
            ProcDesc = CliqueProc ^ cpr_proc_summary ^ perf_row_subject,
            map.det_insert(Map0, ProcDesc, CliqueProc, Map)
        ), CliqueProcs, map.init, CliqueProcMap).

    % candidate_parallel_conjunctions_clique_proc(Opts, Deep, 
    %   CliqueIsRecursive, ParentCostInfo, ProcsAnalysed, CliquePtr,
    %   CliqueProc, Candidates, Messages) :-
    %
    % Find candidate parallel conjunctions within a clique_proc_report.
    %
    % ParentCostInfo gives the cost of the call site calling this clique so
    % that we may correctly calculate the per-call costs of recursive cliques
    % and their call sites.
    %
    % ProcsAnalysed keeps a set of procs we've visited to prevent unbound
    % recursion in this algorithm.
    %
    % CliqueProcMap is a map of proc_desc to clique_proc_report structures
    % extracted from the clique_report.
    %
    % CliquePtr is the clique that this proc belongs to.
    %
:- pred candidate_parallel_conjunctions_clique_proc(
    candidate_parallel_conjunctions_opts::in, deep::in, 
    clique_is_recursive::in, cost_info::in, set(proc_desc)::in,
    map(proc_desc, clique_proc_report)::in,
    clique_ptr::in, clique_proc_report::in,
    candidate_par_conjunctions::out, cord(message)::out) is det.

candidate_parallel_conjunctions_clique_proc(Opts, Deep, 
        CliqueIsRecursive, ParentCostInfo, ProcsAnalysed0, CliqueProcMap, 
        CliquePtr, CliqueProc, Candidates, Messages) :-
    some [!Messages]
    (
        !:Messages = cord.empty,
        % Use the total cost the call to this procedure to decide if we should
        % stop recursing the call graph at this point.  If the procedure does
        % not contribute to the runtime of the program in an absolute way then
        % do not recurse further.  This test is performed here rather than in
        % the callees of this predicate to avoid duplication of code.
        ParentCostInfo = cost_info(_Calls, TotalCost),
        ( TotalCost > Opts ^ cpc_clique_threshold ->
            % Determine the costs of the call sites in the procedure.
            (
                CliqueIsRecursive = clique_is_recursive,
                build_recursive_call_site_cost_map(Deep, CliqueProc, CliquePtr,
                    ParentCostInfo, RecursiveCallSiteCostMap, CSCMMessages),
                !:Messages = !.Messages ++ CSCMMessages,
                trace [compile_time(flag("debug_cpc_search")), io(!IO)]
                  io.format(
                    "D: In clique %s recursive call site cost map is: %s\n",
                    [s(string(CliquePtr)), s(string(RecursiveCallSiteCostMap))],
                    !IO)
            ;
                CliqueIsRecursive = clique_is_not_recursive,
                RecursiveCallSiteCostMap = map.init
            ),

            % Analyse this procedure for parallelism opportunities.
            candidate_parallel_conjunctions_proc(Opts, Deep, CliqueProc,
                RecursiveCallSiteCostMap, ProcCandidates, ProcMessages),
            !:Messages = !.Messages ++ ProcMessages,

            ProcDesc = CliqueProc ^ cpr_proc_summary ^ perf_row_subject,

            % Get a list of call sites
            ( CliqueProc ^ cpr_other_proc_dynamics = [_ | _] ->
                proc_label_from_proc_desc(Deep, ProcDesc, ProcLabel),
                append_message(proc(ProcLabel),
                    error_extra_proc_dynamics_in_clique_proc,
                    !Messages)
            ;
                true
            ),
            CallSiteReports = 
                CliqueProc ^ cpr_first_proc_dynamic ^ cpdr_call_sites,
            % Analyse child cliques of this clique proc for parallelism
            % opportunities.  Recursive calls point to this same clique, in
            % these cases call candidate_parallel_conjunctions_clique_proc on
            % the procedure within this clique that they call.
            set.insert(ProcsAnalysed0, ProcDesc, ProcsAnalysed),
            list.map2(candidate_parallel_conjunctions_call_site(Opts, Deep,
                    ProcsAnalysed, CliqueIsRecursive, RecursiveCallSiteCostMap,
                    CliqueProcMap, CliquePtr),
                CallSiteReports, CSCandidatesList, CSMessagesList),
          
            list.foldl(multi_map.merge, CSCandidatesList, multi_map.init,
                CSCandidates),
            Candidates = multi_map.merge(ProcCandidates, CSCandidates),
            !:Messages = !.Messages ++ cord_list_to_cord(CSMessagesList)
        ;
            Candidates = multi_map.init,
            trace [compile_time(flag("debug_cpc_search")), io(!IO)]
                io.format("D: Not entering cheap clique: %s with cost %s\n",
                    [s(string(CliquePtr)), s(string(ParentCostInfo))], !IO)
        ),
        Messages = !.Messages
    ).

:- pred candidate_parallel_conjunctions_call_site(
    candidate_parallel_conjunctions_opts::in, deep::in, set(proc_desc)::in,
    clique_is_recursive::in, map(goal_path, cost_info)::in,
    map(proc_desc, clique_proc_report)::in, clique_ptr::in,
    clique_call_site_report::in, candidate_par_conjunctions::out,
    cord(message)::out) is det.

candidate_parallel_conjunctions_call_site(Opts, Deep, ProcsAnalysed,
        CliqueIsRecursive, RecursiveCallSiteCostMap, CliqueProcMap, CliquePtr,
        CallSiteReport, Candidates, Messages) :-
    % XXX: This does not weight the callees by the probability that they will
    % be called.  This is only a problem for higher order call sites.
    CalleePerfs = CallSiteReport ^ ccsr_callee_perfs,
    CallSiteDesc = CallSiteReport ^ ccsr_call_site_summary ^ perf_row_subject,
    list.map2(candidate_parallel_conjunctions_callee(Opts, Deep, ProcsAnalysed,
            CliqueIsRecursive, RecursiveCallSiteCostMap, CliqueProcMap,
            CliquePtr, CallSiteDesc),
        CalleePerfs, CandidatesList, MessagesList),
    list.foldl(multi_map.merge, CandidatesList, multi_map.init, Candidates),
    Messages = cord_list_to_cord(MessagesList).

:- pred candidate_parallel_conjunctions_callee(
    candidate_parallel_conjunctions_opts::in, deep::in, set(proc_desc)::in,
    clique_is_recursive::in, map(goal_path, cost_info)::in,
    map(proc_desc, clique_proc_report)::in, clique_ptr::in, call_site_desc::in,
    perf_row_data(clique_desc)::in, candidate_par_conjunctions::out,
    cord(message)::out) is det.

candidate_parallel_conjunctions_callee(Opts, Deep, ProcsAnalysed0,
        ParentCliqueIsRecursive, RecursiveCallSiteCostMap, CliqueProcReportMap,
        ParentCliquePtr, CallSiteDesc, CliquePerf, Candidates, Messages) :-
    CliqueDesc = CliquePerf ^ perf_row_subject,
    CliquePtr = CliqueDesc ^ cdesc_clique_ptr,
    CliqueEntryProc = CliqueDesc ^ cdesc_entry_member,
    MaybePerfTotal = CliquePerf ^ perf_row_maybe_total,
    (
        MaybePerfTotal = yes(PerfTotal)
    ;
        MaybePerfTotal = no,
        error(this_file ++ 
            "Could not retrive total callseqs cost from clique")
    ),
    CliqueCost = PerfTotal ^ perf_row_callseqs,
    Calls = CliquePerf ^ perf_row_calls,
    CostInfo = cost_info(Calls, CliqueCost),
    ( ParentCliquePtr = CliquePtr ->
        % This is a recursive call within the same clique.
        ( member(CliqueEntryProc, ProcsAnalysed0) ->
            % If we've analysed this clique in this proc already then don't do
            % it again.
            Candidates = multi_map.init,
            Messages = cord.empty
        ;
            map.lookup(CliqueProcReportMap, CliqueEntryProc, CliqueProcReport),
            ProcsAnalysed = set.insert(ProcsAnalysed0, CliqueEntryProc),
            % We determine the cost of the call site we're following within
            % this clique to this procedure so that it can have correct cost
            % information.
            map.lookup(RecursiveCallSiteCostMap, 
                CallSiteDesc ^ csdesc_goal_path, CallCostInfo),
            candidate_parallel_conjunctions_clique_proc(Opts, Deep,
                ParentCliqueIsRecursive, CallCostInfo, ProcsAnalysed, 
                CliqueProcReportMap, ParentCliquePtr, CliqueProcReport, 
                Candidates, Messages)
        )
    ;
        ( CliqueCost > Opts ^ cpc_clique_threshold ->
            candidate_parallel_conjunctions_clique(Opts, Deep, CostInfo,
                CliquePtr, Candidates, Messages)
        ;
            Candidates = multi_map.init, 
            Messages = cord.empty
        )
    ).

%----------------------------------------------------------------------------%

:- pred build_recursive_call_site_cost_map(deep::in, clique_proc_report::in,
    clique_ptr::in, cost_info::in, map(goal_path, cost_info)::out,
    cord(message)::out) is det.

build_recursive_call_site_cost_map(Deep, CliqueProc, CliquePtr,
        ParentCostInfo, RecursiveCallSiteCostMap, Messages) :-
    % Lookup the proc static to find the ProcLabel.
    PerfRowData = CliqueProc ^ cpr_proc_summary,
    TotalProcCalls = PerfRowData ^ perf_row_calls,
    ProcDesc = PerfRowData ^ perf_row_subject,
    proc_label_from_proc_desc(Deep, ProcDesc, ProcLabel),

    ( CliqueProc ^ cpr_other_proc_dynamics = [_ | _] ->
        append_message(proc(ProcLabel),
            error_extra_proc_dynamics_in_clique_proc, cord.empty, Messages)
    ;
        Messages = cord.empty
    ),

    cost_info(ParentCSCalls, ParentCSCost) = ParentCostInfo,
    CallSites = CliqueProc ^ cpr_first_proc_dynamic ^ cpdr_call_sites,
    % Divide ParentCalls by the number of recursive calls to determine the
    % fraction of calls into this procedure from outside the clique compared to
    % calls from within the clique, use this to calculate the number of calls
    % at the top level of the recursion of the call sites.
    ProcCallsRecFraction = float(ParentCSCalls) / float(TotalProcCalls),
    list.foldl3(get_callsite_cost_infos(CliquePtr, ProcCallsRecFraction), 
        CallSites, 0, NonRecCSCost, 0.0, RecursiveCSCalls, 
        set.init, RecursiveCS),

    % The negative one here represents the call from the parent into this
    % procedure.
    RecursiveCost = ParentCSCost - 0 - NonRecCSCost,
    ( RecursiveCSCalls = 0.0 ->
        RecursiveCostPerCall = 0.0
    ;
        RecursiveCostPerCall = 
            float(RecursiveCost) / RecursiveCSCalls
    ),
    trace [compile_time(flag("debug_recursive_costs")), io(!IO)]
        io.format(
            "D: In clique proc: %s-%s\n\tRecursiveCostPerCall = %d/%f = %f\n",
            [s(string(CliquePtr)), s(string(ProcLabel)), 
             i(RecursiveCost), f(RecursiveCSCalls), f(RecursiveCostPerCall)], 
            !IO),

    set.fold(
        build_recursive_call_site_cost_map_call_site(RecursiveCostPerCall),
        RecursiveCS, map.init, RecursiveCallSiteCostMap). 

:- pred get_callsite_cost_infos(clique_ptr::in, float::in, 
    clique_call_site_report::in, int::in, int::out, float::in, float::out, 
    set(clique_call_site_report)::in, set(clique_call_site_report)::out) is det.

get_callsite_cost_infos(ThisClique, ParentCallsRecFraction, CallSite,
        !NonRecCSCost, !RecursiveCalls, !RecursiveCallSites) :-
    CSSummary = CallSite ^ ccsr_call_site_summary,
    MaybeTotal = CSSummary ^ perf_row_maybe_total,
    (
        MaybeTotal = yes(Total),
        Cost = Total ^ perf_row_callseqs
    ;
        MaybeTotal = no,
        error("clique_call_site has 'no' for perf_row_maybe_total")
    ),
    (
        % Note that according to this any higher order call site that is
        % recursive in some cases and non-recursive in others is considered to
        % be recursive.  The cost of it's non-recursive calls is not factored
        % into the calculation of the cost of recursive call sites.
        member(CalleePerf, CallSite ^ ccsr_callee_perfs),
        CalleePerf ^ perf_row_subject ^ cdesc_clique_ptr = ThisClique
    ->
        !:RecursiveCalls = !.RecursiveCalls + 
            (float(CSSummary ^ perf_row_calls) * ParentCallsRecFraction),
        svset.insert(CallSite, !RecursiveCallSites)
    ;
        !:NonRecCSCost = !.NonRecCSCost + Cost
    ).

:- pred build_recursive_call_site_cost_map_call_site(float::in,
    clique_call_site_report::in, map(goal_path, cost_info)::in,
    map(goal_path, cost_info)::out) is det.
    
build_recursive_call_site_cost_map_call_site(RecursiveCostPerCall, CallSite,
        !Map) :-
    CSSummary = CallSite ^ ccsr_call_site_summary,
    Calls = CSSummary ^ perf_row_calls,
    Cost = RecursiveCostPerCall * float(Calls),
    CostInfo = cost_info(Calls, round_to_int(Cost)),
    GoalPath = CSSummary ^ perf_row_subject ^ csdesc_goal_path,
    svmap.det_insert(GoalPath, CostInfo, !Map).

%----------------------------------------------------------------------------%

    % Find candidate parallel conjunctions within the given procedure.
    %
:- pred candidate_parallel_conjunctions_proc(
    candidate_parallel_conjunctions_opts::in, deep::in,
    clique_proc_report::in, map(goal_path, cost_info)::in,
    candidate_par_conjunctions::out,
    cord(message)::out) is det.

candidate_parallel_conjunctions_proc(Opts, Deep, CliqueProc,
        RecursiveCallSiteCostMap, Candidates, Messages) :-
    % Lookup the proc static to find the ProcLabel.
    PerfRowData = CliqueProc ^ cpr_proc_summary,
    ProcDesc = PerfRowData ^ perf_row_subject,
    proc_label_from_proc_desc(Deep, ProcDesc, ProcLabel),
    
    CallSites = CliqueProc ^ cpr_first_proc_dynamic ^ cpdr_call_sites,
    ( CliqueProc ^ cpr_other_proc_dynamics = [_ | _] ->
        append_message(proc(ProcLabel), 
            error_extra_proc_dynamics_in_clique_proc,
            cord.empty, Messages0)
    ;
        Messages0 = cord.empty
    ),
    list.foldl(add_call_site_report_to_map,
        CallSites, map.init, CallSitesMap),
    deep_get_progrep_det(Deep, ProgRep),
    ( progrep_search_proc(ProgRep, ProcLabel, ProcRep) ->
        ProcRep ^ pr_defn = ProcDefnRep,
        ProcDefnRep ^ pdr_goal = Goal,
        ProcDefnRep ^ pdr_var_table = VarTable,
        Info = implicit_parallelism_info(Deep, ProgRep, Opts,
            CallSitesMap, RecursiveCallSiteCostMap, VarTable),
        goal_get_conjunctions_worth_parallelising(Goal, empty_goal_path, Info,
            ProcLabel, Candidates0, _, _,
            Messages, initial_inst_map(ProcDefnRep), _),
        list.foldl((pred(Candidate::in, Map0::in, Map::out) is det :-
                multi_map.set(Map0, ProcLabel, Candidate, Map)
            ), Candidates0, multi_map.init, Candidates)
    ;
        % Builtin procedures cannot be found in the program representation, and
        % cannot be parallelised either.
        Candidates = multi_map.init,
        append_message(proc(ProcLabel), warning_cannot_lookup_proc_defn,
            Messages0, Messages)
    ).
    
:- pred goal_get_conjunctions_worth_parallelising(goal_rep::in, goal_path::in,
    implicit_parallelism_info::in, string_proc_label::in, 
    list(candidate_par_conjunction)::out, seen_duplicate_instantiation::out,
    maybe_call_conjunct::out, cord(message)::out, inst_map::in, inst_map::out) 
    is det.

goal_get_conjunctions_worth_parallelising(Goal, GoalPath, Info, ProcLabel, 
        Candidates, SeenDuplicateInstantiation, MaybeCallConjunct,
        Messages, !InstMap) :-
    Goal = goal_rep(GoalExpr, Detism, _),
    (
        (
            GoalExpr = conj_rep(Conjuncts),
            conj_get_conjunctions_worth_parallelising(Conjuncts, GoalPath,
                Info, ProcLabel, Candidates, SeenDuplicateInstantiation,
                Messages, !InstMap) 
        ;
            GoalExpr = disj_rep(Disjuncts),
            disj_get_conjunctions_worth_parallelising(Disjuncts, GoalPath, 1,
                Info, ProcLabel, Candidates, SeenDuplicateInstantiation,
                Messages, !InstMap)
        ;
            GoalExpr = switch_rep(_, _, Cases),
            switch_case_get_conjunctions_worth_parallelising(Cases, GoalPath, 1,
                Info, ProcLabel, Candidates, SeenDuplicateInstantiation,
                Messages, !InstMap)
        ;
            GoalExpr = ite_rep(Cond, Then, Else),
            ite_get_conjunctions_worth_parallelising(Cond, Then, Else,
                GoalPath, Info, ProcLabel, Candidates,
                SeenDuplicateInstantiation, Messages, !InstMap)
        ;
            GoalExpr = scope_rep(SubGoal, MaybeCut),
            ScopeGoalPath = 
                goal_path_add_at_end(GoalPath, step_scope(MaybeCut)),
            goal_get_conjunctions_worth_parallelising(SubGoal, ScopeGoalPath,
                Info, ProcLabel, Candidates, SeenDuplicateInstantiation, _,
                Messages, !InstMap) 
        ;
            GoalExpr = negation_rep(SubGoal),
            NegGoalPath = goal_path_add_at_end(GoalPath, step_neg),
            goal_get_conjunctions_worth_parallelising(SubGoal, NegGoalPath,
                Info, ProcLabel, Candidates, SeenDuplicateInstantiation, _,
                Messages, !InstMap) 
        ),
        % TODO: Parallelising conjunctions like 
        %   ( call1(A, B) , not call2(C, D) )
        % may be easy to do when writing the compiler's part of the code, if so
        % then MaybeCallAboveThreshold should probably be set from some of
        % these non-atomic goals based on goals within them.
        MaybeCallConjunct = non_atomic_goal 
    ;
        GoalExpr = atomic_goal_rep(_, _, BoundVars, AtomicGoal),
        InstMapBeforeCall = !.InstMap,
        % The binding of a variable may depend on any number of other
        % variables, and recursively the variables that those depended-on
        % variables depend upon.  
        % Except that variables involved in control flow (switched on vars,
        % vars in ITE conds) however this never comes up as for-now we only
        % consider atomic goals.
        atomic_goal_get_vars(AtomicGoal, AtomicGoalVars0),
        list.foldl((pred(Var::in, Set0::in, Set::out) is det :-
                ( set.remove(Set0, Var, SetPrime) ->
                    Set = SetPrime
                ;
                    Set = Set0
                )
            ), BoundVars, AtomicGoalVars0, AtomicGoalVars),
        inst_map_ground_vars(BoundVars, AtomicGoalVars, !InstMap,
            SeenDuplicateInstantiation),
        maybe_call_site_conjunct(Info, GoalPath, AtomicGoal, Detism,
            InstMapBeforeCall, !.InstMap, BoundVars, MaybeCallConjunct),
        Messages = cord.empty,
        Candidates = []
    ).

:- pred conj_get_conjunctions_worth_parallelising(list(goal_rep)::in,
    goal_path::in, implicit_parallelism_info::in, string_proc_label::in,
    list(candidate_par_conjunction)::out, seen_duplicate_instantiation::out, 
    cord(message)::out, inst_map::in, inst_map::out) is det.

conj_get_conjunctions_worth_parallelising(Conjs, GoalPath, Info,
        ProcLabel, Candidates, SeenDuplicateInstantiation, Messages, 
        !InstMap) :-
    some [!Messages] 
    (
        % Note: it will be better to look at each pair of conjuncts, determine
        % if they are parallelisable (perhaps by placing middle goals in either
        % of the parallel conjuncts to create the optimum amount of
        % parallelism.  This will need to have an in-order representation of
        % goals, and for each variable seen have a tree of variables it depends
        % upon.
        %
        % For now consider parallelising conjuncts that separated only by other
        % atomic goals.
        conj_get_conjunctions_worth_parallelising_2(Conjs, GoalPath, 1, Info,
            ProcLabel, Candidates0, CallSiteConjuncts, 
            SeenDuplicateInstantiation, !:Messages, !InstMap),
         
        build_candidate_conjunctions(Info, !.InstMap, GoalPath, ProcLabel,
            list(CallSiteConjuncts), MessagesBCC, pqueue.init, CandidatesQueue),
        !:Messages = !.Messages ++ MessagesBCC,
        % Pick best candidate from queue.
        (
            SeenDuplicateInstantiation = have_not_seen_duplicate_instantiation,
            ( pqueue.remove(CandidatesQueue, _, Candidate, CandidatesQueuePrime) ->
                Candidates = [Candidate | Candidates0],
                (
                    pqueue.length(CandidatesQueuePrime) = Length,
                    Length > 0
                ->
                    append_message(goal(ProcLabel, GoalPath),
                        notice_extra_callpairs_in_conjunction(Length), 
                        !Messages)
                ;
                    true
                ),
                append_message(goal(ProcLabel, GoalPath),
                    info_found_candidate_conjunction,
                    !Messages)
            ;
                Candidates = Candidates0
            )
        ;
            SeenDuplicateInstantiation = seen_duplicate_instantiation,
            Candidates = Candidates0,
            (
                pqueue.length(CandidatesQueue) = Length,
                Length >= 1 
            ->
                append_message(goal(ProcLabel, GoalPath), 
                    notice_duplicate_instantiation(Length), 
                    !Messages)
            ;
                true
            )
        ),
        Messages = !.Messages
    ).

:- pred conj_get_conjunctions_worth_parallelising_2(list(goal_rep)::in,
    goal_path::in, int::in, implicit_parallelism_info::in,
    string_proc_label::in, list(candidate_par_conjunction)::out,
    cord(maybe_call_conjunct)::out, seen_duplicate_instantiation::out,
    cord(message)::out, inst_map::in, inst_map::out) is det.

conj_get_conjunctions_worth_parallelising_2([], _, _, _, _, [], cord.empty, 
        have_not_seen_duplicate_instantiation, cord.empty, !InstMap).
conj_get_conjunctions_worth_parallelising_2([Conj | Conjs], GoalPath,
        ConjunctNum, Info, ProcLabel, Candidates, CallSiteConjuncts,
        SeenDuplicateInstantiation, Messages, !InstMap) :-
    ConjGoalPath = goal_path_add_at_end(GoalPath, step_conj(ConjunctNum)),
    goal_get_conjunctions_worth_parallelising(Conj, ConjGoalPath, Info,
        ProcLabel, CandidatesHead, SeenDuplicateInstantiationHead,
        MaybeCallSiteConjunct, MessagesHead, !InstMap), 
    
    conj_get_conjunctions_worth_parallelising_2(Conjs, GoalPath, ConjunctNum+1,
        Info, ProcLabel, CandidatesTail, CallSiteConjuncts0,
        SeenDuplicateInstantiationTail, MessagesTail, !InstMap),

    Candidates = CandidatesHead ++ CandidatesTail,
    Messages = MessagesHead ++ MessagesTail,
    CallSiteConjuncts = cord.cons(MaybeCallSiteConjunct, CallSiteConjuncts0),
    SeenDuplicateInstantiation = merge_seen_duplicate_instantiation(
        SeenDuplicateInstantiationHead,
        SeenDuplicateInstantiationTail).

:- pred disj_get_conjunctions_worth_parallelising(list(goal_rep)::in,
    goal_path::in, int::in, implicit_parallelism_info::in, 
    string_proc_label::in, list(candidate_par_conjunction)::out,
    seen_duplicate_instantiation::out, cord(message)::out,
    inst_map::in, inst_map::out) is det.

disj_get_conjunctions_worth_parallelising([], _, _, _, _, [],
    have_not_seen_duplicate_instantiation, cord.empty, !InstMap).
disj_get_conjunctions_worth_parallelising([Disj | Disjs], GoalPath, DisjNum,
        Info, ProcLabel, Candidates, SeenDuplicateInstantiation, 
        Messages, InstMap0, InstMap) :-
    DisjGoalPath = goal_path_add_at_end(GoalPath, step_disj(DisjNum)),
    HeadDetism = Disj ^ goal_detism_rep,
    goal_get_conjunctions_worth_parallelising(Disj, DisjGoalPath, Info,
        ProcLabel, HeadCandidates, HeadSeenDuplicateInstantiation, 
        _MaybeCallConjunct, HeadMessages, InstMap0, HeadInstMap),
    disj_get_conjunctions_worth_parallelising(Disjs, GoalPath, DisjNum + 1,
        Info, ProcLabel, TailCandidates, TailSeenDuplicateInstantiation,
        TailMessages, InstMap0, TailInstMap),
    Candidates = HeadCandidates ++ TailCandidates,
    Messages = HeadMessages ++ TailMessages,
    % merge_inst_map requires the detism of goals that produce both inst maps,
    % we can create fake values that satisfy merge_inst_map easily.
    (
        Disjs = [],
        TailDetism = failure_rep
    ;
        Disjs = [_ | _],
        TailDetism = det_rep
    ),
    InstMap = merge_inst_map(HeadInstMap, HeadDetism, TailInstMap, TailDetism),
    SeenDuplicateInstantiation = merge_seen_duplicate_instantiation(
        HeadSeenDuplicateInstantiation,
        TailSeenDuplicateInstantiation).

:- pred switch_case_get_conjunctions_worth_parallelising(list(case_rep)::in,
    goal_path::in, int::in, implicit_parallelism_info::in,
    string_proc_label::in, list(candidate_par_conjunction)::out,
    seen_duplicate_instantiation::out, cord(message)::out, 
    inst_map::in, inst_map::out) is det.

switch_case_get_conjunctions_worth_parallelising([], _, _, _, _, [],
    have_not_seen_duplicate_instantiation, cord.empty, !InstMap).
switch_case_get_conjunctions_worth_parallelising([Case | Cases], GoalPath,
        CaseNum, Info, ProcLabel, Candidates, SeenDuplicateInstantiation,
        Messages, InstMap0, InstMap) :-
    Case = case_rep(_, _, Goal),
    HeadDetism = Goal ^ goal_detism_rep,
    CaseGoalPath = goal_path_add_at_end(GoalPath, step_switch(CaseNum, no)),
    goal_get_conjunctions_worth_parallelising(Goal, CaseGoalPath, Info,
        ProcLabel, HeadCandidates, HeadSeenDuplicateInstantiation, 
        _MaybeCallConjs, HeadMessages, InstMap0, HeadInstMap),
    switch_case_get_conjunctions_worth_parallelising(Cases, GoalPath, 
        CaseNum + 1, Info, ProcLabel, TailCandidates,
        TailSeenDuplicateInstantiation, TailMessages, InstMap0, TailInstMap),
    Candidates = HeadCandidates ++ TailCandidates,
    Messages = HeadMessages ++ TailMessages,
    % merge_inst_map requires the detism of goals that produce both inst maps,
    % we can create fake values that satisfy merge_inst_map easily.
    (
        Cases = [],
        TailDetism = failure_rep
    ;
        Cases = [_ | _],
        TailDetism = det_rep
    ),
    InstMap = merge_inst_map(HeadInstMap, HeadDetism, TailInstMap, TailDetism),
    SeenDuplicateInstantiation = merge_seen_duplicate_instantiation(
        HeadSeenDuplicateInstantiation,
        TailSeenDuplicateInstantiation).

:- pred ite_get_conjunctions_worth_parallelising(goal_rep::in, goal_rep::in,
    goal_rep::in, goal_path::in, implicit_parallelism_info::in,
    string_proc_label::in, list(candidate_par_conjunction)::out,
    seen_duplicate_instantiation::out, cord(message)::out,
    inst_map::in, inst_map::out) is det.

ite_get_conjunctions_worth_parallelising(Cond, Then, Else, GoalPath, Info,
        ProcLabel, Candidates, SeenDuplicateInstantiation, Messages, !InstMap) :-
    CondGoalPath = goal_path_add_at_end(GoalPath, step_ite_cond),
    ThenGoalPath = goal_path_add_at_end(GoalPath, step_ite_then),
    ElseGoalPath = goal_path_add_at_end(GoalPath, step_ite_else),
    goal_get_conjunctions_worth_parallelising(Cond, CondGoalPath, Info,
        ProcLabel, CondCandidates, CondSeenDuplicateInstantiation, _,
        CondMessages, !.InstMap, PostCondInstMap),
    goal_get_conjunctions_worth_parallelising(Then, ThenGoalPath, Info, 
        ProcLabel, ThenCandidates, ThenSeenDuplicateInstantiation, _,
        ThenMessages, PostCondInstMap, PostThenInstMap),
    goal_get_conjunctions_worth_parallelising(Else, ElseGoalPath, Info, 
        ProcLabel, ElseCandidates, ElseSeenDuplicateInstantiation, _, 
        ElseMessages, PostCondInstMap, PostElseInstMap),
    Candidates = CondCandidates ++ ThenCandidates ++ ElseCandidates,
    (
        CondSeenDuplicateInstantiation = have_not_seen_duplicate_instantiation,
        ThenSeenDuplicateInstantiation = have_not_seen_duplicate_instantiation,
        ElseSeenDuplicateInstantiation = have_not_seen_duplicate_instantiation
    ->
        SeenDuplicateInstantiation = have_not_seen_duplicate_instantiation
    ;
        SeenDuplicateInstantiation = seen_duplicate_instantiation
    ),
    Messages = CondMessages ++ ThenMessages ++ ElseMessages,
    ThenDetism = Then ^ goal_detism_rep,
    ElseDetism = Else ^ goal_detism_rep,
    !:InstMap = merge_inst_map(PostThenInstMap, ThenDetism, 
        PostElseInstMap, ElseDetism).

    % This type represents a goal, if the goal is a call extra information used
    % for parallelising it with another call is provided.
    %
    % This is similar to candidate_parallel_conjunct, except that it stores
    % information that's used for the rest of the implicit parallelism
    % analysis.  It must contain information for the following.
    %
    %   - Average cost information for this call site,
    %
    %   - Enough information to resolve the procedure call, (detism and
    %     argument modes).
    %
    %   - Information that can be used to determine if this is part of a
    %     dependant conjunction, and its role in the dependant conjunction.
    %
    %   - Enough information so that a candidate_par_conjuct structure can be
    %     constructed from it and the deep profiling info.
    %
:- type maybe_call_conjunct 
    --->    call(
                mccc_callee             :: maybe(pair(string, string)),
                mccc_detism             :: detism_rep,
                mccc_args               :: list(var_mode_and_use),
                mccc_call_site          :: clique_call_site_report
            )
    ;       trivial_atomic_goal(
                mccag_detism            :: detism_rep,
                mccag_bound_vars        :: list(var_rep)
            )
    ;       non_atomic_goal.

:- inst call
    --->    call(ground, ground, ground, ground).

    % A variable, it's mode and it's usage in the callee.  The mode information
    % is also summarised within the variable use information.
    %
:- type var_mode_and_use
    --->    var_mode_and_use(
                vmu_var                 :: var_rep,
                vmu_mode                :: var_mode_rep,
                vmu_use                 :: var_use_info
            ).

:- pred maybe_call_site_conjunct(implicit_parallelism_info::in, goal_path::in,
    atomic_goal_rep::in, detism_rep::in, inst_map::in, inst_map::in,
    list(var_rep)::in, maybe_call_conjunct::out) is det.

maybe_call_site_conjunct(Info, GoalPath, AtomicGoal, Detism,
        InstMapBefore, InstMapAfter, BoundVars, MaybeCallConjunct) :-
    (
        ( AtomicGoal = unify_construct_rep(_, _, _)
        ; AtomicGoal = unify_deconstruct_rep(_, _, _)
        ; AtomicGoal = partial_construct_rep(_, _, _)
        ; AtomicGoal = partial_deconstruct_rep(_, _, _)
        ; AtomicGoal = unify_assign_rep(_, _)
        ; AtomicGoal = cast_rep(_, _)
        ; AtomicGoal = unify_simple_test_rep(_, _)
        % Don't bother parallelising foreign code, builtins or events.
        ; AtomicGoal = pragma_foreign_code_rep(_)
        ; AtomicGoal = builtin_call_rep(_, _, _)
        ; AtomicGoal = event_call_rep(_, _)
        ),
        MaybeCallConjunct = trivial_atomic_goal(Detism, BoundVars) 
    ;
        ( AtomicGoal = higher_order_call_rep(_, Args)
        ; AtomicGoal = method_call_rep(_, _, Args)
        ; AtomicGoal = plain_call_rep(_, _, Args)
        ),
        (
            ( AtomicGoal = higher_order_call_rep(_, _)
            ; AtomicGoal = method_call_rep(_, _, _)
            ),
            MaybeCallee = no
        ; 
            AtomicGoal = plain_call_rep(ModuleName, CalleeName, _),
            MaybeCallee = yes(ModuleName - CalleeName)
        ),
        map.lookup(Info ^ ipi_call_sites, GoalPath, CallSite),
        % Lookup var use information.
        CallSiteKind = CallSite ^ ccsr_kind_and_callee,
        (
            CallSiteKind = normal_call_and_callee(NormalCalleeId, _),
            PSPtr = NormalCalleeId ^ pdesc_ps_ptr,
            Deep = Info ^ ipi_deep,
            create_proc_var_use_dump_report(Deep, PSPtr,
                MaybeVarUseDumpInfo),
            MaybeVarUseDumpInfo = ok(VarUseDumpInfo)
        ->
            VarUseInfos = VarUseDumpInfo ^ pvui_var_uses, 
            list.map_corresponding((pred(Arg::in, VarUseInfo::in, 
                        VarModeAndUse::out) is det :-
                    var_get_mode(InstMapBefore, InstMapAfter, Arg, ArgMode),
                    VarModeAndUse = var_mode_and_use(Arg, ArgMode,
                        VarUseInfo)
                ), Args, VarUseInfos, VarModeAndUses)
        ;
            list.map((pred(Arg::in, VarModeAndUse::out) is det :-
                    var_get_mode(InstMapBefore, InstMapAfter, Arg, ArgMode),
                    var_mode_to_var_use(ArgMode, VarUseType),
                    pessimistic_var_use_info(VarUseType, VarUseInfo),
                    VarModeAndUse = var_mode_and_use(Arg, ArgMode,
                        VarUseInfo)
                ), Args, VarModeAndUses)
        ),
        MaybeCallConjunct = 
            call(MaybeCallee, Detism, VarModeAndUses, CallSite)
    ).

:- pred var_get_mode(inst_map::in, inst_map::in, var_rep::in, var_mode_rep::out)
    is det.

var_get_mode(InstMapBefore, InstMapAfter, VarRep, VarModeRep) :-
    inst_map_get(InstMapBefore, VarRep, InstBefore, _),
    inst_map_get(InstMapAfter, VarRep, InstAfter, _),
    VarModeRep = var_mode_rep(InstBefore, InstAfter).

    % Note: this runs in quadratic time.
    %
:- pred build_candidate_conjunctions(implicit_parallelism_info::in,
    inst_map::in, goal_path::in, string_proc_label::in, 
    list(maybe_call_conjunct)::in, cord(message)::out,
    pqueue(float, candidate_par_conjunction)::in, 
    pqueue(float, candidate_par_conjunction)::out) is det.

build_candidate_conjunctions(_, _, _, _, [], cord.empty, !Candidates).
build_candidate_conjunctions(Info, InstMap, GoalPath, ProcLabel,
        [MaybeCall | MaybeCalls], Messages, !Candidates) :-
    (
        MaybeCall = call(_, _, _, CallSiteReport),
        PercallCost = percall_cost(get_call_site_cost(Info, CallSiteReport)),
        ( PercallCost > float(Info ^ ipi_opts ^ cpc_call_site_threshold) ->
            % This conjunction is a call and is expensive enough to
            % parallelise, find some later conjunct to parallelise against it.
            build_candidate_conjunctions_2(Info, InstMap, GoalPath, ProcLabel,
                MaybeCall, cord.empty, MaybeCalls, Messages0, !Candidates)
            % XXX: pick the most expensive non-overlapping candidates from the
            % result.
        ;
            Messages0 = cord.empty,
            trace [compile_time(flag("debug_cpc_search")), io(!IO)]
                io.format("D: Call too cheap: %s %s %f\n", 
                    [s(string(ProcLabel)), 
                     s(goal_path_to_string(CallSiteReport 
                        ^ ccsr_call_site_summary 
                        ^ perf_row_subject 
                        ^ csdesc_goal_path)),
                     f(PercallCost)], !IO)
        )
    ;
        MaybeCall = non_atomic_goal,
        Messages0 = cord.empty
    ;
        MaybeCall = trivial_atomic_goal(_, _),
        Messages0 = cord.empty
    ),
    build_candidate_conjunctions(Info, InstMap, GoalPath, ProcLabel, MaybeCalls,
        MessagesTail, !Candidates),
    Messages = Messages0 ++ MessagesTail.

:- pred build_candidate_conjunctions_2(implicit_parallelism_info::in,
    inst_map::in, goal_path::in, string_proc_label::in, 
    maybe_call_conjunct::in(call), cord(maybe_call_conjunct)::in,
    list(maybe_call_conjunct)::in, cord(message)::out,
    pqueue(float, candidate_par_conjunction)::in, 
    pqueue(float, candidate_par_conjunction)::out) is det.

build_candidate_conjunctions_2(_, _, _, _, _, _, [], cord.empty, !Candidates).
build_candidate_conjunctions_2(Info, InstMap, GoalPath, ProcLabel, CallA,
        IntermediateGoals, [MaybeCall | MaybeCalls], Messages, !Candidates) :-
    (
        some [!Messages]
        (
            MaybeCall = call(_, _, _, CallSiteReport),
            !:Messages = cord.empty,
            CallB = MaybeCall,
            Cost = percall_cost(get_call_site_cost(Info, CallSiteReport)),
            ( Cost > float(Info ^ ipi_opts ^ cpc_call_site_threshold) ->
                % This conjunct is a call and is expensive enough to
                % parallelise.
                are_conjuncts_dependant(CallA, CallB, InstMap, Dependance),
                (
                    Dependance = conjuncts_are_dependant(DepVars),
                    compute_optimal_dependant_parallelisation(Info, 
                        CallA, CallB, DepVars, IntermediateGoals, InstMap,
                        CPCA, CPCB, Speedup, CODPMessages),
                    !:Messages = !.Messages ++ CODPMessages
                ;
                    Dependance = conjuncts_are_independent,
                    compute_independent_parallelisation_speedup(Info, 
                        CallA, CallB, CPCA, CPCB, Speedup)
                ),
                % XXX: This threshold should be configurable.
                ( Speedup > 0.0 ->
                    ( length(IntermediateGoals) = 0 -> 
                        GoalPathString = goal_path_to_string(GoalPath),
                        Candidate = candidate_par_conjunction(GoalPathString, 
                            CPCA, CPCB, Dependance, Speedup),
                        % So that the candidates with the greatest speedup are
                        % removed first multiply speedup by -1.0.
                        pqueue.insert(!.Candidates, Speedup * -1.0, Candidate,
                            !:Candidates)
                    ;
                        append_message(goal(ProcLabel, GoalPath),
                            notice_candidate_callpairs_not_adjacent,
                            !Messages)
                    )
                ;
                    true
                )
            ;
                % Don't recurse here, we don't parallelise over call goals.
                append_message(goal(ProcLabel, GoalPath),
                    notice_cannot_parallelise_over_cheap_call_goal, !Messages)
            ),
            Messages = !.Messages
        )
    ;
        MaybeCall = trivial_atomic_goal(_, _),
        build_candidate_conjunctions_2(Info, InstMap, GoalPath, ProcLabel, 
            CallA, cord.snoc(IntermediateGoals, MaybeCall), MaybeCalls,
            Messages, !Candidates)
    ;
        MaybeCall = non_atomic_goal,
        % Don't recurse in this case, we don't parallelise over non-atomic
        % goals yet.
        append_message(goal(ProcLabel, GoalPath),
            notice_cannot_parallelise_over_nonatomic_goal,
            cord.empty, Messages)
    ).

:- pred are_conjuncts_dependant(maybe_call_conjunct::in(call),
    maybe_call_conjunct::in(call), inst_map::in, conjuncts_are_dependant::out)
    is det.

are_conjuncts_dependant(CallA, CallB, InstMap, Dependance) :-
    % Conjuncts are dependant if there exists an input variable in CallB that
    % is made ground by CallA or depends upon a variable made ground by CallA.
    list.foldl(add_output_var_to_set, CallA ^ mccc_args, 
        set.init, CallAOutputs),
    list.foldl(are_conjuncts_dependant_var(CallAOutputs, InstMap), 
        CallB ^ mccc_args, set.init, DepVars),
    ( set.empty(DepVars) ->
        Dependance = conjuncts_are_independent
    ;
        Dependance = conjuncts_are_dependant(DepVars)
    ).

:- pred are_conjuncts_dependant_var(set(var_rep)::in, inst_map::in,
    var_mode_and_use::in, set(var_rep)::in, set(var_rep)::out) is det.

are_conjuncts_dependant_var(CallAOutputs, InstMap, VarModeAndUse, !DepVars) :-
    VarModeAndUse = var_mode_and_use(VarRep, VarModeRep, _),
    ( VarModeRep = var_mode_rep(ir_ground_rep, ir_ground_rep) ->
        inst_map_get_var_deps(InstMap, VarRep, VarDeps),
        (
            (
                contains(CallAOutputs, VarRep)
            ;
                member(VarDep, VarDeps),
                contains(CallAOutputs, VarDep)
            )
        ->
            svset.insert(VarRep, !DepVars)
        ;
            true
        )
    ;
        true
    ).

:- pred add_output_var_to_set(var_mode_and_use::in, 
    set(var_rep)::in, set(var_rep)::out) is det.

add_output_var_to_set(var_mode_and_use(VarRep, VarModeRep, _), !Set) :-
    ( VarModeRep = var_mode_rep(ir_free_rep, ir_ground_rep) ->
        svset.insert(VarRep, !Set)
    ;
        true
    ).

    % Retrieve the average cost of a call site.
    %
:- func get_call_site_cost(implicit_parallelism_info, clique_call_site_report) 
    = cost_info.

get_call_site_cost(Info, CallSite) = CostInfo :-
    CSSummary = CallSite ^ ccsr_call_site_summary,
    GoalPath = CSSummary ^ perf_row_subject ^ csdesc_goal_path,
    ( map.search(Info ^ ipi_rec_call_sites, GoalPath, CostInfoPrime) ->
        CostInfo = CostInfoPrime
    ;
        MaybePerfTotal = CSSummary ^ perf_row_maybe_total, 
        (
            MaybePerfTotal = yes(PerfTotal),
            Cost = PerfTotal ^ perf_row_callseqs,
            Calls = CSSummary ^ perf_row_calls
        ;
            MaybePerfTotal = no,
            error(this_file ++ 
                "Could not retrive total callseqs cost from call site")
        ),
        CostInfo = cost_info(Calls, Cost)
    ).

    % Given a cost_info structure calculate the percall cost.
    %
:- func percall_cost(cost_info) = float.

percall_cost(cost_info(Calls, Cost)) = PercallCost :-
    ( Calls = 0 ->
        % While this should be infinity or NaN if a call is never made then we
        % don't know it's potential cost, it should probably not be
        % parallelised and might as well be zero.
        PercallCost = 0.0
    ;
        PercallCost = float(Cost) / float(Calls)
    ).

:- pred compute_independent_parallelisation_speedup(
    implicit_parallelism_info::in, 
    maybe_call_conjunct::in(call), maybe_call_conjunct::in(call),
    candidate_par_conjunct::out, candidate_par_conjunct::out,
    float::out) is det.

compute_independent_parallelisation_speedup(Info, CallA, CallB, 
        CPCA, CPCB, Speedup) :-
    CostA = percall_cost(get_call_site_cost(Info, CallA ^ mccc_call_site)),
    CostB = percall_cost(get_call_site_cost(Info, CallB ^ mccc_call_site)),
    SequentialCost = CostA + CostB,
    ParallelCost = max(CostA, CostB) + 
        float(Info ^ ipi_opts ^ cpc_sparking_cost),
    Speedup = SequentialCost - ParallelCost,
    call_site_conj_to_candidate_par_conjunct(Info, CallA, CPCA),
    call_site_conj_to_candidate_par_conjunct(Info, CallB, CPCB).

:- pred compute_optimal_dependant_parallelisation(
    implicit_parallelism_info::in,
    maybe_call_conjunct::in(call), maybe_call_conjunct::in(call),
    set(var_rep)::in, cord(maybe_call_conjunct)::in, inst_map::in,
    candidate_par_conjunct::out, candidate_par_conjunct::out,
    float::out, cord(message)::out) is det.

compute_optimal_dependant_parallelisation(Info, CallA, CallB,
        DepVars, _IntermediateGoals, InstMap, CPCA, CPCB,
        Speedup, Messages) :-
    CostA = percall_cost(get_call_site_cost(Info, CallA ^ mccc_call_site)),
    CostB = percall_cost(get_call_site_cost(Info, CallB ^ mccc_call_site)),
    SequentialCost = CostA + CostB,
    ( singleton_set(DepVars, DepVar) ->
        % Only parallelise conjunctions with a single dependant variable for
        % now.
        ( get_var_use_from_args(CallB ^ mccc_args, DepVar, DepVarConsume) ->
            DepVarConsume = var_use_info(CostUntilConsume, _),
            (
                get_var_use_from_args(CallA ^ mccc_args, DepVar,
                    DepVarProduce)
            ->
                DepVarProduce = var_use_info(CostUntilProduction, _),
                CostBeforeProduction = 
                    cost_until_to_cost_since_start(CostUntilProduction, CostA),
                CostBeforeConsume = 
                    cost_until_to_cost_since_start(CostUntilConsume, CostB),
                CostAfterConsume = 
                    cost_until_to_cost_before_end(CostUntilConsume, CostB)
            ;
                inst_map_get_var_deps(InstMap, DepVar, DepVarDeps),
                set.fold(get_var_use_add_to_queue(CallA ^ mccc_args), 
                    DepVarDeps, pqueue.init, ProduceDepVarQueue),
                ( 
                    pqueue.remove(ProduceDepVarQueue, _,
                        CostUntilProductionPrime, _)
                ->
                    CostUntilProduction = CostUntilProductionPrime
                ;
                    error(this_file ++ 
                        "Expected to find at least one dependant variable")
                ),
                CostBeforeProduction0 = 
                    cost_until_to_cost_since_start(CostUntilProduction, CostA),
                CostAfterProduction0 = 
                    cost_until_to_cost_before_end(CostUntilProduction, CostA),
                CostBeforeConsume0 = 
                    cost_until_to_cost_since_start(CostUntilConsume, CostB),
                CostAfterConsume0 = 
                    cost_until_to_cost_before_end(CostUntilConsume, CostB),
                % Compare time before consume vs time after production, the
                % lesser one should have the unifications added to it.  This
                % maximises the amount of parallelism.
                ( CostBeforeConsume0 > CostAfterProduction0 ->
                    CostBeforeProduction = CostBeforeProduction0,
                    CostBeforeConsume = CostA,
                    CostAfterConsume = 0.0
                ;
                    CostBeforeProduction = 0.0,
                    CostBeforeConsume = CostA - CostAfterConsume0,
                    CostAfterConsume = CostAfterConsume0 
                )
            ),
            SparkingCost = float(Info ^ ipi_opts ^ cpc_sparking_cost),
            LockingCost = float(Info ^ ipi_opts ^ cpc_locking_cost),
            ParallelCost = max(CostA, 
                    max(CostBeforeProduction, CostBeforeConsume) 
                        + CostAfterConsume) 
                + SparkingCost + LockingCost,
            Speedup = SequentialCost - ParallelCost
        ;
            error("Dependant var not in consumer's arguments")
        ),
        Messages = cord.empty
    ;
        % Post a notice saying that we tried to parallelise this but gave up.
        CallSiteDesc = 
            CallA ^ mccc_call_site ^ ccsr_call_site_summary ^ perf_row_subject,
        PSPtr = CallSiteDesc ^ csdesc_container,
        deep_lookup_proc_statics(Info ^ ipi_deep, PSPtr, ProcStatic),
        ProcLabel = ProcStatic ^ ps_id,
        GoalPath = CallSiteDesc ^ csdesc_goal_path,
        append_message(goal(ProcLabel, GoalPath), 
            notice_callpair_has_more_than_one_dependant_var,
            cord.empty, Messages),
        Speedup = -1.0
    ),
    call_site_conj_to_candidate_par_conjunct(Info, CallA, CPCA),
    call_site_conj_to_candidate_par_conjunct(Info, CallB, CPCB).

:- pred get_var_use_from_args(list(var_mode_and_use)::in, var_rep::in, 
    var_use_info::out) is semidet.

get_var_use_from_args([], _, _) :- false.
get_var_use_from_args([Arg | Args], Var, VarUse) :-
    ( Arg = var_mode_and_use(Var, _, VarUsePrime) ->
        VarUse = VarUsePrime
    ;
        get_var_use_from_args(Args, Var, VarUse)
    ).

:- pred get_var_use_add_to_queue(list(var_mode_and_use)::in, var_rep::in,
    pqueue(float, cost_until_var_use)::in,
    pqueue(float, cost_until_var_use)::out) is det.

get_var_use_add_to_queue(VarsModeAndUse, VarRep, !Queue) :-
    ( get_var_use_from_args(VarsModeAndUse, VarRep, VarUse) ->
        VarUse = var_use_info(CostUntilVarUse, _),
        % Priority queues return the smallest items first,  And we want to find
        % the most pessimistic variable production so use the cost before the
        % procedure's end.
        Key = cost_until_to_cost_before_end(CostUntilVarUse, 0.0),
        pqueue.insert(!.Queue, Key, CostUntilVarUse, !:Queue)
    ;
        true
    ).

:- pred call_site_conj_to_candidate_par_conjunct(
    implicit_parallelism_info::in, maybe_call_conjunct::in(call),
    candidate_par_conjunct::out) is det.

call_site_conj_to_candidate_par_conjunct(Info, Call, CPC) :-
    Call = call(MaybeCallee, _Detism, Args, Perf),
    VarTable = Info ^ ipi_var_table,
    list.map(var_mode_use_to_var_in_par_conj(VarTable), Args, Vars),
    Cost = percall_cost(get_call_site_cost(Info, Perf)),
    CPC = candidate_par_conjunct(MaybeCallee, Vars, Cost).

:- pred var_mode_use_to_var_in_par_conj(var_table::in, var_mode_and_use::in,
    maybe(string)::out) is det.

var_mode_use_to_var_in_par_conj(VarTable, var_mode_and_use(Var, _, _),
        MaybeName) :-
    ( search_var_name(VarTable, Var, Name) ->
        MaybeName = yes(Name)
    ;
        MaybeName = no
    ).

%----------------------------------------------------------------------------%
%
% Jerome's implicit parallelism feedback information.
%

css_list_above_threshold(Options, Deep, !Feedback) :-
    Options = calls_above_threshold_sorted_opts(MeasureType, Threshold),
    compute_css_list_above_threshold(0, Deep, Threshold,
        MeasureType, cord.empty, AboveThresholdCSSCord),
    AboveThresholdCSSs = cord.list(AboveThresholdCSSCord),
    list.map(css_to_call(Deep), AboveThresholdCSSs, Calls),
    FeedbackData = feedback_data_calls_above_threshold_sorted(Threshold,
        MeasureType, Calls),
    put_feedback_data(FeedbackData, !Feedback).

    % Determine those CSSs whose CSDs' average/median call sequence counts
    % exceed the given threshold.
    %
:- pred compute_css_list_above_threshold(int::in, deep::in, int::in,
    stat_measure::in,
    cord(call_site_static)::in, cord(call_site_static)::out) is det.

compute_css_list_above_threshold(Index, Deep, Threshold, Measure, !CSSCord) :-
    array.size(Deep ^ call_site_statics, Size),
    ( Index = Size ->
        true
    ;
        CallSiteCall = array.lookup(Deep ^ call_site_calls, Index),
        CSDListList = map.values(CallSiteCall),
        CSDList = list.condense(CSDListList),
        list.length(CSDList, NumCSD),
        ( NumCSD = 0 ->
            % The CSS doesn't have any CSDs.
            CallSeqs = 0
        ;
            (
                Measure = stat_mean,
                list.foldr(sum_callseqs_csd_ptr(Deep), CSDList,
                    0, SumCallSeqs),
                % NOTE: we have checked that NumCSD is not zero above.
                CallSeqs = SumCallSeqs // NumCSD
            ;
                Measure = stat_median,
                list.sort(compare_csd_ptr(Deep), CSDList, CSDListSorted),
                IndexMedian = NumCSD // 2,
                list.index0_det(CSDListSorted, IndexMedian, MedianPtr),
                sum_callseqs_csd_ptr(Deep, MedianPtr, 0, CallSeqs)
            )
        ),
        ( CallSeqs >= Threshold ->
            CSS = array.lookup(Deep ^ call_site_statics, Index),
            !:CSSCord = snoc(!.CSSCord, CSS),
            compute_css_list_above_threshold(Index + 1, Deep, Threshold,
                Measure, !CSSCord)
        ;
            compute_css_list_above_threshold(Index + 1, Deep, Threshold,
                Measure, !CSSCord)
        )
    ).

    % Add the call sequence counts (own and desc) of CSDPtr to the accumulator.
    %
:- pred sum_callseqs_csd_ptr(deep::in, call_site_dynamic_ptr::in,
    int::in, int::out) is det.

sum_callseqs_csd_ptr(Deep, CSDPtr, !Sum) :-
    lookup_call_site_dynamics(Deep ^ call_site_dynamics, CSDPtr, CSD),
    lookup_csd_desc(Deep ^ csd_desc, CSDPtr, IPO),
    !:Sum = !.Sum + callseqs(CSD ^ csd_own_prof) + inherit_callseqs(IPO).

    % Compare two CSD pointers on the basis of their call sequence counts
    % (own and desc).
    %
:- pred compare_csd_ptr(deep::in, call_site_dynamic_ptr::in,
    call_site_dynamic_ptr::in, comparison_result::out) is det.

compare_csd_ptr(Deep, CSDPtrA, CSDPtrB, Result) :-
    sum_callseqs_csd_ptr(Deep, CSDPtrA, 0, SumA),
    sum_callseqs_csd_ptr(Deep, CSDPtrB, 0, SumB),
    compare(Result, SumA, SumB).

    % Write to the output the list of CSSs.
    %
:- pred css_to_call(deep::in, call_site_static::in, call_site::out) is det.

css_to_call(Deep, CSS, Call) :-
    % Get the caller.
    lookup_proc_statics(Deep ^ proc_statics, CSS ^ css_container, CallerPS),
    Caller = CallerPS ^ ps_id,

    % Get the slot number.
    Slot = CSS ^ css_slot_num,

    % Get the Callee and Call Type.
    (
        CSS ^ css_kind = normal_call_and_callee(PSPtr, _),
        lookup_proc_statics(Deep ^ proc_statics, PSPtr, CalleePS),
        CallTypeAndCallee = plain_call(CalleePS ^ ps_id)
    ;
        CSS ^ css_kind = special_call_and_no_callee,
        CallTypeAndCallee = special_call
    ;
        CSS ^ css_kind = higher_order_call_and_no_callee,
        CallTypeAndCallee = higher_order_call
    ;
        CSS ^ css_kind = method_call_and_no_callee,
        CallTypeAndCallee = method_call
    ;
        CSS ^ css_kind = callback_and_no_callee,
        CallTypeAndCallee = callback_call
    ),

    % Build the call datastructure.
    Call = call_site(Caller, Slot, CallTypeAndCallee).

%----------------------------------------------------------------------------%
%
% Useful utility predicates.
%

:- pred proc_label_from_proc_desc(deep::in, proc_desc::in,
    string_proc_label::out) is det.

proc_label_from_proc_desc(Deep, ProcDesc, ProcLabel) :-
    PSPtr = ProcDesc ^ pdesc_ps_ptr,
    deep_lookup_proc_statics(Deep, PSPtr, ProcStatic),
    ProcLabel = ProcStatic ^ ps_id.

:- pred add_call_site_report_to_map(clique_call_site_report::in, 
    map(goal_path, clique_call_site_report)::in, 
    map(goal_path, clique_call_site_report)::out) is det.

add_call_site_report_to_map(CallSite, !Map) :-
    GoalPath = CallSite ^ ccsr_call_site_summary ^ perf_row_subject 
        ^ csdesc_goal_path,
    svmap.det_insert(GoalPath, CallSite, !Map).

:- func this_file = string.

this_file = "mdprof_fb.automatic_parallelism.m: ".

%-----------------------------------------------------------------------------%
:- end_module mdprof_fb.automatic_parallelism.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2010 The University of Melbourne.
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

:- import_module coverage.
:- import_module create_report.
:- import_module mdbcomp.program_representation.
:- import_module measurement_units.
:- import_module measurements.
:- import_module program_representation_utils.
:- import_module report.
:- import_module var_use_analysis.

:- import_module array.
:- import_module assoc_list.
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
    % runtime since it is modeled here as a call site that in reality does not
    % exist.
    RootCliqueCost = build_cs_cost_csq(1, float(TotalCallseqs) + 1.0),
    RootParallelism = no_parallelism,
    candidate_parallel_conjunctions_clique(Opts, Deep, RootCliqueCost,
        RootParallelism, RootCliquePtr, ConjunctionsMultiMap, Messages),

    multi_map.to_flat_assoc_list(ConjunctionsMultiMap,
        ConjunctionsAssocList0),
    ConjunctionsAssocList =
        map_values_only(internal_cpconjunction_to_cpconjunction,
        ConjunctionsAssocList0),
    CandidateParallelConjunctions =
        feedback_data_candidate_parallel_conjunctions(DesiredParallelism,
        SparkingCost, LockingCost, ConjunctionsAssocList),
    put_feedback_data(CandidateParallelConjunctions, !Feedback).

:- func internal_cpconjunction_to_cpconjunction(
        candidate_par_conjunction_internal)
    = candidate_par_conjunction.

internal_cpconjunction_to_cpconjunction(CPCI) = CPC :-
    CPCI = candidate_par_conjunction_internal(GoalPath, PartNum, Dependance, 
        ConjsI, Speedup),
    map(internal_seq_conj_to_seq_conj, ConjsI, Conjs),
    CPC = candidate_par_conjunction(GoalPath, PartNum, Dependance, Conjs,
        Speedup).

:- pred internal_seq_conj_to_seq_conj(seq_conj_internal::in, seq_conj::out) 
    is det.

internal_seq_conj_to_seq_conj(seq_conj_internal(ConjsI), seq_conj(Conjs)) :-
    map(inner_goal_internal_to_inner_goal, ConjsI, Conjs).

:- pred inner_goal_internal_to_inner_goal(inner_goal_internal::in, 
    inner_goal::out) is det.

inner_goal_internal_to_inner_goal(inner_goal_internal(IGT, _, _, _), IG) :-
    (
        IGT = igt_call(Callee, Vars, CostPercall, _, _),
        IG = ig_call(Callee, Vars, CostPercall)
    ;
        IGT = igt_cheap_call(Callee, Vars, _, _),
        IG = ig_cheap_call(Callee, Vars)
    ;
        IGT = igt_other_atomic_goal,
        IG = ig_other_atomic_goal
    ;
        IGT = igt_non_atomic_goal,
        error(this_file ++ 
            "Unexpected: non atomic goal in inner_goal_internal_to_inner_goal")
    ).

%----------------------------------------------------------------------------%

:- type implicit_parallelism_info
    --->    implicit_parallelism_info(
                ipi_deep            :: deep,
                ipi_progrep         :: prog_rep,
                ipi_opts            :: candidate_parallel_conjunctions_opts,
                ipi_call_sites      :: map(goal_path, clique_call_site_report),
                ipi_rec_call_sites  :: map(goal_path, cs_cost_csq),
                ipi_var_table       :: var_table
            ).

    % These data types reflect those in mdbcomp/feedback.m.  These are
    % internal to this module and may be changed without changing the
    % feedback file format or the compiler.  They often contain extra
    % information that it not needed by the compiler and therefore not
    % present in the feedback file format. 
    %
:- type candidate_par_conjunction_internal
    --->    candidate_par_conjunction_internal(
                cpci_goal_path          :: goal_path_string,
                    % The path within the procedure to this conjunction.

                cpci_partition_number   :: int,
                    % Used to locate the goals to be parallelised within the
                    % conjunction.  Partitions are separated by non atomic
                    % goals, the first partition has the number 1.

                cpci_is_dependant       :: conjuncts_are_dependant,

                cpci_conjs              :: list(seq_conj_internal),
                    % The suggested parallel expression.

                cpci_speedup            :: float
            ).

:- type seq_conj_internal
    --->    seq_conj_internal(
                sci_conjs           :: list(inner_goal_internal)
            ).

    % A representation of a goal within a parallel conjunction.  We don't have
    % to represent many types of goals or details about them, at least for now.
    %
:- type inner_goal_internal
    --->    inner_goal_internal(
                igi_ig_type             :: inner_goal_type,
                    % The type and type-specific values of the inner goal.

                igi_detism              :: detism_rep,
                    % The determinism of the call.

                igi_conj_num            :: int,
                    % The place within the conjunction that this conjunct
                    % lies.

                igi_inst_map_info       :: inst_map_info
                    % The inst map info attached to the original goal.
            ).

:- type inner_goal_type 
    --->    igt_call(
                igtc_callee             :: callee_rep,
                    
                igtc_vars               :: list(maybe(string)),
                    % The names of variables (if user defined) given as
                    % arguments to this call.

                igtc_cost_percall       :: float,
                    % The per-call cost of this call in call sequence counts.
             
                igtc_args               :: list(var_mode_and_use),
                    % The argument modes and use information.

                igtc_call_site          :: clique_call_site_report
                    % The call site report from the deep profiler.
            )
    ;       igt_cheap_call(
                igtcc_callee            :: callee_rep,
                igtcc_vars              :: list(maybe(string)),
                igtcc_args              :: list(var_mode_and_use),
                igtcc_cost              :: cs_cost_csq 
            )
    ;       igt_other_atomic_goal
    ;       igt_non_atomic_goal.

:- inst igt_call 
    --->    igt_call(ground, ground, ground, ground, ground).

:- inst igt_atomic_goal
    --->    igt_call(ground, ground, ground, ground, ground)
    ;       igt_cheap_call(ground, ground, ground, ground)
    ;       igt_other_atomic_goal.
    
    % A variable, it's mode and it's usage in the callee.  The mode information
    % is also summarised within the variable use information.
    %
:- type var_mode_and_use
    --->    var_mode_and_use(
                vmu_var                 :: var_rep,
                vmu_mode                :: var_mode_rep,
                vmu_use                 :: var_use_info
            ).

:- type candidate_par_conjunctions ==
    multi_map(string_proc_label, candidate_par_conjunction_internal).

:- type inner_goals_partition
    --->    inner_goals_partition(
                igp_goals               :: list(inner_goal_internal),
                igp_partition_num       :: int
            ).

%----------------------------------------------------------------------------%
%
% Recurse the call graph searching for parallelisation opportunities.
%

    % candidate_parallel_conjunctions_clique(Opts, Deep, ParentCSCost, 
    %   ParentUsedParallelism, CliquePtr, CandidateParallelConjunctions,
    %   Messages)
    %
    % Find any CandidateParallelConjunctions in this clique and it's children.
    % We stop searching when ParentCSCost is too low that the overheads of
    % parallelism are too great or if ParentUsedParallelism becomes greater
    % than the desired amount of parallelism.
    %
:- pred candidate_parallel_conjunctions_clique(
    candidate_parallel_conjunctions_opts::in, deep::in, cs_cost_csq::in,
    parallelism_amount::in, clique_ptr::in, candidate_par_conjunctions::out,
    cord(message)::out) is det.

candidate_parallel_conjunctions_clique(Opts, Deep, ParentCSCostInfo, ParentParallelism,
        CliquePtr, Candidates, Messages) :-
    create_clique_report(Deep, CliquePtr, MaybeCliqueReport),
    (
        MaybeCliqueReport = ok(CliqueReport),
        CliqueProcs = CliqueReport ^ cr_clique_procs,
        % All cliques must contain at least one procedure.
        ( [ FirstCliqueProcPrime | _ ] = CliqueProcs ->
            FirstCliqueProc = FirstCliqueProcPrime
        ;
            error(this_file ++ "A clique must have et least one procedure in " 
                ++ string(CliquePtr))
        ),    
        CliqueIsRecursive = is_clique_recursive(CliqueReport),
        make_clique_proc_map(CliqueProcs, CliqueProcMap),
        candidate_parallel_conjunctions_clique_proc(Opts, Deep,
            CliqueIsRecursive, ParentCSCostInfo, ParentParallelism, set.init,
            CliqueProcMap, CliquePtr, FirstCliqueProc, Candidates, Messages)
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
    clique_is_recursive::in, cs_cost_csq::in, parallelism_amount::in,
    set(proc_desc)::in, map(proc_desc, clique_proc_report)::in, clique_ptr::in,
    clique_proc_report::in, candidate_par_conjunctions::out,
    cord(message)::out) is det.

candidate_parallel_conjunctions_clique_proc(Opts, Deep, 
        CliqueIsRecursive, ParentCSCostInfo, ParentParallelism, ProcsAnalysed0,
        CliqueProcMap, CliquePtr, CliqueProc, Candidates, Messages) :-
    some [!Messages]
    (
        !:Messages = cord.empty,
        CliqueProcCalls = CliqueProc ^ cpr_proc_summary ^ perf_row_calls,
        cs_cost_to_proc_cost(ParentCSCostInfo, CliqueProcCalls, ParentCostInfo),
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
            RecursiveCallSiteCostMap, ProcCandidates, ProcCandidatesByGoalPath,
            ProcMessages),
        !:Messages = !.Messages ++ ProcMessages,

        ProcDesc = CliqueProc ^ cpr_proc_summary ^ perf_row_subject,

        % Get a list of call sites
        ( CliqueProc ^ cpr_other_proc_dynamics = [_ | _] ->
            proc_label_from_proc_desc(Deep, ProcDesc, ProcLabel),
            append_message(proc(ProcLabel),
                error_extra_proc_dynamics_in_clique_proc, !Messages)
        ;
            true
        ),
        CallSiteReports = CliqueProc ^ cpr_first_proc_dynamic ^ cpdr_call_sites,
        
        % Analyse child cliques of this clique proc for parallelism
        % opportunities.  Recursive calls point to this same clique, in these
        % cases call candidate_parallel_conjunctions_clique_proc on the
        % procedure within this clique that they call.
        set.insert(ProcsAnalysed0, ProcDesc, ProcsAnalysed),
        list.map2(candidate_parallel_conjunctions_call_site(Opts, Deep,
                ProcsAnalysed, CliqueIsRecursive, RecursiveCallSiteCostMap,
                CliqueProcMap, CliquePtr, ParentParallelism,
                ProcCandidatesByGoalPath),
            CallSiteReports, CSCandidatesList, CSMessagesList),
      
        list.foldl(multi_map.merge, CSCandidatesList, multi_map.init,
            CSCandidates),
        Candidates = multi_map.merge(ProcCandidates, CSCandidates),
        !:Messages = !.Messages ++ cord_list_to_cord(CSMessagesList),
        Messages = !.Messages
    ).

:- pred candidate_parallel_conjunctions_call_site(
    candidate_parallel_conjunctions_opts::in, deep::in, set(proc_desc)::in,
    clique_is_recursive::in, map(goal_path, cs_cost_csq)::in,
    map(proc_desc, clique_proc_report)::in, clique_ptr::in,
    parallelism_amount::in,
    multi_map(string, candidate_par_conjunction_internal)::in,
    clique_call_site_report::in,
    candidate_par_conjunctions::out,
    cord(message)::out) is det.

candidate_parallel_conjunctions_call_site(Opts, Deep, ProcsAnalysed,
        CliqueIsRecursive, RecursiveCallSiteCostMap, CliqueProcMap, CliquePtr,
        ParentParallelism, ProcParallelismCandidates,
        CallSiteReport, Candidates, Messages) :-
    % XXX: This does not weight the callees by the probability that they will
    % be called.  This is only a problem for higher order call sites.
    CalleePerfs = CallSiteReport ^ ccsr_callee_perfs,
    CallSiteDesc = CallSiteReport ^ ccsr_call_site_summary ^ perf_row_subject,
    GoalPath = CallSiteDesc ^ csdesc_goal_path,
    parallelism_probability(ProcParallelismCandidates, GoalPath,
        ParallelismProbability, SubParallelism),
    sub_computation_parallelism(ParentParallelism, ParallelismProbability,
        SubParallelism, Parallelism), 
    list.map2(candidate_parallel_conjunctions_callee(Opts, Deep, ProcsAnalysed,
            CliqueIsRecursive, RecursiveCallSiteCostMap, CliqueProcMap,
            CliquePtr, CallSiteDesc, Parallelism),
        CalleePerfs, CandidatesList, MessagesList),
    list.foldl(multi_map.merge, CandidatesList, multi_map.init, Candidates),
    Messages = cord_list_to_cord(MessagesList).

:- pred parallelism_probability(
    multi_map(goal_path_string, candidate_par_conjunction_internal)::in,
    goal_path::in, probability::out, parallelism_amount::out) is det.

parallelism_probability(Candidates, ConjGoalPath, ParallelismProbability,
        ParallelismAmount) :-
    ( goal_path_remove_last(ConjGoalPath, GoalPath, step_conj(ConjNum)) ->
        StringGoalPath = goal_path_to_string(GoalPath),
        ( multi_map.search(Candidates, StringGoalPath, CandidateList) ->
            (
                % We assume that we can only ever be a member of one
                % parallel conjunction.  That is that overlapping
                % conjunctions are never recommended/seen.
                promise_equivalent_solutions [Amount] some [Candidate, Conj] (
                    member(Candidate, CandidateList),
                    parallel_amount(Candidate ^ cpci_conjs, Conj, Amount),
                    ConjNum = Conj ^ igi_conj_num
                )
            ->
                % XXX: Wait until we have the new calculations for how
                % dependant parallel conjuncts overlap before we bother to
                % calculate the probability of parallelisation.  For now assume
                % a pesimistic default.
                ParallelismProbability = certain,
                ParallelismAmount = Amount
            ;
                ParallelismProbability = impossible,
                ParallelismAmount = no_parallelism
            )
        ;
            % This callsite is not within a parallel conjunction.
            ParallelismProbability = impossible,
            ParallelismAmount = no_parallelism
        )
    ;
        % We may not be able to remove this goal if it is not directly in a
        % conjunction.  Perhaps it's directly within some branch goal or at the
        % root of a goal path.  In these cases it is not in a parallel
        % conjunction so the parallelism probability is 0.0. 
        ParallelismProbability = impossible,
        ParallelismAmount = no_parallelism 
    ).

:- pred parallel_amount(list(seq_conj_internal)::in, 
    inner_goal_internal::out, parallelism_amount::out) is nondet.

parallel_amount(SeqConjs, Conj, Parallelism) :-
    member(SeqConj, SeqConjs),
    SeqConj = seq_conj_internal(Conjs),
    Parallelism0 = some_parallelism(float(length(Conjs))),
    member(Conj, Conjs),
    sub_computation_parallelism(Parallelism0, certain, Parallelism).

:- pred candidate_parallel_conjunctions_callee(
    candidate_parallel_conjunctions_opts::in, deep::in, set(proc_desc)::in,
    clique_is_recursive::in, map(goal_path, cs_cost_csq)::in,
    map(proc_desc, clique_proc_report)::in, clique_ptr::in, call_site_desc::in,
    parallelism_amount::in, perf_row_data(clique_desc)::in,
    candidate_par_conjunctions::out, cord(message)::out) is det.

candidate_parallel_conjunctions_callee(Opts, Deep, ProcsAnalysed0,
        ParentCliqueIsRecursive, RecursiveCallSiteCostMap, CliqueProcReportMap,
        ParentCliquePtr, CallSiteDesc, Parallelism, CliquePerf, Candidates,
        Messages) :-
    CliqueDesc = CliquePerf ^ perf_row_subject,
    CliquePtr = CliqueDesc ^ cdesc_clique_ptr,
    CliqueEntryProc = CliqueDesc ^ cdesc_entry_member,
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
                ParentCliqueIsRecursive, CallCostInfo, Parallelism, 
                ProcsAnalysed, CliqueProcReportMap, ParentCliquePtr,
                CliqueProcReport, Candidates, Messages)
        )
    ;
        CSCost = build_cs_cost_from_perf(CliquePerf),
        ( 
            % Use the total cost the call to this procedure to decide if we
            % should stop recursing the call graph at this point.  If the
            % procedure does not contribute to the runtime of the program in an
            % absolute way then do not recurse further.  This test is performed
            % here rather than in the callees of this predicate to avoid
            % duplication of code.
            %
            % Also check check if the desired amount of parallelism has been
            % reached, if so do not recurse further to prevent creating too
            % much extra parallelism.
            cs_cost_get_total(CSCost) > float(Opts ^ cpc_clique_threshold), 
            not exceeded_desired_parallelism(Opts ^ cpc_desired_parallelism,
                Parallelism)
        ->
            candidate_parallel_conjunctions_clique(Opts, Deep, CSCost,
                Parallelism, CliquePtr, Candidates, Messages)
        ;
            Candidates = multi_map.init, 
            Messages = cord.empty,
            trace [compile_time(flag("debug_cpc_search")), io(!IO)]
                io.format(
                    "D: Not entering clique: %s with cost %s.  " ++
                    "There are %s parallel resources used\n",
                    [s(string(CliquePtr)), s(string(CSCost)),
                     s(string(Parallelism))],
                    !IO)
        )
    ).

%----------------------------------------------------------------------------%
%
% Estimate the cost of the recursive calls under the assumption that current
% call to this procedure had a particular cost.
%

:- pred build_recursive_call_site_cost_map(deep::in, clique_proc_report::in,
    clique_ptr::in, proc_cost_csq::in, map(goal_path, cs_cost_csq)::out,
    cord(message)::out) is det.

build_recursive_call_site_cost_map(Deep, CliqueProc, CliquePtr,
        ParentCost, RecursiveCallSiteCostMap, Messages) :-
    some [!Messages] (
        !:Messages = cord.empty,

        % Lookup the proc static to find the ProcLabel.
        PerfRowData = CliqueProc ^ cpr_proc_summary,
        ProcDesc = PerfRowData ^ perf_row_subject,
        proc_label_from_proc_desc(Deep, ProcDesc, ProcLabel),

        ( CliqueProc ^ cpr_other_proc_dynamics = [_ | _] ->
            append_message(proc(ProcLabel),
                error_extra_proc_dynamics_in_clique_proc, !Messages)
        ;
            true
        ),
        CallSites = CliqueProc ^ cpr_first_proc_dynamic ^ cpdr_call_sites,
       
        PSPtr = ProcDesc ^ pdesc_ps_ptr,
        create_procrep_coverage_report(Deep, PSPtr, MaybeCoverageReport),
        (
            MaybeCoverageReport = ok(procrep_coverage_info(_, ProcRep)),
            Goal = ProcRep ^ pr_defn ^ pdr_goal,
            foldl(add_call_site_to_map, CallSites, map.init, CallSiteMap),
            goal_get_callsite_cost_info(CliquePtr, CallSiteMap, Goal,
                empty_goal_path, Info),
            Info = callsite_cost_info(NonRecCSCost, RecursiveCSCalls,
                RecursiveCS, CallSiteProbabilityMap)
        ;
            MaybeCoverageReport = error(Error),
            % If we couldn't find the proc rep then use the old method, this
            % will not give accuruate call site probabilities.
            foldl4(get_callsite_cost_info(CliquePtr, ParentCost), 
                CallSites, 0.0, NonRecCSCost, 0.0, RecursiveCSCalls,
                set.init, RecursiveCS, map.init, CallSiteProbabilityMap),
            append_message(proc(ProcLabel),
                warning_cannot_compute_procrep_coverage_fallback(Error), 
                !Messages)
        ),

        % The cost of the recursive calls is the total cost minus the cost of
        % the non recursive calls.
        TotalRecursiveCost = 
            proc_cost_get_total(ParentCost) - NonRecCSCost,

        % This should never divide by zero since it is only called on code that
        % is recursive at runtime, that is a recusrive call is executed.
        RecursiveCostPerCall = TotalRecursiveCost / RecursiveCSCalls,

        % Assign costs to call sites.
        set.fold(
            build_recursive_call_site_cost_map_call_site(RecursiveCostPerCall, 
                CallSiteProbabilityMap),
            RecursiveCS, map.init, RecursiveCallSiteCostMap),
        Messages = !.Messages
    ).

:- pred add_call_site_to_map(clique_call_site_report::in, 
    map(goal_path, clique_call_site_report)::in,
    map(goal_path, clique_call_site_report)::out) is det.

add_call_site_to_map(CallSite, !Map) :-
    GoalPath = CallSite ^ ccsr_call_site_summary ^ perf_row_subject 
        ^ csdesc_goal_path,
    svmap.det_insert(GoalPath, CallSite, !Map).

    % The stateful data of the goal_get_callsite_cost_info code below.
    %
:- type callsite_cost_info
    --->    callsite_cost_info(
                csci_non_rec_cs_cost    :: float,
                csci_rec_calls          :: float,
                csci_rec_cs             :: set(clique_call_site_report),
                csci_cs_prob_map        :: map(goal_path, float)
            ).

:- func empty_callsite_cost_info = callsite_cost_info.

empty_callsite_cost_info = callsite_cost_info(0.0, 0.0, set.init, map.init).

:- pred goal_get_callsite_cost_info(clique_ptr::in,
    map(goal_path, clique_call_site_report)::in, goal_rep(coverage_info)::in,
    goal_path::in, callsite_cost_info::out) is det.

goal_get_callsite_cost_info(CliquePtr, CallSites, Goal, GoalPath, Info) :-
    Goal = goal_rep(GoalExpr, Detism, Coverage),
    (
        GoalExpr = conj_rep(Conjs),
        conj_get_callsite_cost_info(CliquePtr, CallSites, Conjs, 1, GoalPath,
            Info)
    ;
        GoalExpr = disj_rep(Disjs),
        disj_get_callsite_cost_info(CliquePtr, CallSites, Detism, Coverage,
            Disjs, GoalPath, Info)
    ;
        GoalExpr = switch_rep(_Var, _CanFail, Cases),
        switch_get_callsite_cost_info(CliquePtr, CallSites, Coverage, Cases,
            GoalPath, Info)
    ;
        GoalExpr = ite_rep(Cond, Then, Else),
        ite_get_callsite_cost_info(CliquePtr, CallSites, Cond, Then, Else,
            GoalPath, Info)
    ;
        (
            GoalExpr = negation_rep(SubGoal),
            SubGoalPath = goal_path_add_at_end(GoalPath, step_neg)
        ;
            GoalExpr = scope_rep(SubGoal, MaybeCut),
            SubGoalPath = goal_path_add_at_end(GoalPath, step_scope(MaybeCut))
        ),
        goal_get_callsite_cost_info(CliquePtr, CallSites, SubGoal,
            SubGoalPath, Info)
    ;
        GoalExpr = atomic_goal_rep(_, _, _, _AtomicGoal),
        atomic_goal_get_callsite_cost_info(CliquePtr, CallSites, GoalPath,
            Info)
    ).

:- pred atomic_goal_get_callsite_cost_info(clique_ptr::in, 
    map(goal_path, clique_call_site_report)::in, goal_path::in,
    callsite_cost_info::out) is det.

atomic_goal_get_callsite_cost_info(CliquePtr, CallSites, GoalPath, Info) :-
    ( map.search(CallSites, GoalPath, CallSite) ->
        svmap.det_insert(GoalPath, 1.0, map.init, CSProbMap),
        ( call_site_is_recursive(CallSite, CliquePtr) ->
            RecursiveCalls = 1.0,
            NonRecursiveCost = 0.0,
            RecursiveCallSites = set.from_list([CallSite])
        ;
            CSSummary = CallSite ^ ccsr_call_site_summary,
            MaybeTotal = CSSummary ^ perf_row_maybe_total,
            (
                MaybeTotal = yes(Total),
                PercallCost = Total ^ perf_row_callseqs_percall
            ;
                MaybeTotal = no,
                error("clique_call_site has 'no' for perf_row_maybe_total")
            ),
            RecursiveCalls = 0.0,
            NonRecursiveCost = PercallCost,
            RecursiveCallSites = set.init
        ),
        Info = callsite_cost_info(NonRecursiveCost, RecursiveCalls, 
            RecursiveCallSites, CSProbMap)
    ;
        % Not a callsite, there is no information to update since atmoic
        % non-call goals have a trivial cost. 
        Info = empty_callsite_cost_info
    ).

:- pred conj_get_callsite_cost_info(clique_ptr::in, 
    map(goal_path, clique_call_site_report)::in,
    list(goal_rep(coverage_info))::in, int::in,
    goal_path::in, callsite_cost_info::out) is det.

conj_get_callsite_cost_info(_, _, [], _, _, empty_callsite_cost_info).
conj_get_callsite_cost_info(CliquePtr, CallSites, [Conj | Conjs], ConjNum,
        GoalPath, Info) :-
    ConjGoalPath = goal_path_add_at_end(GoalPath, step_conj(ConjNum)),
    goal_get_callsite_cost_info(CliquePtr, CallSites, Conj, ConjGoalPath, 
        ConjInfo),
    
    Coverage = Conj ^ goal_annotation,
    ( get_coverage_before_and_after(Coverage, Calls, Exits) ->
        % ContProb is the probability that this conjunction will continue with
        % the execution of the next goal.
        ( Calls = 0 ->
            % If this was never called, then we will have a probability of 0 of
            % executing the next conjunct.
            ContProb = 0.0
        ;
            ContProb = float(Exits) / float(Calls)
        )
    ;
        error(this_file ++ "Expected complete coverage information")
    ),
    conj_get_callsite_cost_info(CliquePtr, CallSites, Conjs, ConjNum + 1, 
        GoalPath, ConjsInfo),
    Info = multiply_and_add(ConjsInfo, ContProb, ConjInfo).

:- pred disj_get_callsite_cost_info(clique_ptr::in, 
    map(goal_path, clique_call_site_report)::in, detism_rep::in,
    coverage_info::in, list(goal_rep(coverage_info))::in,
    goal_path::in, callsite_cost_info::out) is det.

disj_get_callsite_cost_info(CliquePtr, CallSites, _Detism, Coverage,
        Disjs, GoalPath, Info) :-
    % Some disjunctions may have redos, however these are rare and are not
    % modeled by this code.
    list.map_foldl(
        disj_get_callsite_cost_info2(CliquePtr, CallSites, GoalPath),
        Disjs, DisjInfos, 1, _),
    
    map_corresponding_foldl2(
        callsite_collect_branch_probabilities(Coverage),
        Disjs, DisjInfos, Probs, 0.0, _NonRecProb, 0.0, _RecProb),
    foldl_corresponding(
        (pred(NewInfo::in, BranchProb::in, Info0I::in, InfoI::out) is det :-
            % This is a special case of callsite_cost_merge_branches that is
            % used for disjunctions.  Since disjunctions don't behave like
            % switches or ITEs if a disjunct does not call recursive code it is
            % still included as part of the code that would execute during a
            % call that will recurse provided that some other disjuct recurses.
            InfoI = multiply_and_add(NewInfo, BranchProb, Info0I)
        ), DisjInfos, Probs, empty_callsite_cost_info, Info).

:- pred disj_get_callsite_cost_info2(clique_ptr::in, 
    map(goal_path, clique_call_site_report)::in, goal_path::in,
    goal_rep(coverage_info)::in, callsite_cost_info::out, int::in, int::out) 
    is det.

disj_get_callsite_cost_info2(CliquePtr, CallSites, GoalPath, Goal, Info,
        DisjNum, DisjNum+1) :-
    DisjGoalPath = goal_path_add_at_end(GoalPath, step_disj(DisjNum)),
    goal_get_callsite_cost_info(CliquePtr, CallSites, Goal, DisjGoalPath,
        Info).

:- pred switch_get_callsite_cost_info(clique_ptr::in, 
    map(goal_path, clique_call_site_report)::in, coverage_info::in,
    list(case_rep(coverage_info))::in, goal_path::in, callsite_cost_info::out)
    is det.

switch_get_callsite_cost_info(CliquePtr, CallSites, Coverage, Cases, GoalPath,
        Info) :-
    % Since switches are similar to disjunctions so some of this code is
    % similar or shared.
    list.map(case_get_goal, Cases, Goals),
    list.map_foldl(
        case_get_callsite_cost_info(CliquePtr, CallSites, GoalPath),
        Goals, GoalInfos, 1, _),
    map_corresponding_foldl2(
        callsite_collect_branch_probabilities(Coverage),
        Goals, GoalInfos, Probs, 0.0, NonRecProb, 0.0, RecProb),
    foldl_corresponding(callsite_cost_merge_branches(NonRecProb, RecProb),
        GoalInfos, Probs, empty_callsite_cost_info, Info).

:- pred case_get_callsite_cost_info(clique_ptr::in, 
    map(goal_path, clique_call_site_report)::in, goal_path::in,
    goal_rep(coverage_info)::in, callsite_cost_info::out, int::in, int::out) 
    is det.

case_get_callsite_cost_info(CliquePtr, CallSites, GoalPath, Goal, Info,
        CaseNum, CaseNum+1) :-
    CaseGoalPath = goal_path_add_at_end(GoalPath, step_switch(CaseNum, no)),
    goal_get_callsite_cost_info(CliquePtr, CallSites, Goal, CaseGoalPath,
        Info).

:- pred ite_get_callsite_cost_info(clique_ptr::in, 
    map(goal_path, clique_call_site_report)::in, 
    goal_rep(coverage_info)::in, goal_rep(coverage_info)::in,
    goal_rep(coverage_info)::in, goal_path::in, callsite_cost_info::out) is det.

ite_get_callsite_cost_info(CliquePtr, CallSites, Cond, Then, Else,
        GoalPath, Info) :-
    CondGoalPath = goal_path_add_at_end(GoalPath, step_ite_cond),
    ThenGoalPath = goal_path_add_at_end(GoalPath, step_ite_then),
    ElseGoalPath = goal_path_add_at_end(GoalPath, step_ite_else),
    goal_get_callsite_cost_info(CliquePtr, CallSites, Cond, CondGoalPath,
        CondInfo),
    goal_get_callsite_cost_info(CliquePtr, CallSites, Then, ThenGoalPath,
        ThenInfo),
    goal_get_callsite_cost_info(CliquePtr, CallSites, Else, ElseGoalPath,
        ElseInfo),
    
    % Find the probability of entering either branch and merge the
    % callsite_cost_info structures.
    CondCoverage = Cond ^ goal_annotation,
    ( get_coverage_before_and_after(CondCoverage, CondCalls, CondExits) ->
        ( CondCalls = 0 ->
            % XXX: I don't like these either since their sum is 0.0
            ThenProb = 0.0,
            ElseProb = 0.0
        ;
            ThenProb = float(CondExits) / float(CondCalls),
            ElseProb = 1.0 - ThenProb
        )
    ;
        error(this_file ++ "couldn't retrieve coverage information")
    ),
    add_probability_to_rec_non_rec_totals(ThenInfo, ThenProb, 
        0.0, NonRecProb0, 0.0, RecProb0),
    add_probability_to_rec_non_rec_totals(ElseInfo, ElseProb, 
        NonRecProb0, NonRecProb, RecProb0, RecProb),
    callsite_cost_merge_branches(NonRecProb, RecProb, ThenInfo, ThenProb, 
        empty_callsite_cost_info, BranchInfo0),
    callsite_cost_merge_branches(NonRecProb, RecProb, ElseInfo, ElseProb, 
        BranchInfo0, BranchInfo),
   
    % Add the info from the condition goal.
    Info = add_callsite_cost_info(CondInfo, BranchInfo).

:- pred callsite_collect_branch_probabilities(coverage_info::in, 
    goal_rep(coverage_info)::in, callsite_cost_info::in, float::out,
    float::in, float::out, float::in, float::out) is det.

callsite_collect_branch_probabilities(TotalCoverage, Goal, Info, Prob, 
        !NonRecProb, !RecProb) :-
    (
        get_coverage_before(TotalCoverage, TotalCalls),
        get_coverage_before(Goal ^ goal_annotation, Calls)
    ->
        % The probability of entering this branch given that the parent goal
        % was called.
        ( TotalCalls = 0 ->
            % I'm not sure I'm comfortable with this, since in this case the
            % probability of entering each branch will be 0.0, and the sum of
            % this will be 0.0 which is not correct.
            Prob = 0.0
        ;
            Prob = float(Calls) / float(TotalCalls)
        )
    ;
        error(this_file ++ "could not retrieve coverage information")
    ),
    add_probability_to_rec_non_rec_totals(Info, Prob, !NonRecProb, !RecProb).

:- pred add_probability_to_rec_non_rec_totals(callsite_cost_info::in, float::in,
    float::in, float::out, float::in, float::out) is det.

add_probability_to_rec_non_rec_totals(Info, Prob, !NonRecProb, !RecProb) :-
    ( empty(Info ^ csci_rec_cs) ->
        % This branch has no recursive calls in it (ie it forms a base-case),
        % therefore it doesn't contribute to probability that we enter this
        % branch since we're trying to calculate the probability of a goal
        % being executed given that we're executing a procedure that will
        % eventually recurse.  We track the probability of entering a
        % non-recursive branch so that this probability can be added to the
        % recursive cases below.
        !:NonRecProb = !.NonRecProb + Prob
    ;   
        !:RecProb = !.RecProb + Prob
    ).

:- pred callsite_cost_merge_branches(float::in, float::in,
    callsite_cost_info::in, float::in,
    callsite_cost_info::in, callsite_cost_info::out) is det.

callsite_cost_merge_branches(NonRecProb, RecProb, NewInfo, BranchProb, 
        !Info) :-
    ( empty(NewInfo ^ csci_rec_cs) ->
        % This branch is non-recursive, therefore we don't count it's
        % contribution to the execution time because we're calculating the
        % execution time for a non-recursive invocation of this procedure.
        true
    ;
        % Add the probability of a non-recursive branch to this branch weighted
        % by the probability that we execute this branch given that we execute a
        % recursive branch.
        !:Info = multiply_and_add(NewInfo, 
            BranchProb + (BranchProb / RecProb * NonRecProb), !.Info)
    ).


:- func multiply_and_add(callsite_cost_info, float, callsite_cost_info) =
    callsite_cost_info.

multiply_and_add(Cost, Scalar, CostAddend) = Result :-
    Cost = callsite_cost_info(NonRecCSCost0, RecCalls0, RecCSA, CSProbMap0), 
    CostAddend = callsite_cost_info(NonRecCSCostAddend, RecCallsAddend, RecCSB,
        CSProbMapAddend), 
    NonRecCSCost = (NonRecCSCost0 * Scalar) + NonRecCSCostAddend,
    RecCalls = (RecCalls0 * Scalar) + RecCallsAddend,
    % This set is simply 'added' multiplication doesn't make sense and merge is
    % exactly what we want here.  Sets are given in this order for efficiency,
    % see set.union/2
    RecCS = set.union(RecCSB, RecCSA),
    map.foldl(multiply_probability_merge(Scalar),
        CSProbMap0, CSProbMapAddend, CSProbMap),
    Result = callsite_cost_info(NonRecCSCost, RecCalls, RecCS, CSProbMap).

:- pred multiply_probability_merge(float::in, goal_path::in, float::in,
    map(goal_path, float)::in, map(goal_path, float)::out) is det.

multiply_probability_merge(M, Key, Value0, !Map) :-
    Value = Value0 * M,
    svmap.det_insert(Key, Value, !Map).

:- func add_callsite_cost_info(callsite_cost_info, callsite_cost_info) =
    callsite_cost_info.

add_callsite_cost_info(CSCIA, CSCIB) = 
    multiply_and_add(CSCIA, 1.0, CSCIB).

:- pred get_callsite_cost_info(clique_ptr::in, proc_cost_csq::in, 
    clique_call_site_report::in, float::in, float::out, float::in, float::out, 
    set(clique_call_site_report)::in, set(clique_call_site_report)::out,
    map(goal_path, float)::in, map(goal_path, float)::out) is det.

get_callsite_cost_info(ThisClique, ParentCost, CallSite, 
        !NonRecCSCost, !RecursiveCalls, !RecursiveCallSites, !CallSiteProbMap)
    :-
    CSSummary = CallSite ^ ccsr_call_site_summary,
    GoalPath = CSSummary ^ perf_row_subject ^ csdesc_goal_path,
    CSCalls = float(CSSummary ^ perf_row_calls),
    % Note that Prob represents the probability of this call occurring on any
    % invocation of the clique proc, not on the top-level invocation of the
    % clique proc which is what we take it to mean here.  This is why this
    % method is not used with the procedure representation is available.
    Prob = CSCalls / float(proc_cost_get_calls_total(ParentCost)),
    svmap.det_insert(GoalPath, Prob, !CallSiteProbMap),
    ( call_site_is_recursive(CallSite, ThisClique) ->
        !:RecursiveCalls = !.RecursiveCalls + Prob, 
        svset.insert(CallSite, !RecursiveCallSites)
    ;
        MaybeTotal = CSSummary ^ perf_row_maybe_total,
        (
            MaybeTotal = yes(Total),
            PercallCost = Total ^ perf_row_callseqs_percall
        ;
            MaybeTotal = no,
            error("clique_call_site has 'no' for perf_row_maybe_total")
        ),
        !:NonRecCSCost = !.NonRecCSCost + PercallCost
    ).

:- pred call_site_is_recursive(clique_call_site_report::in, clique_ptr::in) 
    is semidet.

call_site_is_recursive(CallSite, ThisClique) :-
    % Note that according to this any higher order call site that
    % is recursive in some cases and non-recursive in others is
    % considered to be recursive.  The cost of it's non-recursive
    % calls is not factored into the calculation of the cost of
    % recursive call sites.
    member(CalleePerf, CallSite ^ ccsr_callee_perfs),
    CalleePerf ^ perf_row_subject ^ cdesc_clique_ptr = ThisClique.

:- pred build_recursive_call_site_cost_map_call_site(float::in,
    map(goal_path, float)::in, clique_call_site_report::in, 
    map(goal_path, cs_cost_csq)::in, map(goal_path, cs_cost_csq)::out) is det.
    
build_recursive_call_site_cost_map_call_site(RecursiveCostPerCall,
        CallSiteProbabilityMap, CallSite, !Map) :-
    CSSummary = CallSite ^ ccsr_call_site_summary,
    GoalPath = CSSummary ^ perf_row_subject ^ csdesc_goal_path,
    
    map.lookup(CallSiteProbabilityMap, GoalPath, Calls),
    Cost = build_cs_cost_csq_percall(Calls, RecursiveCostPerCall),
    svmap.det_insert(GoalPath, Cost, !Map).

%----------------------------------------------------------------------------%
%
% Search for paralleliation opportunities within a procedure.
%

    % Find candidate parallel conjunctions within the given procedure.
    %
:- pred candidate_parallel_conjunctions_proc(
    candidate_parallel_conjunctions_opts::in, deep::in,
    clique_proc_report::in, map(goal_path, cs_cost_csq)::in,
    candidate_par_conjunctions::out,
    multi_map(goal_path_string, candidate_par_conjunction_internal)::out,
    cord(message)::out) is det.

candidate_parallel_conjunctions_proc(Opts, Deep, CliqueProc,
        RecursiveCallSiteCostMap, Candidates, CandidatesByGoalPath,
        Messages) :-
    some [!Messages] (
        !:Messages = cord.empty,

        % Lookup the proc static to find the ProcLabel.
        PerfRowData = CliqueProc ^ cpr_proc_summary,
        ProcDesc = PerfRowData ^ perf_row_subject,
        proc_label_from_proc_desc(Deep, ProcDesc, ProcLabel),
        
        CallSites = CliqueProc ^ cpr_first_proc_dynamic ^ cpdr_call_sites,
        ( CliqueProc ^ cpr_other_proc_dynamics = [_ | _] ->
            append_message(proc(ProcLabel), 
                error_extra_proc_dynamics_in_clique_proc,
                !Messages)
        ;
            true
        ),
        list.foldl(add_call_site_report_to_map,
            CallSites, map.init, CallSitesMap),
        deep_get_progrep_det(Deep, ProgRep),
        ( progrep_search_proc(ProgRep, ProcLabel, ProcRep) ->
            ProcRep ^ pr_defn = ProcDefnRep,
            ProcDefnRep ^ pdr_goal = Goal0,
            ProcDefnRep ^ pdr_var_table = VarTable,
            Info = implicit_parallelism_info(Deep, ProgRep, Opts,
                CallSitesMap, RecursiveCallSiteCostMap, VarTable),
            goal_annotate_with_instmap(Goal0, Goal,
                initial_inst_map(ProcDefnRep), _FinalInstMap,
                SeenDuplicateInstantiation, _AllVars),
            goal_get_conjunctions_worth_parallelising(Goal, empty_goal_path,
                Info, ProcLabel, Candidates0, MessagesA),
            !:Messages = !.Messages ++ MessagesA,
            (
                SeenDuplicateInstantiation =
                    have_not_seen_duplicate_instantiation,
                list.foldl2(build_candidate_par_conjunction_maps(ProcLabel),
                    Candidates0, multi_map.init, Candidates, 
                    map.init, CandidatesByGoalPath)
            ;
                SeenDuplicateInstantiation = seen_duplicate_instantiation,
                Candidates = multi_map.init,
                CandidatesByGoalPath = map.init,
                append_message(proc(ProcLabel), 
                    notice_duplicate_instantiation(length(Candidates0)),
                    !Messages)
            )
        ;
            % Builtin procedures cannot be found in the program representation,
            % and cannot be parallelised either.
            Candidates = multi_map.init,
            CandidatesByGoalPath = map.init,
            append_message(proc(ProcLabel), warning_cannot_lookup_proc_defn,
                !Messages)
        ),
        Messages = !.Messages
    ).

:- pred build_candidate_par_conjunction_maps(string_proc_label::in,
    candidate_par_conjunction_internal::in, 
    candidate_par_conjunctions::in, candidate_par_conjunctions::out,
    multi_map(goal_path_string, candidate_par_conjunction_internal)::in,
    multi_map(goal_path_string, candidate_par_conjunction_internal)::out)
    is det.

build_candidate_par_conjunction_maps(ProcLabel, Candidate, !Map, !GPMap) :- 
    multi_map.set(!.Map, ProcLabel, Candidate, !:Map),
    GoalPath = Candidate ^ cpci_goal_path,
    multi_map.set(!.GPMap, GoalPath, Candidate, !:GPMap).
    
:- pred goal_get_conjunctions_worth_parallelising(goal_rep(inst_map_info)::in, 
    goal_path::in, implicit_parallelism_info::in, string_proc_label::in,
    list(candidate_par_conjunction_internal)::out,
    cord(message)::out) is det.

goal_get_conjunctions_worth_parallelising(Goal, GoalPath, Info, ProcLabel, 
        Candidates, Messages) :-
    Goal = goal_rep(GoalExpr, _, _),
    (
        (
            GoalExpr = conj_rep(Conjuncts),
            conj_get_conjunctions_worth_parallelising(Conjuncts, GoalPath, 1,
                Info, ProcLabel, CandidatesA, MessagesA),
            conj_build_candidate_conjunctions(Conjuncts, GoalPath, 
                Info, ProcLabel, MessagesB, CandidatesB),
            Messages = MessagesA ++ MessagesB,
            Candidates = CandidatesA ++ CandidatesB
        ;
            GoalExpr = disj_rep(Disjuncts),
            disj_get_conjunctions_worth_parallelising(Disjuncts, GoalPath, 1,
                Info, ProcLabel, Candidates, Messages)
        ;
            GoalExpr = switch_rep(_, _, Cases),
            switch_case_get_conjunctions_worth_parallelising(Cases, GoalPath, 1,
                Info, ProcLabel, Candidates, Messages)
        ;
            GoalExpr = ite_rep(Cond, Then, Else),
            ite_get_conjunctions_worth_parallelising(Cond, Then, Else,
                GoalPath, Info, ProcLabel, Candidates, Messages)
        ;
            GoalExpr = scope_rep(SubGoal, MaybeCut),
            ScopeGoalPath = 
                goal_path_add_at_end(GoalPath, step_scope(MaybeCut)),
            goal_get_conjunctions_worth_parallelising(SubGoal, ScopeGoalPath,
                Info, ProcLabel, Candidates, Messages) 
        ;
            GoalExpr = negation_rep(SubGoal),
            NegGoalPath = goal_path_add_at_end(GoalPath, step_neg),
            goal_get_conjunctions_worth_parallelising(SubGoal, NegGoalPath,
                Info, ProcLabel, Candidates, Messages) 
        )
    ;
        GoalExpr = atomic_goal_rep(_, _, _, _),
        Messages = cord.empty,
        Candidates = []
    ).

:- pred conj_get_conjunctions_worth_parallelising(
    list(goal_rep(inst_map_info))::in, goal_path::in, int::in,
    implicit_parallelism_info::in, string_proc_label::in,
    list(candidate_par_conjunction_internal)::out,
    cord(message)::out) is det.

conj_get_conjunctions_worth_parallelising([], _, _, _, _, [], cord.empty).
conj_get_conjunctions_worth_parallelising([Conj | Conjs], GoalPath,
        ConjunctNum, Info, ProcLabel, Candidates, Messages) :-
    ConjGoalPath = goal_path_add_at_end(GoalPath, step_conj(ConjunctNum)),
    goal_get_conjunctions_worth_parallelising(Conj, ConjGoalPath, Info,
        ProcLabel, CandidatesHead, MessagesHead), 
    
    conj_get_conjunctions_worth_parallelising(Conjs, GoalPath, ConjunctNum+1,
        Info, ProcLabel, CandidatesTail, MessagesTail),

    Candidates = CandidatesHead ++ CandidatesTail,
    Messages = MessagesHead ++ MessagesTail.

:- pred disj_get_conjunctions_worth_parallelising(
    list(goal_rep(inst_map_info))::in, goal_path::in, int::in,
    implicit_parallelism_info::in, string_proc_label::in,
    list(candidate_par_conjunction_internal)::out, cord(message)::out) is det.

disj_get_conjunctions_worth_parallelising([], _, _, _, _, [], cord.empty).
disj_get_conjunctions_worth_parallelising([Disj | Disjs], GoalPath, DisjNum,
        Info, ProcLabel, Candidates, Messages) :-
    DisjGoalPath = goal_path_add_at_end(GoalPath, step_disj(DisjNum)),
    goal_get_conjunctions_worth_parallelising(Disj, DisjGoalPath, Info,
        ProcLabel, HeadCandidates, HeadMessages),
    disj_get_conjunctions_worth_parallelising(Disjs, GoalPath, DisjNum + 1,
        Info, ProcLabel, TailCandidates, TailMessages),
    Candidates = HeadCandidates ++ TailCandidates,
    Messages = HeadMessages ++ TailMessages.

:- pred switch_case_get_conjunctions_worth_parallelising(
    list(case_rep(inst_map_info))::in, goal_path::in, int::in,
    implicit_parallelism_info::in, string_proc_label::in,
    list(candidate_par_conjunction_internal)::out, cord(message)::out) is det.

switch_case_get_conjunctions_worth_parallelising([], _, _, _, _, [],
        cord.empty).
switch_case_get_conjunctions_worth_parallelising([Case | Cases], GoalPath,
        CaseNum, Info, ProcLabel, Candidates, Messages) :-
    Case = case_rep(_, _, Goal),
    CaseGoalPath = goal_path_add_at_end(GoalPath, step_switch(CaseNum, no)),
    goal_get_conjunctions_worth_parallelising(Goal, CaseGoalPath, Info,
        ProcLabel, HeadCandidates, HeadMessages),
    switch_case_get_conjunctions_worth_parallelising(Cases, GoalPath, 
        CaseNum + 1, Info, ProcLabel, TailCandidates,
        TailMessages),
    Candidates = HeadCandidates ++ TailCandidates,
    Messages = HeadMessages ++ TailMessages.

:- pred ite_get_conjunctions_worth_parallelising(goal_rep(inst_map_info)::in,
    goal_rep(inst_map_info)::in, goal_rep(inst_map_info)::in, goal_path::in,
    implicit_parallelism_info::in, string_proc_label::in,
    list(candidate_par_conjunction_internal)::out, cord(message)::out) is det.

ite_get_conjunctions_worth_parallelising(Cond, Then, Else, GoalPath, Info,
        ProcLabel, Candidates, Messages) :-
    CondGoalPath = goal_path_add_at_end(GoalPath, step_ite_cond),
    ThenGoalPath = goal_path_add_at_end(GoalPath, step_ite_then),
    ElseGoalPath = goal_path_add_at_end(GoalPath, step_ite_else),
    goal_get_conjunctions_worth_parallelising(Cond, CondGoalPath, Info,
        ProcLabel, CondCandidates, CondMessages),
    goal_get_conjunctions_worth_parallelising(Then, ThenGoalPath, Info, 
        ProcLabel, ThenCandidates, ThenMessages),
    goal_get_conjunctions_worth_parallelising(Else, ElseGoalPath, Info, 
        ProcLabel, ElseCandidates, ElseMessages),
    Candidates = CondCandidates ++ ThenCandidates ++ ElseCandidates,
    Messages = CondMessages ++ ThenMessages ++ ElseMessages.

    % At the end of every conjunction we call this predicate to check the list
    % of calls we've found and make any parallelisation decisions.
    %
:- pred conj_build_candidate_conjunctions(list(goal_rep(inst_map_info))::in, 
    goal_path::in, implicit_parallelism_info::in, string_proc_label::in,
    cord(message)::out, list(candidate_par_conjunction_internal)::out)
    is det.

conj_build_candidate_conjunctions(Conjs, GoalPath, Info, ProcLabel, Messages,
        Candidates) :-
    some [!Messages] 
    (
        !:Messages = cord.empty,
        Location = goal(ProcLabel, GoalPath),

        conj_to_inner_goal_list(Conjs, GoalPath, 1, Info, [], InnerGoals, 0,
            NumCostlyCalls),
        ( NumCostlyCalls > 1 -> 
            append_message(Location,
                info_found_conjs_above_callsite_threshold(NumCostlyCalls),
                !Messages), 
            build_dependency_maps(InnerGoals, DependencyMaps),
            % We don't parallelise across non-atomic goals, so split a list of
            % inner goals into partitions where non-atomic goals seperate the
            % partitions.
            partition_inner_goals(Location, InnerGoals, [], _, 
                1, _NumPartitions, 0, _, [], PartitionedInnerGoals, !Messages),
            map(innergoals_build_candidate_conjunction(Info, 
                    DependencyMaps, Location, GoalPath), 
                PartitionedInnerGoals, MaybeCandidates),
            filter_map(maybe_is_yes, MaybeCandidates, Candidates),
            append_message(Location,
                info_found_n_conjunctions_with_positive_speedup(
                    length(Candidates)), !Messages)
        ;
            Candidates = []
        ),
        Messages = !.Messages
    ).

:- pred partition_inner_goals(program_location::in, 
    list(inner_goal_internal)::in,
    list(inner_goal_internal)::in, list(inner_goal_internal)::out,
    int::in, int::out, int::in, int::out,
    list(inner_goals_partition)::in, list(inner_goals_partition)::out,
    cord(message)::in, cord(message)::out) is det.

partition_inner_goals(Location, [], !Partition, !PartitionNum, !NumCostlyCalls,
        !Partitions, !Messages) :-
    ( !.NumCostlyCalls > 1 ->
        Partition = 
            inner_goals_partition(reverse(!.Partition), !.PartitionNum),
        !:Partitions = [ Partition | !.Partitions ]
    ;
        true     
    ),
    ( !.PartitionNum \= 1 ->
        append_message(Location,
            info_split_conjunction_into_partitions(!.PartitionNum), !Messages)
    ;
        true
    ),
    reverse(!Partitions).
partition_inner_goals(Location, [ IG | IGs ], !Partition, !PartitionNum,
        !NumCostlyCalls, !Partitions, !Messages) :-
    IGType = IG ^ igi_ig_type,
    (
        (
            IGType = igt_call(_, _, _, _, _),
            !:NumCostlyCalls = !.NumCostlyCalls + 1
        ;
            IGType = igt_cheap_call(_, _, _, _)
        ;
            IGType = igt_other_atomic_goal
        ),
        !:Partition = [ IG | !.Partition ]
    ;
        IGType = igt_non_atomic_goal,
        ( !.NumCostlyCalls > 1 ->
            Partition = 
                inner_goals_partition(reverse(!.Partition), !.PartitionNum),
            !:Partitions = [ Partition | !.Partitions ]
        ;
            append_message(Location,
                notice_partition_does_not_have_costly_calls(!.PartitionNum,
                    !.NumCostlyCalls), !Messages)
        ),
        !:PartitionNum = !.PartitionNum + 1,
        !:NumCostlyCalls = 0,
        !:Partition = [] 
    ),
    partition_inner_goals(Location, IGs, !Partition, !PartitionNum,
        !NumCostlyCalls, !Partitions, !Messages).

:- pred innergoals_build_candidate_conjunction(implicit_parallelism_info::in,
    dependency_maps::in, program_location::in, goal_path::in,
    inner_goals_partition::in, maybe(candidate_par_conjunction_internal)::out)
    is det.

innergoals_build_candidate_conjunction(_Info, DependencyMaps, Location, GoalPath,
        InnerGoalsPartition, MaybeCandidate) :-
    % Setting up the first parallel conjunct is a different algorithm to the
    % latter ones, at this point we have the option of moving goals from before
    % the first costly call to either before or during the parallel
    % conjunction.  Executing them during the parallel conjunction can be more
    % efficient.  However if goals within other parallel conjuncts depend on
    % them and don't depend upon the first costly call then this would make the
    % conjunction dependant when it could be independent.
    inner_goals_partition(InnerGoalsList, PartNum) = InnerGoalsPartition,
    InnerGoals = cord.from_list(InnerGoalsList),
    find_costly_call(InnerGoals, cord.empty, GoalsBefore0, MaybeFirstCall,
        GoalsDuringAfter),
    (
        MaybeFirstCall = yes(FirstCall),
        FirstCallType = FirstCall ^ igi_ig_type,
        (
            FirstCallType = igt_call(_, _, _, _, FirstCallCallSite)
        ;
            ( FirstCallType = igt_cheap_call(_, _, _, _)
            ; FirstCallType = igt_other_atomic_goal
            ; FirstCallType = igt_non_atomic_goal
            ),
            location_to_string(Location, LocationString),
            error(format(
                "%sFirst call in conjunction is not a call in %s partition %d",
                [s(this_file), s(LocationString), i(PartNum)]))
        )
    ;
        MaybeFirstCall = no,
        location_to_string(Location, LocationString),
        error(format(
            "%sCouldn't find first call in conjunction in %s partition %d",
            [s(this_file), s(LocationString), i(PartNum)]))

    ),
    build_first_seq_conjunction(DependencyMaps, 
        FirstCall ^ igi_conj_num, length(GoalsBefore0),
        length(GoalsDuringAfter), GoalsBefore0,
        cord.singleton(FirstCall), FirstSeqConjunction0, GoalsBefore),
    ( get_first(FirstSeqConjunction0, FirstConj) ->
        FirstConjNumOfFirstParConjunct = FirstConj ^ igi_conj_num
    ;
        error(this_file ++ "Found empty first parallel conjunct")
    ),
    innergoals_build_par_conjs(GoalsDuringAfter, DependencyMaps,
        FirstConjNumOfFirstParConjunct, FirstSeqConjunction0, GoalsAfter,
        cord.empty, ParConjsCord, 0.0, ParConjCost, 
        conjuncts_are_independent, IsDependant),
    
    % Calculate Speedup.
    foldl_pred(innergoal_calc_cost, InnerGoals, 0.0, SequentialCost),
    foldl_pred(innergoal_calc_cost, GoalsBefore, 0.0, GoalsBeforeCost),
    foldl_pred(innergoal_calc_cost, GoalsAfter, 0.0, GoalsAfterCost),
    ParallelCost = GoalsBeforeCost + GoalsAfterCost + ParConjCost,
    NumCalls = FirstCallCallSite ^ ccsr_call_site_summary ^ perf_row_calls,
    Speedup = (SequentialCost - ParallelCost) * float(NumCalls), 
    ( 
        length(ParConjsCord) > 1, 
        Speedup > 0.0
    ->
        ParConjs = list(ParConjsCord),
        MaybeCandidate = yes(candidate_par_conjunction_internal(
            goal_path_to_string(GoalPath), PartNum, IsDependant, ParConjs,
            Speedup))
    ;
        MaybeCandidate = no
    ).

:- pred build_first_seq_conjunction(dependency_maps::in, 
    int::in, int::in, int::in, cord(inner_goal_internal)::in, 
    cord(inner_goal_internal)::in, cord(inner_goal_internal)::out,
    cord(inner_goal_internal)::out) is det. 

build_first_seq_conjunction(DependencyMaps, CallConjNum, NumGoalsBefore,
        NumGoalsAfter, Goals0, !FirstSeqConjunction, GoalsBefore) :-
    % Move over goals in reverse order.
    ( split_last(Goals0, Goals, Goal) ->
        ConjNum = Goal ^ igi_conj_num,
        depends_lookup_rev(DependencyMaps, ConjNum, GoalRevDeps),
        depends_lookup_rev(DependencyMaps, CallConjNum, CallRevDeps),
        (
            % If later goals depend on this goal but not on the first call.
            member(LaterGoalNum, (CallConjNum+1)..(CallConjNum+NumGoalsAfter)),
            (
                member(LaterGoalNum, GoalRevDeps)
            =>
                not member(LaterGoalNum, CallRevDeps)
            )
        ->
            % Then this goal and all those before it must be in GoalsBefore.
            % This can be pessimistic but we're not allowed to re-order goals.
            GoalsBefore = Goals0
        ;
            % This goal may be parallelised as part of the first conjunction.
            !:FirstSeqConjunction = cons(Goal, !.FirstSeqConjunction),
            build_first_seq_conjunction(DependencyMaps, CallConjNum,
                NumGoalsBefore, NumGoalsAfter, Goals, !FirstSeqConjunction,
                GoalsBefore)
        )
    ;
        GoalsBefore = cord.empty
    ).

:- pred innergoals_build_par_conjs(cord(inner_goal_internal)::in,
    dependency_maps::in, int::in,
    cord(inner_goal_internal)::in, cord(inner_goal_internal)::out,
    cord(seq_conj_internal)::in, cord(seq_conj_internal)::out,
    float::in, float::out, 
    conjuncts_are_dependant::in, conjuncts_are_dependant::out) is det.

innergoals_build_par_conjs(Goals0, DependencyMaps,
        FirstConjNumOfFirstParConjunct, CurSeqConjunction0, GoalsAfter,
        !ParConjs, !Cost, !ConjsAreDependant) :-
    % Find the next costly call.
    find_costly_call(Goals0, cord.empty, GoalsBefore, MaybeNextCall, 
        Goals),
    (
        MaybeNextCall = yes(NextCall),
        
        % XXX: This seems to work, but it's terrible, we need to implement the
        % algorithm discussed in Zoltan's lecture slides and use my work to
        % compute when things may be produced and consumed in dependant
        % parallel conjunctions. -pbone
        % XXX: At the very least we need to compute !Cost to reduce false
        % positives.
        CurSeqConjunction = CurSeqConjunction0,
        NewSeqConjunction = snoc(GoalsBefore, NextCall),
      
        % Has the conjunction become dependant.
        map_pred(ig_get_conj_num, CurSeqConjunction, CurSeqConjNumsCord),
        CurSeqConjNumsSet = set(list(CurSeqConjNumsCord)),
        map_pred(ig_get_conj_num, NewSeqConjunction, NewSeqConjNumsCord),
        (
            !.ConjsAreDependant = conjuncts_are_independent,
            member(NewSeqConjNum, NewSeqConjNumsCord),
            depends_lookup(DependencyMaps, NewSeqConjNum, Dependencies),
            intersect(Dependencies, CurSeqConjNumsSet, Intersection),
            not set.empty(Intersection)
        ->
            !:ConjsAreDependant = conjuncts_are_dependant
        ;
            true
        ),

        SeqConjunction = seq_conj_internal(list(CurSeqConjunction)),
        !:ParConjs = snoc(!.ParConjs, SeqConjunction),
        innergoals_build_par_conjs(Goals, DependencyMaps,
            FirstConjNumOfFirstParConjunct, NewSeqConjunction, GoalsAfter,
            !ParConjs, !Cost, !ConjsAreDependant)
    ;
        MaybeNextCall = no,
        ( cord.get_first(CurSeqConjunction0, FirstGoalInLastConjunct) ->
            FirstConjNumOfLastParConjunct = 
                FirstGoalInLastConjunct ^ igi_conj_num
        ; 
            error(this_file ++ " empty parallel conjunct")
        ),
        % Because this is the last parallel conjunction we have the
        % option of putting some remaining goals into the parallel conjunction
        % as part of the last parallel conjunct.
        sorted_list_to_set(
            FirstConjNumOfFirstParConjunct..(FirstConjNumOfLastParConjunct-1),
            GoalsInOtherParConjuncts),
        build_last_seq_conjunction(Goals0, DependencyMaps,
            GoalsInOtherParConjuncts, GoalsAfter, 
            cord.empty, GoalsInLastConjunct),
        SeqConjunction = seq_conj_internal(list(
            CurSeqConjunction0 ++ GoalsInLastConjunct)),
        !:ParConjs = snoc(!.ParConjs, SeqConjunction)
    ).

:- pred find_costly_call(cord(inner_goal_internal)::in,
    cord(inner_goal_internal)::in, cord(inner_goal_internal)::out,
    maybe(inner_goal_internal)::out,
    cord(inner_goal_internal)::out) is det.

find_costly_call(Goals, !GoalsBefore, MaybeCall, GoalsAfter) :-
    ( head_tail(Goals, Goal, GoalsTail) ->
        GoalType = Goal ^ igi_ig_type,
        ( 
            GoalType = igt_call(_, _, _, _, _),
            MaybeCall = yes(Goal),
            GoalsAfter = GoalsTail
        ;
            ( GoalType = igt_cheap_call(_, _, _, _)
            ; GoalType = igt_other_atomic_goal
            ),
            !:GoalsBefore = snoc(!.GoalsBefore, Goal),
            find_costly_call(GoalsTail, !GoalsBefore, MaybeCall, GoalsAfter)
        ;
            GoalType = igt_non_atomic_goal,
            error(this_file ++ "Found non-atomic goal")
        )
    ;
        MaybeCall = no,
        GoalsAfter = cord.empty
    ).

:- pred build_last_seq_conjunction(cord(inner_goal_internal)::in, 
    dependency_maps::in, set(int)::in, cord(inner_goal_internal)::out,
    cord(inner_goal_internal)::in, cord(inner_goal_internal)::out) is det.

build_last_seq_conjunction(Goals0, DependencyMaps, GoalsInOtherParConjuncts, 
        GoalsAfter, !GoalsInLastConjunct) :-
    ( head_tail(Goals0, Goal, Goals) ->
        ConjNum = Goal ^ igi_conj_num,
        depends_lookup(DependencyMaps, ConjNum, Dependencies),
        intersect(Dependencies, GoalsInOtherParConjuncts, Intersection),
        (
            % The goal is dependant apon a goal in a previous parallel conjunct.
            not set.empty(Intersection) => 
            % But not dependant upon a goal in the current parallel conjunct.
            (
                map_pred(ig_get_conj_num, !.GoalsInLastConjunct, 
                    ConjNumsInLastConjunct),
                intersect(Dependencies, set(list(ConjNumsInLastConjunct)),
                    Intersection2),
                set.empty(Intersection2)
            )
        ->
            % This goal and all those after it must be placed after the
            % conjunction.
            GoalsAfter = Goals0
        ;
            % This goal does not depend on goals in other parallel conjuncts,
            % we can parallelise it here without introducing an extra future.
            !:GoalsInLastConjunct = snoc(!.GoalsInLastConjunct, Goal),
            build_last_seq_conjunction(Goals, DependencyMaps,
                GoalsInOtherParConjuncts, GoalsAfter, !GoalsInLastConjunct)
        )
    ;
        GoalsAfter = cord.empty
    ).

:- pred ig_get_conj_num(inner_goal_internal::in, int::out) is det.

ig_get_conj_num(IG, IG ^ igi_conj_num).

:- pred innergoal_calc_cost(inner_goal_internal::in, float::in, float::out) 
    is det.

innergoal_calc_cost(Goal, !Cost) :-
    GoalType = Goal ^ igi_ig_type,
    (
        GoalType = igt_call(_, _, CostPercall, _, _),
        !:Cost = !.Cost + CostPercall
    ; 
        GoalType = igt_cheap_call(_, _, _, Cost),
        ( cs_cost_get_calls(Cost) > 0.0 ->
            !:Cost = !.Cost + cs_cost_get_percall(Cost)
        ;
            % Goals that are never called have no cost
            true
        )
    ;
        GoalType = igt_other_atomic_goal
    ;
        GoalType = igt_non_atomic_goal,
        error(this_file ++ "unexpected non atomic goal")
    ).

:- type dependency_maps
    ---> dependency_maps(
            dm_depends_on           :: map(int, set(int)),
                % This map maps from conjunct numbers to the conjunct numbers
                % of goals that they depend upon.

            dm_is_depended_on_by    :: map(int, set(int))
                % This is the reverse dependency map.  It maps from a dependee
                % to conjunct numbers that depend on this goal.
         ).

:- pred build_dependency_maps(list(inner_goal_internal)::in, 
    dependency_maps::out) is det.

build_dependency_maps(InnerGoals, Maps) :-
    length(InnerGoals, InnerGoalsLen),
    % Both maps are initialised equally.
    fold_up(insert_empty_set, 1, InnerGoalsLen, map.init, InitialisedMap), 
    build_dependency_map(InnerGoals, 1, map.init, _VarDepMap, 
        InitialisedMap, Map, InitialisedMap, RevMap),
    Maps = dependency_maps(Map, RevMap).

:- pred depends_lookup(dependency_maps::in, int::in, set(int)::out) is det.

depends_lookup(DependencyMaps, GoalNum, Dependencies) :-
    Map = DependencyMaps ^ dm_depends_on,
    lookup(Map, GoalNum, Dependencies).

:- pred depends_lookup_rev(dependency_maps::in, int::in, set(int)::out) is det.

depends_lookup_rev(DependencyMaps, GoalNum, Dependencies) :-
    RevMap = DependencyMaps ^ dm_is_depended_on_by,
    lookup(RevMap, GoalNum, Dependencies).

:- pred insert_empty_set(int::in, 
    map(int, set(int))::in, map(int, set(int))::out) is det.

insert_empty_set(K, !Map) :-
    svmap.det_insert(K, set.init, !Map).

:- pred build_dependency_map(list(inner_goal_internal)::in, int::in, 
    map(var_rep, set(int))::in, map(var_rep, set(int))::out,
    map(int, set(int))::in, map(int, set(int))::out,
    map(int, set(int))::in, map(int, set(int))::out) is det.

build_dependency_map([], _ConjNum, !VarDepMap, !Map, !RevMap).
build_dependency_map([IG | IGs], ConjNum, !VarDepMap, !Map, !RevMap) :-
    InstMapInfo = IG ^ igi_inst_map_info,
    
    % For each variable consumed by a goal we find out which goals instantiate
    % that variable and add them as it's dependencies along with their
    % dependencies.  NOTE: We only consider variables that are read
    % and not those that are set.  This is safe because we only bother
    % analysing single assignment code.
    AllVars = InstMapInfo ^ im_all_vars,
    set.difference(AllVars, InstVars, RefedVars), 
    list.map((pred(RefedVar::in, DepConjsI::out) is det :-
        map.search(!.VarDepMap, RefedVar, DepConjsPrime) -> 
            DepConjsI = DepConjsPrime
        ;
            % If we consume a variable that we don't know the producer of, it
            % may be a parameter to the procedure or have been produced by a
            % goal outside of this conjunction.  Add an empty set of
            % dependencies.
            set.init(DepConjsI)
        ), to_sorted_list(RefedVars), DepConjss),
    DepConjs = union_list(DepConjss),
    fold(transitive_map_insert(ConjNum), DepConjs, !Map),
    fold((pred(K::in, MapI0::in, MapI::out) is det :-
            transitive_map_insert(K, ConjNum, MapI0, MapI)
        ), DepConjs, !RevMap), 
    
    % For each variable instantiated by this goal add it to the VarDepMap with
    % this goal as it's instantiator.  That is a maping from the variable to
    % the conj num.  The var to conjnum map is not transitive.
    calc_inst_map_delta(InstMapInfo ^ im_before, InstMapInfo ^ im_after, 
        InstMapDelta),
    inst_map_delta_get_var_set(InstMapDelta, InstVars),
    fold(add_var_to_var_dep_map(ConjNum), InstVars, !VarDepMap),

    build_dependency_map(IGs, ConjNum + 1, !VarDepMap, !Map, !RevMap).

:- pred add_var_to_var_dep_map(int::in, var_rep::in, 
    map(var_rep, set(int))::in, map(var_rep, set(int))::out) is det. 

add_var_to_var_dep_map(ConjNum, Var, !VarDepMap) :-
    ( map.search(!.VarDepMap, Var, ConjNums0) ->
        % This is a multiple instantiation.
        svmap.det_update(Var, insert(ConjNums0, ConjNum), !VarDepMap)
    ;
        singleton_set(ConjNums, ConjNum),
        svmap.det_insert(Var, ConjNums, !VarDepMap)
    ).

    % Check if it is appropriate to parallelise this call.  That is it must be
    % model_det and have a cost above the call site cost threshold.
    %
:- pred can_parallelise_call(implicit_parallelism_info::in,
    detism_rep::in, clique_call_site_report::in) is semidet.

can_parallelise_call(Info, Detism, CallSiteReport) :-
    ( Detism = det_rep
    ; Detism = cc_multidet_rep ),
    CallSiteCost = get_call_site_cost(Info, CallSiteReport),
    ( cs_cost_get_calls(CallSiteCost) > 0.0 ->
        % This is conditional so that we can gauretee that it never causes a
        % divide by zero error,
        PercallCost = cs_cost_get_percall(CallSiteCost),
        PercallCost > float(Info ^ ipi_opts ^ cpc_call_site_threshold)
    ;
        fail 
    ).

:- pred maybe_costly_call(implicit_parallelism_info::in, goal_path::in,
    atomic_goal_rep::in, detism_rep::in, inst_map_info::in,
    inner_goal_type::out(igt_atomic_goal)) is det.

maybe_costly_call(Info, GoalPath, AtomicGoal, Detism,
        InstMapInfo, InnerGoalType) :-
    InstMapBefore = InstMapInfo ^ im_before,
    InstMapAfter = InstMapInfo ^ im_after,
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
        InnerGoalType = igt_other_atomic_goal 
    ;
        ( AtomicGoal = higher_order_call_rep(_, Args)
        ; AtomicGoal = method_call_rep(_, _, Args)
        ; AtomicGoal = plain_call_rep(_, _, Args)
        ),
        (
            ( AtomicGoal = higher_order_call_rep(_, _)
            ; AtomicGoal = method_call_rep(_, _, _)
            ),
            Callee = unknown_callee 
        ; 
            AtomicGoal = plain_call_rep(ModuleName, CalleeName, _),
            Callee = named_callee(ModuleName, CalleeName)
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
        map(maybe_search_var_name(Info ^ ipi_var_table), Args, Vars),

        ( can_parallelise_call(Info, Detism, CallSite) ->
            CostPercall = cs_cost_get_percall(get_call_site_cost(Info,
                CallSite)),
            InnerGoalType = 
                igt_call(Callee, Vars, CostPercall, VarModeAndUses, CallSite)
        ;
            CallSiteCost = get_call_site_cost(Info, CallSite),
            InnerGoalType = igt_cheap_call(Callee, Vars, VarModeAndUses,
                CallSiteCost)
        )
    ).

:- pred var_get_mode(inst_map::in, inst_map::in, var_rep::in, var_mode_rep::out)
    is det.

var_get_mode(InstMapBefore, InstMapAfter, VarRep, VarModeRep) :-
    inst_map_get(InstMapBefore, VarRep, InstBefore, _),
    inst_map_get(InstMapAfter, VarRep, InstAfter, _),
    VarModeRep = var_mode_rep(InstBefore, InstAfter).

    % Transform a conjunction of goals into a list of inner goals..
    %
    % The results are returned in the order that they appear.
    %
:- pred conj_to_inner_goal_list(list(goal_rep(inst_map_info))::in,
    goal_path::in, int::in, implicit_parallelism_info::in,
    list(inner_goal_internal)::in, list(inner_goal_internal)::out,
    int::in, int::out) is det.

conj_to_inner_goal_list([], _, _, _, !InnerGoals, !NumCostlyCalls) :-
    list.reverse(!InnerGoals).

conj_to_inner_goal_list([Goal | Goals], GoalPath0, ConjNum, Info,
        !InnerGoals, !NumCostlyCalls) :-
    Goal = goal_rep(GoalExpr, Detism, InstMapInfo),
    (
        ( GoalExpr = conj_rep(_)
        ; GoalExpr = disj_rep(_)
        ; GoalExpr = switch_rep(_, _, _)
        ; GoalExpr = ite_rep(_, _, _)
        ; GoalExpr = negation_rep(_)
        ; GoalExpr = scope_rep(_, _)
        ),
        % XXX: We my consider lifting calls out of non-atomic goals so that
        % they can be parallelised,  or parallelising the whole non-atomic
        % goal.
        InnerGoalType = igt_non_atomic_goal
    ;
        GoalExpr = atomic_goal_rep(_Context, _Line, _BoundVars, AtomicGoal),
        GoalPath = goal_path_add_at_end(GoalPath0, step_conj(ConjNum)),
        maybe_costly_call(Info, GoalPath, AtomicGoal, Detism,
            InstMapInfo, InnerGoalType),
        (
            InnerGoalType = igt_call(_, _, _, _, _),
            !:NumCostlyCalls = !.NumCostlyCalls + 1
        ;
            InnerGoalType = igt_cheap_call(_, _, _, _)
        ;
            InnerGoalType = igt_other_atomic_goal
        )
    ),
    InnerGoal = inner_goal_internal(InnerGoalType, Detism, ConjNum,
        InstMapInfo),
    !:InnerGoals = [InnerGoal | !.InnerGoals],
    conj_to_inner_goal_list(Goals, GoalPath0, ConjNum+1, Info, 
        !InnerGoals, !NumCostlyCalls).

    % are_conjuncts_dependant(CallOutputs, InstMap, VarModeAndUse, !DepVars),
    %
    % Determine if a variables depends on an output from an earlier call either
    % directly or indirectly.  If it does add it to the DepVars set.
    %
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
    = cs_cost_csq.

get_call_site_cost(Info, CallSite) = Cost :-
    CSSummary = CallSite ^ ccsr_call_site_summary,
    GoalPath = CSSummary ^ perf_row_subject ^ csdesc_goal_path,
    ( map.search(Info ^ ipi_rec_call_sites, GoalPath, CostPrime) ->
        Cost = CostPrime
    ;
        MaybePerfTotal = CSSummary ^ perf_row_maybe_total, 
        (
            MaybePerfTotal = yes(PerfTotal),
            TotalCost = PerfTotal ^ perf_row_callseqs
        ;
            MaybePerfTotal = no,
            error(this_file ++ 
                "Could not retrieve total callseqs cost from call site")
        ),
        Calls = CSSummary ^ perf_row_calls,
        Cost = build_cs_cost_csq(Calls, float(TotalCost))
    ).

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

%----------------------------------------------------------------------------%
%
% Annotate a goal with instantiation information.
%

    % XXX: This is poor, we need to associate an inst map delta with each goal
    % rather than a pair of inst maps, without this some information cannot be
    % recovered when trying to build the inst map, for example duplicate
    % instantiations can't be recovered properly.
:- type inst_map_info
    --->    inst_map_info(
                im_before           :: inst_map,
                    % The inst map before this goal is executed.

                im_after            :: inst_map,
                    % The inst map after this goal was executed.

                im_all_vars         :: set(var_rep)
                    % Variables referenced by this goal, both consumed and
                    % produced.
            ).

    % Note: It may be useful to add other annotations such as goal path or cost 
    % information.
    %
    % SeenDuplicateInstiation is used to assert that we're analysing single
    % assignment code only.
    %
    % Vars is the set of variables used by this goal, both consumed and
    % produced.
    %
:- pred goal_annotate_with_instmap(goal_rep::in, goal_rep(inst_map_info)::out, 
    inst_map::in, inst_map::out, seen_duplicate_instantiation::out, 
    set(var_rep)::out) is det.

goal_annotate_with_instmap(Goal0, Goal, !InstMap, SeenDuplicateInstantiation,
        Vars) :-
    Goal0 = goal_rep(GoalExpr0, Detism, _),
    InstMapBefore = !.InstMap,
    (
        GoalExpr0 = conj_rep(Conjs0),
        conj_annotate_with_instmap(Conjs0, Conjs, !InstMap, 
            SeenDuplicateInstantiation, Vars),
        GoalExpr = conj_rep(Conjs)
    ;
        GoalExpr0 = disj_rep(Disjs0),
        disj_annotate_with_instmap(Disjs0, Disjs, !InstMap,
            SeenDuplicateInstantiation, Vars),
        GoalExpr = disj_rep(Disjs)
    ;
        GoalExpr0 = switch_rep(Var, CanFail, Cases0),
        switch_annotate_with_instmap(Cases0, Cases, !InstMap,
            SeenDuplicateInstantiation, Vars0),
        set.insert(Vars0, Var, Vars),
        GoalExpr = switch_rep(Var, CanFail, Cases)
    ;
        GoalExpr0 = ite_rep(Cond0, Then0, Else0),
        ite_annotate_with_instmap(Cond0, Cond, Then0, Then, Else0, Else,
            !InstMap, SeenDuplicateInstantiation, Vars),
        GoalExpr = ite_rep(Cond, Then, Else)
    ;
        GoalExpr0 = scope_rep(Subgoal0, MaybeCut),
        goal_annotate_with_instmap(Subgoal0, Subgoal, !InstMap, 
            SeenDuplicateInstantiation, Vars),
        GoalExpr = scope_rep(Subgoal, MaybeCut)
    ;
        GoalExpr0 = negation_rep(Subgoal0),
        % A negated goal cannot affect instantiation.
        goal_annotate_with_instmap(Subgoal0, Subgoal, !.InstMap, 
            _InstMap, SeenDuplicateInstantiation, Vars),
        GoalExpr = negation_rep(Subgoal)
    ;
        GoalExpr0 = atomic_goal_rep(File, Line, BoundVars, AtomicGoal),
        % The binding of a variable may depend on any number of other
        % variables, and recursively the variables that those depended-on
        % variables depend upon.  
        % XXX: This doesn't include variables that can affect control flow and
        % therefore the values of other variables, this includes variables
        % referenced from conditions in ITE goals, and variables switched-on.
        % We may get away with this as our new system for determining
        % goal-dependance takes these into account.
        atomic_goal_get_vars(AtomicGoal, Vars),
        BoundVarsSet = set.from_list(BoundVars),
        set.difference(Vars, BoundVarsSet, RefedVars),
        inst_map_ground_vars(BoundVars, RefedVars, !InstMap,
            SeenDuplicateInstantiation),
        GoalExpr = atomic_goal_rep(File, Line, BoundVars, AtomicGoal) 
    ),
    InstMapAfter = !.InstMap,
    InstMapInfo = inst_map_info(InstMapBefore, InstMapAfter, Vars),
    Goal = goal_rep(GoalExpr, Detism, InstMapInfo).

:- pred conj_annotate_with_instmap(list(goal_rep)::in,
    list(goal_rep(inst_map_info))::out, inst_map::in, inst_map::out,
    seen_duplicate_instantiation::out, set(var_rep)::out) is det.

conj_annotate_with_instmap([], [], !InstMap,
    have_not_seen_duplicate_instantiation, set.init).
conj_annotate_with_instmap([Conj0 | Conjs0], [Conj | Conjs], !InstMap, 
        SeenDuplicateInstantiation, Vars) :-
    goal_annotate_with_instmap(Conj0, Conj, !InstMap,
        SeenDuplicateInstantiationHead, VarsHead),
    conj_annotate_with_instmap(Conjs0, Conjs, !InstMap,
        SeenDuplicateInstantiationTail, VarsTail),
    set.union(VarsTail, VarsHead, Vars),
    SeenDuplicateInstantiation = merge_seen_duplicate_instantiation(
        SeenDuplicateInstantiationHead,
        SeenDuplicateInstantiationTail).

:- pred disj_annotate_with_instmap(list(goal_rep)::in,
    list(goal_rep(inst_map_info))::out, inst_map::in, inst_map::out,
    seen_duplicate_instantiation::out, set(var_rep)::out) is det.

disj_annotate_with_instmap([], [], !InstMap,
        have_not_seen_duplicate_instantiation, set.init).
disj_annotate_with_instmap([Disj0 | Disjs0], [Disj | Disjs], InstMap0, InstMap,
        SeenDuplicateInstantiation, Vars) :-
    HeadDetism = Disj0 ^ goal_detism_rep,
    goal_annotate_with_instmap(Disj0, Disj, InstMap0, InstMapHead,
        SeenDuplicateInstantiationHead, VarsHead),
    disj_annotate_with_instmap(Disjs0, Disjs, InstMap0, InstMapTail,
        SeenDuplicateInstantiationTail, VarsTail),
    
    set.union(VarsTail, VarsHead, Vars),

    % merge_inst_map requires the detism of goals that produce both inst maps,
    % we can create fake values that satisfy merge_inst_map easily.
    % XXX: Consider inferring determinism as another simple analysis.
    (
        Disjs = [],
        TailDetism = failure_rep
    ;
        Disjs = [_ | _],
        TailDetism = det_rep
    ),
    InstMap = merge_inst_map(InstMapHead, HeadDetism, InstMapTail, TailDetism),
    SeenDuplicateInstantiation = merge_seen_duplicate_instantiation(
        SeenDuplicateInstantiationHead,
        SeenDuplicateInstantiationTail).

:- pred switch_annotate_with_instmap(list(case_rep)::in, 
    list(case_rep(inst_map_info))::out, inst_map::in, inst_map::out,
    seen_duplicate_instantiation::out, set(var_rep)::out) is det.

switch_annotate_with_instmap([], [], !InstMap,
        have_not_seen_duplicate_instantiation, set.init).
switch_annotate_with_instmap([Case0 | Cases0], [Case | Cases], 
        InstMap0, InstMap, SeenDuplicateInstantiation, Vars) :-
    Case0 = case_rep(ConsIdArity, ExtraConsIdAritys, Goal0),
    HeadDetism = Goal0 ^ goal_detism_rep,
    goal_annotate_with_instmap(Goal0, Goal, InstMap0, InstMapHead,
        SeenDuplicateInstantiationHead, VarsHead),
    Case = case_rep(ConsIdArity, ExtraConsIdAritys, Goal),
    switch_annotate_with_instmap(Cases0, Cases, InstMap0, InstMapTail,
        SeenDuplicateInstantiationTail, VarsTail),
    set.union(VarsTail, VarsHead, Vars),
    % merge_inst_map requires the detism of goals that produce both inst maps,
    % we can create fake values that satisfy merge_inst_map easily.
    (
        Cases = [],
        TailDetism = failure_rep
    ;
        Cases = [_ | _],
        TailDetism = det_rep
    ),
    InstMap = merge_inst_map(InstMapHead, HeadDetism, InstMapTail, TailDetism),
    SeenDuplicateInstantiation = merge_seen_duplicate_instantiation(
        SeenDuplicateInstantiationHead,
        SeenDuplicateInstantiationTail).

:- pred ite_annotate_with_instmap(goal_rep::in, goal_rep(inst_map_info)::out,
    goal_rep::in, goal_rep(inst_map_info)::out,
    goal_rep::in, goal_rep(inst_map_info)::out,
    inst_map::in, inst_map::out, 
    seen_duplicate_instantiation::out, set(var_rep)::out) is det.

ite_annotate_with_instmap(Cond0, Cond, Then0, Then, Else0, Else, InstMap0, InstMap,
        SeenDuplicateInstantiation, Vars) :-
    goal_annotate_with_instmap(Cond0, Cond, InstMap0, InstMapAfterCond,
        SeenDuplicateInstantiationCond, VarsCond),
    goal_annotate_with_instmap(Then0, Then, InstMapAfterCond, InstMapAfterThen,
        SeenDuplicateInstantiationThen, VarsThen),
    goal_annotate_with_instmap(Else0, Else, InstMap0, InstMapAfterElse,
        SeenDuplicateInstantiationElse, VarsElse),
    (
        SeenDuplicateInstantiationCond = have_not_seen_duplicate_instantiation,
        SeenDuplicateInstantiationThen = have_not_seen_duplicate_instantiation,
        SeenDuplicateInstantiationElse = have_not_seen_duplicate_instantiation
    ->
        SeenDuplicateInstantiation = have_not_seen_duplicate_instantiation
    ;
        SeenDuplicateInstantiation = seen_duplicate_instantiation
    ),
    set.union(VarsCond, VarsThen, VarsCondThen),
    set.union(VarsCondThen, VarsElse, Vars),
    ThenDetism = Then ^ goal_detism_rep,
    ElseDetism = Else ^ goal_detism_rep,
    InstMap = merge_inst_map(InstMapAfterThen, ThenDetism, 
        InstMapAfterElse, ElseDetism).

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

:- func build_cs_cost_from_perf(perf_row_data(T)) = cs_cost_csq.

build_cs_cost_from_perf(Perf) = CSCost :-
    MaybePerfTotal = Perf ^ perf_row_maybe_total,
    (
        MaybePerfTotal = yes(PerfTotal)
    ;
        MaybePerfTotal = no,
        error(this_file ++ 
            "Could not retrieve total cost from perf data")
    ),
    TotalCSQ = PerfTotal ^ perf_row_callseqs,
    Calls = Perf ^ perf_row_calls,
    CSCost = build_cs_cost_csq(Calls, float(TotalCSQ)).

:- pred transitive_map_insert(T::in, T::in, 
    map(T, set(T))::in, map(T, set(T))::out) is det.

transitive_map_insert(K, V, !Map) :-
    ( map.search(!.Map, K, Vs0) ->
        ( member(V, Vs0) ->
            true
        ;
            insert(Vs0, V, Vs1),
            ( map.search(!.Map, V, VsTransitive) ->
                union(Vs1, VsTransitive, Vs)
            ;
                Vs = Vs1
            ),
            svmap.det_update(K, Vs, !Map)
        )
    ;
        ( map.search(!.Map, V, VsTransitive) ->
            insert(VsTransitive, V, Vs)
        ;
            singleton_set(Vs, V)
        ),
        svmap.det_insert(K, Vs, !Map)
    ).

    % split_on(Pred, List, SplitLists).
    %
    % Split a list on items for which Pred is true, the delimiting items are
    % not returned in the result, empty sections are not removed.
    %
    % split_on(unify(1), [2, 3, 1, 2, 6, 1, 1, 3, 1], 
    %   [[2, 3], [2, 6], [], 3, []])
    %
:- pred split_on(pred(T), list(T), list(list(T))).
:- mode split_on(pred(in) is semidet, in, out(non_empty_list)) is det.

split_on(_Pred, [], [[]]).
split_on(Pred, [X | Xs], SplitLists) :-
    split_on(Pred, Xs, SplitLists0),
    ( Pred(X) ->
        SplitLists = [ [] | SplitLists0 ]
    ;
        SplitLists0 = [SplitList | T], 
        SplitLists = [ [ X | SplitList ] | T ]
    ).

%-----------------------------------------------------------------------------%
:- end_module mdprof_fb.automatic_parallelism.
%-----------------------------------------------------------------------------%

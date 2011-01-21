%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2011 The University of Melbourne.
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
:- import_module mdbcomp.feedback.automatic_parallelism.
:- import_module mdbcomp.program_representation.

:- import_module cord.
:- import_module pair.

%-----------------------------------------------------------------------------%

    % Build the candidate parallel conjunctions feedback information used for
    % implicit parallelism.
    %
:- pred candidate_parallel_conjunctions(
    candidate_par_conjunctions_params::in, deep::in, cord(message)::out,
    feedback_info::in, feedback_info::out) is det.

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

:- pred create_candidate_parallel_conj_proc_report(
    pair(string_proc_label, candidate_par_conjunctions_proc)::in,
    cord(string)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module analysis_utils.
:- import_module branch_and_bound.
:- import_module coverage.
:- import_module create_report.
:- import_module mdbcomp.goal_path.
:- import_module measurement_units.
:- import_module measurements.
:- import_module program_representation_utils.
:- import_module recursion_patterns.
:- import_module report.
:- import_module solutions.
:- import_module var_use_analysis.

:- import_module array.
:- import_module assoc_list.
:- import_module bool.
:- import_module digraph.
:- import_module float.
:- import_module io.
:- import_module int.
:- import_module lazy.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module svmap.

%----------------------------------------------------------------------------%
%
% The code in this section has some trace goals that can be enabled with:
%
%   --trace-flag=debug_cpc_search
%     Debug the traversal through the clique tree.
%
%   --trace-flag=debug_parallel_conjunction_speedup
%     Print candidate parallel conjunctions that are pessimisations.
%
%   --trace-flag=debug_branch_and_bound
%     Debug the branch and bound search for the best parallelisation.
%

candidate_parallel_conjunctions(Params, Deep, Messages, !Feedback) :-
    % Find opportunities for parallelism by walking the clique tree.
    % Do not descend into cliques cheaper than the threshold.
    deep_lookup_clique_index(Deep, Deep ^ root, RootCliquePtr),
    % The +1 here accounts for the cost of the pseudo call into the mercury
    % runtime since it is modeled here as a call site that in reality
    % does not exist.
    RootParallelism = no_parallelism,
    candidate_parallel_conjunctions_clique(Params, Deep,
        RootParallelism, RootCliquePtr, ConjunctionsMap, Messages),

    map.to_assoc_list(ConjunctionsMap, ConjunctionsAssocList0),
    assoc_list.map_values_only(
        convert_candidate_par_conjunctions_proc(pard_goal_detail_to_pard_goal),
        ConjunctionsAssocList0, ConjunctionsAssocList),
    CandidateParallelConjunctions =
        feedback_data_candidate_parallel_conjunctions(Params,
            ConjunctionsAssocList),
    put_feedback_data(CandidateParallelConjunctions, !Feedback).

:- pred pard_goal_detail_to_pard_goal(
    candidate_par_conjunction(pard_goal_detail)::in,
    pard_goal_detail::in, pard_goal::out) is det.

pard_goal_detail_to_pard_goal(CPC, !Goal) :-
    IsDependent = CPC ^ cpc_is_dependent,
    (
        IsDependent = conjuncts_are_dependent(SharedVars)
    ;
        IsDependent = conjuncts_are_independent,
        SharedVars = set.init
    ),
    transform_goal_rep(pard_goal_detail_annon_to_pard_goal_annon(SharedVars),
        !Goal).

:- pred pard_goal_detail_annon_to_pard_goal_annon(set(var_rep)::in,
    pard_goal_detail_annotation::in, pard_goal_annotation::out) is det.

pard_goal_detail_annon_to_pard_goal_annon(SharedVarsSet, PGD, PG) :-
    CostPercall = goal_cost_get_percall(PGD ^ pgd_cost),
    CostAboveThreshold = PGD ^ pgd_cost_above_threshold,
    SharedVars = to_sorted_list(SharedVarsSet),

    Coverage = PGD ^ pgd_coverage,
    get_coverage_before_det(Coverage, Calls),
    ( Calls > 0 ->
        list.foldl(build_var_use_list(PGD ^ pgd_var_production_map),
            SharedVars, [], Productions),
        list.foldl(build_var_use_list(PGD ^ pgd_var_consumption_map),
            SharedVars, [], Consumptions)
    ;
        Productions = [],
        Consumptions = []
    ),

    PG = pard_goal_annotation(CostPercall, CostAboveThreshold, Productions,
        Consumptions).

:- pred build_var_use_list(map(var_rep, lazy(var_use_info))::in, var_rep::in,
    assoc_list(var_rep, float)::in, assoc_list(var_rep, float)::out) is det.

build_var_use_list(Map, Var, !List) :-
    (
        map.search(Map, Var, LazyUse),
        read_if_val(LazyUse, Use)
    ->
        UseTime = Use ^ vui_cost_until_use,
        !:List = [Var - UseTime | !.List]
    ;
        true
    ).

%----------------------------------------------------------------------------%

:- type implicit_parallelism_info
    --->    implicit_parallelism_info(
                ipi_deep            :: deep,
                ipi_progrep         :: prog_rep,
                ipi_opts            :: candidate_par_conjunctions_params,
                ipi_clique          :: clique_ptr,
                ipi_call_sites      :: map(reverse_goal_path, cost_and_callees),
                ipi_rec_call_sites  :: map(reverse_goal_path, cs_cost_csq),
                ipi_containing_goal_map
                                    :: containing_goal_map,
                ipi_coverage_array  :: goal_attr_array(coverage_info),
                ipi_inst_map_array  :: goal_attr_array(inst_map_info),
                ipi_recursion_type  :: recursion_type,
                ipi_var_table       :: var_table,
                ipi_proc_label      :: string_proc_label
            ).

    % A representation of a goal within a parallel conjunction.  We don't have
    % to represent many types of goals or details about them, at least for now.
    % This type provides more detail than feedback.pard_goal, this detail isn't
    % required by the compiler and therefore not part of the feedback file
    % format.
    %
:- type pard_goal_detail == goal_rep(pard_goal_detail_annotation).

:- type pard_goal_detail_annotation
    --->    pard_goal_detail(
                % The type and type-specific values of the pard goal.
                pgd_pg_type                 :: pard_goal_type,

                % The inst map info attached to the original goal.
                pgd_inst_map_info           :: inst_map_info,

                % The original goal path of this goal.
                pgd_original_path           :: reverse_goal_path,

                % Coverage data for this goal.
                pgd_coverage                :: coverage_info,

                % The per-call cost of this call in call sequence counts.
                pgd_cost                    :: goal_cost_csq,

                pgd_cost_above_threshold    :: cost_above_par_threshold,

                % Variable production and consumption information.
                pgd_var_production_map      :: map(var_rep, lazy(var_use_info)),
                pgd_var_consumption_map     :: map(var_rep, lazy(var_use_info))
            ).

:- type pard_goal_type
    --->    pgt_call(
                % The argument modes and use information.
                pgtc_args                   :: list(var_and_mode),

                % The call site report from the deep profiler.
                pgtc_call_site              :: cost_and_callees
            )
    ;       pgt_other_atomic_goal
    ;       pgt_non_atomic_goal.

    % A variable and its mode.
    %
:- type var_and_mode
    --->    var_and_mode(
                vmu_var                 :: var_rep,
                vmu_mode                :: var_mode_rep
            ).

:- type candidate_par_conjunctions ==
    map(string_proc_label, candidate_par_conjunctions_proc(pard_goal_detail)).

%----------------------------------------------------------------------------%
%
% Recurse the call graph searching for parallelisation opportunities.
%

    % candidate_parallel_conjunctions_clique(Opts, Deep, ParentCSCost,
    %   ParentUsedParallelism, CliquePtr, CandidateParallelConjunctions,
    %   Messages)
    %
    % Find any CandidateParallelConjunctions in this clique and its children.
    % We stop searching when ParentCSCost is too low (in which case the
    % relative overheads of parallelism would be too great), or if
    % ParentUsedParallelism becomes greater than the desired amount
    % of parallelism.
    %
:- pred candidate_parallel_conjunctions_clique(
    candidate_par_conjunctions_params::in, deep::in,
    parallelism_amount::in, clique_ptr::in, candidate_par_conjunctions::out,
    cord(message)::out) is det.

candidate_parallel_conjunctions_clique(Opts, Deep, ParentParallelism,
        CliquePtr, Candidates, Messages) :-
    find_clique_first_and_other_procs(Deep, CliquePtr, MaybeFirstPDPtr,
        OtherPDPtrs),
    (
        MaybeFirstPDPtr = yes(FirstPDPtr),
        PDPtrs = [FirstPDPtr | OtherPDPtrs]
    ;
        MaybeFirstPDPtr = no,
        deep_lookup_clique_index(Deep, Deep ^ root, RootCliquePtr),
        ( CliquePtr = RootCliquePtr ->
            % It's okay, this clique never has an entry procedure.
            PDPtrs = OtherPDPtrs
        ;
            CliquePtr = clique_ptr(CliqueNum),
            string.format("Clique %d has no entry proc", [i(CliqueNum)], Msg),
            unexpected($module, $pred, Msg)
        )
    ),

    create_clique_recursion_costs_report(Deep, CliquePtr,
        MaybeRecursiveCostsReport),
    (
        MaybeRecursiveCostsReport = ok(RecursiveCostsReport),
        RecursionType = RecursiveCostsReport ^ crr_recursion_type
    ;
        MaybeRecursiveCostsReport = error(ErrorB),
        RecursionType = rt_errors([ErrorB])
    ),

    % Look for parallelisation opportunities in this clique.
    list.map2(
        candidate_parallel_conjunctions_clique_proc(Opts, Deep,
            RecursionType, CliquePtr),
        PDPtrs, CandidateLists, MessageCords),
    list.foldl(map.union(merge_candidate_par_conjs_proc), CandidateLists,
        map.init, CliqueCandidates),
    CliqueMessages = cord_list_to_cord(MessageCords),

    % Look in descendent cliques.
    some [!ChildCliques] (
        map(proc_dynamic_callees(Deep, ParentParallelism), PDPtrs,
            ChildCliquess),
        !:ChildCliques = cord_list_to_cord(ChildCliquess),
        list.map2(
            candidate_parallel_conjunctions_callee(Opts, Deep,
                CliquePtr, CliqueCandidates),
            list(!.ChildCliques), CSCandidateLists, CSMessageCords)
    ),
    list.foldl(map.union(merge_candidate_par_conjs_proc), CSCandidateLists,
        map.init, CSCandidates),
    CSMessages = cord_list_to_cord(CSMessageCords),

    map.union(merge_candidate_par_conjs_proc, CliqueCandidates, CSCandidates,
        Candidates),
    Messages = CliqueMessages ++ CSMessages.

:- type candidate_child_clique
    --->    candidate_child_clique(
                ccc_clique              :: clique_ptr,
                ccc_cs_cost             :: cs_cost_csq,

                % The context of the call site that calls this clique.
                ccc_proc                :: string_proc_label,
                ccc_goal_path           :: reverse_goal_path,

                % The amount of parallelism already used during the call due to
                % parallelisations in the parents.
                ccc_parallelism         :: parallelism_amount
            ).

:- pred proc_dynamic_callees(deep::in, parallelism_amount::in,
    proc_dynamic_ptr::in, cord(candidate_child_clique)::out) is det.

proc_dynamic_callees(Deep, Parallelism, PDPtr, ChildCliques) :-
    deep_lookup_proc_dynamics(Deep, PDPtr, PD),
    PSPtr = PD ^ pd_proc_static,
    deep_lookup_proc_statics(Deep, PSPtr, PS),
    ProcLabel = PS ^ ps_id,
    proc_dynamic_paired_call_site_slots(Deep, PDPtr, Slots),
    list.map(pd_slot_callees(Deep, Parallelism, ProcLabel),
        Slots, ChildCliqueCords),
    ChildCliques = cord_list_to_cord(ChildCliqueCords).

:- pred pd_slot_callees(deep::in, parallelism_amount::in,
    string_proc_label::in,
    pair(call_site_static_ptr, call_site_array_slot)::in,
    cord(candidate_child_clique)::out) is det.

pd_slot_callees(Deep, Parallelism, ProcLabel, CSSPtr - Slot, ChildCliques) :-
    deep_lookup_call_site_statics(Deep, CSSPtr, CSS),
    RevGoalPath = CSS ^ css_goal_path,
    (
        Slot = slot_normal(CSDPtr),
        call_site_dynamic_callees(Deep, Parallelism, ProcLabel, RevGoalPath,
            CSDPtr, ChildCliques)
    ;
        Slot = slot_multi(_, CSDPtrs),
        list.map(
            call_site_dynamic_callees(Deep, Parallelism, ProcLabel,
                RevGoalPath),
            to_list(CSDPtrs), ChildCliqueCords),
        ChildCliques = cord_list_to_cord(ChildCliqueCords)
    ).

:- pred call_site_dynamic_callees(deep::in, parallelism_amount::in,
    string_proc_label::in, reverse_goal_path::in, call_site_dynamic_ptr::in,
    cord(candidate_child_clique)::out) is det.

call_site_dynamic_callees(Deep, Parallelism, ProcLabel, RevGoalPath, CSDPtr,
        ChildCliques) :-
    ( valid_call_site_dynamic_ptr(Deep, CSDPtr) ->
        deep_lookup_clique_maybe_child(Deep, CSDPtr, MaybeClique),
        (
            MaybeClique = yes(CliquePtr),
            deep_lookup_csd_own(Deep, CSDPtr, Own),
            deep_lookup_csd_desc(Deep, CSDPtr, Desc),
            Cost = build_cs_cost_csq(calls(Own),
                float(callseqs(Own) + inherit_callseqs(Desc))),
            ChildCliques = singleton(
                candidate_child_clique(CliquePtr, Cost, ProcLabel, RevGoalPath,
                    Parallelism))
        ;
            MaybeClique = no,
            ChildCliques = empty
        )
    ;
        ChildCliques = empty
    ).

:- pred candidate_parallel_conjunctions_callee(
    candidate_par_conjunctions_params::in, deep::in,
    clique_ptr::in, candidate_par_conjunctions::in,
    candidate_child_clique::in, candidate_par_conjunctions::out,
    cord(message)::out) is det.

candidate_parallel_conjunctions_callee(Opts, Deep, CliquePtr, CliqueCandidates,
        !.Callee, Candidates, Messages) :-
    ( not_callee(CliquePtr, !.Callee) ->
        ( cost_threshold(Opts, !.Callee) ->
            update_parallelism_available(CliqueCandidates, !Callee),
            ( not exceeded_parallelism(Opts, !.Callee) ->
                AnalyzeChild = yes
            ;
                trace [compile_time(flag("debug_cpc_search")), io(!IO)] (
                    debug_cliques_exceeded_parallelism(!.Callee, !IO)
                ),
                AnalyzeChild = no
            )
        ;
            trace [compile_time(flag("debug_cpc_search")), io(!IO)] (
                debug_cliques_below_threshold(!.Callee, !IO)
            ),
            AnalyzeChild = no
        )
    ;
        AnalyzeChild = no
    ),

    (
        AnalyzeChild = yes,
        Parallelism = !.Callee ^ ccc_parallelism,
        ChildCliquePtr = !.Callee ^ ccc_clique,
        candidate_parallel_conjunctions_clique(Opts, Deep, Parallelism,
            ChildCliquePtr, Candidates, Messages)
    ;
        AnalyzeChild = no,
        Candidates = map.init,
        Messages = cord.empty
    ).

:- pred cost_threshold(candidate_par_conjunctions_params::in,
    candidate_child_clique::in) is semidet.

cost_threshold(Opts, ChildClique) :-
    Threshold = Opts ^ cpcp_clique_threshold,
    Cost = ChildClique ^ ccc_cs_cost,
    cs_cost_get_percall(Cost) >= float(Threshold).

:- pred not_callee(clique_ptr::in, candidate_child_clique::in) is semidet.

not_callee(CliquePtr, ChildClique) :-
    CliquePtr \= ChildClique ^ ccc_clique.

:- pred exceeded_parallelism(candidate_par_conjunctions_params::in,
    candidate_child_clique::in) is semidet.

exceeded_parallelism(Opts, ChildClique) :-
    Parallelism = ChildClique ^ ccc_parallelism,
    DesiredParallelism = Opts ^ cpcp_desired_parallelism,
    exceeded_desired_parallelism(DesiredParallelism, Parallelism).

:- pred update_parallelism_available(candidate_par_conjunctions::in,
    candidate_child_clique::in, candidate_child_clique::out) is det.

update_parallelism_available(CandidateConjunctions, !ChildClique) :-
    ProcLabel = !.ChildClique ^ ccc_proc,
    ( map.search(CandidateConjunctions, ProcLabel, ProcConjs) ->
        Conjs = ProcConjs ^ cpcp_par_conjs,
        list.foldl(update_parallelism_available_conj, Conjs, !ChildClique)
    ;
        true
    ).

:- pred update_parallelism_available_conj(candidate_par_conjunction(T)::in,
    candidate_child_clique::in, candidate_child_clique::out) is det.

update_parallelism_available_conj(Conj, !ChildClique) :-
    RevGoalPath = !.ChildClique ^ ccc_goal_path,
    rev_goal_path_from_string_det(Conj ^ cpc_goal_path, RevConjGoalPath),
    % XXX: This needs revisiting if we allow parallelised conjuncts to be
    % re-ordered.
    FirstConjunct = Conj ^ cpc_first_conj_num +
        list.length(Conj ^ cpc_goals_before),
    Length = list.foldl((func(seq_conj(ConjsI), Acc) = Acc + length(ConjsI)),
        Conj ^ cpc_conjs, 0),
    (
        RevGoalPath \= RevConjGoalPath,
        rev_goal_path_inside_relative(RevConjGoalPath, RevGoalPath,
            RevRelativePath),
        RevRelativePath = rgp(RevRelativePathSteps),
        list.last(RevRelativePathSteps, Step),
        Step = step_conj(ConjNum),
        ConjNum > FirstConjunct,
        ConjNum =< FirstConjunct + Length
    ->
        % The call into this clique gets parallelised by Conj.
        % XXX: If we knew the parallelisation type used for Conj we can do this
        % calculation more accurately.  For instance, if this is a loop, then
        % we use as many cores as the loop has iterations.  (Except for dead
        % time).
        Metrics = Conj ^ cpc_par_exec_metrics,
        CPUTime = parallel_exec_metrics_get_cpu_time(Metrics),
        DeadTime = Metrics ^ pem_first_conj_dead_time +
            Metrics ^ pem_future_dead_time,
        Efficiency = CPUTime / (CPUTime + DeadTime),
        Parallelism0 = !.ChildClique ^ ccc_parallelism,
        sub_computation_parallelism(Parallelism0, probable(Efficiency),
            Parallelism),
        !ChildClique ^ ccc_parallelism := Parallelism
    ;
        true
    ).

    % candidate_parallel_conjunctions_clique_proc(Opts, Deep,
    %   RecursionType, CliquePtr, PDPtr, Candidates, Messages).
    %
    % Find candidate parallel conjunctions within a proc dynamic.
    %
    % RecursionType: The type of recursion used in the current clique.
    %
    % CliquePtr: The current clique,
    %
:- pred candidate_parallel_conjunctions_clique_proc(
    candidate_par_conjunctions_params::in, deep::in, recursion_type::in,
    clique_ptr::in, proc_dynamic_ptr::in, candidate_par_conjunctions::out,
    cord(message)::out) is det.

candidate_parallel_conjunctions_clique_proc(Opts, Deep, RecursionType,
        CliquePtr, PDPtr, Candidates, Messages) :-
    some [!Messages]
    (
        !:Messages = cord.empty,

        % Determine the costs of the call sites in the procedure.
        recursion_type_get_interesting_parallelisation_depth(RecursionType,
            MaybeDepth),
        build_recursive_call_site_cost_map(Deep, CliquePtr, PDPtr,
            RecursionType, MaybeDepth, MaybeRecursiveCallSiteCostMap),
        (
            MaybeRecursiveCallSiteCostMap = ok(RecursiveCallSiteCostMap)
        ;
            MaybeRecursiveCallSiteCostMap = error(Error),
            append_message(pl_clique(CliquePtr),
                warning_cannot_compute_cost_of_recursive_calls(Error),
                !Messages),
            RecursiveCallSiteCostMap = map.init
        ),

        % Analyse this procedure for parallelism opportunities.
        candidate_parallel_conjunctions_proc(Opts, Deep, PDPtr, RecursionType,
            RecursiveCallSiteCostMap, Candidates, ProcMessages),
        !:Messages = !.Messages ++ ProcMessages,
        Messages = !.Messages
    ).

:- pred merge_candidate_par_conjs_proc(
    candidate_par_conjunctions_proc(T)::in,
    candidate_par_conjunctions_proc(T)::in,
    candidate_par_conjunctions_proc(T)::out) is det.

merge_candidate_par_conjs_proc(A, B, Result) :-
    A = candidate_par_conjunctions_proc(VarTableA, PushGoalsA, CPCsA),
    B = candidate_par_conjunctions_proc(VarTableB, PushGoalsB, CPCsB),
    Result = candidate_par_conjunctions_proc(VarTableA, PushGoals, CPCs),
    CPCs = CPCsA ++ CPCsB,
    % XXX Merge pushes here.
    PushGoals = PushGoalsA ++ PushGoalsB,
    ( VarTableA = VarTableB ->
        true
    ;
        unexpected($module, $pred, "var tables do not match")
    ).

%----------------------------------------------------------------------------%
%
% Search for paralleliation opportunities within a procedure.
%

    % Find candidate parallel conjunctions within the given procedure.
    %
:- pred candidate_parallel_conjunctions_proc(
    candidate_par_conjunctions_params::in, deep::in, proc_dynamic_ptr::in,
    recursion_type::in, map(reverse_goal_path, cs_cost_csq)::in,
    candidate_par_conjunctions::out, cord(message)::out) is det.

candidate_parallel_conjunctions_proc(Opts, Deep, PDPtr, RecursionType,
        RecursiveCallSiteCostMap, Candidates, !:Messages) :-
    !:Messages = cord.empty,

    % Lookup the proc static to find the ProcLabel.
    deep_lookup_proc_dynamics(Deep, PDPtr, PD),
    deep_lookup_proc_statics(Deep, PD ^ pd_proc_static, PS),
    ProcLabel = PS ^ ps_id,
    ( ProcLabel = str_ordinary_proc_label(_, ModuleName, _, _, _, _)
    ; ProcLabel = str_special_proc_label(_, ModuleName, _, _, _, _)
    ),

    (
        ( ModuleName = "Mercury runtime"
        ; ModuleName = "exception"
        )
    ->
        % Silently skip over any code from the runtime, since
        % we can't expect to find its procedure representation.
        Candidates = map.init
    ;
        deep_lookup_clique_index(Deep, PDPtr, CliquePtr),
        PSPtr = PD ^ pd_proc_static,
        deep_get_maybe_procrep(Deep, PSPtr, MaybeProcRep),
        (
            MaybeProcRep = ok(ProcRep),
            ProcDefnRep = ProcRep ^ pr_defn,
            Goal0 = ProcDefnRep ^ pdr_goal,
            VarTable = ProcDefnRep ^ pdr_var_table,

            % Label the goals with IDs,
            label_goals(LastGoalId, ContainingGoalMap, Goal0, Goal),

            % Gather call site information.
            proc_dynamic_paired_call_site_slots(Deep, PDPtr, Slots),
            list.foldl(build_dynamic_call_site_cost_and_callee_map(Deep),
                Slots, map.init, CallSitesMap),

            % Gather information about the procedure.
            deep_lookup_pd_own(Deep, PDPtr, Own),

            % Get coverage points.
            MaybeCoveragePointsArray = PD ^ pd_maybe_coverage_points,
            (
                MaybeCoveragePointsArray = yes(CoveragePointsArray),

                % Build coverage annotation.
                coverage_point_arrays_to_list(PS ^ ps_coverage_point_infos,
                    CoveragePointsArray, CoveragePoints),
                list.foldl2(add_coverage_point_to_map,
                    CoveragePoints, map.init, SolnsCoveragePointMap,
                    map.init, BranchCoveragePointMap),
                goal_annotate_with_coverage(ProcLabel, Goal, Own,
                    CallSitesMap, SolnsCoveragePointMap,
                    BranchCoveragePointMap, ContainingGoalMap, LastGoalId,
                    CoverageArray),

                % Build inst_map annotation.
                goal_annotate_with_instmap(Goal,
                    SeenDuplicateInstantiation, _ConsumedVars, _BoundVars,
                    initial_inst_map(ProcDefnRep), _FinalInstMap,
                    create_goal_id_array(LastGoalId), InstMapArray),

                deep_get_progrep_det(Deep, ProgRep),
                Info = implicit_parallelism_info(Deep, ProgRep, Opts,
                    CliquePtr, CallSitesMap, RecursiveCallSiteCostMap,
                    ContainingGoalMap, CoverageArray, InstMapArray,
                    RecursionType, VarTable, ProcLabel),
                goal_to_pard_goal(Info, [], Goal, PardGoal, !Messages),
                goal_get_conjunctions_worth_parallelising(Info,
                    [], PardGoal, _, CandidatesCord0, PushesCord, _Singles,
                    MessagesA),
                Candidates0 = cord.list(CandidatesCord0),
                Pushes = cord.list(PushesCord),
                !:Messages = !.Messages ++ MessagesA,
                (
                    SeenDuplicateInstantiation =
                        have_not_seen_duplicate_instantiation,
                    % XXX Merge pushes here.
                    CandidateProc = candidate_par_conjunctions_proc(VarTable,
                        Pushes, Candidates0),
                    svmap.set(ProcLabel, CandidateProc, map.init, Candidates)
                ;
                    SeenDuplicateInstantiation = seen_duplicate_instantiation,
                    Candidates = map.init,
                    append_message(pl_proc(ProcLabel),
                        notice_duplicate_instantiation(length(Candidates0)),
                        !Messages)
                )
            ;
                MaybeCoveragePointsArray = no,
                Candidates = map.init,
                append_message(pl_proc(ProcLabel),
                    error_cannot_lookup_coverage_points, !Messages)
            )
        ;
            MaybeProcRep = error(_),
            Candidates = map.init,
            append_message(pl_proc(ProcLabel),
                warning_cannot_lookup_proc_defn, !Messages)
        )
    ).

:- pred build_candidate_par_conjunction_maps(string_proc_label::in,
    var_table::in, candidate_par_conjunction(pard_goal_detail)::in,
    candidate_par_conjunctions::in, candidate_par_conjunctions::out) is det.

build_candidate_par_conjunction_maps(ProcLabel, VarTable, Candidate, !Map) :-
    % XXX: This predicate will also need to add pushes to CandidateProc.
    ( map.search(!.Map, ProcLabel, CandidateProc0) ->
        CandidateProc0 = candidate_par_conjunctions_proc(VarTablePrime,
            PushGoals, CPCs0),
        CPCs = [Candidate | CPCs0],
        ( VarTable = VarTablePrime ->
            true
        ;
            unexpected($module, $pred, "var tables do not match")
        )
    ;
        CPCs = [Candidate],
        PushGoals = []
    ),
    CandidateProc = candidate_par_conjunctions_proc(VarTable, PushGoals,
        CPCs),
    svmap.set(ProcLabel, CandidateProc, !Map).

:- pred goal_get_conjunctions_worth_parallelising(
    implicit_parallelism_info::in, list(goal_path_step)::in,
    pard_goal_detail::in, pard_goal_detail::out,
    cord(candidate_par_conjunction(pard_goal_detail))::out,
    cord(push_goal)::out, cord(pard_goal_detail)::out, cord(message)::out)
    is det.

goal_get_conjunctions_worth_parallelising(Info, RevGoalPathSteps,
        !Goal, Candidates, Pushes, Singles, Messages) :-
    % XXX: This predicate should return a list of costly goals within compund
    % goals that next to which it might be desirable to push later goals to
    % allow parallelisation within a branch of a compound goal.
    !.Goal = goal_rep(GoalExpr0, DetismRep, Annotation0),
    Coverage = Annotation0 ^ pgd_coverage,
    get_coverage_before_det(Coverage, Calls),
    (
        (
            GoalExpr0 = conj_rep(Conjs0),
            conj_get_conjunctions_worth_parallelising(Info, RevGoalPathSteps,
                Conjs0, Conjs, 1, [], SinglesSoFar, [], RevSingleCands,
                cord.empty, CandidatesBelow, cord.empty, PushesBelow,
                cord.empty, MessagesBelow),
            GoalExpr = conj_rep(Conjs),
            list.reverse(RevSingleCands, SingleCands),
            (
                SingleCands = [],
                Candidates = CandidatesBelow,
                PushesThisLevel = cord.empty,
                Singles = cord.from_list(SinglesSoFar),
                Messages = MessagesBelow,
                Cost = Annotation0 ^ pgd_cost
            ;
                SingleCands = [CostlyIndex - SinglesBefore],
                push_and_build_candidate_conjunctions(Info, RevGoalPathSteps,
                    Conjs, CostlyIndex, SinglesBefore,
                    MessagesThisLevel, CandidatesThisLevel, PushesThisLevel),
                Candidates = CandidatesBelow ++ CandidatesThisLevel,
                ( cord.is_empty(CandidatesThisLevel) ->
                    % No candidate was built, pass our singles to our caller.
                    Singles = cord.from_list(SinglesSoFar)
                ;
                    % Reset signals if we made a candidate our caller cannot
                    % use them.
                    Singles = cord.empty
                ),
                Messages = MessagesBelow ++ MessagesThisLevel,
                % XXX We should update the cost for CandidatesThisLevel.
                Cost = Annotation0 ^ pgd_cost
            ;
                SingleCands = [_, _ | _],
                assoc_list.keys(SingleCands, CostlyIndexes),
                build_candidate_conjunction(Info, RevGoalPathSteps,
                    Conjs, CostlyIndexes, MessagesThisLevel, MaybeCandidate),
                PushesThisLevel = cord.empty,
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
            ),
            Pushes = PushesBelow ++ PushesThisLevel
        ;
            GoalExpr0 = disj_rep(Disjs0),
            list.map_foldl5(
                disj_get_conjunctions_worth_parallelising(Info,
                    RevGoalPathSteps),
                Disjs0, Disjs, 1, _, cord.empty, Candidates,
                cord.empty, Pushes, cord.empty, Singles,
                cord.empty, Messages),
            disj_calc_cost(Disjs, Calls, Cost),
            GoalExpr = disj_rep(Disjs)
        ;
            GoalExpr0 = switch_rep(Var, CanFail, Cases0),
            list.map_foldl5(
                switch_case_get_conjunctions_worth_parallelising(Info,
                    RevGoalPathSteps),
                Cases0, Cases, 1, _, cord.empty, Candidates,
                cord.empty, Pushes, cord.empty, Singles,
                cord.empty, Messages),
            switch_calc_cost(Cases, Calls, Cost),
            GoalExpr = switch_rep(Var, CanFail, Cases)
        ;
            GoalExpr0 = ite_rep(Cond0, Then0, Else0),
            ite_get_conjunctions_worth_parallelising(Info, RevGoalPathSteps,
                Cond0, Cond, Then0, Then, Else0, Else,
                Candidates, Pushes, Singles, Messages),
            ite_calc_cost(Cond, Then, Else, Cost),
            GoalExpr = ite_rep(Cond, Then, Else)
        ;
            GoalExpr0 = scope_rep(SubGoal0, MaybeCut),
            RevSubGoalPathSteps = [step_scope(MaybeCut) | RevGoalPathSteps],
            goal_get_conjunctions_worth_parallelising(Info,
                RevSubGoalPathSteps, SubGoal0, SubGoal,
                Candidates, Pushes, Singles, Messages),
            Cost = SubGoal ^ goal_annotation ^ pgd_cost,
            GoalExpr = scope_rep(SubGoal, MaybeCut)
        ;
            GoalExpr0 = negation_rep(SubGoal0),
            RevSubGoalPathSteps = [step_neg | RevGoalPathSteps],
            % We ignore _Singles here because you cannot push goals
            % after a negation into the negation.
            goal_get_conjunctions_worth_parallelising(Info,
                RevSubGoalPathSteps, SubGoal0, SubGoal,
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
    implicit_parallelism_info::in, list(goal_path_step)::in,
    pard_goal_detail::in, pard_goal_detail::out,
    int::in, int::out,
    cord(candidate_par_conjunction(pard_goal_detail))::in,
    cord(candidate_par_conjunction(pard_goal_detail))::out,
    cord(push_goal)::in, cord(push_goal)::out,
    cord(pard_goal_detail)::in, cord(pard_goal_detail)::out,
    cord(message)::in, cord(message)::out) is det.

disj_get_conjunctions_worth_parallelising(Info, RevGoalPathSteps,
        !Disj, !DisjNum, !Candidates, !Pushes, !Singles, !Messages) :-
    RevDisjGoalPathSteps = [step_disj(!.DisjNum) | RevGoalPathSteps],
    goal_get_conjunctions_worth_parallelising(Info, RevDisjGoalPathSteps,
        !Disj, Candidates, Pushes, Singles, Messages),
    !:Candidates = !.Candidates ++ Candidates,
    !:Pushes = !.Pushes ++ Pushes,
    !:Singles = !.Singles ++ Singles,
    !:Messages = !.Messages ++ Messages,
    !:DisjNum = !.DisjNum + 1.

:- pred switch_case_get_conjunctions_worth_parallelising(
    implicit_parallelism_info::in, list(goal_path_step)::in,
    case_rep(pard_goal_detail_annotation)::in,
    case_rep(pard_goal_detail_annotation)::out,
    int::in, int::out,
    cord(candidate_par_conjunction(pard_goal_detail))::in,
    cord(candidate_par_conjunction(pard_goal_detail))::out,
    cord(push_goal)::in, cord(push_goal)::out,
    cord(pard_goal_detail)::in, cord(pard_goal_detail)::out,
    cord(message)::in, cord(message)::out) is det.

switch_case_get_conjunctions_worth_parallelising(Info, RevGoalPathSteps, !Case,
        !CaseNum, !Candidates, !Pushes, !Singles, !Messages) :-
    !.Case = case_rep(MainConsIdRep, OtherConsIdReps, Goal0),
    RevCaseGoalPathSteps = [step_switch(!.CaseNum, no) | RevGoalPathSteps],
    goal_get_conjunctions_worth_parallelising(Info, RevCaseGoalPathSteps,
        Goal0, Goal, Candidates, Pushes, Singles, Messages),
    !:Case = case_rep(MainConsIdRep, OtherConsIdReps, Goal),
    !:Candidates = !.Candidates ++ Candidates,
    !:Pushes = !.Pushes ++ Pushes,
    !:Singles = !.Singles ++ Singles,
    !:Messages = !.Messages ++ Messages,
    !:CaseNum = !.CaseNum + 1.

:- pred ite_get_conjunctions_worth_parallelising(
    implicit_parallelism_info::in,  list(goal_path_step)::in,
    pard_goal_detail::in, pard_goal_detail::out,
    pard_goal_detail::in, pard_goal_detail::out,
    pard_goal_detail::in, pard_goal_detail::out,
    cord(candidate_par_conjunction(pard_goal_detail))::out,
    cord(push_goal)::out, cord(pard_goal_detail)::out,
    cord(message)::out) is det.

ite_get_conjunctions_worth_parallelising(Info, RevGoalPathSteps,
        !Cond, !Then, !Else, Candidates, Pushes, Singles, Messages) :-
    RevCondGoalPathSteps = [step_ite_cond | RevGoalPathSteps],
    RevThenGoalPathSteps = [step_ite_then | RevGoalPathSteps],
    RevElseGoalPathSteps = [step_ite_else | RevGoalPathSteps],
    % We ignore _CondSingles here because you cannot push goals
    % following an if-then-else into the condition.
    goal_get_conjunctions_worth_parallelising(Info, RevCondGoalPathSteps,
        !Cond, CondCandidates, CondPushes, _CondSingles, CondMessages),
    goal_get_conjunctions_worth_parallelising(Info, RevThenGoalPathSteps,
        !Then, ThenCandidates, ThenPushes, ThenSingles, ThenMessages),
    goal_get_conjunctions_worth_parallelising(Info, RevElseGoalPathSteps,
        !Else, ElseCandidates, ElsePushes, ElseSingles, ElseMessages),
    Candidates = CondCandidates ++ ThenCandidates ++ ElseCandidates,
    Pushes = CondPushes ++ ThenPushes ++ ElsePushes,
    Singles = ThenSingles ++ ElseSingles,
    Messages = CondMessages ++ ThenMessages ++ ElseMessages.

:- pred conj_get_conjunctions_worth_parallelising(
    implicit_parallelism_info::in, list(goal_path_step)::in,
    list(pard_goal_detail)::in, list(pard_goal_detail)::out, int::in,
    list(pard_goal_detail)::in, list(pard_goal_detail)::out,
    assoc_list(int, list(pard_goal_detail))::in,
    assoc_list(int, list(pard_goal_detail))::out,
    cord(candidate_par_conjunction(pard_goal_detail))::in,
    cord(candidate_par_conjunction(pard_goal_detail))::out,
    cord(push_goal)::in, cord(push_goal)::out,
    cord(message)::in, cord(message)::out) is det.

conj_get_conjunctions_worth_parallelising(_Info, _RevGoalPathSteps,
        [], [], _ConjNum, !SinglesSoFar, !RevSingleCands,
        !CandidatesBelow, !Pushes, !MessagesBelow).
conj_get_conjunctions_worth_parallelising(Info, RevGoalPathSteps,
        [Conj0 | Conjs0], [Conj | Conjs], ConjNum, SinglesSoFar0, SinglesSoFar,
        !RevSingleCands, !CandidatesBelow, !Pushes, !MessagesBelow) :-
    RevConjGoalPathSteps = [step_conj(ConjNum) | RevGoalPathSteps],
    goal_get_conjunctions_worth_parallelising(Info, RevConjGoalPathSteps,
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
    conj_get_conjunctions_worth_parallelising(Info, RevGoalPathSteps,
        Conjs0, Conjs, ConjNum + 1, SinglesSoFar1, SinglesSoFar,
        !RevSingleCands, !CandidatesBelow, !Pushes, !MessagesBelow).

    % Given a conjunction with two or more costly goals (identified by
    % CostlyGoalsIndexes), check whether executing the conjunction in parallel
    % can yield a speedup.
    %
:- pred build_candidate_conjunction(implicit_parallelism_info::in,
    list(goal_path_step)::in, list(pard_goal_detail)::in, list(int)::in,
    cord(message)::out,
    maybe(candidate_par_conjunction(pard_goal_detail))::out) is det.

build_candidate_conjunction(Info, RevGoalPathSteps, Conjs, CostlyGoalsIndexes,
        !:Messages, MaybeCandidate) :-
    ProcLabel = Info ^ ipi_proc_label,
    !:Messages = cord.empty,
    NumCostlyGoals = list.length(CostlyGoalsIndexes),
    Location = pl_goal(ProcLabel, rgp(RevGoalPathSteps)),
    append_message(Location,
        info_found_conjs_above_callsite_threshold(NumCostlyGoals),
        !Messages),

    pardgoals_build_candidate_conjunction(Info, Location, RevGoalPathSteps,
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
    list(goal_path_step)::in, list(pard_goal_detail)::in, int::in,
    list(pard_goal_detail)::in, cord(message)::out,
    cord(candidate_par_conjunction(pard_goal_detail))::out,
    cord(push_goal)::out) is det.

push_and_build_candidate_conjunctions(_Info, _RevGoalPathSteps, _Conjs,
        _CostlyIndex, [], cord.empty, cord.empty, cord.empty).
push_and_build_candidate_conjunctions(Info, RevGoalPathSteps, Conjs,
        CostlyIndex, [Single | Singles], Messages, Candidates, Pushes) :-
    push_and_build_candidate_conjunction(Info, RevGoalPathSteps, Conjs,
        CostlyIndex, Single, HeadMessages, MaybeHeadCandidateAndPush),
    push_and_build_candidate_conjunctions(Info, RevGoalPathSteps, Conjs,
        CostlyIndex, Singles, TailMessages, TailCandidates, TailPushes),
    Messages = HeadMessages ++ TailMessages,
    (
        MaybeHeadCandidateAndPush = yes(HeadCandidate - HeadPush),
        Candidates = cord.cons(HeadCandidate, TailCandidates),
        Pushes = cord.cons(HeadPush, TailPushes)
    ;
        MaybeHeadCandidateAndPush = no,
        Candidates = TailCandidates,
        Pushes = TailPushes
    ).

:- pred push_and_build_candidate_conjunction(implicit_parallelism_info::in,
    list(goal_path_step)::in, list(pard_goal_detail)::in, int::in,
    pard_goal_detail::in, cord(message)::out,
    maybe(pair(candidate_par_conjunction(pard_goal_detail), push_goal))::out)
    is det.

push_and_build_candidate_conjunction(Info, RevGoalPathSteps, Conjs,
        CostlyIndex, Single, !:Messages, MaybeCandidateAndPush) :-
    SingleRevPath = Single ^ goal_annotation ^ pgd_original_path,
    SingleRevPath = rgp(SingleRevSteps),
    list.reverse(SingleRevSteps, SingleSteps),
    list.reverse(RevGoalPathSteps, GoalPathSteps),
    (
        goal_path_inside_relative(fgp(GoalPathSteps), fgp(SingleSteps),
            RelativePath),
        RelativePath = fgp(RelativeSteps),
        RelativeSteps = [step_conj(RelConjStep) | TailRelativeSteps],
        RelConjStep < CostlyIndex,
        list.take(CostlyIndex, Conjs, ConjsUptoCostly),
        list.drop(RelConjStep - 1, ConjsUptoCostly,
            [GoalToPushInto | GoalsToPush])
    ->
        RevPushGoalPathSteps = [step_conj(RelConjStep) | RevGoalPathSteps],
        push_goals_create_candidate(Info, RevPushGoalPathSteps,
            TailRelativeSteps, GoalToPushInto, GoalsToPush,
            RevCandidateGoalPathSteps, CandidateConjs),

        ProcLabel = Info ^ ipi_proc_label,
        !:Messages = cord.empty,
        % XXX Location is a lie
        Location = pl_goal(ProcLabel, rgp(RevGoalPathSteps)),
        append_message(Location,
            info_found_pushed_conjs_above_callsite_threshold,
            !Messages),

        (
            list.split_last(RelativeSteps, MostRelativeSteps,
                LastRelativeStep),
            LastRelativeStep = step_conj(_)
        ->
            % We push GoalsToPush into the existing conjunction
            % containing Single.
            PushGoalSteps = MostRelativeSteps
        ;
            % We push GoalsToPush next to Single in a newly created
            % conjunction.
            PushGoalSteps = RelativeSteps
        ),

        PushGoal = push_goal(rev_goal_path_to_string(rgp(RevGoalPathSteps)),
            RelConjStep + 1, CostlyIndex,
            [goal_path_to_string(fgp(PushGoalSteps))]),
        pardgoals_build_candidate_conjunction(Info, Location,
            RevCandidateGoalPathSteps, CandidateConjs, MaybeCandidate,
            !Messages),
        (
            MaybeCandidate = yes(Candidate),
            append_message(Location,
                info_found_n_conjunctions_with_positive_speedup(1),
                !Messages),
            MaybeCandidateAndPush = yes(Candidate - PushGoal)
        ;
            MaybeCandidate = no,
            MaybeCandidateAndPush = no
        )
    ;
        unexpected($module, $pred, "bad goal path for Single")
    ).

:- pred push_goals_create_candidate(implicit_parallelism_info::in,
    list(goal_path_step)::in, list(goal_path_step)::in,
    pard_goal_detail::in, list(pard_goal_detail)::in,
    list(goal_path_step)::out, list(pard_goal_detail)::out) is det.

push_goals_create_candidate(_Info, RevCurPathSteps,
        [], GoalToPushInto, GoalsToPush,
        RevCandidateGoalPathSteps, CandidateConjs) :-
    RevCandidateGoalPathSteps = RevCurPathSteps,
    CandidateConjs = [GoalToPushInto | GoalsToPush].
push_goals_create_candidate(Info, RevCurPathSteps,
        [HeadRelStep | TailRelSteps], GoalToPushInto, GoalsToPush,
        RevCandidateGoalPathSteps, CandidateConjs) :-
    GoalToPushInto = goal_rep(GoalExpr, _, _),
    (
        HeadRelStep = step_conj(N),
        ( GoalExpr = conj_rep(Goals) ->
            (
                TailRelSteps = [],
                % Conjoin GoalsToPush not with just the expensive goal,
                % but with the whole conjunction containing it.
                RevCandidateGoalPathSteps = RevCurPathSteps,
                CandidateConjs = Goals ++ GoalsToPush
            ;
                TailRelSteps = [_ | _],
                list.det_drop(N - 1, Goals, Tail),
                ( Tail = [SubGoal] ->
                    push_goals_create_candidate(Info,
                        [HeadRelStep | RevCurPathSteps],
                        TailRelSteps, SubGoal, GoalsToPush,
                        RevCandidateGoalPathSteps, CandidateConjs)
                ;
                    unexpected($module, $pred, "push into non-last conjunct")
                )
            )
        ;
            unexpected($module, $pred, "not conj")
        )
    ;
        HeadRelStep = step_disj(N),
        ( GoalExpr = disj_rep(Goals) ->
            list.index1_det(Goals, N, SubGoal),
            push_goals_create_candidate(Info, [HeadRelStep | RevCurPathSteps],
                TailRelSteps, SubGoal, GoalsToPush,
                RevCandidateGoalPathSteps, CandidateConjs)
        ;
            unexpected($module, $pred, "not disj")
        )
    ;
        HeadRelStep = step_switch(N, _),
        ( GoalExpr = switch_rep(_, _, Cases) ->
            list.index1_det(Cases, N, Case),
            Case = case_rep(_, _, SubGoal),
            push_goals_create_candidate(Info, [HeadRelStep | RevCurPathSteps],
                TailRelSteps, SubGoal, GoalsToPush,
                RevCandidateGoalPathSteps, CandidateConjs)
        ;
            unexpected($module, $pred, "not switch")
        )
    ;
        HeadRelStep = step_ite_then,
        ( GoalExpr = ite_rep(_, Then, _) ->
            push_goals_create_candidate(Info, [HeadRelStep | RevCurPathSteps],
                TailRelSteps, Then, GoalsToPush,
                RevCandidateGoalPathSteps, CandidateConjs)
        ;
            unexpected($module, $pred, "not ite_then")
        )
    ;
        HeadRelStep = step_ite_else,
        ( GoalExpr = ite_rep(_, _, Else) ->
            push_goals_create_candidate(Info, [HeadRelStep | RevCurPathSteps],
                TailRelSteps, Else, GoalsToPush,
                RevCandidateGoalPathSteps, CandidateConjs)
        ;
            unexpected($module, $pred, "not ite_else")
        )
    ;
        HeadRelStep = step_ite_cond,
        % We cannot push into a condition.
        unexpected($module, $pred, "ite_cond")
    ;
        HeadRelStep = step_neg,
        % We cannot push into a negated goal.
        unexpected($module, $pred, "neg")
    ;
        HeadRelStep = step_scope(_),
        ( GoalExpr = scope_rep(SubGoal, _) ->
            push_goals_create_candidate(Info, [HeadRelStep | RevCurPathSteps],
                TailRelSteps, SubGoal, GoalsToPush,
                RevCandidateGoalPathSteps, CandidateConjs)
        ;
            unexpected($module, $pred, "not scope")
        )
    ;
        HeadRelStep = step_lambda,
        % These should not exist in a profiled program.
        unexpected($module, $pred, "lambda")
    ;
        HeadRelStep = step_try,
        % These should not exist in a profiled program.
        unexpected($module, $pred, "try")
    ;
        HeadRelStep = step_atomic_main,
        % These should not exist in a profiled program.
        unexpected($module, $pred, "atomic_main")
    ;
        HeadRelStep = step_atomic_orelse(_),
        % These should not exist in a profiled program.
        unexpected($module, $pred, "atomic_orelse")
    ).

:- pred pardgoals_build_candidate_conjunction(implicit_parallelism_info::in,
    program_location::in, list(goal_path_step)::in, list(pard_goal_detail)::in,
    maybe(candidate_par_conjunction(pard_goal_detail))::out,
    cord(message)::in, cord(message)::out) is det.

pardgoals_build_candidate_conjunction(Info, Location, RevGoalPathSteps,
        Goals, MaybeCandidate, !Messages) :-
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
        BestParallelisation = bp_parallel_execution(GoalsBefore, ParConjs,
            GoalsAfter, IsDependent, Metrics),
        Speedup = parallel_exec_metrics_get_speedup(Metrics),
        Calls = Metrics ^ pem_num_calls,
        conj_calc_cost(GoalsBefore, Calls, GoalsBeforeCost0),
        GoalsBeforeCost = goal_cost_get_percall(GoalsBeforeCost0),
        conj_calc_cost(GoalsAfter, Calls, GoalsAfterCost0),
        GoalsAfterCost = goal_cost_get_percall(GoalsAfterCost0),
        RevGoalPathString = rev_goal_path_to_string(rgp(RevGoalPathSteps)),
        Candidate = candidate_par_conjunction(RevGoalPathString,
            FirstConjNum, IsDependent, GoalsBefore, GoalsBeforeCost, ParConjs,
            GoalsAfter, GoalsAfterCost, Metrics),
        (
            Speedup > 1.0,
            (
                ParalleliseDepConjs = do_not_parallelise_dep_conjs
            =>
                IsDependent = conjuncts_are_independent
            )
        ->
            MaybeCandidate = yes(Candidate)
        ;
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
                    unexpected($module, $pred, "location is a clique")
                ;
                    Location = pl_csd(_),
                    unexpected($module, $pred, "location is a csd")
                ),

                convert_candidate_par_conjunction(
                    pard_goal_detail_to_pard_goal, Candidate, FBCandidate),
                VarTable = Info ^ ipi_var_table,
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

:- type best_parallelisation
    --->    bp_parallel_execution(
                bp_goals_before         :: list(pard_goal_detail),
                bp_par_conjs            :: list(seq_conj(pard_goal_detail)),
                bp_goals_after          :: list(pard_goal_detail),
                bp_is_dependent         :: conjuncts_are_dependent,
                bp_par_exec_metrics     :: parallel_exec_metrics
            ).

:- pred find_best_parallelisation(implicit_parallelism_info::in,
    program_location::in, list(pard_goal_detail)::in,
    maybe(best_parallelisation)::out,
    cord(message)::in, cord(message)::out) is det.

find_best_parallelisation(Info, Location, Goals, MaybeBestParallelisation,
        !Messages) :-
    % Decide which algorithm to use.
    ConjunctionSize = length(Goals),
    choose_algorithm(Info, ConjunctionSize, Algorithm),
    preprocess_conjunction(Goals, MaybePreprocessedGoals, Location, !Messages),
    (
        MaybePreprocessedGoals = yes(PreprocessedGoals),
        find_best_parallelisation_complete_bnb(Info, Location,
            Algorithm, PreprocessedGoals, MaybeBestParallelisation)
    ;
        MaybePreprocessedGoals = no,
        MaybeBestParallelisation = no
    ).

:- type best_par_algorithm_simple
    --->    bpas_complete(maybe(int))
    ;       bpas_greedy.

:- pred choose_algorithm(implicit_parallelism_info::in,
    int::in, best_par_algorithm_simple::out) is det.

choose_algorithm(Info, ConjunctionSize, Algorithm) :-
    Algorithm0 = Info ^ ipi_opts ^ cpcp_best_par_alg,
    (
        (
            Algorithm0 = bpa_complete_branches(BranchesLimit),
            MaybeBranchesLimit = yes(BranchesLimit)
        ;
            Algorithm0 = bpa_complete,
            MaybeBranchesLimit = no
        ),
        Algorithm = bpas_complete(MaybeBranchesLimit)
    ;
        Algorithm0 = bpa_complete_size(SizeLimit),
        ( SizeLimit < ConjunctionSize ->
            Algorithm = bpas_greedy
        ;
            Algorithm = bpas_complete(no)
        )
    ;
        Algorithm0 = bpa_greedy,
        Algorithm = bpas_greedy
    ).

:- type goal_group(T)
    --->    gg_single(
                ggs_index               :: int,
                ggs_abstract            :: T
            )
    ;       gg_multiple(
                ggm_index               :: int,
                ggm_count               :: int,
                ggm_abstract            :: T
            ).

:- pred gg_get_details(goal_group(T)::in, int::out, int::out, T::out) is det.

gg_get_details(gg_single(Index, P), Index, 1, P).
gg_get_details(gg_multiple(Index, Num, P), Index, Num, P).

% NOTE: These commented out types are relevant for some work that
% hasn't yet been done. They will either be used or removed in a future change.

%:- type goal_placement
%    --->    goal_placement(
%                gp_can_split_group          :: can_split_group,
%                gp_placement_choices        :: set(goal_placement_enum)
%            ).

    % The valid placement decisions that can be made for a given goal.
    %
    % Note that this type is not as expressive as it could be, this is
    % deliberate to avoid symmetry.  It is however still complete.
    %
    %  + place_independent is just gpe_place_in_new_conj with the next goal
    %    having gpe_place_in_new_conj.
    %
    %  + place_with_next is just gpe_place_in_new_conj with the next goal
    %    having place_with_previous.
    %
%:- type goal_placement_enum
%    --->    gpe_place_before_par_conj
%    ;       gpe_place_after_par_conj
%    ;       gpe_place_with_previous
%    ;       gpe_place_in_new_conj.
%
%:- type can_split_group
%    --->    can_split_group
%    ;       cannot_split_group.

:- type goal_classification
    --->    gc_cheap_goals
    ;       gc_costly_goals.

    % A summerisation of goals before parallelistion.
    %
    % All indexes are 0-based except where otherwise noded.
    %
:- type goals_for_parallelisation
    --->    goals_for_parallelisation(
                gfp_goals                   :: array(pard_goal_detail),

                gfp_first_costly_goal       :: int,
                gfp_last_costly_goal        :: int,

                gfp_groups                  ::
                    list(goal_group(goal_classification)),

                % The indexes in the dependency graph are 1-based.
                gfp_dependency_graphs       :: dependency_graphs,

                gfp_costly_goal_indexes     :: list(int),
                gfp_num_calls               :: int
            ).

:- inst goals_for_parallelisation
    --->    goals_for_parallelisation(
                ground, ground, ground,
                non_empty_list,
                ground,
                non_empty_list,
                ground
            ).

:- pred preprocess_conjunction(list(pard_goal_detail)::in,
    maybe(goals_for_parallelisation)::out(maybe(goals_for_parallelisation)),
    program_location::in, cord(message)::in, cord(message)::out) is det.

preprocess_conjunction(Goals, MaybeGoalsForParallelisation, Location,
        !Messages) :-
    GoalsArray = array.from_list(Goals),

    % Phase 1: Build a dependency map.
    build_dependency_graphs(Goals, DependencyGraphs),

    % Phase 2: Find the costly calls.
    identify_costly_goals(Goals, 0, CostlyGoalsIndexes),
    (
        CostlyGoalsIndexes = [FirstCostlyGoalIndexPrime | OtherIndexes],
        last(OtherIndexes, LastCostlyGoalIndexPrime)
    ->
        FirstCostlyGoalIndex = FirstCostlyGoalIndexPrime,
        LastCostlyGoalIndex = LastCostlyGoalIndexPrime
    ;
        unexpected($module, $pred, "too few costly goals")
    ),

    % Phase 3: Check that all the middle goals are model det.
    foldl_sub_array(goal_accumulate_detism, GoalsArray,
        FirstCostlyGoalIndex, LastCostlyGoalIndex, det_rep, Detism),
    (
        ( Detism = det_rep
        ; Detism = cc_multidet_rep
        ),

        % Phase 3: Process the middle section into groups.
        foldl_sub_array(preprocess_conjunction_into_groups, GoalsArray,
            FirstCostlyGoalIndex, LastCostlyGoalIndex, [], RevGoalGroups),
        reverse(RevGoalGroups, GoalGroups),

        FirstCostlyGoal = lookup(GoalsArray, FirstCostlyGoalIndex),
        Cost = FirstCostlyGoal ^ goal_annotation ^ pgd_cost,
        NumCalls = goal_cost_get_calls(Cost),

        GoalsForParallelisation = goals_for_parallelisation(GoalsArray,
            FirstCostlyGoalIndex, LastCostlyGoalIndex, GoalGroups,
            DependencyGraphs, CostlyGoalsIndexes, NumCalls),
        MaybeGoalsForParallelisation = yes(GoalsForParallelisation)
    ;
        ( Detism = semidet_rep
        ; Detism = multidet_rep
        ; Detism = nondet_rep
        ; Detism = erroneous_rep
        ; Detism = failure_rep
        ; Detism = cc_nondet_rep
        ),
        MaybeGoalsForParallelisation = no,
        append_message(Location, notice_candidate_conjunction_not_det(Detism),
            !Messages)
    ).

:- pred foldl_sub_array(pred(int, T, A, A), array(T), int, int, A, A).
:- mode foldl_sub_array(pred(in, in, in, out) is det,
    in, in, in, in, out) is det.

foldl_sub_array(Pred, Array, Index, Last, !A) :-
    ( Index =< Last ->
        X = lookup(Array, Index),
        Pred(Index, X, !A),
        foldl_sub_array(Pred, Array, Index + 1, Last, !A)
    ;
        true
    ).

:- pred goal_accumulate_detism(int::in, pard_goal_detail::in,
    detism_rep::in, detism_rep::out) is det.

goal_accumulate_detism(_, Goal, !Detism) :-
    detism_components(!.Detism, Solutions0, CanFail0),
    detism_committed_choice(!.Detism, CommittedChoice0),
    Detism = Goal ^ goal_detism_rep,
    detism_components(Detism, GoalSolutions, GoalCanFail),
    detism_committed_choice(Detism, GoalCommittedChoice),
    (
        GoalSolutions = at_most_zero_rep,
        Solutions = at_most_zero_rep
    ;
        GoalSolutions = at_most_one_rep,
        Solutions = Solutions0
    ;
        GoalSolutions = at_most_many_rep,
        (
            Solutions0 = at_most_zero_rep,
            Solutions = at_most_zero_rep
        ;
            ( Solutions0 = at_most_one_rep
            ; Solutions0 = at_most_many_rep
            ),
            Solutions = GoalSolutions
        )
    ),
    (
        GoalCanFail = can_fail_rep,
        CanFail = can_fail_rep
    ;
        GoalCanFail = cannot_fail_rep,
        CanFail = CanFail0
    ),
    (
        GoalCommittedChoice = committed_choice,
        CommittedChoice = CommittedChoice0
    ;
        GoalCommittedChoice = not_committed_cnoice,
        CommittedChoice = not_committed_cnoice
    ),
    (
        promise_equivalent_solutions [FinalDetism] (
            detism_components(FinalDetism, Solutions, CanFail),
            detism_committed_choice(FinalDetism, CommittedChoice)
        )
    ->
        !:Detism = FinalDetism
    ;
        unexpected($module, $pred, "cannot compute detism from components.")
    ).

    % foldl(preprocess_conjunction_into_groups, Goals, FirstCostlyGoalIndex,
    %   LastCostlyGoalIndex, RevGoalGroups)).
    %
    % GoalGroups are Goals divided into groups of single costly goals, Larger
    % groups are not currently used.  Only the goals within the range of costly
    % goals inclusive are analysed.
    %
:- pred preprocess_conjunction_into_groups(int::in, pard_goal_detail::in,
    list(goal_group(goal_classification))::in,
    list(goal_group(goal_classification))::out) is det.

preprocess_conjunction_into_groups(Index, Goal, !RevGoalGroups) :-
    identify_costly_goal(Goal ^ goal_annotation, Costly),
    (
        Costly = is_costly_goal,
        GoalClassification = gc_costly_goals
    ;
        Costly = is_not_costly_goal,
        GoalClassification = gc_cheap_goals
    ),
    % XXX Goal grouping is not yet implemented/tested.
    GoalGroup = gg_single(Index, GoalClassification),
    !:RevGoalGroups = [GoalGroup | !.RevGoalGroups].

%----------------------------------------------------------------------------%

    % Find the best parallelisation using the branch and bound algorithm.
    %
:- pred find_best_parallelisation_complete_bnb(implicit_parallelism_info::in,
    program_location::in, best_par_algorithm_simple::in,
    goals_for_parallelisation::in, maybe(best_parallelisation)::out) is det.

find_best_parallelisation_complete_bnb(Info, Location, Algorithm,
        PreprocessedGoals, MaybeBestParallelisation) :-
    trace [compile_time(flag("debug_branch_and_bound")), io(!IO)] (
        io.format("D: Find best parallelisation for:\n%s\n",
            [s(LocationString)], !IO),
        location_to_string(1, Location, LocationCord),
        string.append_list(list(LocationCord), LocationString),
        NumGoals = size(PreprocessedGoals ^ gfp_goals),
        NumGoalsBefore = PreprocessedGoals ^ gfp_first_costly_goal,
        NumGoalsAfter = NumGoals - PreprocessedGoals ^ gfp_last_costly_goal - 1,
        NumGoalsMiddle = NumGoals - NumGoalsBefore - NumGoalsAfter,
        io.format("D: Problem size (before, middle, after): %d,%d,%d\n",
            [i(NumGoalsBefore), i(NumGoalsMiddle), i(NumGoalsAfter)], !IO),
        io.flush_output(!IO)
    ),

    branch_and_bound(
        generate_parallelisations(Info, Algorithm, PreprocessedGoals),
        parallelisation_get_objective_value,
        Solutions, Profile),

    trace [compile_time(flag("debug_branch_and_bound")), io(!IO)] (
        io.format("D: Solutions: %d\n",
            [i(set.count(Solutions))], !IO),
        io.format("D: Branch and bound profile: %s\n\n",
            [s(string(Profile))], !IO),
        io.flush_output(!IO)
    ),

    ( set.remove_least(Solutions, BestParallelisation, _) ->
        MaybeBestParallelisation = yes(BestParallelisation)
    ;
        % Solutions is empty.
        ParalleliseDepConjs = Info ^ ipi_opts ^ cpcp_parallelise_dep_conjs,
        (
            ( ParalleliseDepConjs = parallelise_dep_conjs_overlap
            ; ParalleliseDepConjs = parallelise_dep_conjs_naive
            ; ParalleliseDepConjs = parallelise_dep_conjs_num_vars
            ),
            MaybeBestParallelisation = no
        ;
            ParalleliseDepConjs = do_not_parallelise_dep_conjs,
            % Try again to get the best dependent parallelisation.
            % This is used for guided parallelisation.
            TempInfo = Info ^ ipi_opts ^ cpcp_parallelise_dep_conjs :=
                parallelise_dep_conjs_overlap,
            find_best_parallelisation_complete_bnb(TempInfo, Location,
                Algorithm, PreprocessedGoals, MaybeBestParallelisation)
        )
    ).

    % The objective function for the branch and bound search.
    % This is ParTime + ParOverheads * 2.  That is we are willing to pay
    % 1 unit of parallel overheads to get a 2 unit improvement
    % of parallel execution time.
    %
:- func parallelisation_get_objective_value(best_parallelisation) = float.

parallelisation_get_objective_value(Parallelisation) = Value :-
    Metrics = Parallelisation ^ bp_par_exec_metrics,
    Value = Metrics ^ pem_par_time + Metrics ^ pem_par_overheads * 2.0.

:- impure pred generate_parallelisations(implicit_parallelism_info::in,
    best_par_algorithm_simple::in, goals_for_parallelisation::in,
    bnb_state(best_parallelisation)::in, best_parallelisation::out) is nondet.

generate_parallelisations(Info, Algorithm, GoalsForParallelisation,
        BNBState, BestParallelisation) :-
    some [!Parallelisation, !GoalGroups] (
        start_building_parallelisation(Info, GoalsForParallelisation,
            !:Parallelisation),

        !:GoalGroups = GoalsForParallelisation ^ gfp_groups,
        start_first_par_conjunct(!GoalGroups, !Parallelisation),
        impure generate_parallelisations_body(Info, BNBState, Algorithm,
            !.GoalGroups, !Parallelisation),

        ( semipure should_expand_search(BNBState, Algorithm) ->
            % Try to push goals into the first and last parallel conjuncts
            % from outside the parallel conjunction.
            semipure add_goals_into_first_par_conj(BNBState, !Parallelisation),
            semipure add_goals_into_last_par_conj(BNBState, !Parallelisation)
        ;
            true
        ),

        finalise_parallelisation(!.Parallelisation, BestParallelisation)
    ),
    semipure test_incomplete_solution(BNBState, BestParallelisation).

:- semipure pred add_goals_into_first_par_conj(
    bnb_state(best_parallelisation)::in,
    incomplete_parallelisation::in, incomplete_parallelisation::out) is multi.

add_goals_into_first_par_conj(BNBState, !Parallelisation) :-
    FirstGoal0 = !.Parallelisation ^ ip_first_par_goal,
    (
        FirstGoal0 > 0,
        Goals = !.Parallelisation ^ ip_goals,
        Goal = lookup(Goals, FirstGoal0 - 1),
        can_parallelise_goal(Goal),

        % There are goals before the parallel conjunction that can be included
        % in the parallel conjunction.
        add_one_goal_into_first_par_conj(!Parallelisation),
        semipure test_parallelisation(BNBState, !Parallelisation),
        semipure add_goals_into_first_par_conj(BNBState, !Parallelisation)
    ;
        true
    ).

:- semipure pred add_goals_into_last_par_conj(
    bnb_state(best_parallelisation)::in,
    incomplete_parallelisation::in, incomplete_parallelisation::out) is multi.

add_goals_into_last_par_conj(BNBState, !Parallelisation) :-
    NumGoals = ip_get_num_goals(!.Parallelisation),
    LastParGoal = !.Parallelisation ^ ip_last_par_goal,
    (
        LastParGoal < NumGoals - 1,
        Goals = !.Parallelisation ^ ip_goals,
        Goal = lookup(Goals, LastParGoal + 1),
        can_parallelise_goal(Goal),

        % Try to move a goal from after the parallelisation into the
        % parallelisation.
        add_one_goal_into_last_par_conj(!Parallelisation),
        semipure test_parallelisation(BNBState, !Parallelisation),
        semipure add_goals_into_last_par_conj(BNBState, !Parallelisation)
    ;
        true
    ).

    % Set the last scheduled goal to the goal at the end of the first group,
    % popping the first group off the list. This initialises the
    % parallelisation with the first goal group occurring first in the first
    % parallel conjunction.
    %
    % This is done outside of the loop below since the first goal group will
    % always be added to the first (initially empty) parallel conjunction.
    %
:- pred start_first_par_conjunct(
    list(goal_group(T))::in, list(goal_group(T))::out,
    incomplete_parallelisation::in, incomplete_parallelisation::out) is det.

start_first_par_conjunct(!GoalGroups, !Parallelisation) :-
    (
        !.GoalGroups = [],
        unexpected($module, $pred, "no goal groups")
    ;
        !.GoalGroups = [Group | !:GoalGroups],
        gg_get_details(Group, Index, Num, _),
        LastScheduledGoal = Index + Num - 1,
        !Parallelisation ^ ip_last_scheduled_goal := LastScheduledGoal
    ).

:- impure pred generate_parallelisations_body(implicit_parallelism_info::in,
    bnb_state(best_parallelisation)::in, best_par_algorithm_simple::in,
    list(goal_group(goal_classification))::in,
    incomplete_parallelisation::in, incomplete_parallelisation::out) is nondet.

generate_parallelisations_body(_, _, _, [], !Parallelisation) :-
    % Verify that we've generated at least two parallel conjuncts.
    ip_get_num_parallel_conjuncts(!.Parallelisation) >= 2.
generate_parallelisations_body(Info, BNBState, Algorithm,
        [GoalGroup | GoalGroups], !Parallelisation) :-
    LastScheduledGoal0 = !.Parallelisation ^ ip_last_scheduled_goal,
    gg_get_details(GoalGroup, _Index, Num, _Classification),

    LastScheduledGoal = LastScheduledGoal0 + Num,
    some [!AddToLastParallelisation, !AddToNewParallelisation] (
        !:AddToLastParallelisation = !.Parallelisation,
        !:AddToNewParallelisation = !.Parallelisation,

        % Consider adding this goal to the last parallel conjunct.
        !AddToLastParallelisation ^ ip_last_scheduled_goal
            := LastScheduledGoal,
        score_parallelisation(BNBState, MaybeAddToLastScore,
            !AddToLastParallelisation),

        % Consider putting this goal into a new parallel conjunct.
        ParConjsRevLastGoal0 = !.Parallelisation ^ ip_par_conjs_rev_last_goal,
        ParConjsRevLastGoal = [LastScheduledGoal0 | ParConjsRevLastGoal0],
        !AddToNewParallelisation ^ ip_par_conjs_rev_last_goal :=
            ParConjsRevLastGoal,
        !AddToNewParallelisation ^ ip_last_scheduled_goal := LastScheduledGoal,
        score_parallelisation(BNBState, MaybeAddToNewScore,
            !AddToNewParallelisation),

        (
            MaybeAddToLastScore = yes(AddToLastScore),
            (
                MaybeAddToNewScore = yes(AddToNewScore),
                (
                    % Smaller scores are better.
                    AddToNewScore > AddToLastScore
                ->
                    % Adding to the last parallel conjunct is better.
                    BestOption = !.AddToLastParallelisation,
                    MaybeSndBestOption = yes(!.AddToNewParallelisation)
                ;
                    % Adding to a new parallel conjunct is better.
                    BestOption = !.AddToNewParallelisation,
                    MaybeSndBestOption = yes(!.AddToLastParallelisation)
                )
            ;
                MaybeAddToNewScore = no,
                % Adding to the last parallel conjunct is the only option.
                BestOption = !.AddToLastParallelisation,
                MaybeSndBestOption = no
            )
        ;
            MaybeAddToLastScore = no,
            % Adding to a new parallel conjunct is the only option.
            BestOption = !.AddToNewParallelisation,
            MaybeSndBestOption = no
        )
    ),

    (
        MaybeSndBestOption = no,
        !:Parallelisation = BestOption
    ;
        MaybeSndBestOption = yes(SndBestOption),
        (
            % Should an alternative branch be created here?
            semipure should_expand_search(BNBState, Algorithm)
        ->
            % Create a branch.
            impure add_alternative(BNBState),
            % This tries the leftmost disjunct first, so try the best option
            % there.
            (
                !:Parallelisation = BestOption
            ;
                impure close_alternative(BNBState),
                !:Parallelisation = SndBestOption
            )
        ;
            !:Parallelisation = BestOption
        )
    ),

    semipure test_parallelisation(BNBState, !Parallelisation),

    impure generate_parallelisations_body(Info, BNBState, Algorithm,
        GoalGroups, !Parallelisation).

    % True if we should expand the search for parallelisation alternatives by
    % creating a choice point.
    %
:- semipure pred should_expand_search(bnb_state(T)::in,
    best_par_algorithm_simple::in) is semidet.

should_expand_search(BNBState, Algorithm) :-
    Algorithm = bpas_complete(MaybeLimit),
    (
        MaybeLimit = yes(Limit),
        semipure num_alternatives(BNBState, Open, Closed),
        Open + Closed < Limit
    ;
        MaybeLimit = no
    ).

    % Test the parallelisation against the best one known to the branch and
    % bound solver.
    %
:- semipure pred test_parallelisation(bnb_state(best_parallelisation)::in,
    incomplete_parallelisation::in, incomplete_parallelisation::out)
    is semidet.

test_parallelisation(BNBState, !Parallelisation) :-
    calculate_parallel_cost(CostData, !Parallelisation),
    Info = !.Parallelisation ^ ip_info,
    test_dependance(Info, CostData),
    % XXX: We shouldn't need to finalize the parallelisation before testing it.
    % This is a limitation of the branch and bound module.
    finalise_parallelisation(!.Parallelisation, TestParallelisation),
    semipure test_incomplete_solution(BNBState, TestParallelisation).

    % Score the parallelisation.
    %
:- pred score_parallelisation(bnb_state(best_parallelisation)::in,
    maybe(float)::out,
    incomplete_parallelisation::in, incomplete_parallelisation::out) is det.

score_parallelisation(BNBState, MaybeScore, !Parallelisation) :-
    calculate_parallel_cost(CostData, !Parallelisation),
    Info = !.Parallelisation ^ ip_info,
    ( test_dependance(Info, CostData) ->
        finalise_parallelisation(!.Parallelisation, TestParallelisation),
        score_solution(BNBState, TestParallelisation, Score),
        MaybeScore = yes(Score)
    ;
        MaybeScore = no
    ).

    % Test that the parallelisation only included dependant parallelism
    % if permitted by the user.
    %
:- pred test_dependance(implicit_parallelism_info::in,
    parallelisation_cost_data::in) is semidet.

test_dependance(Info, CostData) :-
    Overlap = CostData ^ pcd_par_exec_overlap,
    ParalleliseDepConjs = Info ^ ipi_opts ^ cpcp_parallelise_dep_conjs,
    par_conj_overlap_is_dependent(Overlap, IsDependent),
    (
        ParalleliseDepConjs = do_not_parallelise_dep_conjs
    =>
        IsDependent = conjuncts_are_independent
    ).

%----------------------------------------------------------------------------%

:- type incomplete_parallelisation
    --->    incomplete_parallelisation(
                ip_info                     :: implicit_parallelism_info,

                ip_goals                    :: array(pard_goal_detail),

                % The index of the first goal in the parallelised goals,
                % This is also the number of goals executed in sequence before
                % the parallel conjunction.
                ip_first_par_goal           :: int,

                % The index of the last goal in the parallel conjunction.
                ip_last_par_goal            :: int,

                % The index of the last goal that has been (tentatively)
                % scheduled.  All goals between this +1 and ip_last_par_goal
                % have not been scheduled.
                ip_last_scheduled_goal      :: int,

                % The index of the last goal in each of the parallel conjuncts.
                % the very last parallel conjunct is donated by
                % ip_last_par_goal.
                ip_par_conjs_rev_last_goal  :: list(int),

                % The number of calls into this conjunction.
                ip_num_calls                :: int,

                % Dependency relationships between goals.
                ip_dependency_graphs        :: dependency_graphs,

                % These are implied by the above fields but we maintain them
                % here to provide a cache.
                ip_maybe_goals_before_cost  :: maybe(goal_cost_csq),
                ip_maybe_goals_after_cost   :: maybe(goal_cost_csq),

                ip_maybe_par_cost_data      :: maybe(parallelisation_cost_data)
            ).

:- type parallelisation_cost_data
    ---> parallelisation_cost_data(
                pcd_par_exec_overlap    :: parallel_execution_overlap,
                pcd_par_exec_metrics    :: parallel_exec_metrics_incomplete,
                pcd_productions_map     :: map(var_rep, float)
        ).

:- func ip_get_goals_before(incomplete_parallelisation) =
    list(pard_goal_detail).

ip_get_goals_before(Parallelisation) = GoalsBefore :-
    Goals = Parallelisation ^ ip_goals,
    FirstParGoalIndex = Parallelisation ^ ip_first_par_goal,
    fetch_items(Goals, 0, FirstParGoalIndex - 1, GoalsBefore).

:- func ip_get_goals_after(incomplete_parallelisation) =
    list(pard_goal_detail).

ip_get_goals_after(Parallelisation) = GoalsAfter :-
    Goals = Parallelisation ^ ip_goals,
    LastParGoalIndex = Parallelisation ^ ip_last_par_goal,
    NumGoals = size(Goals),
    fetch_items(Goals, LastParGoalIndex + 1, NumGoals - 1, GoalsAfter).

:- func ip_get_par_conjs(incomplete_parallelisation) =
    list(seq_conj(pard_goal_detail)).

ip_get_par_conjs(Incomplete) = ParConjs :-
    Goals = Incomplete ^ ip_goals,
    Start = Incomplete ^ ip_first_par_goal,
    Last = Incomplete ^ ip_last_scheduled_goal,
    LastGoalsRev0 = Incomplete ^ ip_par_conjs_rev_last_goal,
    LastGoalsRev = [Last | LastGoalsRev0],
    reverse(LastGoalsRev, LastGoals),
    ip_get_par_conjs2(Goals, Start, LastGoals, ParConjs).

:- pred ip_get_par_conjs2(array(pard_goal_detail)::in, int::in,
    list(int)::in, list(seq_conj(pard_goal_detail))::out) is det.

ip_get_par_conjs2(_, _, [], []).
ip_get_par_conjs2(Array, First, [Last | Lasts], [Conj | Conjs]) :-
    ip_get_par_conjs2(Array, Last + 1, Lasts, Conjs),
    fetch_items(Array, First, Last, Goals),
    Conj = seq_conj(Goals).

:- func ip_get_num_goals(incomplete_parallelisation) = int.

ip_get_num_goals(Incomplete) = size(Incomplete ^ ip_goals).

:- func ip_get_num_parallel_conjuncts(incomplete_parallelisation) = int.

ip_get_num_parallel_conjuncts(Incomplete) =
    length(Incomplete ^ ip_par_conjs_rev_last_goal) + 1.

:- func ip_get_num_goals_middle(incomplete_parallelisation) = int.

ip_get_num_goals_middle(Incomplete) = LastParGoal - FirstParGoal + 1 :-
    FirstParGoal = Incomplete ^ ip_first_par_goal,
    LastParGoal = Incomplete ^ ip_last_par_goal.

:- pred start_building_parallelisation(implicit_parallelism_info::in,
    goals_for_parallelisation::in,
    incomplete_parallelisation::out) is det.

start_building_parallelisation(Info, PreprocessedGoals, Parallelisation) :-
    GoalsArray = PreprocessedGoals ^ gfp_goals,
    FirstParGoal = PreprocessedGoals ^ gfp_first_costly_goal,
    LastParGoal = PreprocessedGoals ^ gfp_last_costly_goal,
    NumCalls = PreprocessedGoals ^ gfp_num_calls,
    DependencyGraphs = PreprocessedGoals ^ gfp_dependency_graphs,
    Parallelisation = incomplete_parallelisation(Info, GoalsArray,
        FirstParGoal, LastParGoal, FirstParGoal, [], NumCalls,
        DependencyGraphs, no, no, no).

    % Finalise the parallelisation.
    %
:- pred finalise_parallelisation(incomplete_parallelisation::in,
    best_parallelisation::out) is det.

finalise_parallelisation(Incomplete, Best) :-
    GoalsBefore = ip_get_goals_before(Incomplete),
    GoalsAfter = ip_get_goals_after(Incomplete),

    MaybeCostData = Incomplete ^ ip_maybe_par_cost_data,
    (
        MaybeCostData = yes(CostData)
    ;
        MaybeCostData = no,
        unexpected($module, $pred, "parallelisation has no cost data")
    ),
    CostData = parallelisation_cost_data(Overlap, Metrics0, _),

    Metrics = finalise_parallel_exec_metrics(Metrics0),
    par_conj_overlap_is_dependent(Overlap, IsDependent),
    ParConjs = ip_get_par_conjs(Incomplete),
    Best = bp_parallel_execution(GoalsBefore, ParConjs,
        GoalsAfter, IsDependent, Metrics).

:- pred add_one_goal_into_first_par_conj(incomplete_parallelisation::in,
    incomplete_parallelisation::out) is det.

add_one_goal_into_first_par_conj(!Parallelisation) :-
    FirstGoal0 = !.Parallelisation ^ ip_first_par_goal,
    FirstGoal = FirstGoal0 - 1,
    !Parallelisation ^ ip_first_par_goal := FirstGoal,
    !Parallelisation ^ ip_maybe_goals_before_cost := no,
    !Parallelisation ^ ip_maybe_par_cost_data := no.

:- pred add_one_goal_into_last_par_conj(incomplete_parallelisation::in,
    incomplete_parallelisation::out) is det.

add_one_goal_into_last_par_conj(!Parallelisation) :-
    LastGoal0 = !.Parallelisation ^ ip_last_par_goal,
    LastGoal = LastGoal0 + 1,
    !Parallelisation ^ ip_last_par_goal := LastGoal,
    !Parallelisation ^ ip_maybe_goals_after_cost := no,
    !Parallelisation ^ ip_maybe_par_cost_data := no.

%----------------------------------------------------------------------------%

    % This datastructure represents the execution of dependent conjuncts, it
    % tracks which variabes are produced and consumed.
    %
    % TODO: Implement a pretty printer for this data.
    %
:- type parallel_execution_overlap
    --->    peo_empty_conjunct
    ;       peo_conjunction(
                poec_left_conjunct      :: parallel_execution_overlap,
                poec_right_conjunct     :: dependent_conjunct_execution,
                poec_dependent_vars     :: set(var_rep)
                    % The variables produced by the left conjunct and consumed
                    % by the right conjunct.
            ).

:- type dependent_conjunct_execution
    --->    dependent_conjunct_execution(
                % Pairs of start and stop times of the execution.
                % Assume that the list is not sorted.
                dce_execution           :: assoc_list(float, float),

                % The variable productions. This may be a superset of the
                % dependent variables.
                dce_productions         :: map(var_rep, float),

                % The variable consumptions. This will contain only
                % references for those variables that will become futures.
                dce_consumptions        :: map(var_rep, float)
            ).

    % calculate_parallel_cost(Info, !Parallelisation).
    %
    % Analyse the parallel conjuncts and determine their likely performance.
    %
    % This is the new parallel execution overlap algorithm, it is general and
    % therefore we also use is for independent conjunctions.
    %
:- pred calculate_parallel_cost(parallelisation_cost_data::out,
    incomplete_parallelisation::in, incomplete_parallelisation::out) is det.

calculate_parallel_cost(CostData, !Parallelisation) :-
    ParConj = ip_get_par_conjs(!.Parallelisation),
    NumCalls = !.Parallelisation ^ ip_num_calls,

    maybe_calc_sequential_cost(
        (func(P) = P ^ ip_maybe_goals_before_cost),
        (func(P0, MaybeCost) = P0 ^ ip_maybe_goals_before_cost := MaybeCost),
        ip_get_goals_before, CostBeforePercall, NumCalls, !Parallelisation),
    maybe_calc_sequential_cost(
        (func(P) = P ^ ip_maybe_goals_after_cost),
        (func(P0, MaybeCost) = P0 ^ ip_maybe_goals_after_cost := MaybeCost),
        ip_get_goals_after, CostAfterPercall, NumCalls, !Parallelisation),

    Info = !.Parallelisation ^ ip_info,
    Opts = Info ^ ipi_opts,
    SparkCost = Opts ^ cpcp_sparking_cost,
    SparkDelay = Opts ^ cpcp_sparking_delay,
    ContextWakeupDelay = Opts ^ cpcp_context_wakeup_delay,
    Metrics0 = init_empty_parallel_exec_metrics(CostBeforePercall,
        CostAfterPercall, NumCalls, float(SparkCost), float(SparkDelay),
        float(ContextWakeupDelay)),
    Overlap0 = peo_empty_conjunct,

    CostData0 = parallelisation_cost_data(Overlap0, Metrics0, init),
    NumMiddleGoals = ip_get_num_goals_middle(!.Parallelisation),
    foldl3(calculate_parallel_cost_step(Info, NumMiddleGoals), ParConj, 1, _,
        0, _, CostData0, CostData),
    !Parallelisation ^ ip_maybe_par_cost_data := yes(CostData).

:- pred maybe_calc_sequential_cost((func(T) = maybe(goal_cost_csq))::in,
    (func(T, maybe(goal_cost_csq)) = T)::in,
    (func(T) = list(pard_goal_detail))::in, float::out,
    int::in, T::in, T::out) is det.

maybe_calc_sequential_cost(GetMaybe, SetMaybe, GetGoals, CostPercall, Calls,
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
    !.CostData = parallelisation_cost_data(Overlap0, Metrics0, PM0),
    !:NumGoals = !.NumGoals + length(Conjuncts),
    ( !.NumGoals = NumMiddleGoals ->
        IsLastConjunct = is_last_par_conjunct
    ;
        IsLastConjunct = not_last_par_conjunct
    ),
    Conjunct = seq_conj(Conjuncts),
    calculate_parallel_cost_step(Info, IsLastConjunct, Conjuncts, !ConjNum,
        PM0, PM, Overlap0, Overlap, Metrics0, Metrics),
    !:CostData = parallelisation_cost_data(Overlap, Metrics, PM).

:- pred calculate_parallel_cost_step(implicit_parallelism_info::in,
    is_last_par_conjunct::in, list(pard_goal_detail)::in, int::in, int::out,
    map(var_rep, float)::in, map(var_rep, float)::out,
    parallel_execution_overlap::in, parallel_execution_overlap::out,
    parallel_exec_metrics_incomplete::in,
    parallel_exec_metrics_incomplete::out) is det.

calculate_parallel_cost_step(Info, IsLastConjunct, Conjunct, !ConjNum,
        !ProductionsMap, !Overlap, !Metrics) :-
    Algorithm = Info ^ ipi_opts ^ cpcp_parallelise_dep_conjs,

    Calls = parallel_exec_metrics_get_num_calls(!.Metrics),
    conj_calc_cost(Conjunct, Calls, CostB0),
    CostB = goal_cost_get_percall(CostB0),
    foldl(pardgoal_consumed_vars_accum, Conjunct, set.init,
        RightConsumedVars),
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
        SparkCost = Info ^ ipi_opts ^ cpcp_sparking_cost,
        StartTime = StartTime0 + float(SparkCost)
    ;
        IsLastConjunct = is_last_par_conjunct,
        StartTime = StartTime0
    ),

    (
        Algorithm = parallelise_dep_conjs_overlap,

        % Get the list of variables consumed by this conjunct
        % that will be turned into futures.
        foldl3(get_consumptions_list, Conjunct, Vars, _, 0.0, _,
            [], ConsumptionsList0),
        reverse(ConsumptionsList0, ConsumptionsList),

        % Determine how the parallel conjuncts overlap.
        foldl5(calculate_dependent_parallel_cost_2(Info, !.ProductionsMap),
            ConsumptionsList, 0.0, LastSeqConsumeTime,
            StartTime, LastParConsumeTime, StartTime, LastResumeTime,
            [], RevExecution0, map.init, ConsumptionsMap),

        % Calculate the point at which this conjunct finishes execution
        % and complete the RevExecutions structure..
        reverse(RevExecution, Execution),
        CostBPar = LastParConsumeTime + (CostB - LastSeqConsumeTime),
        RevExecution = [ (LastResumeTime - CostBPar) | RevExecution0 ],

        CostSignals =
            float(Info ^ ipi_opts ^ cpcp_future_signal_cost * count(Vars))
    ;
        ( Algorithm = parallelise_dep_conjs_naive
        ; Algorithm = do_not_parallelise_dep_conjs
        ; Algorithm = parallelise_dep_conjs_num_vars
        ),

        CostBPar = StartTime + CostB,
        Execution = [StartTime - CostBPar],
        ConsumptionsMap = init,
        CostSignals = 0.0
    ),

    !:Metrics = init_parallel_exec_metrics_incomplete(!.Metrics, CostSignals,
        CostB, CostBPar),

    % Build the productions map for the next conjunct. This map contains
    % all the variables produced by this code, not just that are used for
    % dependent parallelisation.
    foldl3(get_productions_map, Conjunct, StartTime, _, Execution, _,
        !ProductionsMap),

    DepConjExec = dependent_conjunct_execution(Execution,
        !.ProductionsMap, ConsumptionsMap),
    !:Overlap = peo_conjunction(!.Overlap, DepConjExec, Vars).

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
    % * SeqConsTime: The consumption time of the Var during sequential
    %   execution.
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
    % * !ConsumptionsMap: Accumuates a map of variable consumptions.
    %
:- pred calculate_dependent_parallel_cost_2(implicit_parallelism_info::in,
    map(var_rep, float)::in, pair(var_rep, float)::in, float::in, float::out,
    float::in, float::out, float::in, float::out,
    assoc_list(float, float)::in, assoc_list(float, float)::out,
    map(var_rep, float)::in, map(var_rep, float)::out) is det.

calculate_dependent_parallel_cost_2(Info, ProductionsMap, Var - SeqConsTime,
        !PrevSeqConsumeTime, !PrevParConsumeTime, !ResumeTime,
        !RevExecution, !ConsumptionsMap) :-
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

    (
        % True if Q had to suspend waiting for P. Note that we don't include
        % FutureSyncTime here. This is true if Q has to block at all even if
        % it can be made runable before the context switch is complete.
        ProdTime > ParConsTimeNotBlocked
    ->
        % Include the time that it may take to resume this thread.
        ParConsTime = ParConsTime0 +
            float(Info ^ ipi_opts ^ cpcp_context_wakeup_delay),
        !:RevExecution =
            [(!.ResumeTime - ParConsTimeNotBlocked) | !.RevExecution],
        !:ResumeTime = ParConsTime
    ;
        ParConsTime = ParConsTime0
    ),

    !:PrevSeqConsumeTime = SeqConsTime,
    !:PrevParConsumeTime = ParConsTime,

    svmap.det_insert(Var, ParConsTime, !ConsumptionsMap).

:- pred par_conj_overlap_is_dependent(parallel_execution_overlap::in,
    conjuncts_are_dependent::out) is det.

par_conj_overlap_is_dependent(peo_empty_conjunct, conjuncts_are_independent).
par_conj_overlap_is_dependent(peo_conjunction(Left, _, VarSet0), IsDependent) :-
    par_conj_overlap_is_dependent(Left, IsDependent0),
    (
        IsDependent0 = conjuncts_are_dependent(VarSetLeft),
        VarSet = union(VarSet0, VarSetLeft),
        IsDependent = conjuncts_are_dependent(VarSet)
    ;
        IsDependent0 = conjuncts_are_independent,
        ( set.empty(VarSet0) ->
            IsDependent = conjuncts_are_independent
        ;
            IsDependent = conjuncts_are_dependent(VarSet0)
        )
    ).

:- type dependency_graphs
    --->    dependency_graphs(
                dm_forward              :: digraph(int),
                dm_forward_tc           :: digraph(int)
            ).

:- pred build_dependency_graphs(list(pard_goal_detail)::in,
    dependency_graphs::out) is det.

build_dependency_graphs(Goals, Maps) :-
    Graph0 = digraph.init,
    build_dependency_graph(Goals, 1, map.init, _VarDepMap, Graph0, Graph),
    Maps = dependency_graphs(Graph, tc(Graph)).

:- pred depends_lookup(dependency_graphs::in, int::in, set(int)::out) is det.

depends_lookup(DependencyGraphs, GoalNum, Deps) :-
    graph_do_lookup(lookup_from, DependencyGraphs ^ dm_forward, GoalNum, Deps).

:- pred depends_lookup_tc(dependency_graphs::in, int::in, set(int)::out)
    is det.

depends_lookup_tc(DependencyGraphs, GoalNum, Deps) :-
    graph_do_lookup(lookup_from, DependencyGraphs ^ dm_forward_tc, GoalNum,
        Deps).

:- pred depends_lookup_rev(dependency_graphs::in, int::in, set(int)::out)
    is det.

depends_lookup_rev(DependencyGraphs, GoalNum, RevDeps) :-
    graph_do_lookup(lookup_to, DependencyGraphs ^ dm_forward, GoalNum,
        RevDeps).

:- pred depends_lookup_tc_rev(dependency_graphs::in, int::in, set(int)::out)
    is det.

depends_lookup_tc_rev(DependencyGraphs, GoalNum, RevDeps) :-
    graph_do_lookup(lookup_to, DependencyGraphs ^ dm_forward_tc, GoalNum,
        RevDeps).

    % Abstract code for querying a graph for a goal dependency.
    %
:- pred graph_do_lookup(
    pred(digraph(int), digraph_key(int), set(digraph_key(int))),
    digraph(int), int, set(int)).
:- mode graph_do_lookup(
    pred(in, in, out) is det,
    in, in, out) is det.

graph_do_lookup(Lookup, Graph, GoalNum, Deps) :-
    Lookup(Graph, lookup_key(Graph, GoalNum), DepsKeys),
    Deps = set(map(lookup_vertex(Graph), to_sorted_list(DepsKeys))).

:- pred build_dependency_graph(list(pard_goal_detail)::in, int::in,
    map(var_rep, int)::in, map(var_rep, int)::out,
    digraph(int)::in, digraph(int)::out) is det.

build_dependency_graph([], _ConjNum, !VarDepMap, !Graph).
build_dependency_graph([PG | PGs], ConjNum, !VarDepMap, !Graph) :-
    InstMapInfo = PG ^ goal_annotation ^ pgd_inst_map_info,

    % For each variable consumed by a goal we find out which goals instantiate
    % that variable and add them as it's dependencies along with their
    % dependencies.  NOTE: We only consider variables that are read
    % and not those that are set.  This is safe because we only bother
    % analysing single assignment code.
    RefedVars = InstMapInfo ^ im_consumed_vars,
    digraph.add_vertex(ConjNum, ThisConjKey, !Graph),
    list.foldl((pred(RefedVar::in, GraphI0::in, GraphI::out) is det :-
        map.search(!.VarDepMap, RefedVar, DepConj) ->
            % DepConj should already be in the graph.
            digraph.lookup_key(GraphI0, DepConj, DepConjKey),
            digraph.add_edge(DepConjKey, ThisConjKey, GraphI0, GraphI)
        ;
            GraphI = GraphI0
        ), to_sorted_list(RefedVars), !Graph),

    % For each variable instantiated by this goal add it to the VarDepMap with
    % this goal as it's instantiator.  That is a maping from the variable to
    % the conj num.
    InstVars = InstMapInfo ^ im_bound_vars,
    fold((pred(InstVar::in, MapI0::in, MapI::out) is det :-
            svmap.det_insert(InstVar, ConjNum, MapI0, MapI)
        ), InstVars, !VarDepMap),

    build_dependency_graph(PGs, ConjNum + 1, !VarDepMap, !Graph).

    % foldl(get_productions_map(Goals, 0,0, _, Vars, _, map.init, Map).
    %
    % If Goals is semidet this can produce incorrect values in the !Time
    % accumulator that lead to exceptions.  This is prevented by only
    % attempting to parallelise goals that are det or cc_multi.
    %
    % Build a map of variable productions in Goals.
    %
:- pred get_productions_map(pard_goal_detail::in, float::in, float::out,
    assoc_list(float, float)::in, assoc_list(float, float)::out,
    map(var_rep, float)::in, map(var_rep, float)::out) is det.

get_productions_map(Goal, !Time, !Executions, !Map) :-
    InstMapInfo = Goal ^ goal_annotation ^ pgd_inst_map_info,
    BoundVars = InstMapInfo ^ im_bound_vars,
    adjust_time_for_waits(!Time, !Executions),
    fold(var_production_time_to_map(!.Time, Goal), BoundVars, !Map),
    !:Time = !.Time + goal_cost_get_percall(Goal ^ goal_annotation ^ pgd_cost).

:- pred adjust_time_for_waits(float::in, float::out,
    assoc_list(float, float)::in, assoc_list(float, float)::out) is det.

adjust_time_for_waits(!Time, !Executions) :-
    (
        !.Executions = [ Execution | NextExecution ],
        ( Start - End ) = Execution,
        ( (!.Time + adjust_time_for_waits_epsilon) < Start ->
            error("adjust_time_for_waits: " ++
                "Time occurs before the current execution")
        ; !.Time =< (End + adjust_time_for_waits_epsilon) ->
            % The production is within the current execution, no adjustment is
            % necessary.
            true
        ;
            % The time is after this execution.
            !:Executions = NextExecution,
            adjust_time_for_waits_2(End, !Time, !Executions)
        )
    ;
        !.Executions = [],
        error("adjust_time_for_waits: Time occurs after all executions")
    ).

:- pred adjust_time_for_waits_2(float::in, float::in, float::out,
    assoc_list(float, float)::in, assoc_list(float, float)::out) is det.

adjust_time_for_waits_2(LastEnd, !Time, !Executions) :-
    (
        !.Executions = [ Execution | NextExecution ],
        ( Start - End ) = Execution,

        % Do the adjustment.
        !:Time = !.Time + (Start - LastEnd),

        ( (!.Time + adjust_time_for_waits_epsilon) < Start ->
            error(format("adjust_time_for_waits: Adjustment didn't work, " ++
                "time occurs before the current execution. " ++
                "Time: %f, Start: %f.", [f(!.Time), f(Start)]))
        ; !.Time =< (End + adjust_time_for_waits_epsilon) ->
            % The adjustment worked.
            true
        ;
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
    svmap.det_insert(Var, Time, !Map).

    % foldl(get_consumptions_list(Vars), Goals, 0.0, _, [], RevConsumptions),
    %
    % Compute the order and time of variable consumptions in goals.
    %
:- pred get_consumptions_list(pard_goal_detail::in,
    set(var_rep)::in, set(var_rep)::out, float::in, float::out,
    assoc_list(var_rep, float)::in, assoc_list(var_rep, float)::out) is det.

get_consumptions_list(Goal, !Vars, !Time, !List) :-
    InstMapInfo = Goal ^ goal_annotation ^ pgd_inst_map_info,
    AllConsumptionVars = InstMapInfo ^ im_consumed_vars,
    ConsumptionVars = intersect(!.Vars, AllConsumptionVars),
    map(var_consumptions(!.Time, Goal),
        ConsumptionVars, ConsumptionTimesSet0),
    % Since we re-sort the list we don't need a sorted one to start with,
    % but the set module doesn't export a "to_list" predicate. (Getting
    % a sorted list has no cost since the set is a sorted list internally).
    set.to_sorted_list(ConsumptionTimesSet0, ConsumptionTimes0),
    list.sort((pred((_ - TimeA)::in, (_ - TimeB)::in, Result::out) is det :-
            % Note that the Time arguments are swapped, this list must be
            % produced in latest to earliest order.
            compare(Result, TimeB, TimeA)
        ), ConsumptionTimes0, ConsumptionTimes),
    !:List = ConsumptionTimes ++ !.List,
    !:Vars = difference(!.Vars, ConsumptionVars),
    !:Time = !.Time + goal_cost_get_percall(Goal ^ goal_annotation ^ pgd_cost).

:- pred var_consumptions(float::in, pard_goal_detail::in, var_rep::in,
    pair.pair(var_rep, float)::out) is det.

var_consumptions(TimeBefore, Goal, Var, Var - Time) :-
    var_first_use_time(find_consumption, TimeBefore, Goal, Var, Time).

:- type find_production_or_consumption
    --->    find_production
    ;       find_consumption.

    % var_first_use_time(FindProdOrCons, Time0, Goal, Var, Time).
    %
    % if FindProdOrCons = find_production
    %   Time is Time0 + the time that Goal produces Var.
    % elif FindProdOrCons = find_consumption
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
                error("var_first_use_time: "
                    ++ "Found production when looking for consumption")
            )
        ;
            UseType = var_use_consumption,
            (
                FindProdOrCons = find_production,
                error("var_first_use_time: "
                    ++ "Found consumption when looking for production")
            ;
                FindProdOrCons = find_consumption
            )
        ),
        UseTime = Use ^ vui_cost_until_use
    ;
        UseType = var_use_other,
        % The analysis didn't recognise the instantiation here, so use a
        % conservative default for the production time.
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

%----------------------------------------------------------------------------%

:- pred pardgoal_consumed_vars_accum(pard_goal_detail::in,
    set(var_rep)::in, set(var_rep)::out) is det.

pardgoal_consumed_vars_accum(Goal, !Vars) :-
    RefedVars = Goal ^ goal_annotation ^ pgd_inst_map_info ^ im_consumed_vars,
    set.union(RefedVars, !Vars).

    % Check if it is appropriate to parallelise this call. That is it must be
    % model_det and have a cost above the call site cost threshold.
    %
:- pred can_parallelise_goal(goal_rep(T)::in) is semidet.

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

:- pred atomic_pard_goal_type(implicit_parallelism_info::in,
    list(goal_path_step)::in, atomic_goal_rep::in, inst_map_info::in,
    pard_goal_type::out, cord(message)::out) is det.

atomic_pard_goal_type(Info, RevGoalPathSteps, AtomicGoal, InstMapInfo,
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
        map.lookup(Info ^ ipi_call_sites, rgp(RevGoalPathSteps), CallSite),
        map_foldl(compute_var_modes(InstMapBefore, InstMapAfter),
            Args, VarsAndModes, 0, _),
        GoalType = pgt_call(VarsAndModes, CallSite)
    ).

:- pred atomic_pard_goal_cost(implicit_parallelism_info::in,
    list(goal_path_step)::in, coverage_info::in, atomic_goal_rep::in,
    goal_cost_csq::out) is det.

atomic_pard_goal_cost(Info, RevGoalPathSteps, Coverage, AtomicGoal, Cost) :-
    atomic_goal_is_call(AtomicGoal, IsCall),
    (
        IsCall = atomic_goal_is_trivial,
        get_coverage_before_det(Coverage, Calls),
        Cost = atomic_goal_cost(Calls)
    ;
        IsCall = atomic_goal_is_call(_),
        RevGoalPath = rgp(RevGoalPathSteps),
        map.lookup(Info ^ ipi_call_sites, RevGoalPath, CallSite),
        (
            cost_and_callees_is_recursive(Info ^ ipi_clique, CallSite),
            map.search(Info ^ ipi_rec_call_sites, RevGoalPath, RecCost)
        ->
            CSCost = RecCost
        ;
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

:- pred atomic_goal_build_use_map(atomic_goal_rep::in,
    list(goal_path_step)::in, implicit_parallelism_info::in,
    var_use_type::in, var_rep::in,
    map(var_rep, lazy(var_use_info))::in,
    map(var_rep, lazy(var_use_info))::out) is det.

atomic_goal_build_use_map(AtomicGoal, RevGoalPathSteps, Info, VarUseType, Var,
        !Map) :-
    atomic_goal_is_call(AtomicGoal, IsCall),
    (
        IsCall = atomic_goal_is_trivial,
        (
            VarUseType = var_use_consumption,
            CostUntilUse = 0.0
        ;
            ( VarUseType = var_use_production
            ; VarUseType = var_use_other
            ),
            CostUntilUse = 1.0
        ),
        LazyUse = val(var_use_info(CostUntilUse, 1.0, VarUseType))
    ;
        IsCall = atomic_goal_is_call(Args),
        LazyUse = delay(
            (func) = compute_var_use_lazy(Info, RevGoalPathSteps, Var,
                Args, VarUseType))
    ),
    svmap.det_insert(Var, LazyUse, !Map).

:- func compute_var_use_lazy(implicit_parallelism_info, list(goal_path_step),
    var_rep, list(var_rep), var_use_type) = var_use_info.

compute_var_use_lazy(Info, RevGoalPathSteps, Var, Args, VarUseType) = Use :-
    CliquePtr = Info ^ ipi_clique,
    RevGoalPath = rgp(RevGoalPathSteps),
    map.lookup(Info ^ ipi_call_sites, RevGoalPath, CostAndCallee),
    (
        cost_and_callees_is_recursive(CliquePtr, CostAndCallee),
        map.search(Info ^ ipi_rec_call_sites, RevGoalPath, RecCost)
    ->
        Cost = RecCost
    ;
        Cost = CostAndCallee ^ cac_cost
    ),

    solutions(
        compute_var_use_lazy_arg(Info, Var, Args, CostAndCallee,
            Cost, VarUseType),
        Uses),
    (
        VarUseType = var_use_consumption,
        Uses = [FirstUse | OtherUses],
        foldl(earliest_use, OtherUses, FirstUse, Use)
    ;
        ( VarUseType = var_use_production
        ; VarUseType = var_use_other
        ),
        (
            Uses = [Use]
        ;
            Uses = [_, _ | _],
            unexpected($module, $pred, "Too many solutions ")
        )
    ).

:- pred earliest_use(var_use_info::in, var_use_info::in, var_use_info::out)
    is det.

earliest_use(A, B, Ealiest) :-
    TimeA = A ^ vui_cost_until_use,
    TimeB = B ^ vui_cost_until_use,
    ( TimeA < TimeB ->
        Ealiest = A
    ;
        Ealiest = B
    ).

:- pred compute_var_use_lazy_arg(implicit_parallelism_info::in, var_rep::in,
    list(var_rep)::in, cost_and_callees::in, cs_cost_csq::in, var_use_type::in,
    var_use_info::out) is multi.

compute_var_use_lazy_arg(Info, Var, Args, CostAndCallee, Cost, VarUseType,
        Use) :-
    ( 0.0 < cs_cost_get_calls(Cost) ->
        CostPercall = cs_cost_get_percall(Cost),
        ( member_index0(Var, Args, ArgNum) ->
            HigherOrder = CostAndCallee ^ cac_call_site_is_ho,
            (
                HigherOrder = higher_order_call,
                % We cannot push signals or waits into higher order calls.
                pessimistic_var_use_info(VarUseType, CostPercall, Use)
            ;
                HigherOrder = first_order_call,
                ( singleton_set(CostAndCallee ^ cac_callees, CalleePrime) ->
                    Callee = CalleePrime
                ;
                    unexpected($module, $pred,
                        "first-order call site has wrong number of CSDs")
                ),
                CSDPtr = Callee ^ c_csd,
                RecursionType = Info ^ ipi_recursion_type,
                recursion_type_get_interesting_parallelisation_depth(
                    RecursionType, MaybeCurDepth),
                compute_var_use_2(Info, ArgNum, RecursionType, MaybeCurDepth,
                    VarUseType, CostPercall, CSDPtr, Use, Messages),
                trace [io(!IO)] (
                    stderr_stream(Stderr, !IO),
                    write_out_messages(Stderr, Messages, !IO)
                )
            )
        ;
            Use = var_use_info(0.0, CostPercall, VarUseType),
            ( unify(VarUseType, var_use_consumption) ->
                true
            ;
                unexpected($module, $pred,
                    "Var use type most be consumption if " ++
                    "\\+ member(Var, Args)")
            )
        )
    ;
        % This call site is never called.
        pessimistic_var_use_info(VarUseType, 0.0, Use)
    ).

:- pred compute_var_use_2(implicit_parallelism_info::in, int::in,
    recursion_type::in, maybe(recursion_depth)::in, var_use_type::in,
    float::in, call_site_dynamic_ptr::in, var_use_info::out,
    cord(message)::out) is det.

compute_var_use_2(Info, ArgNum, RecursionType, MaybeCurDepth, VarUseType, Cost,
        CSDPtr, Use, !:Messages) :-
    !:Messages = empty,
    Deep = Info ^ ipi_deep,
    CliquePtr = Info ^ ipi_clique,
    implicit_par_info_intermodule_var_use(Info, FollowCallsAcrossModules),
    VarUseOptions = var_use_options(Deep, FollowCallsAcrossModules,
        VarUseType),
    call_site_dynamic_var_use_info(CliquePtr, CSDPtr, ArgNum,
        RecursionType, MaybeCurDepth, Cost, set.init, VarUseOptions, MaybeUse),
    (
        MaybeUse = ok(Use)
    ;
        MaybeUse = error(Error),
        pessimistic_var_use_info(VarUseType, Cost, Use),
        append_message(pl_csd(CSDPtr),
            warning_cannot_compute_first_use_time(Error),
            !Messages)
    ).

:- pred goal_build_use_map(goal_rep(goal_id)::in,
    list(goal_path_step)::in, goal_cost_csq::in, implicit_parallelism_info::in,
    var_use_type::in, var_rep::in,
    map(var_rep, lazy(var_use_info))::in,
    map(var_rep, lazy(var_use_info))::out) is det.

goal_build_use_map(Goal, RevGoalPathSteps, Cost, Info, VarUseType, Var, !Map) :-
    LazyUse = delay((func) = compute_goal_var_use_lazy(Goal, RevGoalPathSteps,
        Cost, Info, VarUseType, Var)),
    svmap.det_insert(Var, LazyUse, !Map).

:- func compute_goal_var_use_lazy(goal_rep(goal_id), list(goal_path_step),
    goal_cost_csq, implicit_parallelism_info, var_use_type, var_rep)
    = var_use_info.

compute_goal_var_use_lazy(Goal, RevGoalPathSteps, Cost, Info, VarUseType, Var)
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
            rgp(RevGoalPathSteps), CostPercall, Var, VarUseOptions, Use)
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

:- pred implicit_par_info_intermodule_var_use(implicit_parallelism_info::in,
    intermodule_var_use::out) is det.

implicit_par_info_intermodule_var_use(Info, FollowCallsAcrossModules) :-
    IntermoduleVarUse = Info ^ ipi_opts ^ cpcp_intermodule_var_use,
    (
        IntermoduleVarUse = yes,
        FollowCallsAcrossModules = follow_any_call
    ;
        IntermoduleVarUse = no,
        ProcLabel = Info ^ ipi_proc_label,
        ( ProcLabel = str_ordinary_proc_label(_, _, Module, _, _, _)
        ; ProcLabel = str_special_proc_label(_, _, Module, _, _, _)
        ),
        FollowCallsAcrossModules = follow_calls_into_module(Module)
    ).

:- pred recursion_type_get_interesting_parallelisation_depth(
    recursion_type, maybe(recursion_depth)).
:- mode recursion_type_get_interesting_parallelisation_depth(
    in(recursion_type_known_costs), out(maybe_yes(ground))) is det.
:- mode recursion_type_get_interesting_parallelisation_depth(
    in, out) is det.

recursion_type_get_interesting_parallelisation_depth(RecursionType,
        MaybeDepth) :-
    (
        RecursionType = rt_not_recursive,
        MaybeDepth = yes(recursion_depth_from_float(0.0))
    ;
        RecursionType = rt_single(_, _, _DepthF, _, _),
        % The interesting recursion depth is at the bottom of the recursion, if
        % we can't parallelise here then there's no point parallelising the
        % loop in general.
        % XXX: Update metrics to understand that this is a loop.
        MaybeDepth = yes(recursion_depth_from_float(2.0))
    ;
        ( RecursionType = rt_divide_and_conquer(_, _)
        ; RecursionType = rt_mutual_recursion(_)
        ; RecursionType = rt_other(_)
        ; RecursionType = rt_errors(_)
        ),
        MaybeDepth = no
    ).

:- type is_costly_goal
    --->    is_not_costly_goal
    ;       is_costly_goal.

:- pred identify_costly_goal(pard_goal_detail_annotation::in,
    is_costly_goal::out) is det.

identify_costly_goal(Annotation, Costly) :-
    CostAboveThreshold = Annotation ^ pgd_cost_above_threshold,
    (
        CostAboveThreshold = cost_above_par_threshold,
        % TODO: distinguish between compound goals with one costly branch,
        % and compound goals where all branches are costly.
        % TODO: Provide information about how many costly goals are within
        % the goal so that we can try to parallelise each of those against
        % an outer costly goal.
        Costly = is_costly_goal
    ;
        CostAboveThreshold = cost_not_above_par_threshold,
        Costly = is_not_costly_goal
    ).

:- pred identify_costly_goals(list(pard_goal_detail)::in, int::in,
    list(int)::out) is det.

identify_costly_goals([], _, []).
identify_costly_goals([Goal | Goals], Index, Indexes) :-
    identify_costly_goals(Goals, Index + 1, Indexes0),
    identify_costly_goal(Goal ^ goal_annotation, Costly),
    (
        Costly = is_costly_goal,
        Indexes = [Index | Indexes0]
    ;
        Costly = is_not_costly_goal,
        Indexes = Indexes0
    ).

:- pred var_get_mode(inst_map::in, inst_map::in, var_rep::in,
    var_mode_rep::out) is det.

var_get_mode(InstMapBefore, InstMapAfter, VarRep, VarModeRep) :-
    inst_map_get(InstMapBefore, VarRep, InstBefore, _),
    inst_map_get(InstMapAfter, VarRep, InstAfter, _),
    VarModeRep = var_mode_rep(InstBefore, InstAfter).

    % Transform a goal in a conjunction into a pard_goal.
    %
:- pred goal_to_pard_goal(implicit_parallelism_info::in,
    list(goal_path_step)::in, goal_rep(goal_id)::in,
    pard_goal_detail::out, cord(message)::in, cord(message)::out) is det.

goal_to_pard_goal(Info, RevGoalPathSteps, !Goal, !Messages) :-
    !.Goal = goal_rep(GoalExpr0, Detism, GoalId),
    InstMapInfo = get_goal_attribute_det(Info ^ ipi_inst_map_array, GoalId),
    Coverage = get_goal_attribute_det(Info ^ ipi_coverage_array, GoalId),
    get_coverage_before_det(Coverage, Before),
    (
        (
            GoalExpr0 = conj_rep(Conjs0),
            list.map_foldl2(conj_to_pard_goals(Info, RevGoalPathSteps),
                Conjs0, Conjs, 1, _, !Messages),
            conj_calc_cost(Conjs, Before, Cost),
            GoalExpr = conj_rep(Conjs)
        ;
            GoalExpr0 = disj_rep(Disjs0),
            list.map_foldl2(disj_to_pard_goals(Info, RevGoalPathSteps),
                Disjs0, Disjs, 1, _, !Messages),
            disj_calc_cost(Disjs, Before, Cost),
            GoalExpr = disj_rep(Disjs)
        ;
            GoalExpr0 = switch_rep(Var, CanFail, Cases0),
            list.map_foldl2(case_to_pard_goal(Info, RevGoalPathSteps),
                Cases0, Cases, 1, _, !Messages),
            switch_calc_cost(Cases, Before, Cost),
            GoalExpr = switch_rep(Var, CanFail, Cases)
        ;
            GoalExpr0 = ite_rep(Cond0, Then0, Else0),
            goal_to_pard_goal(Info, [step_ite_cond | RevGoalPathSteps],
                Cond0, Cond, !Messages),
            goal_to_pard_goal(Info, [step_ite_then | RevGoalPathSteps],
                Then0, Then, !Messages),
            goal_to_pard_goal(Info, [step_ite_else | RevGoalPathSteps],
                Else0, Else, !Messages),
            ite_calc_cost(Cond, Then, Else, Cost),
            GoalExpr = ite_rep(Cond, Then, Else)
        ;
            GoalExpr0 = negation_rep(SubGoal0),
            goal_to_pard_goal(Info, [step_neg | RevGoalPathSteps],
                SubGoal0, SubGoal, !Messages),
            Cost = SubGoal ^ goal_annotation ^ pgd_cost,
            GoalExpr = negation_rep(SubGoal)
        ;
            GoalExpr0 = scope_rep(SubGoal0, MaybeCut),
            goal_to_pard_goal(Info, [step_scope(MaybeCut) | RevGoalPathSteps],
                SubGoal0, SubGoal, !Messages),
            Cost = SubGoal ^ goal_annotation ^ pgd_cost,
            GoalExpr = scope_rep(SubGoal, MaybeCut)
        ),
        PardGoalType = pgt_non_atomic_goal,

        BoundVars = to_sorted_list(InstMapInfo ^ im_bound_vars),
        list.foldl(
            goal_build_use_map(!.Goal, RevGoalPathSteps, Cost, Info,
                var_use_production),
            BoundVars, map.init, ProductionUseMap),
        ConsumedVars = to_sorted_list(InstMapInfo ^ im_consumed_vars),
        list.foldl(goal_build_use_map(!.Goal, RevGoalPathSteps, Cost, Info,
                var_use_consumption),
            ConsumedVars, map.init, ConsumptionUseMap)
    ;
        GoalExpr0 = atomic_goal_rep(Context, Line, BoundVars, AtomicGoal),
        GoalExpr = atomic_goal_rep(Context, Line, BoundVars, AtomicGoal),
        atomic_pard_goal_type(Info, RevGoalPathSteps, AtomicGoal, InstMapInfo,
            PardGoalType, Messages),
        atomic_pard_goal_cost(Info, RevGoalPathSteps, Coverage, AtomicGoal,
            Cost),

        list.foldl(
            atomic_goal_build_use_map(AtomicGoal, RevGoalPathSteps, Info,
                var_use_production),
            BoundVars, map.init, ProductionUseMap),
        ConsumedVars = to_sorted_list(InstMapInfo ^ im_consumed_vars),
        list.foldl(
            atomic_goal_build_use_map(AtomicGoal, RevGoalPathSteps, Info,
                var_use_consumption),
            ConsumedVars, map.init, ConsumptionUseMap),

        !:Messages = !.Messages ++ Messages
    ),
    % XXX: The goal annotations cannot represent reasons why a goal
    % can't be parallelised, for example it could be nondet, semidet or
    % impure.
    (
        can_parallelise_goal(!.Goal),
        goal_cost_above_par_threshold(Info, Cost)
    ->
        CostAboveThreshold = cost_above_par_threshold
    ;
        CostAboveThreshold = cost_not_above_par_threshold
    ),
    PardGoalAnnotation = pard_goal_detail(PardGoalType, InstMapInfo,
        rgp(RevGoalPathSteps), Coverage, Cost, CostAboveThreshold,
        ProductionUseMap, ConsumptionUseMap),
    !:Goal = goal_rep(GoalExpr, Detism, PardGoalAnnotation).

:- pred conj_to_pard_goals(implicit_parallelism_info::in,
    list(goal_path_step)::in, goal_rep(goal_id)::in,
    pard_goal_detail::out, int::in, int::out,
    cord(message)::in, cord(message)::out) is det.

conj_to_pard_goals(Info, RevGoalPathSteps, !Goal, !ConjNum, !Messages) :-
    goal_to_pard_goal(Info, [step_conj(!.ConjNum) | RevGoalPathSteps],
        !Goal, !Messages),
    !:ConjNum = !.ConjNum + 1.

:- pred disj_to_pard_goals(implicit_parallelism_info::in,
    list(goal_path_step)::in, goal_rep(goal_id)::in,
    pard_goal_detail::out, int::in, int::out,
    cord(message)::in, cord(message)::out) is det.

disj_to_pard_goals(Info, RevGoalPathSteps, !Goal, !DisjNum, !Messages) :-
    goal_to_pard_goal(Info, [step_disj(!.DisjNum) | RevGoalPathSteps],
        !Goal, !Messages),
    !:DisjNum = !.DisjNum + 1.

:- pred case_to_pard_goal(implicit_parallelism_info::in,
    list(goal_path_step)::in, case_rep(goal_id)::in,
    case_rep(pard_goal_detail_annotation)::out, int::in, int::out,
    cord(message)::in, cord(message)::out) is det.

case_to_pard_goal(Info, RevGoalPathSteps, !Case, !CaseNum, !Messages) :-
    !.Case = case_rep(ConsId, OtherConsId, Goal0),
    goal_to_pard_goal(Info, [step_switch(!.CaseNum, no) | RevGoalPathSteps],
        Goal0, Goal, !Messages),
    !:CaseNum = !.CaseNum + 1,
    !:Case = case_rep(ConsId, OtherConsId, Goal).

%----------------------------------------------------------------------------%

:- pred conj_calc_cost(list(pard_goal_detail)::in, int::in,
    goal_cost_csq::out) is det.

conj_calc_cost([], Calls, simple_goal_cost(Calls)).
conj_calc_cost([Conj | Conjs], _, Cost) :-
    Coverage = Conj ^ goal_annotation ^ pgd_coverage,
    get_coverage_after_det(Coverage, After),
    conj_calc_cost(Conjs, After, ConjsCost),
    ConjCost = Conj ^ goal_annotation ^ pgd_cost,
    Cost = add_goal_costs_seq(ConjCost, ConjsCost).

:- pred disj_calc_cost(list(pard_goal_detail)::in, int::in,
    goal_cost_csq::out) is det.

disj_calc_cost([], Calls, simple_goal_cost(Calls)).
disj_calc_cost([Disj | Disjs], _, Cost) :-
    Coverage = Disj ^ goal_annotation ^ pgd_coverage,
    get_coverage_before_and_after_det(Coverage, Before, After),
    ( Before = 0 ->
        % Avoid a divide by zero.
        Cost = dead_goal_cost
    ;
        _Successes = After,
        Failures = Before - After,
        % XXX: We assume this is a semidet disjunction
        disj_calc_cost(Disjs, Failures, FailureCost),
        DisjCost = Disj ^ goal_annotation ^ pgd_cost,
        SuccessCost = atomic_goal_cost(After),
        BranchCost = add_goal_costs_branch(Before, FailureCost, SuccessCost),
        Cost = add_goal_costs_seq(DisjCost, BranchCost)
    ).

:- pred switch_calc_cost(list(case_rep(pard_goal_detail_annotation))::in,
    int::in, goal_cost_csq::out) is det.

switch_calc_cost([], Calls, simple_goal_cost(Calls)).
switch_calc_cost([Case | Cases], TotalCalls, Cost) :-
    ( TotalCalls = 0 ->
        % Avoid a divide by zero.
        Cost = dead_goal_cost
    ;
        Coverage = Case ^ cr_case_goal ^ goal_annotation ^ pgd_coverage,
        get_coverage_before_det(Coverage, CaseCalls),
        switch_calc_cost(Cases, TotalCalls - CaseCalls, CasesCost),
        CaseCost = Case ^ cr_case_goal ^ goal_annotation ^ pgd_cost,
        Cost = add_goal_costs_branch(TotalCalls, CaseCost, CasesCost)
    ).

:- pred ite_calc_cost(pard_goal_detail::in, pard_goal_detail::in,
    pard_goal_detail::in, goal_cost_csq::out) is det.

ite_calc_cost(Cond, Then, Else, Cost) :-
    CondCost = Cond ^ goal_annotation ^ pgd_cost,
    ThenCost = Then ^ goal_annotation ^ pgd_cost,
    ElseCost = Else ^ goal_annotation ^ pgd_cost,
    Coverage = Cond ^ goal_annotation ^ pgd_coverage,
    get_coverage_before_det(Coverage, Before),
    ThenElseCost = add_goal_costs_branch(Before, ThenCost, ElseCost),
    Cost = add_goal_costs_seq(CondCost, ThenElseCost).

:- func simple_goal_cost(int) = goal_cost_csq.

simple_goal_cost(Calls) = Cost :-
    ( Calls = 0 ->
        Cost = dead_goal_cost
    ;
        Cost = atomic_goal_cost(Calls)
    ).

%----------------------------------------------------------------------------%
%
% Annotate a goal with instantiation information.
%

    % inst_map_info now contains information that it does not need to contain.
    % Namely, the im_after field can be calculated from the im_before and
    % im_bound_vars fields.  However since this information will probably
    % be attached to a different goal there is not much extra cost in having a
    % pointer to it from here.
    %
:- type inst_map_info
    --->    inst_map_info(
                % The inst map before this goal is executed.
                im_before           :: inst_map,

                % The inst map after this goal was executed.
                im_after            :: inst_map,

                % Variables consumed (read but not bound) by this goal.
                im_consumed_vars    :: set(var_rep),

                % The variables produced by this goal.
                im_bound_vars       :: set(var_rep)
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
:- pred goal_annotate_with_instmap(goal_rep(goal_id)::in,
    seen_duplicate_instantiation::out,
    set(var_rep)::out, set(var_rep)::out,
    inst_map::in, inst_map::out,
    goal_attr_array(inst_map_info)::gaa_di,
    goal_attr_array(inst_map_info)::gaa_uo) is det.

goal_annotate_with_instmap(Goal, SeenDuplicateInstantiation, ConsumedVars,
        BoundVars, !InstMap, !InstMapArray) :-
    Goal = goal_rep(GoalExpr, _, GoalId),
    InstMapBefore = !.InstMap,
    (
        GoalExpr = conj_rep(Conjs),
        conj_annotate_with_instmap(Conjs, SeenDuplicateInstantiation,
            ConsumedVars, BoundVars, !InstMap, !InstMapArray)
    ;
        GoalExpr = disj_rep(Disjs),
        disj_annotate_with_instmap(Disjs, SeenDuplicateInstantiation,
            ConsumedVars, BoundVars, !InstMap, !InstMapArray)
    ;
        GoalExpr = switch_rep(Var, _CanFail, Cases),
        switch_annotate_with_instmap(Cases, SeenDuplicateInstantiation,
            ConsumedVars0, BoundVars, !InstMap, !InstMapArray),
        set.insert(ConsumedVars0, Var, ConsumedVars)
    ;
        GoalExpr = ite_rep(Cond, Then, Else),
        ite_annotate_with_instmap(Cond, Then, Else,
            SeenDuplicateInstantiation, ConsumedVars, BoundVars,
            !InstMap, !InstMapArray)
    ;
        % XXX: Not all scope goals can produce variables, in fact some are used
        % to isolate variables that aren't named apart.  But other scope goals
        % can bind variables.  We don't know which we're looking at here.
        GoalExpr = scope_rep(Subgoal, _MaybeCut),
        goal_annotate_with_instmap(Subgoal, SeenDuplicateInstantiation,
            ConsumedVars, BoundVars, !InstMap, !InstMapArray)
    ;
        GoalExpr = negation_rep(Subgoal),
        % A negated goal cannot affect instantiation.
        goal_annotate_with_instmap(Subgoal, SeenDuplicateInstantiation,
            ConsumedVars, _, !.InstMap, _InstMap, !InstMapArray),
        BoundVars = set.init
    ;
        GoalExpr = atomic_goal_rep(_File, _Line, BoundVarsList, AtomicGoal),
        % The binding of a variable may depend on any number of other
        % variables, and recursively the variables that those depended-on
        % variables depend upon.
        % XXX: This doesn't include variables that can affect control flow and
        % therefore the values of other variables, this includes variables
        % referenced from conditions in ITE goals, and variables switched-on.
        % We may get away with this as our new system for determining
        % goal-dependance takes these into account.
        atomic_goal_get_vars(AtomicGoal, Vars),
        BoundVars = set.from_list(BoundVarsList),
        set.difference(Vars, BoundVars, ConsumedVars),
        inst_map_ground_vars(BoundVarsList, ConsumedVars, !InstMap,
            SeenDuplicateInstantiation)
    ),
    InstMapAfter = !.InstMap,
    InstMapInfo = inst_map_info(InstMapBefore, InstMapAfter, ConsumedVars,
        BoundVars),
    update_goal_attribute(GoalId, InstMapInfo, !InstMapArray).

:- pred conj_annotate_with_instmap(list(goal_rep(goal_id))::in,
    seen_duplicate_instantiation::out, set(var_rep)::out, set(var_rep)::out,
    inst_map::in, inst_map::out,
    goal_attr_array(inst_map_info)::gaa_di,
    goal_attr_array(inst_map_info)::gaa_uo) is det.

conj_annotate_with_instmap([], have_not_seen_duplicate_instantiation,
        set.init, set.init, !InstMap, !InstMapArray).
conj_annotate_with_instmap([Conj | Conjs], SeenDuplicateInstantiation,
        ConsumedVars, BoundVars, !InstMap, !InstMapArray) :-
    goal_annotate_with_instmap(Conj, SeenDuplicateInstantiationHead,
        ConsumedVarsHead, BoundVarsHead, !InstMap, !InstMapArray),
    conj_annotate_with_instmap(Conjs, SeenDuplicateInstantiationTail,
        ConsumedVarsTail, BoundVarsTail, !InstMap, !InstMapArray),
    set.union(ConsumedVarsTail, ConsumedVarsHead, ConsumedVars),
    set.union(BoundVarsTail, BoundVarsHead, BoundVars),
    SeenDuplicateInstantiation = merge_seen_duplicate_instantiation(
        SeenDuplicateInstantiationHead,
        SeenDuplicateInstantiationTail).

:- pred disj_annotate_with_instmap(list(goal_rep(goal_id))::in,
    seen_duplicate_instantiation::out, set(var_rep)::out, set(var_rep)::out,
    inst_map::in, inst_map::out,
    goal_attr_array(inst_map_info)::gaa_di,
    goal_attr_array(inst_map_info)::gaa_uo) is det.

disj_annotate_with_instmap([], have_not_seen_duplicate_instantiation,
        set.init, set.init, !InstMap, !InstMapArray).
disj_annotate_with_instmap([Disj | Disjs], SeenDuplicateInstantiation,
        ConsumedVars, BoundVars, InstMap0, InstMap, !InstMapArray) :-
    HeadDetism = Disj ^ goal_detism_rep,
    goal_annotate_with_instmap(Disj, SeenDuplicateInstantiationHead,
        ConsumedVarsHead, BoundVarsHead, InstMap0, InstMapHead,
        !InstMapArray),
    disj_annotate_with_instmap(Disjs, SeenDuplicateInstantiationTail,
        ConsumedVarsTail, BoundVarsTail, InstMap0, InstMapTail,
        !InstMapArray),

    set.union(ConsumedVarsTail, ConsumedVarsHead, ConsumedVars),

    % merge_inst_map requires the detism of goals that produce both inst maps,
    % we can create fake values that satisfy merge_inst_map easily.
    % XXX: Consider inferring determinism as another simple analysis.
    % A disjunction may only bind a variable if all disjuncts bind that
    % variable.  We respect that here and handle the special case of this being
    % the last disjunct in a disjunction.
    (
        Disjs = [],
        TailDetism = failure_rep,
        BoundVars = BoundVarsHead
    ;
        Disjs = [_ | _],
        TailDetism = det_rep,
        set.intersect(BoundVarsTail, BoundVarsHead, BoundVars)
    ),
    InstMap = merge_inst_map(InstMapHead, HeadDetism, InstMapTail, TailDetism),
    SeenDuplicateInstantiation = merge_seen_duplicate_instantiation(
        SeenDuplicateInstantiationHead,
        SeenDuplicateInstantiationTail).

:- pred switch_annotate_with_instmap(list(case_rep(goal_id))::in,
    seen_duplicate_instantiation::out, set(var_rep)::out, set(var_rep)::out,
    inst_map::in, inst_map::out,
    goal_attr_array(inst_map_info)::gaa_di,
    goal_attr_array(inst_map_info)::gaa_uo) is det.

switch_annotate_with_instmap([], have_not_seen_duplicate_instantiation,
        set.init, set.init, !InstMap, !InstMapArray).
switch_annotate_with_instmap([Case | Cases], SeenDuplicateInstantiation,
        ConsumedVars, BoundVars, InstMap0, InstMap, !InstMapArray) :-
    Case = case_rep(_, _, Goal),
    HeadDetism = Goal ^ goal_detism_rep,
    goal_annotate_with_instmap(Goal, SeenDuplicateInstantiationHead,
        ConsumedVarsHead, BoundVarsHead, InstMap0, InstMapHead,
        !InstMapArray),
    switch_annotate_with_instmap(Cases, SeenDuplicateInstantiationTail,
        ConsumedVarsTail, BoundVarsTail, InstMap0, InstMapTail,
        !InstMapArray),
    set.union(ConsumedVarsTail, ConsumedVarsHead, ConsumedVars),
    % merge_inst_map requires the detism of goals that produce both inst maps,
    % we can create fake values that satisfy merge_inst_map easily.
    (
        Cases = [],
        TailDetism = failure_rep,
        BoundVars = BoundVarsHead
    ;
        Cases = [_ | _],
        TailDetism = det_rep,
        set.intersect(BoundVarsTail, BoundVarsHead, BoundVars)
    ),
    InstMap = merge_inst_map(InstMapHead, HeadDetism, InstMapTail, TailDetism),
    SeenDuplicateInstantiation = merge_seen_duplicate_instantiation(
        SeenDuplicateInstantiationHead,
        SeenDuplicateInstantiationTail).

:- pred ite_annotate_with_instmap(goal_rep(goal_id)::in,
    goal_rep(goal_id)::in, goal_rep(goal_id)::in,
    seen_duplicate_instantiation::out, set(var_rep)::out, set(var_rep)::out,
    inst_map::in, inst_map::out,
    goal_attr_array(inst_map_info)::gaa_di,
    goal_attr_array(inst_map_info)::gaa_uo) is det.

ite_annotate_with_instmap(Cond, Then, Else, SeenDuplicateInstantiation,
        ConsumedVars, BoundVars, InstMap0, InstMap, !InstMapArray) :-
    goal_annotate_with_instmap(Cond, SeenDuplicateInstantiationCond,
        ConsumedVarsCond, _BoundVarsCond, InstMap0, InstMapAfterCond,
        !InstMapArray),
    goal_annotate_with_instmap(Then, SeenDuplicateInstantiationThen,
        ConsumedVarsThen, BoundVarsThen, InstMapAfterCond, InstMapAfterThen,
        !InstMapArray),
    goal_annotate_with_instmap(Else, SeenDuplicateInstantiationElse,
        ConsumedVarsElse, BoundVarsElse, InstMap0, InstMapAfterElse,
        !InstMapArray),
    (
        SeenDuplicateInstantiationCond = have_not_seen_duplicate_instantiation,
        SeenDuplicateInstantiationThen = have_not_seen_duplicate_instantiation,
        SeenDuplicateInstantiationElse = have_not_seen_duplicate_instantiation
    ->
        SeenDuplicateInstantiation = have_not_seen_duplicate_instantiation
    ;
        SeenDuplicateInstantiation = seen_duplicate_instantiation
    ),
    set.union(ConsumedVarsCond, ConsumedVarsThen, ConsumedVarsCondThen),
    set.union(ConsumedVarsCondThen, ConsumedVarsElse, ConsumedVars),
    % Cond is only allowed to bind variables for then.  THe variables bound by
    % the ITE are only those that both Then and Else bind.
    set.intersect(BoundVarsThen, BoundVarsElse, BoundVars),
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

:- pred create_candidate_parallel_conj_report(var_table::in,
    candidate_par_conjunction(pard_goal)::in, cord(string)::out) is det.

create_candidate_parallel_conj_report(VarTable, CandidateParConjunction,
        Report) :-
    CandidateParConjunction = candidate_par_conjunction(GoalPathString,
        FirstConjNum, IsDependent, GoalsBefore, GoalsBeforeCost,
        Conjs, GoalsAfter, GoalsAfterCost, ParExecMetrics),
    ParExecMetrics = parallel_exec_metrics(NumCalls, SeqTime, ParTime,
        ParOverheads, FirstConjDeadTime, FutureDeadTime),
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

    string.format(
        "      Dependent: %s\n" ++
        "      NumCalls: %s\n" ++
        "      SeqTime: %s\n" ++
        "      ParTime: %s\n" ++
        "      ParOverheads: %s\n" ++
        "      Speedup: %s\n" ++
        "      Time saving: %s\n" ++
        "      First conj dead time: %s\n" ++
        "      Future dead time: %s\n" ++
        "      Total dead time: %s\n\n",
        [s(DependanceString),
         s(commas(NumCalls)),
         s(two_decimal_fraction(SeqTime)),
         s(two_decimal_fraction(ParTime)),
         s(two_decimal_fraction(ParOverheads)),
         s(four_decimal_fraction(Speedup)),
         s(two_decimal_fraction(TimeSaving)),
         s(two_decimal_fraction(FirstConjDeadTime)),
         s(two_decimal_fraction(FutureDeadTime)),
         s(two_decimal_fraction(TotalDeadTime))],
        Header2Str),
    Header2 = cord.singleton(Header2Str),

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
    Report = Header1 ++ Header2 ++ ReportGoalsBefore ++ nl
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
        map(format_var_use_line(VarTable), List, Lines),
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

:- pred debug_cliques_below_threshold(candidate_child_clique::in,
    io::di, io::uo) is det.

debug_cliques_below_threshold(Clique, !IO) :-
    CliquePtr = Clique ^ ccc_clique,
    CliquePtr = clique_ptr(CliqueNum),
    Calls = cs_cost_get_calls(Clique ^ ccc_cs_cost),
    PercallCost = cs_cost_get_percall(Clique ^ ccc_cs_cost),
    io.format(
        "D: Not entering clique: %d, " ++
        "it is below the clique threshold\n  " ++
        "It has per-call cost %f and is called %f times\n\n",
        [i(CliqueNum), f(PercallCost), f(Calls)], !IO).

:- pred debug_cliques_exceeded_parallelism(candidate_child_clique::in,
    io::di, io::uo) is det.

debug_cliques_exceeded_parallelism(Clique, !IO) :-
    CliquePtr = Clique ^ ccc_clique,
    CliquePtr = clique_ptr(CliqueNum),
    io.format(
        "D: Not entiring clique %d, " ++
        "no more parallelisation resources available at this context\n\n",
        [i(CliqueNum)], !IO).

%-----------------------------------------------------------------------------%
:- end_module mdprof_fb.automatic_parallelism.
%-----------------------------------------------------------------------------%

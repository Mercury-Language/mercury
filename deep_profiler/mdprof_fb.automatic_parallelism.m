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
:- import_module string.
:- import_module svmap.

%----------------------------------------------------------------------------%
%
% The code in this section has some trace goals that can be enabled with:
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
    % Find opertunities for parallelism by walking the clique tree.  Don't
    % Descened into cliques cheaper than the threshold.
    deep_lookup_clique_index(Deep, Deep ^ root, RootCliquePtr),
    % The +1 here accounts for the cost of the pseudo call into the mercury
    % runtime since it is modeled here as a call site that in reality does not
    % exist.
    RootParallelism = no_parallelism,
    candidate_parallel_conjunctions_clique(Params, Deep,
        RootParallelism, RootCliquePtr, ConjunctionsMap, Messages),

    map.to_assoc_list(ConjunctionsMap, ConjunctionsAssocList0),
    map_values_only(convert_candidate_par_conjunctions_proc(
            pard_goal_detail_to_pard_goal),
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
        foldl(build_var_use_list(PGD ^ pgd_var_production_map), SharedVars, 
            [], Productions),
        foldl(build_var_use_list(PGD ^ pgd_var_consumption_map), SharedVars, 
            [], Consumptions)
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
                ipi_call_sites      :: map(goal_path, cost_and_callees),
                ipi_rec_call_sites  :: map(goal_path, cs_cost_csq),
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
                pgd_pg_type                 :: pard_goal_type,
                    % The type and type-specific values of the pard goal.

                pgd_inst_map_info           :: inst_map_info,
                    % The inst map info attached to the original goal.

                pgd_original_path           :: goal_path,
                    % The original goal path of this goal.

                pgd_coverage                :: coverage_info,
                    % Coverage data for this goal.

                pgd_cost                    :: goal_cost_csq,
                    % The per-call cost of this call in call sequence counts.
                
                pgd_cost_above_threshold    :: cost_above_par_threshold,

                pgd_var_production_map      :: map(var_rep, lazy(var_use_info)),
                pgd_var_consumption_map     :: map(var_rep, lazy(var_use_info))
                    % Variable production and consumption information.
            ).

:- type pard_goal_type 
    --->    pgt_call(
                pgtc_args                   :: list(var_and_mode),
                    % The argument modes and use information.

                pgtc_call_site              :: cost_and_callees
                    % The call site report from the deep profiler.
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
    % Find any CandidateParallelConjunctions in this clique and it's children.
    % We stop searching when ParentCSCost is too low that the overheads of
    % parallelism are too great or if ParentUsedParallelism becomes greater
    % than the desired amount of parallelism.
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
            error(format("%sClique %d has no entry proc", 
                [s(this_file), i(CliqueNum)]))
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
    map2(candidate_parallel_conjunctions_clique_proc(Opts, Deep,
            RecursionType, CliquePtr), 
        PDPtrs, Candidatess, Messagess),
    foldl(union(merge_candidate_par_conjs_proc), Candidatess,
        map.init, CliqueCandidates), 
    CliqueMessages = cord_list_to_cord(Messagess),

    % Look in descendent cliques.
    some [!ChildCliques] (
        map(proc_dynamic_callees(Deep, ParentParallelism), PDPtrs,
            ChildCliquess),
        !:ChildCliques = cord_list_to_cord(ChildCliquess),
        map2(candidate_parallel_conjunctions_callee(Opts, Deep,
                CliquePtr, CliqueCandidates),
            list(!.ChildCliques), CSCandidatess, CSMessagess)
    ),
    foldl(union(merge_candidate_par_conjs_proc), CSCandidatess,
        map.init, CSCandidates), 
    CSMessages = cord_list_to_cord(CSMessagess),

    union(merge_candidate_par_conjs_proc, CliqueCandidates, CSCandidates,
        Candidates),
    Messages = CliqueMessages ++ CSMessages.
    
:- type candidate_child_clique
    --->    candidate_child_clique(
                ccc_clique              :: clique_ptr,
                ccc_cs_cost             :: cs_cost_csq,

                % The context of the call site that calls this clique.
                ccc_proc                :: string_proc_label,
                ccc_goal_path           :: goal_path, 

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
    map(pd_slot_callees(Deep, Parallelism, ProcLabel), Slots, ChildCliquess),
    ChildCliques = cord_list_to_cord(ChildCliquess).

:- pred pd_slot_callees(deep::in, parallelism_amount::in,
    string_proc_label::in, 
    pair(call_site_static_ptr, call_site_array_slot)::in, 
    cord(candidate_child_clique)::out) is det.

pd_slot_callees(Deep, Parallelism, ProcLabel, CSSPtr - Slot, ChildCliques) :-
    deep_lookup_call_site_statics(Deep, CSSPtr, CSS),
    goal_path_from_string_det(CSS ^ css_goal_path, GoalPath),
    (
        Slot = slot_normal(CSDPtr),
        call_site_dynamic_callees(Deep, Parallelism, ProcLabel, GoalPath,
            CSDPtr, ChildCliques)
    ;
        Slot = slot_multi(_, CSDPtrs),
        map(call_site_dynamic_callees(Deep, Parallelism, ProcLabel, GoalPath),
            to_list(CSDPtrs), ChildCliquess),
        ChildCliques = cord_list_to_cord(ChildCliquess)
    ).

:- pred call_site_dynamic_callees(deep::in, parallelism_amount::in,
    string_proc_label::in, goal_path::in, call_site_dynamic_ptr::in,
    cord(candidate_child_clique)::out) is det.

call_site_dynamic_callees(Deep, Parallelism, ProcLabel, GoalPath, CSDPtr,
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
                candidate_child_clique(CliquePtr, Cost, ProcLabel, GoalPath,
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
                    debug_cliques_exeeded_parallelism(!.Callee, !IO)
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
        ChildCliquePtr = 
            !.Callee ^ ccc_clique,
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
        foldl(update_parallelism_available_conj, Conjs, !ChildClique)
    ;
        true
    ).

:- pred update_parallelism_available_conj(candidate_par_conjunction(T)::in,
    candidate_child_clique::in, candidate_child_clique::out) is det.

update_parallelism_available_conj(Conj, !ChildClique) :-
    GoalPath = !.ChildClique ^ ccc_goal_path,
    goal_path_from_string_det(Conj ^ cpc_goal_path, ConjGoalPath),
    % XXX: This needs revisiting if we allow parallelised conjuncts to be
    % re-ordered.
    FirstConjunct = Conj ^ cpc_first_conj_num + length(Conj ^ cpc_goals_before),
    Length = foldl((func(seq_conj(ConjsI), Acc) = Acc + length(ConjsI)),
        Conj ^ cpc_conjs, 0),
    (
        GoalPath \= ConjGoalPath,
        goal_path_inside(ConjGoalPath, GoalPath, RelativePath),
        goal_path_consable(RelativePath, RelativePathCons),
        goal_path_consable_remove_first(RelativePathCons, Step, _),
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
            append_message(clique(CliquePtr),
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
    A = candidate_par_conjunctions_proc(VarTableA, CPCsA),
    B = candidate_par_conjunctions_proc(VarTableB, CPCsB),
    Result = candidate_par_conjunctions_proc(VarTableA, CPCs),
    CPCs = CPCsA ++ CPCsB,
    ( VarTableA = VarTableB ->
        true
    ;
        error(this_file ++ 
            "merge_candidate_par_conjs_proc: var tables do not match")
    ).

%----------------------------------------------------------------------------%
%
% Search for paralleliation opportunities within a procedure.
%

    % Find candidate parallel conjunctions within the given procedure.
    %
:- pred candidate_parallel_conjunctions_proc(
    candidate_par_conjunctions_params::in, deep::in, proc_dynamic_ptr::in,
    recursion_type::in, map(goal_path, cs_cost_csq)::in,
    candidate_par_conjunctions::out, cord(message)::out) is det.

candidate_parallel_conjunctions_proc(Opts, Deep, PDPtr, RecursionType,
        RecursiveCallSiteCostMap, Candidates, Messages) :-
    some [!Messages] (
        !:Messages = cord.empty,

        % Lookup the proc static to find the ProcLabel.
        deep_lookup_proc_dynamics(Deep, PDPtr, PD),
        deep_lookup_proc_statics(Deep, PD ^ pd_proc_static, PS),
        ProcLabel = PS ^ ps_id,
        ( ProcLabel = str_ordinary_proc_label(_, ModuleName, _, _, _, _)
        ; ProcLabel = str_special_proc_label(_, ModuleName, _, _, _, _)
        ),
        
        deep_get_progrep_det(Deep, ProgRep),
        
        (
            ( ModuleName = "Mercury runtime"
            ; ModuleName = "exception"
            )
        ->
            % Silently skip over any code from the runtime, we can't expect to
            % find it's procedure representation.
            Candidates = map.init
        ;
            create_dynamic_procrep_coverage_report(Deep, PDPtr,
                MaybeCoverageReport),
            (
                MaybeCoverageReport = ok(CoverageReport),
                ProcRep = CoverageReport ^ prci_proc_rep,
                ProcRep ^ pr_defn = ProcDefnRep,
                ProcDefnRep ^ pdr_goal = Goal0,
                ProcDefnRep ^ pdr_var_table = VarTable,
               
                deep_lookup_clique_index(Deep, PDPtr, CliquePtr),
                proc_dynamic_paired_call_site_slots(Deep, PDPtr, Slots),
                foldl(build_call_site_cost_and_callee_map(Deep), Slots, 
                    map.init, CallSitesMap), 
                Info = implicit_parallelism_info(Deep, ProgRep, Opts, CliquePtr,
                    CallSitesMap, RecursiveCallSiteCostMap, RecursionType,
                    VarTable, ProcLabel),
                some [!Goal] (
                    !:Goal = Goal0,
                    goal_annotate_with_instmap(!Goal,
                        initial_inst_map(ProcDefnRep), _FinalInstMap,
                        SeenDuplicateInstantiation, _ConsumedVars, _BoundVars),
                    goal_to_pard_goal(Info, empty_goal_path, !Goal, !Messages),
                    goal_get_conjunctions_worth_parallelising(Info, 
                        empty_goal_path, !.Goal, _, Candidates0, MessagesA),
                    !:Messages = !.Messages ++ MessagesA
                ),
                (
                    SeenDuplicateInstantiation =
                        have_not_seen_duplicate_instantiation,
                    list.foldl(build_candidate_par_conjunction_maps(ProcLabel,
                            VarTable),
                        Candidates0, map.init, Candidates)
                ;
                    SeenDuplicateInstantiation = seen_duplicate_instantiation,
                    Candidates = map.init,
                    append_message(proc(ProcLabel), 
                        notice_duplicate_instantiation(length(Candidates0)),
                        !Messages)
                )
            ;
                MaybeCoverageReport = error(Error),
                Candidates = map.init,
                append_message(proc(ProcLabel),
                    error_coverage_procrep_error(Error), !Messages)
            )
        ),
        Messages = !.Messages
    ).

:- type coverage_and_instmap_info
    --->    coverage_and_instmap_info(
                cai_coverage                :: coverage_info,
                cai_inst_map_info           :: inst_map_info
            ).

:- instance goal_annotation_add_instmap(coverage_info, 
            coverage_and_instmap_info) where [
        add_instmap(InstMap, Coverage, 
            coverage_and_instmap_info(Coverage, InstMap))
    ].

:- pred build_candidate_par_conjunction_maps(string_proc_label::in,
    var_table::in, candidate_par_conjunction(pard_goal_detail)::in, 
    candidate_par_conjunctions::in, candidate_par_conjunctions::out) is det.

build_candidate_par_conjunction_maps(ProcLabel, VarTable, Candidate, !Map) :-
    ( map.search(!.Map, ProcLabel, CandidateProc0) ->
        CandidateProc0 = candidate_par_conjunctions_proc(VarTablePrime, CPCs0),
        CPCs = [ Candidate | CPCs0 ],
        ( VarTable = VarTablePrime ->
            true
        ;
            error(this_file ++ 
                "build_candidate_par_conjunction_maps: Var tables do not match")
        )
    ;
        CPCs = [ Candidate ]
    ),
    CandidateProc = candidate_par_conjunctions_proc(VarTable, CPCs),
    svmap.set(ProcLabel, CandidateProc, !Map).

:- pred goal_get_conjunctions_worth_parallelising(
    implicit_parallelism_info::in, goal_path::in,
    pard_goal_detail::in, pard_goal_detail::out,
    list(candidate_par_conjunction(pard_goal_detail))::out,
    cord(message)::out) is det.

goal_get_conjunctions_worth_parallelising(Info, GoalPath, !Goal, Candidates,
        Messages) :-
    GoalExpr0 = !.Goal ^ goal_expr_rep,
    (
        (
            GoalExpr0 = conj_rep(Conjs0),
            map3_foldl(conj_get_conjunctions_worth_parallelising(Info,
                    GoalPath), 
                Conjs0, Conjs, Candidatess, Messagess, 1, _),
            conj_build_candidate_conjunctions(Info, GoalPath, Conjs,
                Cost, MessagesB, MaybeCandidate),
            GoalExpr = conj_rep(Conjs),
            Messages = cord_list_to_cord(Messagess) ++ MessagesB,
            (
                MaybeCandidate = yes(Candidate),
                Candidates = [Candidate | condense(Candidatess)]
            ;
                MaybeCandidate = no,
                Candidates = condense(Candidatess)
            )
        ;
            GoalExpr0 = disj_rep(Disjs0),
            map3_foldl(disj_get_conjunctions_worth_parallelising(Info, 
                    GoalPath),
                Disjs0, Disjs, Candidatess, Messagess, 1, _),
            disj_calc_cost(Disjs, Cost),
            GoalExpr = disj_rep(Disjs),
            Messages = cord_list_to_cord(Messagess),
            Candidates = condense(Candidatess)
        ;
            GoalExpr0 = switch_rep(Var, CanFail, Cases0),
            map3_foldl(switch_case_get_conjunctions_worth_parallelising(Info, 
                    GoalPath),
                Cases0, Cases, Candidatess, Messagess, 1, _),
            get_coverage_before_det(!.Goal ^ goal_annotation ^ pgd_coverage, 
                CountBefore),
            switch_calc_cost(Cases, CountBefore, Cost),
            GoalExpr = switch_rep(Var, CanFail, Cases),
            Messages = cord_list_to_cord(Messagess),
            Candidates = condense(Candidatess)
        ;
            GoalExpr0 = ite_rep(Cond0, Then0, Else0),
            ite_get_conjunctions_worth_parallelising(Info, GoalPath,
                Cond0, Cond, Then0, Then, Else0, Else, Candidates, Messages),
            ite_calc_cost(Cond, Then, Else, Cost),
            GoalExpr = ite_rep(Cond, Then, Else)
        ;
            GoalExpr0 = scope_rep(SubGoal0, MaybeCut),
            ScopeGoalPath = 
                goal_path_add_at_end(GoalPath, step_scope(MaybeCut)),
            goal_get_conjunctions_worth_parallelising(Info, ScopeGoalPath,
                SubGoal0, SubGoal, Candidates, Messages),
            Cost = SubGoal ^ goal_annotation ^ pgd_cost,
            GoalExpr = scope_rep(SubGoal, MaybeCut)
        ;
            GoalExpr0 = negation_rep(SubGoal0),
            NegGoalPath = goal_path_add_at_end(GoalPath, step_neg),
            goal_get_conjunctions_worth_parallelising(Info, NegGoalPath,
                SubGoal0, SubGoal, Candidates, Messages),
            Cost = SubGoal ^ goal_annotation ^ pgd_cost,
            GoalExpr = negation_rep(SubGoal)
        ),
        !Goal ^ goal_annotation ^ pgd_cost := Cost
    ;
        GoalExpr0 = atomic_goal_rep(_, _, _, _),
        Messages = cord.empty,
        Candidates = [],
        GoalExpr = GoalExpr0
    ),
    !Goal ^ goal_expr_rep := GoalExpr.

:- pred conj_get_conjunctions_worth_parallelising(
    implicit_parallelism_info::in, goal_path::in, 
    pard_goal_detail::in, pard_goal_detail::out,
    list(candidate_par_conjunction(pard_goal_detail))::out,
    cord(message)::out, int::in, int::out) is det.

conj_get_conjunctions_worth_parallelising(Info, GoalPath, !Conj, Candidates,
        Messages, !ConjNum) :-
    ConjGoalPath = goal_path_add_at_end(GoalPath, step_conj(!.ConjNum)),
    goal_get_conjunctions_worth_parallelising(Info, ConjGoalPath, !Conj,
        Candidates, Messages),
    !:ConjNum = !.ConjNum + 1.

:- pred disj_get_conjunctions_worth_parallelising(
    implicit_parallelism_info::in, goal_path::in, 
    pard_goal_detail::in, pard_goal_detail::out,
    list(candidate_par_conjunction(pard_goal_detail))::out, 
    cord(message)::out, int::in, int::out) is det.

disj_get_conjunctions_worth_parallelising(Info, GoalPath, !Disj, Candidates,
        Messages, !DisjNum) :-
    DisjGoalPath = goal_path_add_at_end(GoalPath, step_disj(!.DisjNum)),
    goal_get_conjunctions_worth_parallelising(Info, DisjGoalPath, !Disj,
        Candidates, Messages),
    !:DisjNum = !.DisjNum + 1.

:- pred switch_case_get_conjunctions_worth_parallelising(
    implicit_parallelism_info::in, goal_path::in,
    case_rep(pard_goal_detail_annotation)::in,
    case_rep(pard_goal_detail_annotation)::out,
    list(candidate_par_conjunction(pard_goal_detail))::out, 
    cord(message)::out, int::in, int::out) is det.

switch_case_get_conjunctions_worth_parallelising(Info, GoalPath, !Case, 
        Candidates, Messages, !CaseNum) :-
    Goal0 = !.Case ^ cr_case_goal,
    CaseGoalPath = goal_path_add_at_end(GoalPath, step_switch(!.CaseNum, no)),
    goal_get_conjunctions_worth_parallelising(Info, CaseGoalPath, Goal0, Goal,
        Candidates, Messages),
    !Case ^ cr_case_goal := Goal,
    !:CaseNum = !.CaseNum + 1.

:- pred ite_get_conjunctions_worth_parallelising(
    implicit_parallelism_info::in,  goal_path::in,
    pard_goal_detail::in, pard_goal_detail::out, 
    pard_goal_detail::in, pard_goal_detail::out, 
    pard_goal_detail::in, pard_goal_detail::out, 
    list(candidate_par_conjunction(pard_goal_detail))::out, cord(message)::out) 
    is det.

ite_get_conjunctions_worth_parallelising(Info, GoalPath, !Cond, !Then, !Else, 
        Candidates, Messages) :-
    CondGoalPath = goal_path_add_at_end(GoalPath, step_ite_cond),
    ThenGoalPath = goal_path_add_at_end(GoalPath, step_ite_then),
    ElseGoalPath = goal_path_add_at_end(GoalPath, step_ite_else),
    goal_get_conjunctions_worth_parallelising(Info, CondGoalPath, !Cond,
        CondCandidates, CondMessages),
    goal_get_conjunctions_worth_parallelising(Info, ThenGoalPath, !Then,
        ThenCandidates, ThenMessages),
    goal_get_conjunctions_worth_parallelising(Info, ElseGoalPath, !Else,
        ElseCandidates, ElseMessages),
    Candidates = CondCandidates ++ ThenCandidates ++ ElseCandidates,
    Messages = CondMessages ++ ThenMessages ++ ElseMessages.

    % At the end of every conjunction we call this predicate to check the list
    % of calls we've found and make any parallelisation decisions.
    %
:- pred conj_build_candidate_conjunctions(implicit_parallelism_info::in,
    goal_path::in, list(pard_goal_detail)::in, goal_cost_csq::out,
    cord(message)::out, maybe(candidate_par_conjunction(pard_goal_detail))::out)
    is det.

conj_build_candidate_conjunctions(Info, GoalPath, Conjs, Cost, Messages, 
        MaybeCandidate) :-
    ProcLabel = Info ^ ipi_proc_label,
    Location = goal(ProcLabel, GoalPath),
    some [!Messages] 
    (
        !:Messages = cord.empty,

        % Preprocess the conjunction to find the costly calls and where they
        % are.
        foldl2(identify_costly_goals, Conjs, 1, _,
            no_costly_goals, CostlyGoalsInfo), 
        (
            ( CostlyGoalsInfo = no_costly_goals
            ; CostlyGoalsInfo = one_costly_goal(_)
            ),
            conj_calc_cost(Conjs, Cost),
            MaybeCandidate = no
        ;
            CostlyGoalsInfo = many_costly_goals(_, _, NumCostlyCalls),
            
            append_message(Location,
                info_found_conjs_above_callsite_threshold(NumCostlyCalls),
                !Messages),

            pardgoals_build_candidate_conjunction(Info, Location, GoalPath, 
                Conjs, MaybeCandidate),
            (
                MaybeCandidate = yes(Candidate),
                append_message(Location,
                    info_found_n_conjunctions_with_positive_speedup(1), 
                    !Messages),
                ExecMetrics = Candidate ^ cpc_par_exec_metrics,
                Cost = call_goal_cost(ExecMetrics ^ pem_num_calls,
                    ExecMetrics ^ pem_par_time)
            ; 
                MaybeCandidate = no,
                conj_calc_cost(Conjs, Cost)
            )
        ),
        Messages = !.Messages
    ).

:- type costly_goals_info
    --->    no_costly_goals
    ;       one_costly_goal(
                ocg_index               :: int
            )
    ;       many_costly_goals(
                ocg_first_index         :: int,
                ocg_last_index          :: int,
                ocg_mum_goals           :: int
            ).

:- pred identify_costly_goals(pard_goal_detail::in, int::in, int::out, 
    costly_goals_info::in, costly_goals_info::out) is det.

identify_costly_goals(Goal, !Index, !CostlyGoalsInfo) :-
    identify_costly_goal(Goal, Costly),
    (
        ( Costly = is_costly_atomic_goal
        ; Costly = is_costly_compound_goal
        ),
        (
            !.CostlyGoalsInfo = no_costly_goals,
            !:CostlyGoalsInfo = one_costly_goal(!.Index)
        ;
            !.CostlyGoalsInfo = one_costly_goal(FirstIndex),
            !:CostlyGoalsInfo = many_costly_goals(FirstIndex, !.Index, 2)
        ;
            !.CostlyGoalsInfo = many_costly_goals(FirstIndex, _, Num),
            !:CostlyGoalsInfo = many_costly_goals(FirstIndex, !.Index, Num+1)
        )
    ;
        Costly = is_not_costly_goal
    ),
    !:Index = !.Index + 1.

:- pred pardgoals_build_candidate_conjunction(implicit_parallelism_info::in,
    program_location::in, goal_path::in, list(pard_goal_detail)::in,
    maybe(candidate_par_conjunction(pard_goal_detail))::out) is det.

pardgoals_build_candidate_conjunction(Info, Location, GoalPath, Goals,
        MaybeCandidate) :-
    % Setting up the first parallel conjunct is a different algorithm to the
    % latter ones, at this point we have the option of moving goals from before
    % the first costly call to either before or during the parallel
    % conjunction.  Executing them during the parallel conjunction can be more
    % efficient.  However if goals within other parallel conjuncts depend on
    % them and don't depend upon the first costly call then this would make the
    % conjunction dependent when it could be independent.
    find_best_parallelisation(Info, Location, Goals, BestParallelisation),
    FirstConjNum = 1,
    ParalleliseDepConjs = Info ^ ipi_opts ^ cpcp_parallelise_dep_conjs,
    BestParallelisation = bp_parallel_execution(GoalsBefore, ParConjs,
        GoalsAfter, IsDependent, Metrics),
    Speedup = parallel_exec_metrics_get_speedup(Metrics),
    conj_calc_cost(GoalsBefore, GoalsBeforeCost0),
    GoalsBeforeCost = goal_cost_get_percall(GoalsBeforeCost0),
    conj_calc_cost(GoalsAfter, GoalsAfterCost0),
    GoalsAfterCost = goal_cost_get_percall(GoalsAfterCost0),
    Candidate = candidate_par_conjunction(goal_path_to_string(GoalPath),
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
        trace [compile_time(flag("debug_parallel_conjunction_speedup")), 
                io(!IO)] 
        (
            (
                ( Location = proc(ProcLabel)
                ; Location = goal(ProcLabel, _)
                )
            ;
                ( Location = clique(_)
                ; Location = call_site_dynamic(_)
                ),
                error("Location is a clique or CSD when it should be a proc " ++
                    "or goal")
            ),

            convert_candidate_par_conjunction(pard_goal_detail_to_pard_goal,
                Candidate, FBCandidate),
            VarTable = Info ^ ipi_var_table,
            create_candidate_parallel_conj_report(VarTable,
                ProcLabel, FBCandidate, Report),
            io.write_string("Not parallelising conjunction, " ++ 
                "insufficient speedup or too dependent:\n", !IO),
            io.write_string(append_list(cord.list(Report)), !IO),
            io.flush_output(!IO)
        )
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
    best_parallelisation::out) is det.

find_best_parallelisation(Info, Location, Goals, BestParallelisation) :-
    % Decide which algorithm to use.
    ConjunctionSize = length(Goals),
    choose_algorithm(Info, ConjunctionSize, Algorithm),
    
    preprocess_conjunction(Goals, Algorithm, PreprocessedGoals),
    (
        Algorithm = bpa_complete_bnb(_),
        find_best_parallelisation_complete_bnb(Info, Location, 
            PreprocessedGoals, BestParallelisation)
    ;
        Algorithm = bpa_greedy,
        find_best_parallelisation_greedy(Info, Location, 
            PreprocessedGoals, BestParallelisation)
    ).

:- pred choose_algorithm(implicit_parallelism_info::in,
    int::in, best_par_algorithm::out) is det.

choose_algorithm(Info, ConjunctionSize, Algorithm) :-
    Algorithm0 = Info ^ ipi_opts ^ cpcp_best_par_alg,
    (
        Algorithm0 = bpa_complete_bnb(Limit),
        ( Limit \= 0, Limit < ConjunctionSize ->
            Algorithm = bpa_greedy
        ;
            Algorithm = Algorithm0
        )
    ;
        Algorithm0 = bpa_greedy,
        Algorithm = Algorithm0
    ).

:- type goal_group(T)
    --->    gg_singleton(pard_goal_detail, T)
    ;       gg_group(int, list(pard_goal_detail), T).

:- inst gg_singleton
    --->    gg_singleton(ground, ground).

:- pred gg_get_details(goal_group(T)::in, int::out, 
    list(pard_goal_detail)::out, T::out) is det.

gg_get_details(gg_singleton(Goal, P), 1, [Goal], P).
gg_get_details(gg_group(Num, Goals, P), Num, Goals, P).

:- func new_group(list(pard_goal_detail), T) = goal_group(T).

new_group([], _) = _ :-
    error(this_file ++ "Can not construct empty goal group").
new_group([G | Gs], P) = GoalGroup :-
    (
        Gs = [],
        GoalGroup = gg_singleton(G, P)
    ;
        Gs = [_ | _],
        GoalGroup = gg_group(length(Gs)+1, [G | Gs], P)
    ).

% NOTE: These commented out types are relevant for some work that hasn't been
% done.  They will either be used or removed in a future change.

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

:- type goals_for_parallelisation
    --->    goals_for_parallelisation(
                gfp_groups                  :: 
                    list(goal_group(goal_classification)),
                gfp_goals                   ::
                    list(pard_goal_detail),

                gfp_dependency_graphs       :: dependency_graphs,
                gfp_costly_goal_indexes     :: list(int),
                gfp_num_calls               :: int
            ).

:- inst goals_for_parallelisation
    --->    goals_for_parallelisation(
                non_empty_list, ground, ground,
                non_empty_list,
                ground).

:- pred preprocess_conjunction(list(pard_goal_detail)::in,
    best_par_algorithm::in,
    goals_for_parallelisation::out(goals_for_parallelisation)) is det.

preprocess_conjunction(Goals0, Algorithm, GoalsForParallelisation) :-
    % Phase 1: Build a dependency map.
    build_dependency_graphs(Goals0, DependencyGraphs),
    % Phase 2: Find the costly calls.
    preprocess_conjunction_into_groups(Goals0, Algorithm, 1, GoalGroups,
        CostlyGoalsIndexes),

    % Get the number of calls into this conjunction.
    (
        CostlyGoalsIndexes = [FirstCostlyGoalIndex | _],
        list.index1(Goals0, FirstCostlyGoalIndex, FirstCostlyGoal)
    ->
        Cost = FirstCostlyGoal ^ goal_annotation ^ pgd_cost,
        NumCalls = goal_cost_get_calls(Cost)
    ;
        error(this_file ++ "Expected call goal")
    ),

    GoalsForParallelisation = goals_for_parallelisation(GoalGroups,
        Goals0, DependencyGraphs, CostlyGoalsIndexes, NumCalls).

    % identify_costly_goals(Goals, 1, GoalGroups, SortedCostlyIndexes).
    %
    % GoalGroups are Goals divided into groups of single costly calls and
    % multiple goals in-between these calls.  SortedCostlyIndexes are the
    % indexes of the costly calls in the original list (starting at 1).  This
    % predicate is undefined if any of the goals in Goals are non-atomic.
    %
:- pred preprocess_conjunction_into_groups(list(pard_goal_detail)::in,
    best_par_algorithm::in, int::in,
    list(goal_group(goal_classification))::out, list(int)::out) is det.

preprocess_conjunction_into_groups([], _, _, [], []).
preprocess_conjunction_into_groups([Goal | Goals], Alg, Index, GoalGroups,
        Indexes) :-
    preprocess_conjunction_into_groups(Goals, Alg, Index+1, GoalGroups0,
        Indexes0),
    identify_costly_goal(Goal, Costly),
    (
        ( Costly = is_costly_atomic_goal
        ; Costly = is_costly_compound_goal
        ),
        GoalClassification = gc_costly_goals,
        Indexes = [Index | Indexes0]
    ;
        Costly = is_not_costly_goal,
        GoalClassification = gc_cheap_goals,
        Indexes = Indexes0
    ),
    (
        Alg = bpa_greedy,
        % Group cheap goals together.
        (
            GoalClassification = gc_cheap_goals,
            GoalGroups0 = [LastGoalGroup | GoalGroups1Prime],
            gg_get_details(LastGoalGroup, NumLastGoals, LastGoals,
                gc_cheap_goals)
        -> 
            GoalGroup = gg_group(NumLastGoals+1, [Goal | LastGoals],
                gc_cheap_goals),
            GoalGroups1 = GoalGroups1Prime
        ;
            GoalGroup = gg_singleton(Goal, GoalClassification),
            GoalGroups1 = GoalGroups0
        )
    ;
        Alg = bpa_complete_bnb(_),
        GoalGroup = gg_singleton(Goal, GoalClassification),
        GoalGroups1 = GoalGroups0
    ),
    GoalGroups = [GoalGroup | GoalGroups1].

:- type incomplete_parallelisation
    --->    incomplete_parallelisation(
                ip_goals_before         :: list(pard_goal_detail),
                ip_par_conjs            :: list(seq_conj(pard_goal_detail)),
                ip_par_exec_overlap     :: parallel_execution_overlap,
                ip_par_exec_metrics     :: parallel_exec_metrics_incomplete,
                ip_productions_map      :: map(var_rep, float)
            ).

:- pred start_building_parallelisation(implicit_parallelism_info::in,
    int::in, list(pard_goal_detail)::in, incomplete_parallelisation::out) 
    is det.

start_building_parallelisation(Info, NumCalls, GoalsBefore, Parallelisation)
        :-
    SparkCost = Info ^ ipi_opts ^ cpcp_sparking_cost,
    SparkDelay = Info ^ ipi_opts ^ cpcp_sparking_delay,
    ContextWakeupDelay = Info ^ ipi_opts ^ cpcp_context_wakeup_delay,
    conj_calc_cost(GoalsBefore, CostBefore0),
    CostBefore = goal_cost_get_percall(CostBefore0),
    Metrics = init_empty_parallel_exec_metrics(CostBefore, NumCalls, 
        float(SparkCost), float(SparkDelay), float(ContextWakeupDelay)),
    Overlap = peo_empty_conjunct, 
    Parallelisation = incomplete_parallelisation(GoalsBefore, [], Overlap,
        Metrics, init).

    % Finalise the metrics and determine if this is the best solution so
    % far.
    %
:- pred finalise_parallelisation(list(pard_goal_detail)::in,
    incomplete_parallelisation::in, best_parallelisation::out) is det.

finalise_parallelisation(GoalsAfter, !Parallelisation) :-
    !.Parallelisation = incomplete_parallelisation(GoalsBefore, Conjuncts,
        Overlap, Metrics0, _),
    conj_calc_cost(GoalsAfter, CostAfter0),
    CostAfter = goal_cost_get_percall(CostAfter0),
    Metrics = finalise_parallel_exec_metrics(Metrics0, CostAfter),
    par_conj_overlap_is_dependent(Overlap, IsDependent),
    !:Parallelisation = bp_parallel_execution(GoalsBefore, Conjuncts,
        GoalsAfter, IsDependent, Metrics).

%----------------------------------------------------------------------------%

    % Find the best parallelisation using the branch and bound algorithm.
    %
:- pred find_best_parallelisation_complete_bnb(implicit_parallelism_info::in,
    program_location::in, goals_for_parallelisation::in,
    best_parallelisation::out) is det.

find_best_parallelisation_complete_bnb(Info, Location, 
        PreprocessedGoals, BestParallelisation) :-
    PreprocessedGoals = goals_for_parallelisation(GoalGroups, _, 
        DependencyMaps, CostlyCallsIndexes, NumCalls),
    ( last(CostlyCallsIndexes, LastCostlyCallIndexPrime) ->
        LastCostlyCallIndex = LastCostlyCallIndexPrime
    ;
        error(this_file ++ "no costly calls found")
    ),
    
    branch_and_bound(
        generate_parallelisations(Info, Location, LastCostlyCallIndex, 
            NumCalls, DependencyMaps, GoalGroups),
        parallelisation_get_objective_value,
        Solutions, Profile),
    
    trace [compile_time(flag("debug_branch_and_bound")), io(!IO)] (
        io.format("D: Find best parallelisation for:\n%s\n",
            [s(LocationString)], !IO),
        location_to_string(1, Location, LocationCord),
        string.append_list(list(LocationCord), LocationString),
        io.format("D: Problem size: %d Solutions: %d\n",
            [i(length(GoalGroups)), i(count(Solutions))], !IO),
        io.format("D: Branch and bound profile: %s\n\n",
            [s(string(Profile))], !IO),
        io.flush_output(!IO)
    ),
    
    ( 
        promise_equivalent_solutions [BestParallelisationPrime]
        member(BestParallelisationPrime, Solutions) 
    ->
        BestParallelisation = BestParallelisationPrime
    ;
        ParalleliseDepConjs = Info ^ ipi_opts ^ cpcp_parallelise_dep_conjs,
        (
            ( ParalleliseDepConjs = parallelise_dep_conjs_overlap
            ; ParalleliseDepConjs = parallelise_dep_conjs_naive
            ; ParalleliseDepConjs = parallelise_dep_conjs_num_vars
            ),
            error(this_file ++ "Execpted at least one solution from bnb solver")
        ;
            ParalleliseDepConjs = do_not_parallelise_dep_conjs,
            % Try again to get the best dependent parallelisation.  This is
            % used for guided parallelisation.
            TempInfo = Info ^ ipi_opts ^ cpcp_parallelise_dep_conjs := 
                parallelise_dep_conjs_overlap,
            find_best_parallelisation_complete_bnb(TempInfo, Location,
                PreprocessedGoals, BestParallelisation)
        )
    ).

    % The objective function for the branch and bound search.  This is ParTime
    % + ParOverheads * 2.  That is we're willing to pay 1 unit of parallel
    % overheads to get a 2 unit improvement of parallel execution time.
    %
:- func parallelisation_get_objective_value(best_parallelisation) = float.

parallelisation_get_objective_value(Parallelisation) = Value :-
    Metrics = Parallelisation ^ bp_par_exec_metrics,
    Value = Metrics ^ pem_par_time + Metrics ^ pem_par_overheads * 2.0.

:- semipure pred generate_parallelisations(implicit_parallelism_info::in,
    program_location::in, int::in, int::in, dependency_graphs::in,
    list(goal_group(goal_classification))::in, 
    bnb_state(best_parallelisation)::in, best_parallelisation::out) is nondet.

generate_parallelisations(Info, _Location, LastCostlyCallIndex,
        NumCalls, DependencyMaps, !.GoalGroups, BNBState, 
        BestParallelisation) :-
    some [!GoalNum, !Parallelisation] (
        !:GoalNum = 1,
        
        generate_parallelisations_goals_before(GoalsBeforeConj, !GoalNum,
            !GoalGroups),
        start_building_parallelisation(Info, NumCalls, GoalsBeforeConj,
            !:Parallelisation),

        semipure generate_parallelisations_body(Info, DependencyMaps, 0,
            BNBState, LastCostlyCallIndex, !GoalNum, !GoalGroups,
            !Parallelisation),

        generate_parallelisations_goals_after(!.GoalNum, !.GoalGroups,
            GoalsAfterConj),
       
        finalise_parallelisation(GoalsAfterConj, !.Parallelisation,
            BestParallelisation)
    ),
    semipure test_incomplete_solution(BNBState, BestParallelisation).

:- pred generate_parallelisations_goals_before(list(pard_goal_detail)::out, 
    int::in, int::out, 
    list(goal_group(goal_classification))::in, 
    list(goal_group(goal_classification))::out) is multi. 

generate_parallelisations_goals_before([], !GoalNum, !GoalGroups).
generate_parallelisations_goals_before(Goals, !GoalNum, !GoalGroups) :-
    !.GoalGroups = [GoalGroup | !:GoalGroups],
    (
        GoalGroup = gg_singleton(Goal, Classification),
        !:GoalNum = !.GoalNum + 1,
        NewGoals = [Goal]
    ;
        GoalGroup = gg_group(Num, NewGoals, Classification),
        !:GoalNum = !.GoalNum + Num
    ),
    Classification = gc_cheap_goals,
    generate_parallelisations_goals_before(Goals0, !GoalNum, !GoalGroups),
    Goals = NewGoals ++ Goals0.

:- pred generate_parallelisations_goals_after(int::in, 
    list(goal_group(goal_classification))::in, list(pard_goal_detail)::out) 
    is det.

generate_parallelisations_goals_after(_, [], []).
generate_parallelisations_goals_after(Num0, [GG | GGs], Goals) :-
    (
        GG = gg_singleton(Goal, _),
        Num = Num0 + 1,
        NewGoals = [Goal]
    ;
        GG = gg_group(NewNum, NewGoals, _),
        Num = Num0 + NewNum
    ),
    generate_parallelisations_goals_after(Num, GGs, Goals0),
    Goals = NewGoals ++ Goals0.

:- semipure pred generate_parallelisations_body(implicit_parallelism_info::in,
    dependency_graphs::in, int::in, bnb_state(best_parallelisation)::in,
    int::in, int::in, int::out,
    list(goal_group(goal_classification))::in, 
    list(goal_group(goal_classification))::out,
    incomplete_parallelisation::in, incomplete_parallelisation::out) is nondet. 

generate_parallelisations_body(Info, DependencyMaps, NumConjuncts0,
        BNBState, LastCostlyCallIndex, !GoalNum, !GoalGroups,
        !Parallelisation) :-
    (
        !.GoalNum > LastCostlyCallIndex
    ->
        % if we have already visited all the costly calls then there are no
        % further parallelisations to make.
        % Verify that we've generated at least two parallel conjuncts,
        NumConjuncts0 >= 1
    ;
        % We continue building more parallel conjuncts.
        !.GoalGroups = [_ | _],
        generate_parallel_conjunct(ParConjGoals, !GoalNum, !GoalGroups),
        
        % Don't build a single parallel conjunct containing all the costly
        % calls.
        ( NumConjuncts0 = 0 => LastCostlyCallIndex >= !.GoalNum ),
        
        ( LastCostlyCallIndex >= !.GoalNum ->
            IsInnermostConjunct = is_not_innermost_conjunct
        ;
            IsInnermostConjunct = is_innermost_conjunct
        ),
        calculate_parallel_cost_step(Info, IsInnermostConjunct, NumConjuncts0,
            ParConjGoals, !Parallelisation),
        Overlap = !.Parallelisation ^ ip_par_exec_overlap, 
        
        % Reject parallelisations that have dependent variables if we're not
        % allowed to create such parallelisations.
        ParalleliseDepConjs = Info ^ ipi_opts ^ cpcp_parallelise_dep_conjs,
        par_conj_overlap_is_dependent(Overlap, IsDependent),
        (
            ParalleliseDepConjs = do_not_parallelise_dep_conjs
        =>
            IsDependent = conjuncts_are_independent
        ),
     
        % Test the corrent solution.
        finalise_parallelisation([], !.Parallelisation, TestParallelisation),
        semipure test_incomplete_solution(BNBState, TestParallelisation),

        NumConjuncts = NumConjuncts0 + 1,
        semipure generate_parallelisations_body(Info, DependencyMaps, 
            NumConjuncts, BNBState, LastCostlyCallIndex, !GoalNum, !GoalGroups,
            !Parallelisation)
    ).

:- pred generate_parallel_conjunct(
    list(pard_goal_detail)::out(non_empty_list), int::in, int::out, 
    list(goal_group(goal_classification))::in(non_empty_list),
    list(goal_group(goal_classification))::out) is multi.

generate_parallel_conjunct(Goals, !GoalNum, !GoalGroups) :-
    !.GoalGroups = [GoalGroup | !:GoalGroups],
    (
        GoalGroup = gg_singleton(Goal, _),
        NewGoals = [Goal],
        NumNewGoals = 1
    ;
        GoalGroup = gg_group(NumNewGoals, NewGoals, _)
    ),
    !:GoalNum = !.GoalNum + NumNewGoals,
    (
        !.GoalGroups = [],
        Goals0 = []
    ;
        !.GoalGroups = [_ | _],
        % With these disjuncts in this order the predicate will return larger
        % conjuncts first.
        (
            generate_parallel_conjunct(Goals0, !GoalNum, !GoalGroups)
        ;
            Goals0 = []
        )
    ),
    Goals = NewGoals ++ Goals0.

%----------------------------------------------------------------------------%

    % The greedy algorithm for finding the best parallelisation of a
    % conjunction.
    %
:- pred find_best_parallelisation_greedy(implicit_parallelism_info::in,
    program_location::in, 
    goals_for_parallelisation::in(goals_for_parallelisation), 
    best_parallelisation::out) is det.

find_best_parallelisation_greedy(Info, _Location, 
        PreprocessedGoals, !:Parallelisation) :-
    some [!GoalGroups, !ConjNum] (
        PreprocessedGoals = goals_for_parallelisation(!:GoalGroups, _,
            DependencyMaps, CostlyCallIndexes, NumCalls),
        !:ConjNum = 1,

        !.GoalGroups = [ FirstGroup | !:GoalGroups ],
        gg_get_details(FirstGroup, _, FirstGoals, FirstClassification),
        (
            FirstClassification = gc_cheap_goals,
            build_goals_before_greedy(Info, DependencyMaps, CostlyCallIndexes,
                GoalsBeforeConj, !ConjNum, FirstGoals, RemainingGoals),
            (
                RemainingGoals = []
            ;
                RemainingGoals = [_ | _],
                !:GoalGroups = 
                    [new_group(RemainingGoals, gc_cheap_goals) | !.GoalGroups]
            )
        ;
            FirstClassification = gc_costly_goals,
            !:GoalGroups = [ FirstGroup | !.GoalGroups ],
            GoalsBeforeConj = []
        ),
        start_building_parallelisation(Info, NumCalls, GoalsBeforeConj,
            !:Parallelisation),

        build_parallel_conjuncts_greedy(Info, DependencyMaps,
            CostlyCallIndexes, 0, [], [], LastParConj, !ConjNum,
            !GoalGroups, !Parallelisation),
   
        (
            !.GoalGroups = [LastGroup | EmptyGroups],
            require(unify(EmptyGroups, []), "EmptyGroups \\= []"),
            gg_get_details(LastGroup, _, LastGoals0, LastClassification),
            require(unify(LastClassification, gc_cheap_goals), 
                "LastClassification \\= gc_cheap_goals")
        ;
            !.GoalGroups = [],
            LastGoals0 = []
        ),
        
        build_goals_after_greedy(Info, !.ConjNum, LastParConj,
            LastGoals0, LastGoals, !Parallelisation),
        finalise_parallelisation(LastGoals, !Parallelisation)
    ).

    % build_goals_before_greedy(Info, Deps, CostlyCallIndexes,
    %   GoalsBefore, GoalsWithin, !GoalNum, !Goals).
    %
    % Take GoalsBefore goals from !Goals, that should be run sequentially
    % before the parallelisation begins.  !.GaolNum gives the index of the
    % first goal in !.Goals.
    %
:- pred build_goals_before_greedy(implicit_parallelism_info::in, 
    dependency_graphs::in, list(int)::in(non_empty_list),
    list(pard_goal_detail)::out, int::in, int::out, 
    list(pard_goal_detail)::in, list(pard_goal_detail)::out) is det.

build_goals_before_greedy(Info, DependencyMaps, CostlyCallIndexes,
        GoalsBefore, !ConjNum, !Goals) :-
    build_goals_par_before_first_call(Info, DependencyMaps, CostlyCallIndexes,
        [], GoalsBeforeRev, [], GoalsParRev, !ConjNum, !Goals),
    reverse(GoalsBeforeRev, GoalsBefore),
    !:Goals = reverse(GoalsParRev) ++ !.Goals,
    !:ConjNum = !.ConjNum - length(GoalsParRev).

:- pred build_goals_par_before_first_call(implicit_parallelism_info::in,
    dependency_graphs::in, list(int)::in(non_empty_list),
    list(pard_goal_detail)::in, list(pard_goal_detail)::out,
    list(pard_goal_detail)::in, list(pard_goal_detail)::out,
    int::in, int::out,
    list(pard_goal_detail)::in, list(pard_goal_detail)::out) is det.

build_goals_par_before_first_call(Info, DependencyMaps, CostlyCallIndexes,
        !GoalsBeforeRev, !GoalsParRev, !ConjNum, !Goals) :-
    Goals0 = !.Goals,
    (
        !.Goals = [Goal | !:Goals],
        CostlyCallIndexes = [FirstCostlyCallIndex | OtherCostlyCallIndexes],
        (
            !.ConjNum >= FirstCostlyCallIndex
        ->
            % This goal is the first costly call.  This group mustn't be
            % included in the goals before the parallel conjunction.
            % Put it back before we return.
            !:Goals = Goals0
        ;
            depends_lookup_tc(DependencyMaps, !.ConjNum, DepsTC),
            depends_lookup(DependencyMaps, !.ConjNum, Deps),
            (
                (
                    % This goal provides a direct dependency to a costly call
                    % other than the first costly call.  It must be scheduled
                    % before the parallel conjunction.
                    some [Dep] (
                        member(Dep, Deps),
                        member(Dep, OtherCostlyCallIndexes)
                    )
                ;
                    % This goal provides a dependency to a costly call other
                    % than the first and does not provide a dependency to the
                    % first.
                    some [Dep] (
                        member(Dep, DepsTC),
                        member(Dep, OtherCostlyCallIndexes)
                    )
                    % XXX: Comment this out.  This means that we avoid some
                    % dependencies at the cost of not generating some more
                    % optimal solutions.  What we actually want here is to
                    % sequentialize this goal if it provides a transitive
                    % dependency to a later call that the first costly call is
                    % _not_ on the same dependecy path.
                    %not (
                    %    member(FirstCostlyCallIndex, DepsTC)
                    %)
                )
            ->
                % Schedule this goal and all of !.GoalsParRev before the
                % paralle conjunction.
                !:GoalsBeforeRev = [Goal | !.GoalsParRev ++ !.GoalsBeforeRev],
                !:GoalsParRev = []
            ;
                !:GoalsParRev = [Goal | !.GoalsParRev]
            ),
            !:ConjNum = !.ConjNum + 1,
            build_goals_par_before_first_call(Info, DependencyMaps,
                CostlyCallIndexes, !GoalsBeforeRev, !GoalsParRev, !ConjNum,
                !Goals)
        )
    ;
        !.Goals = []
    ).

:- pred build_parallel_conjuncts_greedy(implicit_parallelism_info::in,
    dependency_graphs::in, list(int)::in, int::in, list(pard_goal_detail)::in,
    list(pard_goal_detail)::in, list(pard_goal_detail)::out, int::in, int::out, 
    list(goal_group(goal_classification))::in, 
    list(goal_group(goal_classification))::out, 
    incomplete_parallelisation::in, incomplete_parallelisation::out) is det.

build_parallel_conjuncts_greedy(Info, DependencyMaps, CostlyCallIndexes,
        NumConjuncts0, InbetweenGoals0, !LastParConj, !GoalNum, !GoalGroups,
        !Parallelisation) :-
    (
        !.GoalGroups = [GoalGroup | !:GoalGroups],
        gg_get_details(GoalGroup, NumGoals, Goals, Classification),
        
        (
            Classification = gc_cheap_goals,
            % These cheap goals may be parallelised in between some other goals
            % once we find a costly goal.
            InbetweenGoals = InbetweenGoals0 ++ Goals, 
            NumConjuncts = NumConjuncts0
        ;
            Classification = gc_costly_goals,
            % Find the best construction of the most recent two parallel
            % conjuncts.
            (
                !.LastParConj = [],
                % There is no last parallel conjunction, the current goals will
                % become the last one and the decision will be deffered.
                !:LastParConj = InbetweenGoals0 ++ Goals,
                InbetweenGoals = [],
                NumConjuncts = NumConjuncts0
            ;
                !.LastParConj = [_ | _],
                build_parallel_conjuncts_greedy_pair(Info,
                    InbetweenGoals0, Goals, NumConjuncts0, !LastParConj, 
                    !Parallelisation),
                InbetweenGoals = [],
                NumConjuncts = NumConjuncts0 + 1
            )
        ),
        !:GoalNum = !.GoalNum + NumGoals,
        build_parallel_conjuncts_greedy(Info, DependencyMaps, CostlyCallIndexes,
            NumConjuncts, InbetweenGoals, !LastParConj, !GoalNum, !GoalGroups,
            !Parallelisation)
    ;
        !.GoalGroups = [],
        (
            InbetweenGoals0 = [_ | _],
            !:GoalGroups = [new_group(InbetweenGoals0, gc_cheap_goals)]
        ;
            InbetweenGoals0 = []
        )
    ).

:- pred build_parallel_conjuncts_greedy_pair(implicit_parallelism_info::in,
    list(pard_goal_detail)::in, list(pard_goal_detail)::in, int::in,
    list(pard_goal_detail)::in, list(pard_goal_detail)::out,
    incomplete_parallelisation::in, incomplete_parallelisation::out) is det.

build_parallel_conjuncts_greedy_pair(Info, InbetweenGoals, 
        Goals, ConjunctNum, !LastParConj, !Parallelisation) :-
    ParalleliseDepConjs = Info ^ ipi_opts ^ cpcp_parallelise_dep_conjs,
    branch_and_bound(
        (semipure pred(_State::in, Parallelisation::out) is nondet :-
            append(GoalsA, GoalsB, InbetweenGoals),
            
            ParConjA = !.LastParConj ++ GoalsA,
            calculate_parallel_cost_step(Info, is_not_innermost_conjunct,
                ConjunctNum, ParConjA, !.Parallelisation, Parallelisation0),
            Overlap0 = Parallelisation0 ^ ip_par_exec_overlap,
            par_conj_overlap_is_dependent(Overlap0, IsDependent0),
            (
                ParalleliseDepConjs = do_not_parallelise_dep_conjs
            =>
                IsDependent0 = conjuncts_are_independent
            ),
            
            ParConjB = GoalsB ++ Goals,
            calculate_parallel_cost_step(Info, is_not_innermost_conjunct,
                ConjunctNum + 1, ParConjB, Parallelisation0, Parallelisation1),
            Overlap1 = Parallelisation1 ^ ip_par_exec_overlap,
            par_conj_overlap_is_dependent(Overlap1, IsDependent1),
            (
                ParalleliseDepConjs = do_not_parallelise_dep_conjs
            =>
                IsDependent1 = conjuncts_are_independent
            ),

            finalise_parallelisation([], Parallelisation1, Parallelisation)
        ),
        parallelisation_get_objective_value, Parallelisations, _),
    (
        promise_equivalent_solutions [BestParallelisation]
        member(BestParallelisation, Parallelisations)
    ->
        ParConjsRev = reverse(BestParallelisation ^ bp_par_conjs),
        ( 
            ParConjsRev = 
                [seq_conj(LastParConjPrime) | [seq_conj(SndLastParConj) | _]] 
        ->
            % Commit to the second last parallel conjunction.
            calculate_parallel_cost_step(Info, is_not_innermost_conjunct,
                ConjunctNum, SndLastParConj, !Parallelisation),         
 
            % Save the last parallel conjunction as part of the next one.
            !:LastParConj = LastParConjPrime 
        ;
            error(this_file ++ "Expected at least 2 parallel conjuncts")
        )
    ;
        (
            ( ParalleliseDepConjs = parallelise_dep_conjs_overlap
            ; ParalleliseDepConjs = parallelise_dep_conjs_num_vars
            ; ParalleliseDepConjs = parallelise_dep_conjs_naive
            ),
            error(this_file ++ "Execpted at least one solution from bnb solver")
        ;
            ParalleliseDepConjs = do_not_parallelise_dep_conjs,
            % Try again to get the best dependent parallelisation.  This is
            % used for guided parallelisation.
            TempInfo = Info ^ ipi_opts ^ cpcp_parallelise_dep_conjs := 
                parallelise_dep_conjs_overlap,
            build_parallel_conjuncts_greedy_pair(TempInfo, InbetweenGoals,
                Goals, ConjunctNum, !LastParConj, !Parallelisation)
        )
    ). 

:- pred build_goals_after_greedy(implicit_parallelism_info::in, 
    int::in, list(pard_goal_detail)::in, 
    list(pard_goal_detail)::in, list(pard_goal_detail)::out, 
    incomplete_parallelisation::in, incomplete_parallelisation::out) is det.

build_goals_after_greedy(Info, ConjunctNum, !.LastParConj, !LastGoals,
        !Parallelisation) :-
    ParalleliseDepConjs = Info ^ ipi_opts ^ cpcp_parallelise_dep_conjs,
    
    branch_and_bound(
        (semipure pred(_State::in, (Parallelisation - LastGoals)::out) 
                is nondet :-
            append(GoalsPar, LastGoals, !.LastGoals),
            ParConjTest = !.LastParConj ++ GoalsPar,
            calculate_parallel_cost_step(Info, is_innermost_conjunct,
                ConjunctNum, ParConjTest, !.Parallelisation, Parallelisation0),
            Overlap0 = Parallelisation0 ^ ip_par_exec_overlap,
            par_conj_overlap_is_dependent(Overlap0, IsDependent0),
            (
                ParalleliseDepConjs = do_not_parallelise_dep_conjs
            =>
                IsDependent0 = conjuncts_are_independent
            ),
            finalise_parallelisation([], Parallelisation0, Parallelisation)
        ),
        (func(Parallelisation - _) =
            parallelisation_get_objective_value(Parallelisation)),
        Parallelisations, _),
    
    (
        promise_equivalent_solutions [BestParallelisationPair]
        member(BestParallelisationPair, Parallelisations)
    ->
        BestParallelisationPair = BestParallelisation - !:LastGoals,
        ParConjsRev = reverse(BestParallelisation ^ bp_par_conjs),
        ( 
            ParConjsRev = [seq_conj(LastParConjPrime) | _] 
        ->
            % Commit to the last parallel conjunction.
            calculate_parallel_cost_step(Info, is_innermost_conjunct,
                ConjunctNum, LastParConjPrime, !Parallelisation)
        ;
            error(this_file ++ "Expected at least 1 parallel conjunct")
        )
    ;
        (
            ( ParalleliseDepConjs = parallelise_dep_conjs_overlap
            ; ParalleliseDepConjs = parallelise_dep_conjs_num_vars
            ; ParalleliseDepConjs = parallelise_dep_conjs_naive
            ),
            error(this_file ++ "Execpted at least one solution from bnb solver")
        ;
            ParalleliseDepConjs = do_not_parallelise_dep_conjs,
            % Try again to get the best dependent parallelisation.  This is
            % used for guided parallelisation.
            TempInfo = Info ^ ipi_opts ^ cpcp_parallelise_dep_conjs := 
                parallelise_dep_conjs_overlap,
            build_goals_after_greedy(TempInfo, ConjunctNum, !.LastParConj,
                !LastGoals, !Parallelisation)
        )
    ). 

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
                dce_execution           :: assoc_list(float, float),
                    % Pairs of start and stop times of the execution.  Assume
                    % that the list is not sorted.

                dce_productions         :: map(var_rep, float),
                    % The variable productions.  This may be a superset of the
                    % dependent variables.

                dce_consumptions        :: map(var_rep, float)
                    % The variable consumptions.  This will contain only
                    % references for those variables that will become futures.
            ).

:- type is_innermost_conjunct
    --->    is_innermost_conjunct
    ;       is_not_innermost_conjunct.

    % calculate_parallel_cost(Info, Conjunctions, IsInnermostConjunct,
    %   ParallelExecMetrics, ParallelExecOverlap, ProductionsMap, NumConjuncts).
    %
    % Analyse the parallel conjuncts and determine their likely performance.
    % This code performs one step of the computation the steps are linked
    % together by find_best_parallelisation.
    %
    % This is the new parallel execution overlap algorithm, it is general and
    % therefore we also use is for independent conjunctions.
    %
    % + CallerVars is the set of vars for which our caller wants us to build a
    %   productions map for.
    %
    % + ParallelExecOpverlap: A representation of how the parallel conjuncts'
    %   executions overlap.
    %
    % + ProductionsMap: A productions map that covers the production of
    %   CallerVars.
    %
:- pred calculate_parallel_cost_step(implicit_parallelism_info::in,
    is_innermost_conjunct::in, int::in, list(pard_goal_detail)::in,
    incomplete_parallelisation::in, incomplete_parallelisation::out) is det.

calculate_parallel_cost_step(Info, IsInnermostConjunct, NumConjuncts, Goals,
        !Parallelisation) :-
    Conjs0 = !.Parallelisation ^ ip_par_conjs,
    Conjs = Conjs0 ++ [seq_conj(Goals)],
    !Parallelisation ^ ip_par_conjs := Conjs,
    Algorithm = Info ^ ipi_opts ^ cpcp_parallelise_dep_conjs,
   
    ProductionsMap0 = !.Parallelisation ^ ip_productions_map,

    conj_calc_cost(Goals, CostB0),
    CostB = goal_cost_get_percall(CostB0),
    foldl(pardgoal_consumed_vars_accum, Goals, set.init,
        RightConsumedVars),
    ProducedVars = 
        set.from_sorted_list(map.sorted_keys(ProductionsMap0)),
    Vars = set.intersect(ProducedVars, RightConsumedVars),

    % This conjunct will actually start after it has been sparked by
    % the prevous conjunct.  Which in turn may have been sparked by an
    % earlier conjunct.
    SparkDelay = Info ^ ipi_opts ^ cpcp_sparking_delay, 
    StartTime0 = float(NumConjuncts * SparkDelay),

    % If there are conjuncts after this conjunct we will have the
    % additional cost of sparking them.
    (
        IsInnermostConjunct = is_not_innermost_conjunct,
        % The cost of sparking a computation, (that is calling fork) is charged
        % to the leftmost conjunct.
        SparkCost = Info ^ ipi_opts ^ cpcp_sparking_cost,
        StartTime = StartTime0 + float(SparkCost)
    ;
        IsInnermostConjunct = is_innermost_conjunct,
        StartTime = StartTime0
    ),

    (
        Algorithm = parallelise_dep_conjs_overlap,

        % Get the list of variables consumed by this conjunct that will be
        % turned into futures.
        foldl3(get_consumptions_list, Goals, Vars, _, 0.0, _, 
            [], ConsumptionsList0),
        reverse(ConsumptionsList0, ConsumptionsList),

        % Determine how the parallel conjuncts overlap.
        foldl5(calculate_dependent_parallel_cost_2(Info, ProductionsMap0), 
            ConsumptionsList, 0.0, LastSeqConsumeTime, 
            StartTime, LastParConsumeTime, StartTime, LastResumeTime, 
            [], RevExecution0, map.init, ConsumptionsMap),

        % Calculate the point at which this conjunct finishes execution and
        % complete the RevExecutions structure..
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

    Metrics0 = !.Parallelisation ^ ip_par_exec_metrics,
    Metrics = init_parallel_exec_metrics_incomplete(Metrics0, CostSignals,
        CostB, CostBPar),
    !Parallelisation ^ ip_par_exec_metrics := Metrics,

    % Build the productions map for our parent.  This map contains all
    % the variables produced by this code, not just that are used for
    % dependent parallelisation.
    foldl3(get_productions_map, Goals, StartTime, _, Execution, _,
        ProductionsMap0, ProductionsMap),
    !Parallelisation ^ ip_productions_map := ProductionsMap,

    DepConjExec = dependent_conjunct_execution(Execution,
        ProductionsMap, ConsumptionsMap),
    Overlap0 = !.Parallelisation ^ ip_par_exec_overlap,
    Overlap = peo_conjunction(Overlap0, DepConjExec, Vars),
    !Parallelisation ^ ip_par_exec_overlap := Overlap.

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
    % Q cannot consume the variable until P produces it.  Also Q cannot consume
    % the variable until it is ready for it.  These are the two parameters to
    % max/2.
    %
    % The second parameter can be explained further, Q may have waited on a
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
        % True if Q had to suspend waiting for P,  Not that we don't include
        % FutureSyncTime here.  This is true if Q has to block at all even if
        % it can be made runable before the context switch is complete.
        ProdTime > ParConsTimeNotBlocked 
    ->
        % Include the time that it may take to resume this thread.
        ParConsTime = ParConsTime0 +
            float(Info ^ ipi_opts ^ cpcp_context_wakeup_delay),
        !:RevExecution = 
            [ (!.ResumeTime - ParConsTimeNotBlocked) | !.RevExecution ],
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
    ---> dependency_graphs(
            dm_forward              :: digraph(int),
            dm_forward_tc           :: digraph(int)
        ).

:- pred build_dependency_graphs(list(pard_goal_detail)::in, 
    dependency_graphs::out) is det.

build_dependency_graphs(Goals, Maps) :-
    Graph0 = digraph.init,
    build_dependency_graph(Goals, 1, map.init, _VarDepMap, 
        Graph0, Graph),
    Maps = dependency_graphs(Graph, tc(Graph)).

:- pred depends_lookup(dependency_graphs::in, int::in, set(int)::out) is det.

depends_lookup(DependencyGraphs, GoalNum, Deps) :-
    graph_do_lookup(lookup_from, DependencyGraphs ^ dm_forward, GoalNum, 
        Deps).

:- pred depends_lookup_tc(dependency_graphs::in, int::in, set(int)::out) is det.

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
    % Since we re-sort the list we don't need a sorted one to start with, but
    % the set module doesn't export a "to_list" predicate,  (Sorting has no
    % cost since the set is a sorted list internally).
    set.to_sorted_list(ConsumptionTimesSet0, ConsumptionTimes0),
    sort((pred((_ - TimeA)::in, (_ - TimeB)::in, Result::out) is det :-
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

    % Check if it is appropriate to parallelise this call.  That is it must be
    % model_det and have a cost above the call site cost threshold.
    %
:- pred can_parallelise_goal(implicit_parallelism_info::in,
    detism_rep::in, goal_cost_csq::in) is semidet.

can_parallelise_goal(Info, Detism, Cost) :-
    ( Detism = det_rep
    ; Detism = cc_multidet_rep ),
    goal_cost_get_calls(Cost) > 0,
    PercallCost = goal_cost_get_percall(Cost),
    PercallCost > float(Info ^ ipi_opts ^ cpcp_call_site_threshold).

:- pred atomic_pard_goal_type(implicit_parallelism_info::in, goal_path::in,
    atomic_goal_rep::in, inst_map_info::in, pard_goal_type::out, 
    cord(message)::out) is det.

atomic_pard_goal_type(Info, GoalPath, AtomicGoal, InstMapInfo, GoalType,
        !:Messages) :-
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
        map.lookup(Info ^ ipi_call_sites, GoalPath, CallSite),
        map_foldl(compute_var_modes(InstMapBefore, InstMapAfter),
            Args, VarsAndModes, 0, _),
        GoalType = pgt_call(VarsAndModes, CallSite) 
    ).

:- pred atomic_pard_goal_cost(implicit_parallelism_info::in, goal_path::in,
    atomic_goal_rep::in, goal_cost_csq::out) is det.

atomic_pard_goal_cost(Info, GoalPath, AtomicGoal, Cost) :-
    atomic_goal_is_call(AtomicGoal, IsCall),
    (
        IsCall = atomic_goal_is_trivial,
        % XXX: Should include the number of calls here since the 0 makes this
        % code appear to be dead when it probably isn't.
        Cost = atomic_goal_cost
    ;
        IsCall = atomic_goal_is_call(_),
        map.lookup(Info ^ ipi_call_sites, GoalPath, CallSite),
        (
            cost_and_callees_is_recursive(Info ^ ipi_clique, CallSite), 
            map.search(Info ^ ipi_rec_call_sites, GoalPath, RecCost) 
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

:- pred atomic_goal_build_use_map(atomic_goal_rep::in, goal_path::in, 
    implicit_parallelism_info::in, var_use_type::in, var_rep::in, 
    map(var_rep, lazy(var_use_info))::in,
    map(var_rep, lazy(var_use_info))::out) is det.

atomic_goal_build_use_map(AtomicGoal, GoalPath, Info, VarUseType, Var,
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
        LazyUse = delay((func) = compute_var_use_lazy(Info, GoalPath, Var,
            Args, VarUseType))
    ),
    svmap.det_insert(Var, LazyUse, !Map).

:- func compute_var_use_lazy(implicit_parallelism_info, goal_path, 
    var_rep, list(var_rep), var_use_type) = var_use_info.

compute_var_use_lazy(Info, GoalPath, Var, Args, VarUseType) = Use :-
    CliquePtr = Info ^ ipi_clique,
    map.lookup(Info ^ ipi_call_sites, GoalPath, CostAndCallee),
    ( 
        cost_and_callees_is_recursive(CliquePtr, CostAndCallee),
        map.search(Info ^ ipi_rec_call_sites, GoalPath, RecCost)
    ->
        Cost = RecCost
    ;
        Cost = CostAndCallee ^ cac_cost
    ),
    
    solutions(compute_var_use_lazy_arg(Info, Var, Args, CostAndCallee,
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
            error(this_file ++ "Too many solutions to compute_var_use_lazy_arg"
                ++ " for a production")
        )
    ).

:- pred earliest_use(var_use_info::in, var_use_info::in, var_use_info::out) is det.

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

compute_var_use_lazy_arg(Info, Var, Args, CostAndCallee, Cost, VarUseType, Use) :-
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
                    error(this_file ++ 
                        "First-order call site has wrong number of CSDs")
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
            require(unify(VarUseType, var_use_consumption), this_file ++ 
                "Var use type most be consumption if \\+ member(Var, Args)")
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
        append_message(call_site_dynamic(CSDPtr), 
            warning_cannot_compute_first_use_time(Error),
            !Messages)
    ).

:- pred goal_build_use_map(goal_rep(coverage_and_instmap_info)::in,
    goal_path::in, goal_cost_csq::in, implicit_parallelism_info::in,
    var_use_type::in, var_rep::in, 
    map(var_rep, lazy(var_use_info))::in, 
    map(var_rep, lazy(var_use_info))::out) is det.

goal_build_use_map(Goal, GoalPath, Cost, Info, VarUseType, Var, !Map) :-
    LazyUse = delay((func) = compute_goal_var_use_lazy(Goal, GoalPath, Cost,
        Info, VarUseType, Var)),
    svmap.det_insert(Var, LazyUse, !Map).

:- func compute_goal_var_use_lazy(goal_rep(coverage_and_instmap_info),
    goal_path, goal_cost_csq, implicit_parallelism_info, var_use_type,
    var_rep) = var_use_info.

compute_goal_var_use_lazy(Goal, GoalPath, Cost, Info, VarUseType, Var) = Use :-
    Info = implicit_parallelism_info(Deep, _ProgRep, _Params, CliquePtr,
        CallSiteMap, RecursiveCallSiteMap, RecursionType, _VarTable,
        _ProcLabel),
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
            RecursionType, RecDepth, Goal, GoalPath, CostPercall, Var,
            VarUseOptions, Use)
    ;
        ( RecursionType = rt_divide_and_conquer(_, _)
        ; RecursionType = rt_mutual_recursion(_)
        ; RecursionType = rt_other(_)
        ; RecursionType = rt_errors(_)
        ),
        % var_first_use doesn't work for these recursion types.
        pessimistic_var_use_info(VarUseType, CostPercall, Use),
        append_message(clique(CliquePtr), warning_cannot_compute_first_use_time(
            "Recursion type unknown for var_first_use/12"),
            empty, Messages),
        trace [io(!IO)] (
            io.stderr_stream(Stderr, !IO),
            write_out_messages(Stderr, Messages, !IO)
        )
    ).

:- instance goal_annotation_with_coverage(coverage_and_instmap_info) where [
        (get_coverage(Goal) = Goal ^ goal_annotation ^ cai_coverage)
    ].

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
        RecursionType = rt_single(_, _, DepthF, _, _),
        MaybeDepth = yes(recursion_depth_from_float(DepthF / 2.0))
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
    ;       is_costly_atomic_goal
    ;       is_costly_compound_goal.

:- pred identify_costly_goal(pard_goal_detail::in, is_costly_goal::out) is det.

identify_costly_goal(Goal, Costly) :-
    CostAboveThreshold = Goal ^ goal_annotation ^ pgd_cost_above_threshold,
    (
        CostAboveThreshold = cost_above_par_threshold,
        GoalType = Goal ^ goal_annotation ^ pgd_pg_type,
        (
            GoalType = pgt_call(_, _),
            Costly = is_costly_atomic_goal
        ;
            GoalType = pgt_other_atomic_goal,
            error(this_file ++ "pgt_other_atomic_goal is never costly")
        ;
            GoalType = pgt_non_atomic_goal,
            % TODO: distinguish between compound goals with one branch that is
            % costly, and compound goals where all branches are costly.
            % TODO: Provide information about how many costly goals are within
            % the goal so that we can try to parallelise each of those against
            % an outer costly goal.
            Costly = is_costly_compound_goal
        )
    ;
        CostAboveThreshold = cost_not_above_par_threshold,
        Costly = is_not_costly_goal
    ).

:- pred var_get_mode(inst_map::in, inst_map::in, var_rep::in, var_mode_rep::out)
    is det.

var_get_mode(InstMapBefore, InstMapAfter, VarRep, VarModeRep) :-
    inst_map_get(InstMapBefore, VarRep, InstBefore, _),
    inst_map_get(InstMapAfter, VarRep, InstAfter, _),
    VarModeRep = var_mode_rep(InstBefore, InstAfter).

    % Transform a goal in a conjunction into a pard_goal.
    %
:- pred goal_to_pard_goal(implicit_parallelism_info::in, goal_path::in,
    goal_rep(coverage_and_instmap_info)::in, pard_goal_detail::out,
    cord(message)::in, cord(message)::out) is det.

goal_to_pard_goal(Info, GoalPath, !Goal, !Messages) :-
    !.Goal = goal_rep(GoalExpr0, Detism, CoverageAndInstMapInfo),
    InstMapInfo = CoverageAndInstMapInfo ^ cai_inst_map_info,
    Coverage = CoverageAndInstMapInfo ^ cai_coverage,
    get_coverage_before_det(Coverage, Before),
    (
        (
            GoalExpr0 = conj_rep(Conjs0),
            map_foldl2(conj_to_pard_goals(Info, GoalPath), Conjs0, Conjs, 1, _,
                !Messages),
            conj_calc_cost(Conjs, Cost),
            GoalExpr = conj_rep(Conjs)
        ;
            GoalExpr0 = disj_rep(Disjs0),
            map_foldl2(disj_to_pard_goals(Info, GoalPath), Disjs0, Disjs, 1, _,
                !Messages),
            disj_calc_cost(Disjs, Cost),
            GoalExpr = disj_rep(Disjs)
        ;
            GoalExpr0 = switch_rep(Var, CanFail, Cases0),
            map_foldl2(case_to_pard_goal(Info, GoalPath), Cases0, Cases, 1, _,
                !Messages),
            switch_calc_cost(Cases, Before, Cost),
            GoalExpr = switch_rep(Var, CanFail, Cases)
        ; 
            GoalExpr0 = ite_rep(Cond0, Then0, Else0),
            goal_to_pard_goal(Info, 
                goal_path_add_at_end(GoalPath, step_ite_cond), Cond0, Cond, 
                !Messages),
            goal_to_pard_goal(Info, 
                goal_path_add_at_end(GoalPath, step_ite_then), Then0, Then, 
                !Messages),
            goal_to_pard_goal(Info, 
                goal_path_add_at_end(GoalPath, step_ite_else), Else0, Else,
                !Messages),
            ite_calc_cost(Cond, Then, Else, Cost),
            GoalExpr = ite_rep(Cond, Then, Else)
        ; 
            GoalExpr0 = negation_rep(SubGoal0),
            goal_to_pard_goal(Info, goal_path_add_at_end(GoalPath, step_neg),
                SubGoal0, SubGoal, !Messages),
            Cost = SubGoal ^ goal_annotation ^ pgd_cost,
            GoalExpr = negation_rep(SubGoal)
        ; 
            GoalExpr0 = scope_rep(SubGoal0, MaybeCut),
            goal_to_pard_goal(Info, 
                goal_path_add_at_end(GoalPath, step_scope(MaybeCut)),
                SubGoal0, SubGoal, !Messages),
            Cost = SubGoal ^ goal_annotation ^ pgd_cost,
            GoalExpr = scope_rep(SubGoal, MaybeCut)
        ),
        PardGoalType = pgt_non_atomic_goal,

        BoundVars = to_sorted_list(InstMapInfo ^ im_bound_vars),
        foldl(goal_build_use_map(!.Goal, GoalPath, Cost, Info,
                var_use_production),
            BoundVars, map.init, ProductionUseMap),
        ConsumedVars = to_sorted_list(InstMapInfo ^ im_consumed_vars),
        foldl(goal_build_use_map(!.Goal, GoalPath, Cost, Info,
                var_use_consumption),
            ConsumedVars, map.init, ConsumptionUseMap)
    ;
        GoalExpr0 = atomic_goal_rep(Context, Line, BoundVars, AtomicGoal),
        GoalExpr = atomic_goal_rep(Context, Line, BoundVars, AtomicGoal),
        atomic_pard_goal_type(Info, GoalPath, AtomicGoal, InstMapInfo,
            PardGoalType, Messages),
        atomic_pard_goal_cost(Info, GoalPath, AtomicGoal, Cost),
        
        foldl(atomic_goal_build_use_map(AtomicGoal, GoalPath, Info, 
                var_use_production),
            BoundVars, map.init, ProductionUseMap),
        ConsumedVars = InstMapInfo ^ im_consumed_vars,
        foldl(atomic_goal_build_use_map(AtomicGoal, GoalPath, Info, 
                var_use_consumption), 
            to_sorted_list(ConsumedVars), map.init, ConsumptionUseMap),
        
        !:Messages = !.Messages ++ Messages
    ),
    % XXX: The goal annotations cannot represent reasons why a goal
    % can't be parallelised, for example it could be nondet, semidet or
    % impure.
    ( can_parallelise_goal(Info, Detism, Cost) ->
        CostAboveThreshold = cost_above_par_threshold
    ;
        CostAboveThreshold = cost_not_above_par_threshold
    ),
    PardGoalAnnotation = pard_goal_detail(PardGoalType, InstMapInfo, GoalPath, 
        Coverage, Cost, CostAboveThreshold, ProductionUseMap,
        ConsumptionUseMap),
    !:Goal = goal_rep(GoalExpr, Detism, PardGoalAnnotation).

:- pred conj_to_pard_goals(implicit_parallelism_info::in, goal_path::in,
    goal_rep(coverage_and_instmap_info)::in, pard_goal_detail::out, 
    int::in, int::out, cord(message)::in, cord(message)::out) is det.

conj_to_pard_goals(Info, GoalPath0, !Goal, !ConjNum, !Messages) :-
    GoalPath = goal_path_add_at_end(GoalPath0, step_conj(!.ConjNum)),
    goal_to_pard_goal(Info, GoalPath, !Goal, !Messages),
    !:ConjNum = !.ConjNum + 1.

:- pred disj_to_pard_goals(implicit_parallelism_info::in, goal_path::in,
    goal_rep(coverage_and_instmap_info)::in, pard_goal_detail::out, 
    int::in, int::out, cord(message)::in, cord(message)::out) is det.

disj_to_pard_goals(Info, GoalPath0, !Goal, !DisjNum, !Messages) :-
    GoalPath = goal_path_add_at_end(GoalPath0, step_disj(!.DisjNum)),
    goal_to_pard_goal(Info, GoalPath, !Goal, !Messages),
    !:DisjNum = !.DisjNum + 1.

:- pred case_to_pard_goal(implicit_parallelism_info::in, goal_path::in,
    case_rep(coverage_and_instmap_info)::in, 
    case_rep(pard_goal_detail_annotation)::out, int::in, int::out, 
    cord(message)::in, cord(message)::out) is det.

case_to_pard_goal(Info, GoalPath0, !Case, !CaseNum, !Messages) :-
    !.Case = case_rep(ConsId, OtherConsId, Goal0),
    GoalPath = goal_path_add_at_end(GoalPath0, step_switch(!.CaseNum, no)),
    goal_to_pard_goal(Info, GoalPath, Goal0, Goal, !Messages),
    !:CaseNum = !.CaseNum + 1,
    !:Case = case_rep(ConsId, OtherConsId, Goal).

%----------------------------------------------------------------------------%

:- pred conj_calc_cost(list(pard_goal_detail)::in, goal_cost_csq::out) 
    is det.

conj_calc_cost([], zero_goal_cost).
conj_calc_cost([Conj | Conjs], Cost) :-
    conj_calc_cost(Conjs, ConjsCost),
    ConjCost = Conj ^ goal_annotation ^ pgd_cost,
    Cost = add_goal_costs(ConjsCost, ConjCost).

:- pred disj_calc_cost(list(pard_goal_detail)::in, goal_cost_csq::out) 
    is det.

disj_calc_cost([], zero_goal_cost).
disj_calc_cost([Disj | Disjs], Cost) :-
    Coverage = Disj ^ goal_annotation ^ pgd_coverage,
    get_coverage_before_det(Coverage, Before), 
    ( Before = 0 ->
        % Avoid a divide by zero.
        Cost = zero_goal_cost
    ;
        DisjCost = Disj ^ goal_annotation ^ pgd_cost,
        disj_calc_cost(Disjs, DisjsCost),
        % XXX: We assume this is a semidet disjunction
        Branch = add_goal_costs_branch(Before, DisjsCost, zero_goal_cost),
        Cost = add_goal_costs(DisjCost, Branch)
    ).

:- pred switch_calc_cost(list(case_rep(pard_goal_detail_annotation))::in,
    int::in, goal_cost_csq::out) is det.

switch_calc_cost([], _, zero_goal_cost).
switch_calc_cost([Case | Cases], TotalCalls, Cost) :-
    ( TotalCalls = 0 ->
        % Avoid a divide by zero.
        Cost = zero_goal_cost
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
    Cost = add_goal_costs(CondCost, ThenElseCost).

%----------------------------------------------------------------------------%
%
% Annotate a goal with instantiation information.
%

    % inst_map_info now contains information that it's not necessary to
    % contain.  Namely the im_after field can be calculated from the im_before
    % and im_bound_vars fields.  However since this information will probably
    % be attached to a different goal there is not much extra cost in having a
    % pointer to it from here.
    %
:- type inst_map_info
    --->    inst_map_info(
                im_before           :: inst_map,
                    % The inst map before this goal is executed.

                im_after            :: inst_map,
                    % The inst map after this goal was executed.

                im_consumed_vars    :: set(var_rep),
                    % Variables consumed (read but not bound) by this goal.

                im_bound_vars       :: set(var_rep)
                    % The variables produced by this goal.
            ).

:- typeclass goal_annotation_add_instmap(A, B) where [
        pred add_instmap(inst_map_info, A, B),
        mode add_instmap(in, in, out) is det
    ].

    % Note: It may be useful to add other annotations such as goal path or cost 
    % information.
    %
    % SeenDuplicateInstiation is used to assert that we're analysing single
    % assignment code only.
    %
    % Vars is the set of variables used by this goal, both consumed and
    % produced.
    %
:- pred goal_annotate_with_instmap(goal_rep(A)::in, goal_rep(B)::out, 
    inst_map::in, inst_map::out, seen_duplicate_instantiation::out, 
    set(var_rep)::out, set(var_rep)::out) is det 
    <= goal_annotation_add_instmap(A, B).

goal_annotate_with_instmap(Goal0, Goal, !InstMap, SeenDuplicateInstantiation,
        ConsumedVars, BoundVars) :-
    Goal0 = goal_rep(GoalExpr0, Detism, Ann0),
    InstMapBefore = !.InstMap,
    (
        GoalExpr0 = conj_rep(Conjs0),
        conj_annotate_with_instmap(Conjs0, Conjs, !InstMap, 
            SeenDuplicateInstantiation, ConsumedVars, BoundVars),
        GoalExpr = conj_rep(Conjs)
    ;
        GoalExpr0 = disj_rep(Disjs0),
        disj_annotate_with_instmap(Disjs0, Disjs, !InstMap,
            SeenDuplicateInstantiation, ConsumedVars, BoundVars),
        GoalExpr = disj_rep(Disjs)
    ;
        GoalExpr0 = switch_rep(Var, CanFail, Cases0),
        switch_annotate_with_instmap(Cases0, Cases, !InstMap,
            SeenDuplicateInstantiation, ConsumedVars0, BoundVars),
        set.insert(ConsumedVars0, Var, ConsumedVars),
        GoalExpr = switch_rep(Var, CanFail, Cases)
    ;
        GoalExpr0 = ite_rep(Cond0, Then0, Else0),
        ite_annotate_with_instmap(Cond0, Cond, Then0, Then, Else0, Else,
            !InstMap, SeenDuplicateInstantiation, ConsumedVars, BoundVars),
        GoalExpr = ite_rep(Cond, Then, Else)
    ;
        % XXX: Not all scope goals can produce variables, in fact some are used
        % to isolate variables that aren't named apart.  But other scope goals
        % can bind variables.  We don't know which we're looking at here. 
        GoalExpr0 = scope_rep(Subgoal0, MaybeCut),
        goal_annotate_with_instmap(Subgoal0, Subgoal, !InstMap, 
            SeenDuplicateInstantiation, ConsumedVars, BoundVars),
        GoalExpr = scope_rep(Subgoal, MaybeCut)
    ;
        GoalExpr0 = negation_rep(Subgoal0),
        % A negated goal cannot affect instantiation.
        goal_annotate_with_instmap(Subgoal0, Subgoal, !.InstMap, 
            _InstMap, SeenDuplicateInstantiation, ConsumedVars, _),
        BoundVars = set.init,
        GoalExpr = negation_rep(Subgoal)
    ;
        GoalExpr0 = atomic_goal_rep(File, Line, BoundVarsList, AtomicGoal),
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
            SeenDuplicateInstantiation),
        GoalExpr = atomic_goal_rep(File, Line, BoundVarsList, AtomicGoal) 
    ),
    InstMapAfter = !.InstMap,
    InstMapInfo = inst_map_info(InstMapBefore, InstMapAfter, ConsumedVars,
        BoundVars),
    add_instmap(InstMapInfo, Ann0, Ann), 
    Goal = goal_rep(GoalExpr, Detism, Ann).

:- pred conj_annotate_with_instmap(list(goal_rep(A))::in,
    list(goal_rep(B))::out, inst_map::in, inst_map::out,
    seen_duplicate_instantiation::out, set(var_rep)::out, set(var_rep)::out)
    is det <= goal_annotation_add_instmap(A, B).

conj_annotate_with_instmap([], [], !InstMap,
    have_not_seen_duplicate_instantiation, set.init, set.init).
conj_annotate_with_instmap([Conj0 | Conjs0], [Conj | Conjs], !InstMap, 
        SeenDuplicateInstantiation, ConsumedVars, BoundVars) :-
    goal_annotate_with_instmap(Conj0, Conj, !InstMap,
        SeenDuplicateInstantiationHead, ConsumedVarsHead, BoundVarsHead),
    conj_annotate_with_instmap(Conjs0, Conjs, !InstMap,
        SeenDuplicateInstantiationTail, ConsumedVarsTail, BoundVarsTail),
    set.union(ConsumedVarsTail, ConsumedVarsHead, ConsumedVars),
    set.union(BoundVarsTail, BoundVarsHead, BoundVars),
    SeenDuplicateInstantiation = merge_seen_duplicate_instantiation(
        SeenDuplicateInstantiationHead,
        SeenDuplicateInstantiationTail).

:- pred disj_annotate_with_instmap(list(goal_rep(A))::in,
    list(goal_rep(B))::out, inst_map::in, inst_map::out,
    seen_duplicate_instantiation::out, set(var_rep)::out, set(var_rep)::out)
    is det <= goal_annotation_add_instmap(A, B).

disj_annotate_with_instmap([], [], !InstMap,
        have_not_seen_duplicate_instantiation, set.init, set.init).
disj_annotate_with_instmap([Disj0 | Disjs0], [Disj | Disjs], InstMap0, InstMap,
        SeenDuplicateInstantiation, ConsumedVars, BoundVars) :-
    HeadDetism = Disj0 ^ goal_detism_rep,
    goal_annotate_with_instmap(Disj0, Disj, InstMap0, InstMapHead,
        SeenDuplicateInstantiationHead, ConsumedVarsHead, BoundVarsHead),
    disj_annotate_with_instmap(Disjs0, Disjs, InstMap0, InstMapTail,
        SeenDuplicateInstantiationTail, ConsumedVarsTail, BoundVarsTail),
    
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

:- pred switch_annotate_with_instmap(list(case_rep(A))::in, 
    list(case_rep(B))::out, inst_map::in, inst_map::out,
    seen_duplicate_instantiation::out, set(var_rep)::out, set(var_rep)::out) 
    is det <= goal_annotation_add_instmap(A, B).

switch_annotate_with_instmap([], [], !InstMap,
        have_not_seen_duplicate_instantiation, set.init, set.init).
switch_annotate_with_instmap([Case0 | Cases0], [Case | Cases], 
        InstMap0, InstMap, SeenDuplicateInstantiation, 
        ConsumedVars, BoundVars) :-
    Case0 = case_rep(ConsIdArity, ExtraConsIdAritys, Goal0),
    HeadDetism = Goal0 ^ goal_detism_rep,
    goal_annotate_with_instmap(Goal0, Goal, InstMap0, InstMapHead,
        SeenDuplicateInstantiationHead, ConsumedVarsHead, BoundVarsHead),
    Case = case_rep(ConsIdArity, ExtraConsIdAritys, Goal),
    switch_annotate_with_instmap(Cases0, Cases, InstMap0, InstMapTail,
        SeenDuplicateInstantiationTail, ConsumedVarsTail, BoundVarsTail),
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

:- pred ite_annotate_with_instmap(goal_rep(A)::in, goal_rep(B)::out,
    goal_rep(A)::in, goal_rep(B)::out,
    goal_rep(A)::in, goal_rep(B)::out,
    inst_map::in, inst_map::out, 
    seen_duplicate_instantiation::out, set(var_rep)::out, set(var_rep)::out) 
    is det <= goal_annotation_add_instmap(A, B).

ite_annotate_with_instmap(Cond0, Cond, Then0, Then, Else0, Else, InstMap0, InstMap,
        SeenDuplicateInstantiation, ConsumedVars, BoundVars) :-
    goal_annotate_with_instmap(Cond0, Cond, InstMap0, InstMapAfterCond,
        SeenDuplicateInstantiationCond, ConsumedVarsCond, _BoundVarsCond),
    goal_annotate_with_instmap(Then0, Then, InstMapAfterCond, InstMapAfterThen,
        SeenDuplicateInstantiationThen, ConsumedVarsThen, BoundVarsThen),
    goal_annotate_with_instmap(Else0, Else, InstMap0, InstMapAfterElse,
        SeenDuplicateInstantiationElse, ConsumedVarsElse, BoundVarsElse),
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

:- func this_file = string.

this_file = "mdprof_fb.automatic_parallelism.m: ".

%-----------------------------------------------------------------------------%

create_candidate_parallel_conj_proc_report(Proc - CandidateParConjunctionProc, 
        Report) :-
    CandidateParConjunctionProc = 
        candidate_par_conjunctions_proc(VarTable, CandidateParConjunctions),
    map(create_candidate_parallel_conj_report(VarTable, Proc), 
        CandidateParConjunctions, Reports),
    Report = cord_list_to_cord(Reports). 

:- pred create_candidate_parallel_conj_report(var_table::in,
    string_proc_label::in, candidate_par_conjunction(pard_goal)::in,
    cord(string)::out) is det.

create_candidate_parallel_conj_report(VarTable, Proc, CandidateParConjunction,
        Report) :-
    print_proc_label_to_string(Proc, ProcString),
    CandidateParConjunction = candidate_par_conjunction(GoalPathString,
        FirstConjNum, IsDependent, GoalsBefore, GoalsBeforeCost, Conjs,
        GoalsAfter, GoalsAfterCost, ParExecMetrics),
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
    format("      %s\n" ++
           "      Path: %s\n" ++
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
        [s(ProcString), 
         s(GoalPathString), 
         s(DependanceString),
         s(commas(NumCalls)), 
         s(two_decimal_fraction(SeqTime)),
         s(two_decimal_fraction(ParTime)),
         s(two_decimal_fraction(ParOverheads)),
         s(four_decimal_fraction(Speedup)),
         s(two_decimal_fraction(TimeSaving)),
         s(two_decimal_fraction(FirstConjDeadTime)),
         s(two_decimal_fraction(FutureDeadTime)),
         s(two_decimal_fraction(TotalDeadTime))],
        ReportHeaderStr),
    ReportHeader = singleton(ReportHeaderStr),

    ( goal_path_from_string(GoalPathString, GoalPathPrime) ->
        GoalPath = GoalPathPrime
    ;
        error(this_file ++ "couldn't parse goal path.")
    ),
    some [!ConjNum] (
        !:ConjNum = FirstConjNum,
        format_sequential_conjunction(VarTable, 4, GoalPath, GoalsBefore, 
            GoalsBeforeCost, !.ConjNum, ReportGoalsBefore0),
        ReportGoalsBefore = indent(3) ++ singleton("Goals before:\n") ++
            ReportGoalsBefore0,
        
        !:ConjNum = !.ConjNum + length(GoalsBefore),
        format_parallel_conjunction(VarTable, 4, GoalPath, !.ConjNum, Conjs,
            ReportParConj0),
        ReportParConj = indent(3) ++ singleton("Parallel conjunction:\n") ++
            ReportParConj0,
        
        !:ConjNum = !.ConjNum + 1,
        format_sequential_conjunction(VarTable, 4, GoalPath, GoalsAfter,
            GoalsAfterCost, !.ConjNum, ReportGoalsAfter0),
        ReportGoalsAfter = indent(3) ++ singleton("Goals after:\n") ++
            ReportGoalsAfter0
    ),
    Report = ReportHeader ++ ReportGoalsBefore ++ nl ++ ReportParConj ++ nl ++
        ReportGoalsAfter ++ nl.

:- pred format_parallel_conjunction(var_table::in, int::in,
    goal_path::in, int::in,
    list(seq_conj(pard_goal))::in, cord(string)::out) is det.

format_parallel_conjunction(VarTable, Indent, GoalPath0, ConjNum, Conjs,
        !:Report) :-
    IndentStr = indent(Indent),
    !:Report = IndentStr ++ singleton("(\n"),
    GoalPath = goal_path_add_at_end(GoalPath0, step_conj(ConjNum)),
    format_parallel_conjuncts(VarTable, Indent, GoalPath, 1, Conjs, !Report). 

:- pred format_parallel_conjuncts(var_table::in, int::in, goal_path::in, 
    int::in, list(seq_conj(pard_goal))::in, 
    cord(string)::in, cord(string)::out) is det.

format_parallel_conjuncts(_VarTable, Indent, _GoalPath, _ConjNum0, [], 
        !Report) :-
    IndentStr = indent(Indent),
    !:Report = snoc(!.Report ++ IndentStr, ")\n").
format_parallel_conjuncts(VarTable, Indent, GoalPath0, ConjNum0, 
        [Conj | Conjs], !Report) :-
    Conj = seq_conj(Goals),
    (
        Goals = [],
        error(this_file ++ " empty conjunct in parallel conjunction")
    ;
        Goals = [Goal | GoalsTail],
        GoalPath = goal_path_add_at_end(GoalPath0, step_conj(ConjNum0)),
        (
            GoalsTail = [],
            % A singleton conjunction gets printed as a single goal.
            print_goal_to_strings(VarTable, Indent + 1, GoalPath, Goal,
                ConjReport) 
        ;
            GoalsTail = [_ | _],
            Cost = foldl(
                (func(GoalI, Acc) = 
                    Acc + GoalI ^ goal_annotation ^ pga_cost_percall),
                Goals, 0.0),
            format_sequential_conjunction(VarTable, Indent + 1, GoalPath,
                Goals, Cost, 1, ConjReport)
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
    format_parallel_conjuncts(VarTable, Indent, GoalPath0, ConjNum, Conjs, 
        !Report).

:- pred format_sequential_conjunction(var_table::in, int::in, goal_path::in,
    list(pard_goal)::in, float::in, int::in, cord(string)::out) is det.

format_sequential_conjunction(VarTable, Indent, GoalPath, Goals, Cost,
        FirstConjNum, !:Report) :-
    !:Report = empty,
    ( FirstConjNum = 1 ->
        !:Report = !.Report ++
            indent(Indent) ++
            singleton(format("%% conjunction: %s",
                [s(goal_path_to_string(GoalPath))])) ++
            nl_indent(Indent) ++ 
            singleton(format("%% Cost: %s", [s(two_decimal_fraction(Cost))])) ++
            nl ++ nl
    ;
        true
    ),
    format_sequential_conjuncts(VarTable, Indent, GoalPath, Goals, 
        FirstConjNum, _, !Report).

:- pred format_sequential_conjuncts(var_table::in, int::in, goal_path::in,
    list(pard_goal)::in, int::in, int::out, 
    cord(string)::in, cord(string)::out) is det.

format_sequential_conjuncts(_, _, _, [], !ConjNum, !Report).
format_sequential_conjuncts(VarTable, Indent, GoalPath0, [Conj | Conjs],
        !ConjNum, !Report) :-
    GoalPath = goal_path_add_at_end(GoalPath0, step_conj(!.ConjNum)),
    print_goal_to_strings(VarTable, Indent, GoalPath, Conj, ConjReport),
    !:Report = !.Report ++ ConjReport,
    !:ConjNum = !.ConjNum + 1,
    (
        Conjs = []
    ;
        Conjs = [_ | _],
        !:Report = !.Report ++ indent(Indent) ++ singleton(",\n"),
        format_sequential_conjuncts(VarTable, Indent, GoalPath0, Conjs,
            !ConjNum, !Report)
    ).

:- instance goal_annotation(pard_goal_annotation) where [
        pred(print_goal_annotation_to_strings/3) is 
            format_pard_goal_annotation
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
            [s(two_decimal_fraction(CostPercall)),
             s(CostAboveThresholdStr)])),
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
    io.format("D: Not entering clique: %d, " ++
        "it is below the clique threashold\n  " ++
        "It has per-call cost %f and is called %f times\n\n",
        [i(CliqueNum), f(PercallCost), f(Calls)], !IO).

:- pred debug_cliques_exeeded_parallelism(candidate_child_clique::in,
    io::di, io::uo) is det.

debug_cliques_exeeded_parallelism(Clique, !IO) :-
    CliquePtr = Clique ^ ccc_clique,
    CliquePtr = clique_ptr(CliqueNum),
    io.format("D: Not entiring clique %d, " ++
        "no-more parallelisation resources available at this context\n\n",
        [i(CliqueNum)], !IO).

%-----------------------------------------------------------------------------%
:- end_module mdprof_fb.automatic_parallelism.
%-----------------------------------------------------------------------------%

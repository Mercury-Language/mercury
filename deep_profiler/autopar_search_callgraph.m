%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: autopar_search_callgraph.m
% Author: pbone.
%
% This module contains the code for analysing deep profiles of programs in
% order to determine how best to automatically parallelise the program.  This
% code is used by the mdprof_feedback tool.
%
%-----------------------------------------------------------------------------%

:- module mdprof_fb.automatic_parallelism.autopar_search_callgraph.
:- interface.

:- import_module profile.
:- import_module message.
:- import_module mdbcomp.
:- import_module mdbcomp.feedback.
:- import_module mdbcomp.feedback.automatic_parallelism.
:- import_module mdbcomp.program_representation.
:- import_module mdprof_fb.automatic_parallelism.autopar_types.

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

:- pred create_candidate_parallel_conj_proc_report(
    pair(string_proc_label, candidate_par_conjunctions_proc)::in,
    cord(string)::out) is det.

%-----------------------------------------------------------------------------%

% XXX temporary exports.

    % Check if it is appropriate to parallelise this call. That is it must be
    % model_det and have a cost above the call site cost threshold.
    %
:- pred can_parallelise_goal(goal_rep(T)::in) is semidet.

    % calculate_parallel_cost(Info, !Parallelisation).
    %
    % Analyse the parallel conjuncts and determine their likely performance.
    %
    % This is the new parallel execution overlap algorithm, it is general and
    % therefore we also use is for independent conjunctions.
    %
:- pred calculate_parallel_cost(parallelisation_cost_data::out,
    incomplete_parallelisation::in, incomplete_parallelisation::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdprof_fb.automatic_parallelism.autopar_annotate.
:- import_module mdprof_fb.automatic_parallelism.autopar_find_best_par.

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
        list.map(proc_dynamic_callees(Deep, ParentParallelism), PDPtrs,
            ChildCliquess),
        !:ChildCliques = cord_list_to_cord(ChildCliquess),
        list.map2(
            candidate_parallel_conjunctions_callee(Opts, Deep,
                CliquePtr, CliqueCandidates),
            cord.list(!.ChildCliques), CSCandidateLists, CSMessageCords)
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
    % CliquePtr: The current clique.
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
    CPCs = CPCsA ++ CPCsB,
    merge_pushes_for_proc(PushGoalsA ++ PushGoalsB, PushGoals),
    ( VarTableA = VarTableB ->
        Result = candidate_par_conjunctions_proc(VarTableA, PushGoals, CPCs)
    ;
        unexpected($module, $pred, "var tables do not match")
    ).

:- pred merge_pushes_for_proc(list(push_goal)::in, list(push_goal)::out)
    is det.

merge_pushes_for_proc([], []).
merge_pushes_for_proc(Pushes0 @ [_ | _], Pushes) :-
    list.foldl(insert_into_push_map, Pushes0, map.init, PushMap),
    map.foldl(extract_from_push_map, PushMap, [], Pushes).

:- pred insert_into_push_map(push_goal::in,
    map(goal_path_string, {int, int, set(goal_path_string)})::in,
    map(goal_path_string, {int, int, set(goal_path_string)})::out) is det.

insert_into_push_map(PushGoal, !Map) :-
    PushGoal = push_goal(GoalPathStr, Lo, Hi, TargetGoalPathStrs),
    ( map.search(!.Map, GoalPathStr, OldTriple) ->
        OldTriple = {OldLo, OldHi, OldTargetGoalPathStrSet},
        (
            Lo = OldLo,
            Hi = OldHi
        ->
            set.insert_list(OldTargetGoalPathStrSet, TargetGoalPathStrs,
                NewTargetGoalPathStrSet),
            NewTriple = {OldLo, OldHi, NewTargetGoalPathStrSet},
            svmap.det_update(GoalPathStr, NewTriple, !Map)
        ;
            % There seem to be separate push requests inside the same
            % conjunction that want to push different seets of conjuncts.
            % Since they could interfere with each other, we keep only one.
            % Since we don't have any good basis on which to make the choice,
            % we keep the earlier pushes.
            true
        )
    ;
        NewTriple = {Lo, Hi, set.list_to_set(TargetGoalPathStrs)},
        svmap.det_insert(GoalPathStr, NewTriple, !Map)
    ).

:- pred extract_from_push_map(goal_path_string::in,
    {int, int, set(goal_path_string)}::in,
    list(push_goal)::in, list(push_goal)::out) is det.

extract_from_push_map(GoalPathStr, Triple, !Pushes) :-
    Triple = {Lo, Hi, TargetGoalPathStrSet},
    Push = push_goal(GoalPathStr, Lo, Hi,
        set.to_sorted_list(TargetGoalPathStrSet)),
    !:Pushes = [Push | !.Pushes].

%----------------------------------------------------------------------------%
%
% Search for parallelisation opportunities within a procedure.
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

            % Label the goals with IDs.
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
                    (
                        Candidates0 = [],
                        Candidates = map.init
                    ;
                        Candidates0 = [_ | _],
                        merge_pushes_for_proc(Pushes, MergedPushes),
                        CandidateProc = candidate_par_conjunctions_proc(
                            VarTable, MergedPushes, Candidates0),
                        map.det_insert(map.init, ProcLabel, CandidateProc,
                            Candidates)
                    )
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
                Pushes = PushesBelow,
                Singles = cord.from_list(SinglesSoFar),
                Messages = MessagesBelow,
                Cost = Annotation0 ^ pgd_cost
            ;
                SingleCands = [CostlyIndex - SinglesBefore],
                push_and_build_candidate_conjunctions(Info, RevGoalPathSteps,
                    Conjs, CostlyIndex, SinglesBefore,
                    MessagesThisLevel, CandidatesThisLevel),
                (
                    CandidatesThisLevel = [],
                    Candidates = CandidatesBelow,
                    Pushes = PushesBelow,
                    % No candidate was built, pass our singles to our caller.
                    Singles = cord.from_list(SinglesSoFar)
                ;
                    CandidatesThisLevel = [FirstCandidate | LaterCandidates],
                    merge_same_level_pushes(FirstCandidate, LaterCandidates,
                        PushThisLevel),
                    Candidates = CandidatesBelow ++
                        cord.from_list(CandidatesThisLevel),
                    Pushes = cord.snoc(PushesBelow, PushThisLevel),
                    % Any single expensive goals inside this conjunction
                    % cannot have later expensive goals pushed next to them
                    % without reanalysis of the whole goal, which we do not do.
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
                Pushes = PushesBelow,
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
            )
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
        % This goal might be costly if it is pushed into the cotexted
        % of one of SinglesSoFar.  This is common for recursive goals.
        filter(single_context_makes_goal_costly(Info, Conj), SinglesSoFar0,
            SinglesSoFarMakeConjCostly),
        (
            SinglesSoFarMakeConjCostly = []
        ;
            SinglesSoFarMakeConjCostly = [_ | _],
            !:RevSingleCands = [ConjNum - SinglesSoFarMakeConjCostly |
                !.RevSingleCands]
        ),

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

:- pred single_context_makes_goal_costly(implicit_parallelism_info::in,
    pard_goal_detail::in, pard_goal_detail::in) is semidet.

single_context_makes_goal_costly(Info, Goal, Single) :-
    SingleCost = Single ^ goal_annotation ^ pgd_cost,
    SingleCount = goal_cost_get_calls(SingleCost),
    fix_goal_counts(Info, SingleCount, Goal, ConjNewCounts),
    identify_costly_goal(ConjNewCounts ^ goal_annotation, is_costly_goal).

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

    pardgoals_build_candidate_conjunction(Info, Location, RevGoalPathSteps, no,
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
    list(candidate_par_conjunction(pard_goal_detail))::out) is det.

push_and_build_candidate_conjunctions(_Info, _RevGoalPathSteps, _Conjs,
        _CostlyIndex, [], cord.empty, []).
push_and_build_candidate_conjunctions(Info, RevGoalPathSteps, Conjs,
        CostlyIndex, [Single | Singles], Messages, Candidates) :-
    push_and_build_candidate_conjunction(Info, RevGoalPathSteps, Conjs,
        CostlyIndex, Single, HeadMessages, MaybeHeadCandidate),
    push_and_build_candidate_conjunctions(Info, RevGoalPathSteps, Conjs,
        CostlyIndex, Singles, TailMessages, TailCandidates),
    Messages = HeadMessages ++ TailMessages,
    (
        MaybeHeadCandidate = yes(HeadCandidate),
        Candidates = [HeadCandidate | TailCandidates]
    ;
        MaybeHeadCandidate = no,
        Candidates = TailCandidates
    ).

:- pred push_and_build_candidate_conjunction(implicit_parallelism_info::in,
    list(goal_path_step)::in, list(pard_goal_detail)::in, int::in,
    pard_goal_detail::in, cord(message)::out,
    maybe(candidate_par_conjunction(pard_goal_detail))::out) is det.

push_and_build_candidate_conjunction(Info, RevGoalPathSteps, Conjs,
        CostlyIndex, Single, !:Messages, MaybeCandidate) :-
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
            RevCandidateGoalPathSteps, yes(PushGoal), CandidateConjs,
            MaybeCandidate, !Messages),
        (
            MaybeCandidate = yes(_),
            append_message(Location,
                info_found_n_conjunctions_with_positive_speedup(1),
                !Messages)
        ;
            MaybeCandidate = no
        )
    ;
        unexpected($module, $pred, "bad goal path for Single")
    ).

:- pred merge_same_level_pushes(
    candidate_par_conjunction(pard_goal_detail)::in,
    list(candidate_par_conjunction(pard_goal_detail))::in,
    push_goal::out) is det.

merge_same_level_pushes(MainCandidate, [], MainPush) :-
    MaybeMainPush = MainCandidate ^ cpc_maybe_push_goal,
    (
        MaybeMainPush = yes(MainPush)
    ;
        MaybeMainPush = no,
        unexpected($module, $pred, "no push")
    ).
merge_same_level_pushes(MainCandidate, [HeadCandidate | TailCandidates],
        Push) :-
    merge_same_level_pushes(HeadCandidate, TailCandidates, RestPush),
    MaybeMainPush = MainCandidate ^ cpc_maybe_push_goal,
    (
        MaybeMainPush = yes(MainPush)
    ;
        MaybeMainPush = no,
        unexpected($module, $pred, "no push")
    ),
    (
        MainPush = push_goal(GoalPathStr, Lo, Hi, [MainPushInto]),
        RestPush = push_goal(GoalPathStr, Lo, Hi, RestPushInto)
    ->
        Push = push_goal(GoalPathStr, Lo, Hi, [MainPushInto | RestPushInto])
    ;
        unexpected($module, $pred, "mismatch on pushed goals")
    ).

:- pred push_goals_create_candidate(implicit_parallelism_info::in,
    list(goal_path_step)::in, list(goal_path_step)::in,
    pard_goal_detail::in, list(pard_goal_detail)::in,
    list(goal_path_step)::out, list(pard_goal_detail)::out) is det.

push_goals_create_candidate(Info, RevCurPathSteps,
        [], GoalToPushInto, GoalsToPush0,
        RevCandidateGoalPathSteps, CandidateConjs) :-
    RevCandidateGoalPathSteps = RevCurPathSteps,
    % The pushed goals will have different costs in this context, in particular
    % the number of times they're called varies, This affects the per-call
    % costs of recursive calls.
    Calls = goal_cost_get_calls(GoalToPushInto ^ goal_annotation ^ pgd_cost),
    map(fix_goal_counts(Info, Calls), GoalsToPush0, GoalsToPush),
    CandidateConjs = [GoalToPushInto | GoalsToPush].
push_goals_create_candidate(Info, RevCurPathSteps,
        [HeadRelStep | TailRelSteps], GoalToPushInto, GoalsToPush0,
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
                % The pushed goals will have different costs in this context,
                % in particular the number of times they're called varies, This
                % affects the per-call costs of recursive calls.
                Cost = GoalToPushInto ^ goal_annotation ^ pgd_cost,
                Calls = goal_cost_get_calls(Cost),
                map(fix_goal_counts(Info, Calls), GoalsToPush0, GoalsToPush),
                CandidateConjs = Goals ++ GoalsToPush
            ;
                TailRelSteps = [_ | _],
                list.det_drop(N - 1, Goals, Tail),
                ( Tail = [SubGoal] ->
                    push_goals_create_candidate(Info,
                        [HeadRelStep | RevCurPathSteps],
                        TailRelSteps, SubGoal, GoalsToPush0,
                        RevCandidateGoalPathSteps, CandidateConjs)
                ;
                    % We can't push goals into the non-last conjunct without
                    % re-ordering, which is currently not supported.  By
                    % building a conjunction here we may still be able to
                    % create a worthwhile parallelisation.  However, there is a
                    % trade-off to explore between this and not generating the
                    % single expensive goal from within the conjunction and
                    % therefore possibly finding other single expensive goals
                    % later in this conjunction.
                    RevCandidateGoalPathSteps = RevCurPathSteps,
                    Cost = GoalToPushInto ^ goal_annotation ^ pgd_cost,
                    Calls = goal_cost_get_calls(Cost),
                    map(fix_goal_counts(Info, Calls), GoalsToPush0,
                        GoalsToPush),
                    CandidateConjs = Goals ++ GoalsToPush
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
                TailRelSteps, SubGoal, GoalsToPush0,
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
                TailRelSteps, SubGoal, GoalsToPush0,
                RevCandidateGoalPathSteps, CandidateConjs)
        ;
            unexpected($module, $pred, "not switch")
        )
    ;
        HeadRelStep = step_ite_then,
        ( GoalExpr = ite_rep(_, Then, _) ->
            push_goals_create_candidate(Info, [HeadRelStep | RevCurPathSteps],
                TailRelSteps, Then, GoalsToPush0,
                RevCandidateGoalPathSteps, CandidateConjs)
        ;
            unexpected($module, $pred, "not ite_then")
        )
    ;
        HeadRelStep = step_ite_else,
        ( GoalExpr = ite_rep(_, _, Else) ->
            push_goals_create_candidate(Info, [HeadRelStep | RevCurPathSteps],
                TailRelSteps, Else, GoalsToPush0,
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
                TailRelSteps, SubGoal, GoalsToPush0,
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

:- pred fix_goal_counts(implicit_parallelism_info::in, int::in,
    pard_goal_detail::in, pard_goal_detail::out) is det.

fix_goal_counts(Info, Count, !Goal) :-
    Annotation0 = !.Goal ^ goal_annotation,
    Cost0 = Annotation0 ^ pgd_cost,
    (
        GoalType = Annotation0 ^ pgd_pg_type,
        GoalType = pgt_call(_, CostAndCallees),
        % XXX This doesn't work if this is a non-atomic goal containing a
        % recursive call.
        set.member(Callee, CostAndCallees ^ cac_callees),
        % The call is recursive if it calls into the current clique.
        Info ^ ipi_clique = Callee ^ c_clique
    ->
        % for recursive calls.
        CostTotal = goal_cost_get_total(Cost0),
        PercallCost = CostTotal / float(Count)
    ;
        PercallCost = goal_cost_get_percall(Cost0)
    ),
    Cost = call_goal_cost(Count, PercallCost),
    !Goal ^ goal_annotation ^ pgd_cost := Cost,
    ( goal_cost_above_par_threshold(Info, Cost) ->
        AboveThreshold = cost_above_par_threshold
    ;
        AboveThreshold = cost_not_above_par_threshold
    ),
    !Goal ^ goal_annotation ^ pgd_cost_above_threshold := AboveThreshold.

:- pred pardgoals_build_candidate_conjunction(implicit_parallelism_info::in,
    program_location::in, list(goal_path_step)::in,
    maybe(push_goal)::in, list(pard_goal_detail)::in,
    maybe(candidate_par_conjunction(pard_goal_detail))::out,
    cord(message)::in, cord(message)::out) is det.

pardgoals_build_candidate_conjunction(Info, Location, RevGoalPathSteps,
        MaybePushGoal, Goals, MaybeCandidate, !Messages) :-
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
        Candidate = candidate_par_conjunction(RevGoalPathString, MaybePushGoal,
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

%----------------------------------------------------------------------------%

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
    BarrierCost = Opts ^ cpcp_barrier_cost,
    ContextWakeupDelay = Opts ^ cpcp_context_wakeup_delay,
    Metrics0 = init_empty_parallel_exec_metrics(CostBeforePercall,
        CostAfterPercall, NumCalls, float(SparkCost), float(SparkDelay),
        float(BarrierCost), float(ContextWakeupDelay)),
    Overlap0 = peo_empty_conjunct,

    SharedVars = ip_calc_sharedvars_set(!.Parallelisation),
    CostData0 = parallelisation_cost_data(SharedVars, Overlap0, Metrics0, init),
    NumMiddleGoals = ip_get_num_goals_middle(!.Parallelisation),
    list.foldl3(calculate_parallel_cost_step(Info, NumMiddleGoals), ParConj,
        1, _, 0, _, CostData0, CostData),
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
    !.CostData = parallelisation_cost_data(SharedVars, Overlap0, Metrics0,
        PM0),
    !:NumGoals = !.NumGoals + length(Conjuncts),
    ( !.NumGoals = NumMiddleGoals ->
        IsLastConjunct = is_last_par_conjunct
    ;
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
        Algorithm = parallelise_dep_conjs_overlap,

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
        % and complete the RevExecutions structure..
        list.reverse(RevExecution, Execution),
        CostBParElapsed = LastParConsumeTime + (CostB - LastSeqConsumeTime),
        RevExecution = [ (LastResumeTime - CostBParElapsed) | RevExecution0 ],

        CostSignals = float(Info ^ ipi_opts ^ cpcp_future_signal_cost *
            count(RightProducedVars)),
        CostWaits = float(Info ^ ipi_opts ^ cpcp_future_wait_cost *
            count(Vars)),
        calc_cost_and_dead_time(Execution, CostBPar, DeadTime)
    ;
        ( Algorithm = parallelise_dep_conjs_naive
        ; Algorithm = do_not_parallelise_dep_conjs
        ; Algorithm = parallelise_dep_conjs_num_vars
        ),

        CostBPar = CostB + SparkCost,
        Execution = [StartTime - (StartTime + CostB)],
        ConsumptionsMap = init,
        CostSignals = 0.0,
        CostWaits = 0.0,
        DeadTime = 0.0
    ),

    % CostB    - the cost of B if it where to be executed in sequence.
    % CostBPar - CostB plus the overheads of parallel exection (not including
    %            the dead time).
    % DeadTime - The time that B spends blocked on other computations.
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
    % * !ConsumptionsMap: Accumuates a map of variable consumptions.
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
    svmap.det_insert(Var, Time, !Map).

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
    ( ProdTime < ConsTime ->
        % Order earlier events first,
        ProdOrCons = ProdVar - production(ProdTime),
        merge_consumptions_and_productions(Cons, Prods0, ProdsAndCons)
    ;
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
                unexpected($module, $pred,
                    "Found production when looking for consumption")
            )
        ;
            UseType = var_use_consumption,
            (
                FindProdOrCons = find_production,
                unexpected($module, $pred,
                    "Found consumption when looking for production")
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
        list.map_foldl(compute_var_modes(InstMapBefore, InstMapAfter),
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
        list.foldl(earliest_use, OtherUses, FirstUse, Use)
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
        ( list.member_index0(Var, Args, ArgNum) ->
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
            ( VarUseType = var_use_consumption ->
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
        list.foldl(
            goal_build_use_map(!.Goal, RevGoalPathSteps, Cost, Info,
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

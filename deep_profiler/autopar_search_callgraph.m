%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: autopar_search_callgraph.m
% Author: pbone.
%
% This module contains the code for analysing deep profiles of programs
% in order to determine how best to automatically parallelise the program.
% This code is used by the mdprof_create_feedback tool.
%
%---------------------------------------------------------------------------%

:- module mdprof_fb.automatic_parallelism.autopar_search_callgraph.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.feedback.
:- import_module mdbcomp.feedback.automatic_parallelism.
:- import_module message.
:- import_module profile.

:- import_module cord.

%---------------------------------------------------------------------------%

    % Build the candidate parallel conjunctions feedback information used for
    % implicit parallelism.
    %
:- pred candidate_parallel_conjunctions(candidate_par_conjunctions_params::in,
    deep::in, cord(message)::out, feedback_info::in, feedback_info::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis_utils.
:- import_module coverage.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.
:- import_module mdprof_fb.automatic_parallelism.autopar_annotate.
:- import_module mdprof_fb.automatic_parallelism.autopar_costs.
:- import_module mdprof_fb.automatic_parallelism.autopar_search_goals.
:- import_module mdprof_fb.automatic_parallelism.autopar_types.
:- import_module measurement_units.
:- import_module measurements.
:- import_module program_representation_utils.
:- import_module recursion_patterns.
:- import_module report.

:- import_module array.
:- import_module assoc_list.
:- import_module bool.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%
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
    % runtime, since it is modeled here as a call site that in reality
    % does not exist.
    RootParallelism = no_parallelism,
    candidate_parallel_conjunctions_clique(Params, Deep,
        RootParallelism, RootCliquePtr, ConjunctionsMap, Messages),

    map.to_assoc_list(ConjunctionsMap, ConjunctionsAssocList0),
    assoc_list.map_values_only(
        convert_candidate_par_conjunctions_proc(pard_goal_detail_to_pard_goal),
        ConjunctionsAssocList0, ConjunctionsAssocList),
    CandidateInfo =
        feedback_info_candidate_parallel_conjunctions(Params,
            ConjunctionsAssocList),
    ProfiledProgramName = Deep ^ profile_stats ^ prs_program_name,
    add_feedback_candidate_parallel_conjunctions(ProfiledProgramName,
        CandidateInfo, !Feedback).

%---------------------------------------------------------------------------%
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
        ( if CliquePtr = RootCliquePtr then
            % It is okay, this clique never has an entry procedure.
            PDPtrs = OtherPDPtrs
        else
            CliquePtr = clique_ptr(CliqueNum),
            string.format("Clique %d has no entry proc", [i(CliqueNum)], Msg),
            unexpected($pred, Msg)
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
            array.to_list(CSDPtrs), ChildCliqueCords),
        ChildCliques = cord_list_to_cord(ChildCliqueCords)
    ).

:- pred call_site_dynamic_callees(deep::in, parallelism_amount::in,
    string_proc_label::in, reverse_goal_path::in, call_site_dynamic_ptr::in,
    cord(candidate_child_clique)::out) is det.

call_site_dynamic_callees(Deep, Parallelism, ProcLabel, RevGoalPath, CSDPtr,
        ChildCliques) :-
    ( if valid_call_site_dynamic_ptr(Deep, CSDPtr) then
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
    else
        ChildCliques = empty
    ).

:- pred candidate_parallel_conjunctions_callee(
    candidate_par_conjunctions_params::in, deep::in,
    clique_ptr::in, candidate_par_conjunctions::in,
    candidate_child_clique::in, candidate_par_conjunctions::out,
    cord(message)::out) is det.

candidate_parallel_conjunctions_callee(Opts, Deep, CliquePtr, CliqueCandidates,
        !.Callee, Candidates, Messages) :-
    ( if not_callee(CliquePtr, !.Callee) then
        ( if cost_threshold(Opts, !.Callee) then
            update_parallelism_available(CliqueCandidates, !Callee),
            ( if not exceeded_parallelism(Opts, !.Callee) then
                AnalyzeChild = yes
            else
                trace [compile_time(flag("debug_cpc_search")), io(!IO)] (
                    debug_cliques_exceeded_parallelism(!.Callee, !IO)
                ),
                AnalyzeChild = no
            )
        else
            trace [compile_time(flag("debug_cpc_search")), io(!IO)] (
                debug_cliques_below_threshold(!.Callee, !IO)
            ),
            AnalyzeChild = no
        )
    else
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
    ( if map.search(CandidateConjunctions, ProcLabel, ProcConjs) then
        Conjs = ProcConjs ^ cpcp_par_conjs,
        list.foldl(update_parallelism_available_conj, Conjs, !ChildClique)
    else
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
    ( if
        RevGoalPath \= RevConjGoalPath,
        rev_goal_path_inside_relative(RevConjGoalPath, RevGoalPath,
            RevRelativePath),
        rgp_to_fgp(RevRelativePath, RelativePath),
        RelativePath = fgp_cons(Step, _),
        Step = step_conj(ConjNum),
        ConjNum > FirstConjunct,
        ConjNum =< FirstConjunct + Length
    then
        % The call into this clique gets parallelised by Conj.
        % XXX: If we knew the parallelisation type used for Conj, we could
        % do this calculation more accurately. For instance, if this is a loop,
        % then we use as many cores as the loop has iterations. (Except for
        % dead time).
        Metrics = Conj ^ cpc_par_exec_metrics,
        CPUTime = parallel_exec_metrics_get_cpu_time(Metrics),
        DeadTime = Metrics ^ pem_first_conj_dead_time +
            Metrics ^ pem_future_dead_time,
        Efficiency = CPUTime / (CPUTime + DeadTime),
        Parallelism0 = !.ChildClique ^ ccc_parallelism,
        sub_computation_parallelism(Parallelism0, probable(Efficiency),
            Parallelism),
        !ChildClique ^ ccc_parallelism := Parallelism
    else
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
        promise_equivalent_solutions [Candidates, ProcMessages]
        ( try [] (
            candidate_parallel_conjunctions_proc(Opts, Deep, PDPtr,
                RecursionType, RecursiveCallSiteCostMap, CandidatesPrime,
                ProcMessagesPrime)
        )
        then (
            Candidates = CandidatesPrime,
            ProcMessages = ProcMessagesPrime
        )
        catch_any Exp -> (
            trace [io(!IO)] (
                PDPtr = proc_dynamic_ptr(PDId),
                deep_lookup_proc_dynamics(Deep, PDPtr, PD),
                deep_lookup_proc_statics(Deep, PD ^ pd_proc_static, PS),
                ProcName = PS ^ ps_q_refined_id,
                io.format(io.stderr_stream,
                    "Exception while analysing proc dynamic %d (%s)\n",
                    [i(PDId), s(ProcName)], !IO)
            ),
            throw(Exp)
        )),
        !:Messages = !.Messages ++ ProcMessages,
        Messages = !.Messages
    ).

:- pred merge_candidate_par_conjs_proc(
    candidate_par_conjunctions_proc(T)::in,
    candidate_par_conjunctions_proc(T)::in,
    candidate_par_conjunctions_proc(T)::out) is det.

merge_candidate_par_conjs_proc(A, B, Result) :-
    A = candidate_par_conjunctions_proc(VarNameTableA, PushGoalsA, CPCsA),
    B = candidate_par_conjunctions_proc(VarNameTableB, PushGoalsB, CPCsB),
    CPCs = CPCsA ++ CPCsB,
    merge_pushes_for_proc(PushGoalsA ++ PushGoalsB, PushGoals),
    ( if VarNameTableA = VarNameTableB then
        Result = candidate_par_conjunctions_proc(VarNameTableA, PushGoals,
            CPCs)
    else
        unexpected($pred, "var tables do not match")
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
    ( if map.search(!.Map, GoalPathStr, OldTriple) then
        OldTriple = {OldLo, OldHi, OldTargetGoalPathStrSet},
        ( if
            Lo = OldLo,
            Hi = OldHi
        then
            set.insert_list(TargetGoalPathStrs,
                OldTargetGoalPathStrSet, NewTargetGoalPathStrSet),
            NewTriple = {OldLo, OldHi, NewTargetGoalPathStrSet},
            map.det_update(GoalPathStr, NewTriple, !Map)
        else
            % There seem to be separate push requests inside the same
            % conjunction that want to push different sets of conjuncts.
            % Since they could interfere with each other, we keep only one.
            % Since we don't have any good basis on which to make the choice,
            % we keep the earlier pushes.
            true
        )
    else
        NewTriple = {Lo, Hi, set.list_to_set(TargetGoalPathStrs)},
        map.det_insert(GoalPathStr, NewTriple, !Map)
    ).

:- pred extract_from_push_map(goal_path_string::in,
    {int, int, set(goal_path_string)}::in,
    list(push_goal)::in, list(push_goal)::out) is det.

extract_from_push_map(GoalPathStr, Triple, !Pushes) :-
    Triple = {Lo, Hi, TargetGoalPathStrSet},
    Push = push_goal(GoalPathStr, Lo, Hi,
        set.to_sorted_list(TargetGoalPathStrSet)),
    !:Pushes = [Push | !.Pushes].

%---------------------------------------------------------------------------%
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

    ( if
        ( ModuleName = "Mercury runtime"
        ; ModuleName = "exception"
        )
    then
        % Silently skip over any code from the runtime, since
        % we can't expect to find its procedure representation.
        Candidates = map.init
    else
        deep_lookup_clique_index(Deep, PDPtr, CliquePtr),
        PSPtr = PD ^ pd_proc_static,
        deep_get_maybe_procrep(Deep, PSPtr, MaybeProcRep),
        (
            MaybeProcRep = ok(ProcRep),
            ProcDefnRep = ProcRep ^ pr_defn,
            Goal0 = ProcDefnRep ^ pdr_goal,
            VarNameTable = ProcDefnRep ^ pdr_var_name_table,

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
                    RecursionType, VarNameTable, ProcLabel),
                goal_to_pard_goal(Info, rgp_nil, Goal, PardGoal, !Messages),
                goal_get_conjunctions_worth_parallelising(Info,
                    rgp_nil, PardGoal, _, CandidatesCord0, PushesCord,
                    _Singles, MessagesA),
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
                            VarNameTable, MergedPushes, Candidates0),
                        Candidates = map.singleton(ProcLabel, CandidateProc)
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
    var_name_table::in, candidate_par_conjunction(pard_goal_detail)::in,
    candidate_par_conjunctions::in, candidate_par_conjunctions::out) is det.

build_candidate_par_conjunction_maps(ProcLabel, VarNameTable, Candidate,
        !Map) :-
    % XXX: This predicate will also need to add pushes to CandidateProc.
    ( if map.search(!.Map, ProcLabel, CandidateProc0) then
        CandidateProc0 = candidate_par_conjunctions_proc(VarNameTablePrime,
            PushGoals, CPCs0),
        CPCs = [Candidate | CPCs0],
        expect(unify(VarNameTable, VarNameTablePrime), $pred,
            "var tables do not match")
    else
        CPCs = [Candidate],
        PushGoals = []
    ),
    CandidateProc = candidate_par_conjunctions_proc(VarNameTable, PushGoals,
        CPCs),
    map.set(ProcLabel, CandidateProc, !Map).

%---------------------------------------------------------------------------%

:- pred pardgoal_consumed_vars_accum(pard_goal_detail::in,
    set(var_rep)::in, set(var_rep)::out) is det.

pardgoal_consumed_vars_accum(Goal, !Vars) :-
    RefedVars = Goal ^ goal_annotation ^ pgd_inst_map_info ^ im_consumed_vars,
    set.union(RefedVars, !Vars).

%---------------------------------------------------------------------------%
%
% Useful utility predicates.
%

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
        "D: Not entering clique %d, " ++
        "no more parallelisation resources available at this context\n\n",
        [i(CliqueNum)], !IO).

%---------------------------------------------------------------------------%

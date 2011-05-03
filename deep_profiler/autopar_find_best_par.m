%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: autopar_find_best_par.m
% Author: pbone.
%
% This module contains the code for finding the best way to parallelize
% a given conjunction.
%
%-----------------------------------------------------------------------------%

:- module mdprof_fb.automatic_parallelism.autopar_find_best_par.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.feedback.
:- import_module mdbcomp.feedback.automatic_parallelism.
:- import_module mdprof_fb.automatic_parallelism.autopar_types.
:- import_module message.

:- import_module cord.
:- import_module list.
:- import_module maybe.

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module branch_and_bound.
:- import_module mdbcomp.program_representation.
:- import_module mdprof_fb.automatic_parallelism.autopar_calc_overlap.
:- import_module mdprof_fb.automatic_parallelism.autopar_search_goals. % XXX
:- import_module measurements.

:- import_module array.
:- import_module digraph.
:- import_module float.
:- import_module io.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.

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

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

:- type goal_classification
    --->    gc_cheap_goals
    ;       gc_costly_goals.

    % A summary of goals before parallelisation.
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
        list.reverse(RevGoalGroups, GoalGroups),

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

%-----------------------------------------------------------------------------%

:- pred build_dependency_graphs(list(pard_goal_detail)::in,
    dependency_graphs::out) is det.

build_dependency_graphs(Goals, Maps) :-
    Graph0 = digraph.init,
    build_dependency_graph(Goals, 1, map.init, _VarDepMap, Graph0, Graph),
    Maps = dependency_graphs(Graph, tc(Graph)).

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
    MaybeAddEdge = ( pred(RefedVar::in, GraphI0::in, GraphI::out) is det :-
        ( map.search(!.VarDepMap, RefedVar, DepConj) ->
            % DepConj should already be in the graph.
            digraph.lookup_key(GraphI0, DepConj, DepConjKey),
            digraph.add_edge(DepConjKey, ThisConjKey, GraphI0, GraphI)
        ;
            GraphI = GraphI0
        )
    ),
    list.foldl(MaybeAddEdge, set.to_sorted_list(RefedVars), !Graph),

    % For each variable instantiated by this goal add it to the VarDepMap with
    % this goal as it's instantiator.  That is a maping from the variable to
    % the conj num.
    InstVars = InstMapInfo ^ im_bound_vars,
    InsertForConjNum = ( pred(InstVar::in, MapI0::in, MapI::out) is det :-
        map.det_insert(InstVar, ConjNum, MapI0, MapI)
    ),
    set.fold(InsertForConjNum, InstVars, !VarDepMap),

    build_dependency_graph(PGs, ConjNum + 1, !VarDepMap, !Graph).

%-----------------------------------------------------------------------------%

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
            ParalleliseDepConjs = parallelise_dep_conjs(_),
            MaybeBestParallelisation = no
        ;
            ParalleliseDepConjs = do_not_parallelise_dep_conjs,
            % Try again to get the best dependent parallelisation.
            % This is used for guided parallelisation.
            TempInfo = Info ^ ipi_opts ^ cpcp_parallelise_dep_conjs :=
                parallelise_dep_conjs(estimate_speedup_by_overlap),
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
    Value = Metrics ^ pem_par_time +
        parallel_exec_metrics_get_overheads(Metrics) * 2.0.

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
    CostData = parallelisation_cost_data(_, Overlap, Metrics0, _),

    Metrics = finalise_parallel_exec_metrics(Metrics0),
    par_conj_overlap_is_dependent(Overlap, IsDependent),
    ParConjs = ip_get_par_conjs(Incomplete),
    Best = bp_parallel_execution(GoalsBefore, ParConjs,
        GoalsAfter, IsDependent, Metrics).

%----------------------------------------------------------------------------%

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

    % Test that the parallelisation includes dependant parallelism
    % only if permitted by the user.
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

%----------------------------------------------------------------------------%

:- pred add_one_goal_into_first_par_conj(incomplete_parallelisation::in,
    incomplete_parallelisation::out) is det.

:- pred add_one_goal_into_last_par_conj(incomplete_parallelisation::in,
    incomplete_parallelisation::out) is det.

add_one_goal_into_first_par_conj(!Parallelisation) :-
    FirstGoal0 = !.Parallelisation ^ ip_first_par_goal,
    FirstGoal = FirstGoal0 - 1,
    !Parallelisation ^ ip_first_par_goal := FirstGoal,
    !Parallelisation ^ ip_maybe_goals_before_cost := no,
    !Parallelisation ^ ip_maybe_par_cost_data := no.

add_one_goal_into_last_par_conj(!Parallelisation) :-
    LastGoal0 = !.Parallelisation ^ ip_last_par_goal,
    LastGoal = LastGoal0 + 1,
    !Parallelisation ^ ip_last_par_goal := LastGoal,
    !Parallelisation ^ ip_maybe_goals_after_cost := no,
    !Parallelisation ^ ip_maybe_par_cost_data := no.

%----------------------------------------------------------------------------%

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

%----------------------------------------------------------------------------%

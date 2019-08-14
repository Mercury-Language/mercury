%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: autopar_types.
% Author: pbone.
%
% This module contains types useful in automatic parallelization.
%
%---------------------------------------------------------------------------%

:- module mdprof_fb.automatic_parallelism.autopar_types.
:- interface.

:- import_module analysis_utils.
:- import_module coverage.
:- import_module mdbcomp.
:- import_module mdbcomp.feedback.
:- import_module mdbcomp.feedback.automatic_parallelism.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.
:- import_module measurements.
:- import_module profile.
:- import_module program_representation_utils.
:- import_module report.
:- import_module var_use_analysis.

:- import_module array.
:- import_module assoc_list.
:- import_module digraph.
:- import_module lazy.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

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
                ipi_var_name_table  :: var_name_table,
                ipi_proc_label      :: string_proc_label
            ).

    % A representation of a goal within a parallel conjunction. We do not have
    % to represent many types of goals or details about them, at least for now.
    % This type provides more detail than feedback.pard_goal. This extra detail
    % is not required by the compiler, and therefore it is not part of the
    % feedback file format.
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

                % Information about when the inputs of this goal are consumed,
                % and when its outputs are produced, relative to the start of
                % this goal. Since we expect that many inputs and outputs
                % will not be shared variables, we do not compute this
                % information unless and until we need it.
                pgd_var_production_map      :: lazy_var_use_map,
                pgd_var_consumption_map     :: lazy_var_use_map
            ).

:- type lazy_var_use_map == map(var_rep, lazy(var_use_info)).

:- type pard_goal_type
    --->    pgt_call(
                % The argument modes and use information.
                pgtc_args                   :: list(var_and_mode),

                % The call site report from the deep profiler.
                pgtc_call_site              :: cost_and_callees
            )
    ;       pgt_other_atomic_goal
    ;       pgt_non_atomic_goal.

:- pred pard_goal_detail_to_pard_goal(
    candidate_par_conjunction(pard_goal_detail)::in,
    pard_goal_detail::in, pard_goal::out) is det.

    % Build sets of produced and consumed vars for a conjunct in a conjunction.
    % Use with foldl to build these sets up for the whole conjunction.
    % At the end of a conjunction, there may be variables in the intersection
    % of the sets, that is okay, those goals are produced early in the
    % conjunction and consumed later in the conjunction.
    %
:- pred conj_produced_and_consumed_vars(pard_goal_detail::in,
    set(var_rep)::in, set(var_rep)::out,
    set(var_rep)::in, set(var_rep)::out) is det.

%---------------------------------------------------------------------------%

:- type is_costly_goal
    --->    is_not_costly_goal
    ;       is_costly_goal.

:- pred identify_costly_goal(pard_goal_detail_annotation::in,
    is_costly_goal::out) is det.

:- pred identify_costly_goals(list(pard_goal_detail)::in, int::in,
    list(int)::out) is det.

%---------------------------------------------------------------------------%

    % A variable and its mode.
    %
:- type var_and_mode
    --->    var_and_mode(
                vmu_var                 :: var_rep,
                vmu_mode                :: var_mode_rep
            ).

:- type candidate_par_conjunctions ==
    map(string_proc_label, candidate_par_conjunctions_proc(pard_goal_detail)).

    % inst_map_info now contains information that it does not need to contain.
    % Namely, the im_after field can be calculated from the im_before and
    % im_bound_vars fields. However since this information will probably
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

:- type dependency_graphs
    --->    dependency_graphs(
                dm_forward              :: digraph(int),
                dm_forward_tc           :: digraph(int)
            ).

:- type incomplete_parallelisation
    --->    incomplete_parallelisation(
                ip_goals                    :: array(pard_goal_detail),

                % The index of the first goal in the parallelised goals.
                % This is also the number of goals executed in sequence before
                % the parallel conjunction.
                ip_first_par_goal           :: int,

                % The index of the last goal in the parallel conjunction.
                ip_last_par_goal            :: int,

                % The index of the last goal that has been (tentatively)
                % scheduled. All goals between this +1 and ip_last_par_goal
                % have not been scheduled.
                ip_last_scheduled_goal      :: int,

                % The index of the last goal in each of the parallel conjuncts.
                % The very last parallel conjunct is denoted by
                % ip_last_par_goal.
                ip_par_conjs_rev_last_goal  :: list(int),

                % The number of calls in this conjunction.
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
    --->    parallelisation_cost_data(
                pcd_shared_vars         :: set(var_rep),
                pcd_par_exec_overlap    :: parallel_execution_overlap,
                pcd_par_exec_metrics    :: parallel_exec_metrics_incomplete,
                pcd_productions_map     :: map(var_rep, float)
            ).

    % This data structure represents the execution of dependent conjuncts,
    % it tracks which variables are produced and consumed.
    %
    % TODO: Implement a pretty printer for this data.
    %
:- type parallel_execution_overlap
    --->    peo_empty_conjunct
    ;       peo_conjunction(
                poec_left_conjunct      :: parallel_execution_overlap,
                poec_right_conjunct     :: dependent_conjunct_execution,

                % The variables produced by the left conjunct and
                % consumed by the right conjunct.
                poec_dependent_vars     :: set(var_rep)
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

:- func ip_get_goals_before(incomplete_parallelisation) =
    list(pard_goal_detail).

:- func ip_get_goals_after(incomplete_parallelisation) =
    list(pard_goal_detail).

:- func ip_get_par_conjs(incomplete_parallelisation) =
    list(seq_conj(pard_goal_detail)).

:- func ip_get_num_goals(incomplete_parallelisation) = int.

:- func ip_get_num_parallel_conjuncts(incomplete_parallelisation) = int.

:- func ip_get_num_goals_middle(incomplete_parallelisation) = int.

:- func ip_calc_sharedvars_set(incomplete_parallelisation) = set(var_rep).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module pair.

%---------------------------------------------------------------------------%

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
    SharedVars = set.to_sorted_list(SharedVarsSet),

    Coverage = PGD ^ pgd_coverage,
    get_coverage_before_det(Coverage, Calls),
    ( if Calls > 0 then
        list.foldl(build_var_use_list(PGD ^ pgd_var_production_map),
            SharedVars, [], Productions),
        list.foldl(build_var_use_list(PGD ^ pgd_var_consumption_map),
            SharedVars, [], Consumptions)
    else
        Productions = [],
        Consumptions = []
    ),
    PG = pard_goal_annotation(CostPercall, CostAboveThreshold,
        Productions, Consumptions).

:- pred build_var_use_list(map(var_rep, lazy(var_use_info))::in, var_rep::in,
    assoc_list(var_rep, float)::in, assoc_list(var_rep, float)::out) is det.

build_var_use_list(Map, Var, !List) :-
    promise_pure (
        ( if
            map.search(Map, Var, LazyUse),
            impure read_if_val(LazyUse, Use)
        then
            UseTime = Use ^ vui_cost_until_use,
            !:List = [Var - UseTime | !.List]
        else
            true
        )
    ).

%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%

conj_produced_and_consumed_vars(Conj, !Produced, !Consumed) :-
    InstMapInfo = Conj ^ goal_annotation ^ pgd_inst_map_info,
    !:Produced = set.union(!.Produced, InstMapInfo ^ im_bound_vars),
    !:Consumed = set.union(!.Consumed, InstMapInfo ^ im_consumed_vars).

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

ip_get_goals_before(Parallelisation) = GoalsBefore :-
    Goals = Parallelisation ^ ip_goals,
    FirstParGoalIndex = Parallelisation ^ ip_first_par_goal,
    LastGoalBefore = FirstParGoalIndex - 1,
    ( if LastGoalBefore < 0 then
        GoalsBefore = []
    else
        array.fetch_items(Goals, 0, LastGoalBefore, GoalsBefore)
    ).

ip_get_goals_after(Parallelisation) = GoalsAfter :-
    Goals = Parallelisation ^ ip_goals,
    LastParGoalIndex = Parallelisation ^ ip_last_par_goal,
    NumGoals = array.size(Goals),
    FirstGoalAfter = LastParGoalIndex + 1,
    ( if FirstGoalAfter >= NumGoals then
        GoalsAfter = []
    else
        array.fetch_items(Goals, FirstGoalAfter, NumGoals - 1, GoalsAfter)
    ).

ip_get_par_conjs(Incomplete) = ParConjs :-
    Goals = Incomplete ^ ip_goals,
    Start = Incomplete ^ ip_first_par_goal,
    Last = Incomplete ^ ip_last_scheduled_goal,
    LastGoalsRev0 = Incomplete ^ ip_par_conjs_rev_last_goal,
    LastGoalsRev = [Last | LastGoalsRev0],
    list.reverse(LastGoalsRev, LastGoals),
    ip_get_par_conjs_2(Goals, Start, LastGoals, ParConjs).

:- pred ip_get_par_conjs_2(array(pard_goal_detail)::in, int::in,
    list(int)::in, list(seq_conj(pard_goal_detail))::out) is det.

ip_get_par_conjs_2(_, _, [], []).
ip_get_par_conjs_2(Array, First, [Last | Lasts], [Conj | Conjs]) :-
    ip_get_par_conjs_2(Array, Last + 1, Lasts, Conjs),
    fetch_items(Array, First, Last, Goals),
    Conj = seq_conj(Goals).

ip_get_num_goals(Incomplete) = array.size(Incomplete ^ ip_goals).

ip_get_num_parallel_conjuncts(Incomplete) =
    list.length(Incomplete ^ ip_par_conjs_rev_last_goal) + 1.

ip_get_num_goals_middle(Incomplete) = LastParGoal - FirstParGoal + 1 :-
    FirstParGoal = Incomplete ^ ip_first_par_goal,
    LastParGoal = Incomplete ^ ip_last_par_goal.

ip_calc_sharedvars_set(Incomplete) = SharedVars :-
    ParConjs = ip_get_par_conjs(Incomplete),
    list.foldl2(build_sharedvars_set, ParConjs,
        set.init, _, set.init, SharedVars).

:- pred build_sharedvars_set(seq_conj(pard_goal_detail)::in,
    set(var_rep)::in, set(var_rep)::out,
    set(var_rep)::in, set(var_rep)::out) is det.

build_sharedvars_set(seq_conj(Conjs), !BoundVars, !SharedVars) :-
    list.foldl2(conj_produced_and_consumed_vars, Conjs,
        set.init, ProducedVars, set.init, ConsumedVars),
    % The new shared vars are previously bound variables that are consumed in
    % this conjunct. This must be calculated before !BoundVars is updated.
    SharedVars = set.intersect(!.BoundVars, ConsumedVars),
    !:SharedVars = set.union(!.SharedVars, SharedVars),
    !:BoundVars = set.union(!.BoundVars, ProducedVars).

%---------------------------------------------------------------------------%

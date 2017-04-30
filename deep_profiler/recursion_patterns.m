%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: recursion_patterns.m.
% Authors: pbone.
%
% This module contains code that analysis the recursive structures of cliques.
% It is intended for use on the automatic parallelisation analysis.
%
%---------------------------------------------------------------------------%

:- module recursion_patterns.
:- interface.

:- import_module report.
:- import_module measurements.
:- import_module profile.

:- import_module maybe.

%---------------------------------------------------------------------------%

:- pred create_clique_recursion_costs_report(deep::in, clique_ptr::in,
    maybe_error(clique_recursion_report)::out) is det.

:- pred create_recursion_types_frequency_report(deep::in,
    maybe_error(recursion_types_frequency_report)::out) is det.

%---------------------------------------------------------------------------%

:- pred recursion_type_get_maybe_avg_max_depth(recursion_type,
    maybe(recursion_depth)).
:- mode recursion_type_get_maybe_avg_max_depth(in(recursion_type_known_costs),
    out(maybe_yes(ground))) is det.
:- mode recursion_type_get_maybe_avg_max_depth(in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module analysis_utils.
:- import_module array_util.
:- import_module coverage.
:- import_module create_report.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.
:- import_module measurement_units.

:- import_module array.
:- import_module assoc_list.
:- import_module float.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%

create_clique_recursion_costs_report(Deep, CliquePtr,
        MaybeCliqueRecursionReport) :-
    find_clique_first_and_other_procs(Deep, CliquePtr, MaybeFirstPDPtr,
        OtherPDPtrs),
    deep_lookup_clique_parents(Deep, CliquePtr, ParentCallPtr),
    ( if valid_call_site_dynamic_ptr(Deep, ParentCallPtr) then
        deep_lookup_call_site_dynamics(Deep, ParentCallPtr, ParentCall),
        ParentCalls = calls(ParentCall ^ csd_own_prof)
    else
        % The first call from the runtime doesn't have a valid CSD.
        ParentCalls = 1
    ),
    (
        MaybeFirstPDPtr = yes(FirstPDPtr),
        NumProcs = length(OtherPDPtrs) + 1,
        (
            OtherPDPtrs = [],
            % Exaclty one procedure
            proc_get_recursion_type(Deep, CliquePtr, FirstPDPtr, ParentCalls,
                MaybeRecursionType)
        ;
            OtherPDPtrs = [_ | _],
            % More than one, this is some sort of multiply recursion.
            MaybeRecursionType = ok(rt_mutual_recursion(NumProcs))
        ),
        (
            MaybeRecursionType = ok(RecursionType),
            CliqueRecursionReport = clique_recursion_report(CliquePtr,
                RecursionType, NumProcs),
            MaybeCliqueRecursionReport = ok(CliqueRecursionReport)
        ;
            MaybeRecursionType = error(Error),
            MaybeCliqueRecursionReport = error(Error)
        )
    ;
        MaybeFirstPDPtr = no,
        MaybeCliqueRecursionReport = error(
            "This clique doesn't appear to have an entry procedure")
    ).

:- pred proc_get_recursion_type(deep::in, clique_ptr::in,
    proc_dynamic_ptr::in, int::in, maybe_error(recursion_type)::out) is det.

proc_get_recursion_type(Deep, ThisClique, PDPtr, ParentCalls,
        MaybeRecursionType) :-
    deep_lookup_pd_own(Deep, PDPtr, PDOwn),
    TotalCalls = calls(PDOwn),
    create_dynamic_procrep_coverage_report(Deep, PDPtr, MaybeCoverageReport),
    (
        MaybeCoverageReport = ok(CoverageReport),
        CoverageReport = procrep_coverage_info(_, ProcRep, CoverageArray),
        Goal = ProcRep ^ pr_defn ^ pdr_goal,
        proc_dynamic_paired_call_site_slots(Deep, PDPtr, Slots),
        foldl(build_dynamic_call_site_cost_and_callee_map(Deep),
            Slots, map.init, CallSitesMap),
        Info = recursion_analysis_info(ThisClique, CallSitesMap,
            CoverageArray),
        goal_recursion_data(Info, rgp_nil, Goal, RecursionData),
        recursion_data_to_recursion_type(ParentCalls, TotalCalls,
            RecursionData, RecursionType),
        MaybeRecursionType = ok(RecursionType)
    ;
        MaybeCoverageReport = error(Error),
        MaybeRecursionType = error(Error)
    ).

:- pred recursion_data_to_recursion_type(int::in, int::in, recursion_data::in,
    recursion_type::out) is det.

recursion_data_to_recursion_type(ParentCallsI, TotalCallsI,
        RecursionData, Type) :-
    (
        RecursionData = no_recursion_data_dead_proc,
        % A procedure that is never called never recurses.
        Type = rt_not_recursive
    ;
        RecursionData = recursion_data(Levels, Maximum, Errors),
        ParentCalls = float(ParentCallsI),
        TotalCalls = float(TotalCallsI),
        ( if assoc_list.search(Levels, 0, RLBase) then
            RLBase = recursion_level(BaseCost, BaseProb),
            BaseCountF = probability_to_float(BaseProb) * TotalCalls,
            BaseCount = round_to_int(BaseCountF)
        else
            BaseCost = 0.0,
            BaseCount = 0,
            BaseProb = impossible
        ),
        BaseLevel =
            recursion_level_report(0, BaseCount, BaseProb, BaseCost, 0.0),
        ( if set.empty(Errors) then
            ( if Maximum < 0 then
                unexpected($module, $pred,
                    "negative number of recursive calls")
            else if Maximum = 0 then
                Type = rt_not_recursive
            else if Maximum = 1 then
                ( if assoc_list.search(Levels, 1, RLRec) then
                    RLRec = recursion_level(RecCost, RecProb),
                    RecCountF = probability_to_float(RecProb) * TotalCalls,
                    RecLevel = recursion_level_report(1,
                        round_to_int(RecCountF), RecProb, RecCost, 1.0)
                else
                    string.format("maximum level %d not found", [i(1)], Msg),
                    unexpected($module, $pred, Msg)
                ),
                AvgMaxDepth = TotalCalls / ParentCalls,
                AvgRecCost = single_rec_average_recursion_cost(BaseCost,
                    RecCost, AvgMaxDepth),
                AnyRecCost = single_rec_recursion_cost(BaseCost, RecCost),
                Type = rt_single(BaseLevel, RecLevel, AvgMaxDepth, AvgRecCost,
                    AnyRecCost)
            else if
                Maximum = 2,
                not assoc_list.search(Levels, 1, _)
            then
                ( if assoc_list.search(Levels, 2, RLRec) then
                    RLRec = recursion_level(RecCost, RecProb),
                    RecCountF = probability_to_float(RecProb) * ParentCalls,
                    RecLevel = recursion_level_report(2,
                        round_to_int(RecCountF), RecProb, RecCost,
                        RecCountF*2.0)
                else
                    string.format("maximum level %d not found", [i(1)], Msg),
                    unexpected($module, $pred, Msg)
                ),
                Type = rt_divide_and_conquer(BaseLevel, RecLevel)
            else
                list.map(recursion_level_report(TotalCalls), Levels,
                    LevelsReport),
                Type = rt_other(LevelsReport)
            )
        else
            Messages = list.map(error_to_string, to_sorted_list(Errors)),
            Type = rt_errors(Messages)
        )
    ).

:- pred recursion_level_report(float::in, pair(int, recursion_level)::in,
    recursion_level_report::out) is det.

recursion_level_report(TotalCalls, Level - recursion_level(NonRecCost, Prob),
        recursion_level_report(Level, Calls, Prob, NonRecCost, CostExChild)) :-
    CallsF = probability_to_float(Prob) * TotalCalls,
    Calls = round_to_int(CallsF),
    CostExChild = float(Level) * CallsF.

    % This uses the formula.
    %
    % Base + Shared + Level (Rec + Shared) = Cost.
    %
    % To calculate the Cost of a recursive call at a depth of Level in a singly
    % recursive procedure from:
    %   Base - The cost of the base case.
    %   Rec - The cost of the recursive call except for the recursive call
    %         itself.
    %   Shared - The cost of code common to both cases.
    %
    % The + 1.0 counts for the cost of the recursive call itself, The Shared
    % variable has already been factored into BaseCost and RecCost.
    %
:- func single_rec_recursion_cost(float, float, int) = float.

single_rec_recursion_cost(BaseCost, RecCost, LevelI) = Cost :-
    Cost = BaseCost + (float(LevelI) * (RecCost + 1.0)).

    % This formula is derived as follows.
    %
    % It's the average (sum of all recursion levels divided by number of
    % levels).
    %
    %   Sum l in 0..MaxLevel ( Base + l * Rec ) / ( MaxLevel + 1 )
    %
    % Factor out the cost of the base case and start the sum from level 1 in
    % the recursive case.
    %
    %   ( Base(MaxLevel + 1) + Sum l in 1..MaxLevel ( l * Rec ) ) /
    %     ( MaxLevel + 1 )
    %
    % Simplify.
    %
    %   Base + Sum l in 1..MaxLevel ( i * Rec ) / (MaxLevel + 1)
    %
    % Recall that Sum i in 1..N (i) = (N^2 + N) / 2
    %
    %   Base + (((L^2 + L) * Rec) / 2) / (MaxLevel + 1)
    %
:- func single_rec_average_recursion_cost(float, float, float) = float.

single_rec_average_recursion_cost(BaseCost, RecCost, AvgMaxDepth) = Cost :-
    Sum = 0.5 * RecCost * ((AvgMaxDepth * AvgMaxDepth) + AvgMaxDepth),
    Cost = BaseCost + ((Sum) / (AvgMaxDepth + 1.0)).

%---------------------------------------------------------------------------%

:- type recursion_data
    --->    no_recursion_data_dead_proc
            % There is no recursion data for this proc, since it is
            % never called.
    ;       recursion_data(
                rd_recursions           :: assoc_list(int, recursion_level),
                rd_maximum              :: int,
                rd_errors               :: set(recursion_error)
            ).

:- type recursion_level
    --->    recursion_level(
                rl_cost                 :: float,

                % The probability the path leading to this recursion level is
                % called given that the goal is called.
                rl_probability          :: probability
            ).

:- type recursion_error
    --->    re_unhandled_determinism(detism_rep).

:- type recursion_analysis_info
    --->    recursion_analysis_info(
                rai_this_clique         :: clique_ptr,
                rai_call_sites          ::
                    map(reverse_goal_path, cost_and_callees),
                rai_coverage_info       :: goal_attr_array(coverage_info)
            ).

    % goal_recursion_data(RecursiveCallees, Goal, GoalPath,
    %   init_recursion_data, RecursionData)
    %
    % Compute RecursionData about Goal if RecursiveCalls are calls
    % that may eventually lead to Goal.
    %
:- pred goal_recursion_data(recursion_analysis_info::in,
    reverse_goal_path::in, goal_rep(goal_id)::in, recursion_data::out)
    is det.

goal_recursion_data(Info, RevGoalPath, GoalRep, !:RecursionData) :-
    GoalRep = goal_rep(GoalExpr, Detism, GoalId),
    CoverageInfo = get_goal_attribute_det(Info ^ rai_coverage_info, GoalId),
    ( if get_coverage_before(CoverageInfo, CallsPrime) then
        Calls = CallsPrime
    else
        unexpected($module, $pred, "couldn't retrieve coverage information")
    ),
    ( if Calls = 0 then
        !:RecursionData = no_recursion_data_dead_proc
    else
        (
            GoalExpr = conj_rep(Conjs),
            conj_recursion_data(Info, RevGoalPath, 1, Conjs,
                !:RecursionData)
        ;
            GoalExpr = disj_rep(Disjs),
            disj_recursion_data(Info, RevGoalPath, 1, Disjs,
                !:RecursionData)
        ;
            GoalExpr = switch_rep(_, _, Cases),
            switch_recursion_data(Info, RevGoalPath, 1, Cases,
                float(Calls), Calls, !:RecursionData)
        ;
            GoalExpr = ite_rep(Cond, Then, Else),
            ite_recursion_data(Info, RevGoalPath, Cond, Then, Else,
                Calls, !:RecursionData)
        ;
            (
                GoalExpr = negation_rep(SubGoal),
                GoalPathStep = step_neg
            ;
                GoalExpr = scope_rep(SubGoal, MaybeCut),
                GoalPathStep = step_scope(MaybeCut)
            ),
            goal_recursion_data(Info, rgp_cons(RevGoalPath, GoalPathStep),
                SubGoal, !:RecursionData)
        ;
            GoalExpr = atomic_goal_rep(_, _, _, AtomicGoalRep),
            atomic_goal_recursion_data(Info, RevGoalPath, AtomicGoalRep,
                !:RecursionData)
        )
    ),
    (
        ( Detism = det_rep
        ; Detism = semidet_rep
        ; Detism = cc_nondet_rep
        ; Detism = cc_multidet_rep
        ; Detism = erroneous_rep
        ; Detism = failure_rep
        )
    ;
        ( Detism = nondet_rep
        ; Detism = multidet_rep
        ),
        recursion_data_add_error(re_unhandled_determinism(Detism),
            !RecursionData)
    ).

:- pred conj_recursion_data(recursion_analysis_info::in,
    reverse_goal_path::in, int::in, list(goal_rep(goal_id))::in,
    recursion_data::out) is det.

conj_recursion_data(_, _, _, [], simple_recursion_data(0.0, 0)).
    % An empty conjunction represents "true", so there is
    % exactly one trivial path through it with 0 recursive calls.
conj_recursion_data(Info, RevGoalPath, ConjNum, [Conj | Conjs],
        RecursionData) :-
    goal_recursion_data(Info, rgp_cons(RevGoalPath, step_conj(ConjNum)), Conj,
        ConjRecursionData),
    (
        ConjRecursionData = no_recursion_data_dead_proc,
        % If the first conjunct is dead then the remaining ones will
        % also be dead. This speeds up execution and avoids a divide by zero
        % when calculating ConjSuccessProb below.
        RecursionData = no_recursion_data_dead_proc
    ;
        ConjRecursionData = recursion_data(_, _, _),

        conj_recursion_data(Info, RevGoalPath, ConjNum + 1, Conjs,
            ConjsRecursionData0),
        CanFail = detism_get_can_fail(Conj ^ goal_detism_rep),
        (
            CanFail = cannot_fail_rep,
            merge_recursion_data_sequence(ConjRecursionData,
                ConjsRecursionData0, RecursionData)
        ;
            CanFail = can_fail_rep,
            % It's possible that the conjunct can fail, in that case the code
            % branches into one branch that continues with the conjunction and
            % one that doesn't.

            CoverageInfo = get_goal_attribute_det(Info ^ rai_coverage_info,
                Conj ^ goal_annotation),
            success_probability_from_coverage(CoverageInfo, ConjSuccessProb),
            recursion_data_and_probability(ConjSuccessProb,
                ConjsRecursionData0, ConjsRecursionData),

            ConjFailureProb = not_probability(ConjSuccessProb),
            Failure0 = simple_recursion_data(0.0, 0),
            recursion_data_and_probability(ConjFailureProb, Failure0, Failure),
            merge_recursion_data_after_branch(ConjsRecursionData, Failure,
                BranchRecursionData),
            merge_recursion_data_sequence(ConjRecursionData,
                BranchRecursionData, RecursionData)
        )
    ).

:- pred disj_recursion_data(recursion_analysis_info::in,
    reverse_goal_path::in, int::in, list(goal_rep(goal_id))::in,
    recursion_data::out) is det.

disj_recursion_data(_, _, _, [], simple_recursion_data(0.0, 0)).
disj_recursion_data(Info, RevGoalPath, DisjNum, [Disj | Disjs],
        RecursionData) :-
    % Handle only semidet and committed-choice disjunctions, which cannot be
    % re-entered once a disjunct succeeds.
    goal_recursion_data(Info, rgp_cons(RevGoalPath, step_disj(DisjNum)), Disj,
        DisjRecursionData),
    (
        DisjRecursionData = no_recursion_data_dead_proc,
        % If the first disjunct was never tried, then no other disjuncts will
        % ever be tried.
        RecursionData = no_recursion_data_dead_proc
    ;
        DisjRecursionData = recursion_data(_, _, _),
        CoverageInfo = get_goal_attribute_det(Info ^ rai_coverage_info,
            Disj ^ goal_annotation),
        success_probability_from_coverage(CoverageInfo,
            DisjSuccessProb),
        DisjFailureProb = not_probability(DisjSuccessProb),

        % The code can branch here, either it tries the next disjunct, which we
        % represent as DisjsRecursionData, ...
        disj_recursion_data(Info, RevGoalPath, DisjNum + 1, Disjs,
            DisjsRecursionData0),
        recursion_data_and_probability(DisjFailureProb, DisjsRecursionData0,
            DisjsRecursionData),

        % ... or it succeeds, which we represent as finished.
        Finish0 = simple_recursion_data(0.0, 0),
        recursion_data_and_probability(DisjSuccessProb, Finish0, Finish),

        % Then the result the branch of (DisjsRecursionData or finish) appended
        % to DisjRecursionData.
        merge_recursion_data_after_branch(Finish, DisjsRecursionData,
            BranchRecursionData),
        merge_recursion_data_sequence(DisjRecursionData, BranchRecursionData,
            RecursionData)
    ).

:- pred success_probability_from_coverage(coverage_info::in, probability::out)
    is det.

success_probability_from_coverage(Coverage, SuccessProb) :-
    ( if get_coverage_before_and_after(Coverage, Before, After) then
        ( if After > Before then
            % Nondet code can overflow this probability.
            SuccessProb = certain
        else
            SuccessProb = probable(float(After) / float(Before))
        )
    else
        unexpected($module, $pred, "expected complete coverage information")
    ).

:- pred ite_recursion_data(recursion_analysis_info::in,
    reverse_goal_path::in,
    goal_rep(goal_id)::in, goal_rep(goal_id)::in, goal_rep(goal_id)::in,
    int::in, recursion_data::out) is det.

ite_recursion_data(Info, RevGoalPath, Cond, Then, Else, Calls,
        !:RecursionData) :-
    goal_recursion_data(Info, rgp_cons(RevGoalPath, step_ite_cond), Cond,
        CondRecursionData),
    goal_recursion_data(Info, rgp_cons(RevGoalPath, step_ite_then), Then,
        ThenRecursionData0),
    goal_recursion_data(Info, rgp_cons(RevGoalPath, step_ite_else), Else,
        ElseRecursionData0),

    % Adjust the probabilities of executing the then and else branches.
    Coverage = Info ^ rai_coverage_info,
    ThenCoverageInfo =
        get_goal_attribute_det(Coverage, Then ^ goal_annotation),
    ElseCoverageInfo =
        get_goal_attribute_det(Coverage, Else ^ goal_annotation),
    get_coverage_before_det(ThenCoverageInfo, ThenCalls),
    get_coverage_before_det(ElseCoverageInfo, ElseCalls),
    CallsF = float(Calls),
    ThenProb = probable(float(ThenCalls) / CallsF),
    ElseProb = probable(float(ElseCalls) / CallsF),
    recursion_data_and_probability(ThenProb,
        ThenRecursionData0, ThenRecursionData),
    recursion_data_and_probability(ElseProb,
        ElseRecursionData0, ElseRecursionData),

    % Because the condition goal has coverage information as if it is
    % entered before either branch, we have to model it in the same way here,
    % even though it would be feasible to model it as something that happens
    % in sequence with both the then and else branches (within each branch).
    merge_recursion_data_after_branch(ThenRecursionData,
        ElseRecursionData, !:RecursionData),
    merge_recursion_data_sequence(CondRecursionData, !RecursionData).

:- pred switch_recursion_data(recursion_analysis_info::in,
    reverse_goal_path::in, int::in, list(case_rep(goal_id))::in,
    float::in, int::in, recursion_data::out) is det.

switch_recursion_data(_, _, _, [], TotalCalls, CallsRemaining,
        RecursionData) :-
    % Can fail switches will have a nonzero probability of reaching this case.
    FailProb = probable(float(CallsRemaining) / TotalCalls),
    RecursionData0 = simple_recursion_data(0.0, 0),
    recursion_data_and_probability(FailProb, RecursionData0, RecursionData).
switch_recursion_data(Info, RevGoalPath, CaseNum, [Case | Cases],
        TotalCalls, CallsRemaining, RecursionData) :-
    Case = case_rep(_, _, Goal),
    RevArmPath = rgp_cons(RevGoalPath,
        step_switch(CaseNum, unknown_num_functors_in_type)),
    goal_recursion_data(Info, RevArmPath, Goal, CaseRecursionData0),
    CoverageInfo = get_goal_attribute_det(Info ^ rai_coverage_info,
        Goal ^ goal_annotation),
    ( if get_coverage_before(CoverageInfo, CallsPrime) then
        Calls = CallsPrime
    else
        unexpected($module, $pred, "expected coverage information")
    ),
    CaseProb = probable(float(Calls) / TotalCalls),
    recursion_data_and_probability(CaseProb, CaseRecursionData0,
        CaseRecursionData),
    switch_recursion_data(Info, RevGoalPath, CaseNum+1,
        Cases, TotalCalls, CallsRemaining - Calls, CasesRecursionData),
    merge_recursion_data_after_branch(CaseRecursionData, CasesRecursionData,
        RecursionData).

:- pred atomic_goal_recursion_data(recursion_analysis_info::in,
    reverse_goal_path::in, atomic_goal_rep::in, recursion_data::out) is det.

atomic_goal_recursion_data(Info, RevGoalPath, AtomicGoal, RecursionData) :-
    (
        % All these things have trivial cost except for foreign code whose cost
        % is unknown (which because it doesn't contribute to the cost of the
        % caller we assume that it is trivial)..
        ( AtomicGoal = unify_construct_rep(_, _, _)
        ; AtomicGoal = unify_deconstruct_rep(_, _, _)
        ; AtomicGoal = partial_deconstruct_rep(_, _, _)
        ; AtomicGoal = partial_construct_rep(_, _, _)
        ; AtomicGoal = unify_assign_rep(_, _)
        ; AtomicGoal = cast_rep(_, _)
        ; AtomicGoal = unify_simple_test_rep(_, _)
        ; AtomicGoal = pragma_foreign_code_rep(_)
        ; AtomicGoal = builtin_call_rep(_, _, _)
        ; AtomicGoal = event_call_rep(_, _)
        ),
        RecursionLevel = 0 - recursion_level(0.0, certain)
    ;
        ( AtomicGoal = higher_order_call_rep(_, _)
        ; AtomicGoal = method_call_rep(_, _, _)
        ; AtomicGoal = plain_call_rep(_, _, _)
        ),

        % Get the cost of the call.
        Info = recursion_analysis_info(ThisClique, CallSiteMap, _),
        map.lookup(CallSiteMap, RevGoalPath, CostAndCallees),
        ( if cost_and_callees_is_recursive(ThisClique, CostAndCallees) then
            % Cost will be 1.0 for for each call to recursive calls but we
            % calculate this later.
            RecursionLevel = 1 - recursion_level(0.0, certain)
        else
            CostPercall = cs_cost_get_percall(CostAndCallees ^ cac_cost),
            RecursionLevel = 0 - recursion_level(CostPercall, certain)
        )
    ),
    RecursionLevel = RecursiveCalls - _,
    RecursionData = recursion_data([RecursionLevel], RecursiveCalls, init).

    % Consider the following nested switches:
    %
    % (
    %     (
    %         base1
    %     ;
    %         rec1
    %     )
    % ;
    %     (
    %         base2
    %     ;
    %         rec2
    %     )
    % )
    %
    % + The cost of entering a base case is the weighted average of the costs
    %   of the two base cases.
    % + The number of times one enters a base case is the sum of the
    %   individual counts.
    % + The above two rules are also true for recursive cases.
    %
:- pred merge_recursion_data_after_branch(recursion_data::in,
    recursion_data::in, recursion_data::out) is det.

merge_recursion_data_after_branch(A, B, Result) :-
    (
        A = recursion_data(RecursionsA, MaxLevelA, ErrorsA),
        B = recursion_data(RecursionsB, MaxLevelB, ErrorsB),
        Recursions0 = assoc_list.merge(RecursionsA, RecursionsB),
        condense_recursions(Recursions0, Recursions),
        MaxLevel = max(MaxLevelA, MaxLevelB),
        Errors = union(ErrorsA, ErrorsB),
        Result = recursion_data(Recursions, MaxLevel, Errors)
    ;
        A = recursion_data(_, _, _),
        B = no_recursion_data_dead_proc,
        Result = A
    ;
        A = no_recursion_data_dead_proc,
        B = recursion_data(_, _, _),
        Result = B
    ;
        A = no_recursion_data_dead_proc,
        B = no_recursion_data_dead_proc,
        Result = no_recursion_data_dead_proc
    ).

    % merge_recursion_data_sequence(A, B, Merged).
    %
    % Merge the recursion datas A and B to produce Merged.
    % This is not commutative; A must represent something occurring before B.
    %
    % Consider the following conjoined switches.
    %
    % (
    %     base1
    % ;
    %     rec1
    % ),
    % (
    %     base2
    % ;
    %     rec2
    % )
    %
    % It's like algebra! Treating the conjunction as multiplication and
    % disjunction as addition we might factorise it as:
    %
    % base1*base2 + base1*rec2 + base2*rec1 + rec1*rec2.
    %
    % That is, there is one base case, two recursive cases, and a doubly
    % recursive case.
    %
    % We have to convert counts to probabilities, then:
    %
    % + The probability of entering the base case is the product of the
    %   probabilities of entering either base case.
    % + Similarly the probability of entering any other case is the product the
    %   probabilities of their components.
    % + The cost of entering the base case is the sum of the costs of the
    %   components.
    % + Similarly for the other cases.
    %
:- pred merge_recursion_data_sequence(recursion_data::in,
    recursion_data::in, recursion_data::out) is det.

merge_recursion_data_sequence(A, B, Result) :-
    (
        A = recursion_data(RecursionsA, MaxLevelA, ErrorsA),
        B = recursion_data(RecursionsB, MaxLevelB, ErrorsB),
        recursions_cross_product(RecursionsA, RecursionsB, Recursions0),
        sort(Recursions0, Recursions1),
        condense_recursions(Recursions1, Recursions),
        % The maximum number of recursions on any path will be the sum of
        % the maximum number of recursions on the two conjoined paths,
        % since all paths are conjoined in the cross product.
        MaxLevel = MaxLevelA + MaxLevelB,
        Errors = union(ErrorsA, ErrorsB),
        Result = recursion_data(Recursions, MaxLevel, Errors)
    ;
        A = recursion_data(_, _, _),
        B = no_recursion_data_dead_proc,
        Result = no_recursion_data_dead_proc
    ;
        A = no_recursion_data_dead_proc,
        Result = no_recursion_data_dead_proc
    ).

:- pred condense_recursions(assoc_list(int, recursion_level)::in,
    assoc_list(int, recursion_level)::out) is det.

condense_recursions([], []).
condense_recursions([Num - Rec | Pairs0], Pairs) :-
    condense_recursions_2(Num - Rec, Pairs0, Pairs).

:- pred condense_recursions_2(pair(int, recursion_level)::in,
    assoc_list(int, recursion_level)::in,
    assoc_list(int, recursion_level)::out) is det.

condense_recursions_2(Pair, [], [Pair]).
condense_recursions_2(NumA - RecA, [NumB - RecB | Pairs0], Pairs) :-
    ( if NumA = NumB then
        RecA = recursion_level(CostA, ProbabilityA),
        RecB = recursion_level(CostB, ProbabilityB),
        weighted_average(
            [probability_to_float(ProbabilityA),
            probability_to_float(ProbabilityB)],
            [CostA, CostB], Cost),
        Probability = or(ProbabilityA, ProbabilityB),
        Rec = recursion_level(Cost, Probability),
        condense_recursions_2(NumA - Rec, Pairs0, Pairs)
    else
        condense_recursions([NumB - RecB | Pairs0], Pairs1),
        Pairs = [NumA - RecA | Pairs1]
    ).

    % recursions_cross_product(A, B, C).
    %
    % A X B = C <=> A.1 * B.1 + A.1 * B.2 + A.2 * B.1 + A.2 * B.2 = C
    %
    % Note that this is not commutative. A represents a computation occurring
    % before B.
    %
:- pred recursions_cross_product(assoc_list(int, recursion_level)::in,
    assoc_list(int, recursion_level)::in,
    assoc_list(int, recursion_level)::out) is det.

recursions_cross_product([], _, []).
recursions_cross_product([NumA - RecA | PairsA], PairsB, Pairs) :-
    recursions_cross_product_2(NumA, RecA, PairsB, InnerLoop),
    recursions_cross_product(PairsA, PairsB, OuterLoopTail),
    Pairs = InnerLoop ++ OuterLoopTail.

:- pred recursions_cross_product_2(int::in, recursion_level::in,
    assoc_list(int, recursion_level)::in,
    assoc_list(int, recursion_level)::out) is det.

recursions_cross_product_2(_Num, _Rec, [], []).
recursions_cross_product_2(NumA, RecA@recursion_level(CostA, ProbA),
        [NumB - recursion_level(CostB, ProbB) | PairsB], Pairs) :-
    recursions_cross_product_2(NumA, RecA, PairsB, Pairs0),
    Num = NumA + NumB,
    Prob = and(ProbA, ProbB),
    Cost = CostA + CostB,
    Pair = Num - recursion_level(Cost, Prob),
    Pairs = [Pair | Pairs0].

:- pred recursion_data_and_probability(probability::in, recursion_data::in,
    recursion_data::out) is det.

recursion_data_and_probability(Prob,
        recursion_data(!.Recursions, MaxLevel, Errors),
        recursion_data(!:Recursions, MaxLevel, Errors)) :-
    map_values(recursion_level_and_probability(Prob), !Recursions).
recursion_data_and_probability(_,
    no_recursion_data_dead_proc,
    no_recursion_data_dead_proc).

:- pred recursion_level_and_probability(probability::in, T::in,
    recursion_level::in, recursion_level::out) is det.

recursion_level_and_probability(AndProb, _,
        recursion_level(Cost, Prob0), recursion_level(Cost, Prob)) :-
    Prob = and(Prob0, AndProb).

:- pred recursion_data_add_error(recursion_error::in, recursion_data::in,
    recursion_data::out) is det.

recursion_data_add_error(Error, !RecursionData) :-
    some [!Errors] (
        (
            !.RecursionData = recursion_data(_, _, !:Errors),
            set.insert(Error, !Errors),
            !RecursionData ^ rd_errors := !.Errors
        ;
            !.RecursionData = no_recursion_data_dead_proc
        )
    ).

    % simple_recursion_data(Cost, RecCalls) = RecursionData.
    %
    % Create a simple recursion data item from a single level.
    %
:- func simple_recursion_data(float, int) = recursion_data.

simple_recursion_data(Cost, Calls) =
    recursion_data([Calls - recursion_level(Cost, certain)], Calls, init).

:- func error_to_string(recursion_error) = string.

error_to_string(re_unhandled_determinism(Detism)) =
    format("%s code is not handled", [s(string(Detism))]).

%---------------------------------------------------------------------------%

create_recursion_types_frequency_report(Deep, MaybeReport) :-
    % This report is impossible without procrep data, but we don't use it
    % directly.
    MaybeProgRepResult = Deep ^ procrep_file,
    (
        MaybeProgRepResult = no,
        MaybeReport = error("There is no readable " ++
            "procedure representation information file.")
    ;
        MaybeProgRepResult = yes(error(Error)),
        MaybeReport = error("Error reading procedure representation " ++
            "information file: " ++ Error)
    ;
        MaybeProgRepResult = yes(ok(_)),
        Cliques = Deep ^ clique_index,
        size(Cliques, NumCliques),
        array_foldl_from_1(rec_types_freq_build_histogram(Deep), Cliques,
            map.init, Histogram0),
        finalize_histogram(Deep, NumCliques, Histogram0, Histogram),
        MaybeReport = ok(recursion_types_frequency_report(Histogram))
    ).

:- pred rec_types_freq_build_histogram(deep::in, int::in, clique_ptr::in,
    map(recursion_type_simple, recursion_type_raw_freq_data)::in,
    map(recursion_type_simple, recursion_type_raw_freq_data)::out) is det.

rec_types_freq_build_histogram(Deep, _, CliquePtr, !Histogram) :-
    trace [io(!IO)] (
        clique_ptr(CliqueNum) = CliquePtr,
        io.format("Analyzing clique: %d\n", [i(CliqueNum)], !IO)
    ),
    create_clique_recursion_costs_report(Deep, CliquePtr,
        MaybeCliqueRecursionReport),
    (
        MaybeCliqueRecursionReport = ok(CliqueRecursionReport),
        Type = CliqueRecursionReport ^ crr_recursion_type,
        recursion_type_to_simple_type(Type, SimpleTypes)
    ;
        MaybeCliqueRecursionReport = error(Error),
        SimpleTypes = [rts_error(Error), rts_total_error_instances]
    ),
    find_clique_first_and_other_procs(Deep, CliquePtr, MaybeFirstPDPtr,
        _OtherPDPtrs),
    (
        MaybeFirstPDPtr = yes(FirstPDPtr),
        lookup_proc_dynamics(Deep ^ proc_dynamics, FirstPDPtr, FirstPD),
        FirstPSPtr = FirstPD ^ pd_proc_static,
        PDesc = describe_proc(Deep, FirstPSPtr),
        lookup_pd_own(Deep ^ pd_own, FirstPDPtr, ProcOwn),
        lookup_pd_desc(Deep ^ pd_desc, FirstPDPtr, ProcInherit),
        FirstProcInfo = first_proc_info(PDesc,
            own_and_inherit_prof_info(ProcOwn, ProcInherit)),
        MaybeFirstProcInfo = yes(FirstProcInfo)
    ;
        MaybeFirstPDPtr = no,
        MaybeFirstProcInfo = no
    ),
    list.foldl(update_histogram(MaybeFirstProcInfo), SimpleTypes, !Histogram).

:- type first_proc_info
    --->    first_proc_info(
                fpi_pdesc               :: proc_desc,
                fpi_prof_info           :: own_and_inherit_prof_info
            ).

    % XXX Consider moving this to measurements.m
    %
:- type own_and_inherit_prof_info
    --->    own_and_inherit_prof_info(
                oai_own                 :: own_prof_info,
                oai_inherit             :: inherit_prof_info
            ).

:- pred add_own_and_inherit_prof_info(own_and_inherit_prof_info::in,
    own_and_inherit_prof_info::in, own_and_inherit_prof_info::out) is det.

add_own_and_inherit_prof_info(
        own_and_inherit_prof_info(OwnA, InheritA),
        own_and_inherit_prof_info(OwnB, InheritB),
        own_and_inherit_prof_info(Own, Inherit)) :-
    Own = add_own_to_own(OwnA, OwnB),
    Inherit = add_inherit_to_inherit(InheritA, InheritB).

:- type recursion_type_raw_freq_data
    --->    recursion_type_raw_freq_data(
                rtrfd_freq              :: int,
                rtrfd_maybe_prof_info   :: maybe(own_and_inherit_prof_info),
                rtrfd_entry_procs       :: map(proc_static_ptr,
                    recursion_type_raw_proc_freq_data)
            ).

:- type recursion_type_raw_proc_freq_data
    --->    recursion_type_raw_proc_freq_data(
                rtrpfd_freq             :: int,
                rtrpfd_prof_info        :: own_and_inherit_prof_info,
                rtrpfd_proc_desc        :: proc_desc
            ).

:- pred update_histogram(maybe(first_proc_info)::in,
    recursion_type_simple::in,
    map(recursion_type_simple, recursion_type_raw_freq_data)::in,
    map(recursion_type_simple, recursion_type_raw_freq_data)::out) is det.

update_histogram(MaybeFirstProcInfo, SimpleType, !Histogram) :-
    ( if map.search(!.Histogram, SimpleType, Data0) then
        Data0 = recursion_type_raw_freq_data(Count0, MaybeProfInfo0, Procs0),
        (
            MaybeFirstProcInfo = yes(FirstProcInfo),
            (
                MaybeProfInfo0 = yes(ProfInfo0),
                add_own_and_inherit_prof_info(FirstProcInfo ^ fpi_prof_info,
                    ProfInfo0, ProfInfo)
            ;
                MaybeProfInfo0 = no,
                ProfInfo = FirstProcInfo ^ fpi_prof_info
            ),
            MaybeProfInfo = yes(ProfInfo),
            update_procs_map(FirstProcInfo, Procs0, Procs)
        ;
            MaybeFirstProcInfo = no,
            MaybeProfInfo = MaybeProfInfo0,
            Procs = Procs0
        ),
        Count = Count0 + 1,
        Data = recursion_type_raw_freq_data(Count, MaybeProfInfo, Procs)
    else
        Count = 1,
        (
            MaybeFirstProcInfo = yes(FirstProcInfo),
            MaybeProfInfo = yes(FirstProcInfo ^ fpi_prof_info),
            update_procs_map(FirstProcInfo, map.init, Procs)
        ;
            MaybeFirstProcInfo = no,
            MaybeProfInfo = no,
            Procs = map.init
        ),
        Data = recursion_type_raw_freq_data(Count, MaybeProfInfo, Procs)
    ),
    map.set(SimpleType, Data, !Histogram).

:- pred update_procs_map(first_proc_info::in,
    map(proc_static_ptr, recursion_type_raw_proc_freq_data)::in,
    map(proc_static_ptr, recursion_type_raw_proc_freq_data)::out) is det.

update_procs_map(FirstProcInfo, !Map) :-
    FirstProcInfo = first_proc_info(PSDesc, FirstProfInfo),
    PsPtr = PSDesc ^ pdesc_ps_ptr,
    ( if map.search(!.Map, PsPtr, ProcFreqData0) then
        ProcFreqData0 =
            recursion_type_raw_proc_freq_data(Count0, ProfInfo0, ProcDesc),
        add_own_and_inherit_prof_info(FirstProfInfo, ProfInfo0, ProfInfo),
        Count = Count0 + 1,
        ProcFreqData =
            recursion_type_raw_proc_freq_data(Count, ProfInfo, ProcDesc)
    else
        ProcFreqData =
            recursion_type_raw_proc_freq_data(1, FirstProfInfo, PSDesc)
    ),
    map.set(PsPtr, ProcFreqData, !Map).

:- pred recursion_type_to_simple_type(recursion_type::in,
    list(recursion_type_simple)::out) is det.

recursion_type_to_simple_type(rt_not_recursive, [rts_not_recursive]).
recursion_type_to_simple_type(rt_single(_, _, _, _, _), [rts_single]).
recursion_type_to_simple_type(rt_divide_and_conquer(_, _),
    [rts_divide_and_conquer]).
recursion_type_to_simple_type(rt_mutual_recursion(NumProcs),
    [rts_mutual_recursion(NumProcs)]).
recursion_type_to_simple_type(rt_other(Levels), [rts_other(SimpleLevels)]) :-
    SimpleLevels = set.from_list(
        map((func(Level) = Level ^ rlr_level), Levels)).
recursion_type_to_simple_type(rt_errors(Errors), SimpleTypes) :-
    SimpleTypes =
        list.map((func(E) = rts_error(E)), Errors)
        ++ [rts_total_error_instances].

:- pred finalize_histogram(deep::in, int::in,
    map(recursion_type_simple, recursion_type_raw_freq_data)::in,
    map(recursion_type_simple, recursion_type_freq_data)::out) is det.

finalize_histogram(Deep, NumCliques, !Histogram) :-
    map_values(finalize_histogram_rec_type(Deep, float(NumCliques)),
        !Histogram).

:- pred finalize_histogram_rec_type(deep::in, float::in,
    recursion_type_simple::in,
    recursion_type_raw_freq_data::in, recursion_type_freq_data::out) is det.

finalize_histogram_rec_type(Deep, NumCliques, _RecursionType,
        recursion_type_raw_freq_data(Freq, MaybeProfInfo, !.EntryProcs),
        recursion_type_freq_data(Freq, Percent, MaybeSummary, !:EntryProcs)) :-
    Percent = percent(float(Freq) / NumCliques),
    (
        MaybeProfInfo = no,
        MaybeSummary = no
    ;
        MaybeProfInfo = yes(ProfInfo),
        ProfInfo = own_and_inherit_prof_info(Own, Inherit),
        own_and_inherit_to_perf_row_data(Deep, unit, Own, Inherit, Summary),
        MaybeSummary = yes(Summary)
    ),
    map_values(finalize_histogram_proc_rec_type(Deep, NumCliques),
        !EntryProcs).

:- pred finalize_histogram_proc_rec_type(deep::in, float::in,
    proc_static_ptr::in,
    recursion_type_raw_proc_freq_data::in, recursion_type_proc_freq_data::out)
    is det.

finalize_histogram_proc_rec_type(Deep, NumCliques, _PSPtr,
        recursion_type_raw_proc_freq_data(Freq, ProfInfo, ProcDesc),
        recursion_type_proc_freq_data(Freq, Percent, Summary)) :-
    Percent = percent(float(Freq) / NumCliques),
    ProfInfo = own_and_inherit_prof_info(Own, Inherit),
    own_and_inherit_to_perf_row_data(Deep, ProcDesc, Own, Inherit, Summary).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

recursion_type_get_maybe_avg_max_depth(rt_not_recursive,
    yes(recursion_depth_from_float(0.0))).
recursion_type_get_maybe_avg_max_depth(rt_single(_, _, Depth, _, _),
    yes(recursion_depth_from_float(Depth))).
recursion_type_get_maybe_avg_max_depth(rt_divide_and_conquer(_, _), no).
recursion_type_get_maybe_avg_max_depth(rt_mutual_recursion(_), no).
recursion_type_get_maybe_avg_max_depth(rt_other(_), no).
recursion_type_get_maybe_avg_max_depth(rt_errors(_), no).

%---------------------------------------------------------------------------%
:- end_module recursion_patterns.
%---------------------------------------------------------------------------%

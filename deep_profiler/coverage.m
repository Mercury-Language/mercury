%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Authors: pbone, zs.
%
% This file implements the coverage propagation algorithm, which attaches
% coverage information to the component goals of a procedure body.
%
%---------------------------------------------------------------------------%

:- module coverage.

:- interface.

:- import_module analysis_utils.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.
:- import_module measurements.

:- import_module array.
:- import_module list.
:- import_module map.

:- type coverage_info
    --->    coverage_unknown
    ;       coverage_known_zero
    ;       coverage_known_same(int)
            % Coverage is known both before and after the goal, and the
            % coverage is the same before as it is after.
    ;       coverage_known(int, int)
            % Coverage is known both before and after the goal.
    ;       coverage_known_before(int)
            % Coverage is known only before the goal.
    ;       coverage_known_after(int).
            % Coverage is known only before after goal.

    % Coverage information helper predicates.
    %
:- pred get_coverage_before(coverage_info::in, int::out) is semidet.
:- pred get_coverage_before_and_after(coverage_info::in, int::out, int::out)
    is semidet.
:- pred get_coverage_after(coverage_info::in, int::out) is semidet.

:- pred get_coverage_before_det(coverage_info::in, int::out) is det.
:- pred get_coverage_before_and_after_det(coverage_info::in,
    int::out, int::out) is det.
:- pred get_coverage_after_det(coverage_info::in, int::out) is det.

%---------------------------------------------------------------------------%

    % This is similar to the coverage_point type in
    % mdbcomp/program_representation.m, however it includes an integer count
    % of how often execution reached this point in the program.
    %
:- type coverage_point
    --->    coverage_point(
                % The number of times execution reached this point,
                int,

                % Identifies the goal that this coverage point is near.
                % If cp_type is cp_type_branch_arm the coverage point is
                % immediately before this goal, otherwise it is immediately
                % after.

                reverse_goal_path,

                % The type of this coverage point.
                cp_type
            ).

    % Produce a list of coverage points from an array of static data and an
    % array of coverage points.
    %
:- pred coverage_point_arrays_to_list(array(coverage_point_info)::in,
    array(int)::in, list(coverage_point)::out) is det.

%---------------------------------------------------------------------------%

    % Annotate the program representation structure with coverage information.
    %
:- pred procrep_annotate_with_coverage(proc_rep(goal_id)::in,
    own_prof_info::in,
    map(reverse_goal_path, cost_and_callees(T))::in,
    map(reverse_goal_path, coverage_point)::in,
    map(reverse_goal_path, coverage_point)::in,
    containing_goal_map::in, goal_id::in,
    goal_attr_array(coverage_info)::gaa_uo) is det.

:- pred goal_annotate_with_coverage(string_proc_label::in,
    goal_rep(goal_id)::in, own_prof_info::in,
    map(reverse_goal_path, cost_and_callees(T))::in,
    map(reverse_goal_path, coverage_point)::in,
    map(reverse_goal_path, coverage_point)::in,
    containing_goal_map::in, goal_id::in,
    goal_attr_array(coverage_info)::gaa_uo) is det.

%---------------------------------------------------------------------------%

    % foldl(add_coverage_point, CoveragePoints, map.init, SolnsCPs,
    %   map.init, BranchCPs),
    %
:- pred add_coverage_point_to_map(coverage_point::in,
    map(reverse_goal_path, coverage_point)::in,
    map(reverse_goal_path, coverage_point)::out,
    map(reverse_goal_path, coverage_point)::in,
    map(reverse_goal_path, coverage_point)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module io.
:- import_module maybe.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

get_coverage_before(coverage_known(Before, _), Before).
get_coverage_before(coverage_known_zero, 0).
get_coverage_before(coverage_known_same(Before), Before).
get_coverage_before(coverage_known_before(Before), Before).

get_coverage_before_and_after(coverage_known(Before, After), Before, After).
get_coverage_before_and_after(coverage_known_same(Count), Count, Count).
get_coverage_before_and_after(coverage_known_zero, 0, 0).

get_coverage_after(coverage_known(_, After), After).
get_coverage_after(coverage_known_zero, 0).
get_coverage_after(coverage_known_same(After), After).
get_coverage_after(coverage_known_after(After), After).

get_coverage_before_det(Coverage, Before) :-
    ( if get_coverage_before(Coverage, BeforePrime) then
        Before = BeforePrime
    else
        complete_coverage_error
    ).

get_coverage_before_and_after_det(Coverage, Before, After) :-
    ( if get_coverage_before_and_after(Coverage, BeforePrime, AfterPrime) then
        Before = BeforePrime,
        After = AfterPrime
    else
        complete_coverage_error
    ).

get_coverage_after_det(Coverage, After) :-
    ( if get_coverage_after(Coverage, AfterPrime) then
        After = AfterPrime
    else
        complete_coverage_error
    ).

:- pred complete_coverage_error is erroneous.

complete_coverage_error :-
    unexpected($pred, "Expected complete coverage information").

%---------------------------------------------------------------------------%

coverage_point_arrays_to_list(StaticArray, DynamicArray, CoveragePoints) :-
    array.bounds(StaticArray, Min, Max),
    ( if array.bounds(DynamicArray, Min, Max) then
        true
    else
        unexpected($pred, "bounds do not match")
    ),
    coverage_point_arrays_to_list_2(Min, Max, StaticArray, DynamicArray,
        [], CoveragePoints).

:- pred coverage_point_arrays_to_list_2(int::in, int::in,
    array(coverage_point_info)::in, array(int)::in,
    list(coverage_point)::in, list(coverage_point)::out) is det.

coverage_point_arrays_to_list_2(Num, Max, StaticArray, DynamicArray,
        !CoveragePoints) :-
    ( if Num =< Max then
        array.lookup(StaticArray, Num, coverage_point_info(GoalPath, CPType)),
        array.lookup(DynamicArray, Num, Count),
        CP = coverage_point(Count, GoalPath, CPType),
        !:CoveragePoints = [CP | !.CoveragePoints],
        coverage_point_arrays_to_list_2(Num + 1, Max,
            StaticArray, DynamicArray, !CoveragePoints)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- type coverage_before
    --->    before_unknown
    ;       before_zero
    ;       before_known(int).

:- type coverage_after
    --->    after_unknown
    ;       after_zero
    ;       after_known(int).

:- type sum_coverage_before
    --->    sum_before_unknown
    ;       sum_before_zero
    ;       sum_before_known(int).

:- type sum_coverage_after
    --->    sum_after_unknown
    ;       sum_after_zero
    ;       sum_after_known(int).

    % Annotate a procedure representation structure with coverage information.
    %
    % The following trace flags control debugging for this predicate.
    %
    %   debug_coverage_propagation:
    %       Print out diagnostic messages to aid in the debugging of the
    %       propagation coverage algorithm.
    %
    %   no_coverage_propagation_assertions:
    %       Disable assertions used to test this algorithm, This allows the
    %       algorithm to proceed past the problem and allow the programmer to
    %       view erroneous output.
    %
procrep_annotate_with_coverage(ProcRep, OwnProf, CallSites,
        SolnsCoveragePoints, BranchCoveragePoints, ContainingGoalMap,
        LastGoalId, CoverageArray) :-
    GoalRep = ProcRep ^ pr_defn ^ pdr_goal,
    ProcLabel = ProcRep ^ pr_id,
    goal_annotate_with_coverage(ProcLabel, GoalRep, OwnProf,
        CallSites, SolnsCoveragePoints, BranchCoveragePoints,
        ContainingGoalMap, LastGoalId, CoverageArray).

goal_annotate_with_coverage(ProcLabel, GoalRep, OwnProf, CallSites,
        SolnsCoveragePoints, BranchCoveragePoints, ContainingGoalMap,
        LastGoalId, CoverageArray) :-
    Calls = calls(OwnProf),
    Exits = exits(OwnProf),
    Before = before_coverage(Calls),
    GoalPathMap = create_reverse_goal_path_map(ContainingGoalMap),
    CoverageReference = coverage_reference_info(ProcLabel, CallSites,
        SolnsCoveragePoints, BranchCoveragePoints, GoalPathMap),
    CoverageArray0 = create_goal_id_array(LastGoalId),
    goal_annotate_coverage(GoalRep, CoverageReference,
        Before, After, CoverageArray0, CoverageArray),
    require(unify(After, after_coverage(Exits)),
        "Coverage after procedure not equal with exit count of" ++
        " procedure").


    % These maps are keyed by reverse_goal_path, comparing these structures
    % is less efficient than comparing simple structures like the alternative
    % goal_path_string, however, that involves frequently constructing strings
    % from goal paths.  Using reverse_goal_path_string may be faster but
    % I'd rather not make this optimisation without first testing it.
    %
:- type coverage_reference_info(Callee)
    --->    coverage_reference_info(
                cri_proc        :: string_proc_label,
                cri_call_sites  ::
                    map(reverse_goal_path, cost_and_callees(Callee)),
                cri_solns_coverage_points
                                :: map(reverse_goal_path, coverage_point),
                cri_branch_coverage_points
                                :: map(reverse_goal_path, coverage_point),
                cri_goal_path_map
                                :: goal_reverse_path_map
            ).

    % Annotate a goal and its children with coverage information.
    %
:- pred goal_annotate_coverage(goal_rep(goal_id)::in,
    coverage_reference_info(Callee)::in,
    coverage_before::in, coverage_after::out,
    goal_attr_array(coverage_info)::gaa_di,
    goal_attr_array(coverage_info)::gaa_uo) is det.

goal_annotate_coverage(Goal, Info, Before, After, !Array) :-
    Goal = goal_rep(GoalExpr, Detism, GoalId),
    RevGoalPath = map.lookup(Info ^ cri_goal_path_map, GoalId),

    % Calculate coverage of any inner goals.
    (
        GoalExpr = conj_rep(Conjs),
        conj_annotate_coverage(Conjs, Info, Before, After0, !Array)
    ;
        GoalExpr = disj_rep(Disjs),
        Solutions = detism_get_solutions(Detism),
        % Note: In theory, we could update Before using information from any
        % counter at the start of the first disjunct, but we don't do that
        % (yet).  This may not be useful for some disjunctions, for example
        % those called from a single solution context or committed-choice.
        disj_annotate_coverage(Disjs, Info, Solutions,
            Before, sum_after_zero, SumAfterDisjuncts, !Array),
        after_count_sum_after_count(SumAfterDisjuncts, After0)
    ;
        GoalExpr = switch_rep(_Var, CanFail, Cases),
        switch_annotate_coverage(Cases, Info, CanFail, Before, After0, !Array)
    ;
        GoalExpr = ite_rep(Cond, Then, Else),
        ite_annotate_coverage(Cond, Then, Else, Info, RevGoalPath,
            Before, After0, !Array)
    ;
        GoalExpr = negation_rep(NegGoal),
        negation_annotate_coverage(NegGoal, Info, Before, After0, !Array)
    ;
        GoalExpr = scope_rep(ScopedGoal, MaybeCut),
        scope_annotate_coverage(ScopedGoal, Info, MaybeCut, Before, After0,
            !Array)
    ;
        GoalExpr = atomic_goal_rep(_, _, _, AtomicGoal),
        (
            ( AtomicGoal = plain_call_rep(_, _, _)
            ; AtomicGoal = higher_order_call_rep(_, _)
            ; AtomicGoal = method_call_rep(_, _, _)
            ),
            ( if
                map.search(Info ^ cri_call_sites, RevGoalPath, Cost)
            then
                % Entry due to redo is not counted at the point before the
                % goal, it is represented when the number of exists is greater
                % than the number of calls. XXX This won't work with nondet
                % code, which should be fixed in the future.
                Calls = round_to_int(cs_cost_get_calls(Cost ^ cac_cost)),
                Exits = Cost ^ cac_exits,
                require(unify(Before, before_coverage(Calls)),
                  "Coverage before call doesn't match calls port on call site"),
                After0 = after_coverage(Exits)
            else
                unexpected($pred,
                    "Couldn't look up call site for port counts GP: " ++
                    rev_goal_path_to_string(RevGoalPath))
            )
        ;
            ( AtomicGoal = builtin_call_rep(_, _, _)
            ; AtomicGoal = unify_construct_rep(_, _, _)
            ; AtomicGoal = unify_deconstruct_rep(_, _, _)
            ; AtomicGoal = partial_construct_rep(_, _, _)
            ; AtomicGoal = partial_deconstruct_rep(_, _, _)
            ; AtomicGoal = unify_assign_rep(_, _)
            ; AtomicGoal = cast_rep(_, _)
            ; AtomicGoal = unify_simple_test_rep(_, _)
            ; AtomicGoal = pragma_foreign_code_rep(_)
            ; AtomicGoal = event_call_rep(_, _)
            ),
            propagate_detism_coverage(Detism, Before, After0)
        )
    ),

    % Search for a coverage point after this goal.  This search is performed
    % even when the coverage has been calculated from inner goals, since this
    % is used to perform an assertion that these two sources agree about the
    % coverage after this goal.
    ( if
        map.search(Info ^ cri_solns_coverage_points, RevGoalPath,
            CoveragePoint)
    then
        CoveragePoint = coverage_point(CoverageAfterCount, _, _),
        after_count_from_either_source(after_coverage(CoverageAfterCount),
            After0, After)
    else
        After0 = After
    ),
    GoalCoverage = construct_before_after_coverage(Before, After),
    update_goal_attribute(GoalId, GoalCoverage, !Array),

    trace [compile_time(flag("debug_coverage_propagation")), io(!IO)] (
        io.write_string("goal_annotate_coverage: done\n", !IO),
        io.format("\tGoalPath: %s\n\tDetism %s\n\tCoverage; %s\n",
            [s(rev_goal_path_to_string(RevGoalPath)),
             s(string(Detism)),
             s(string(GoalCoverage))], !IO)
    ),
    trace [compile_time(not flag("no_coverage_propagation_assertions"))] (
        ( if check_coverage_complete(GoalCoverage, GoalExpr) then
            true
        else
            unexpected($pred,
                string.format("check_coverage_complete failed\n" ++
                    "\tCoverage: %s\n\tGoalPath: %s\n\tProc: %s\n",
                    [s(string(GoalCoverage)),
                     s(rev_goal_path_to_string(RevGoalPath)),
                     s(string(Info ^ cri_proc))]))
        ),
        ( if check_coverage_regarding_detism(GoalCoverage, Detism) then
            true
        else
            unexpected($pred,
                string.format("check_coverage_regarding_detism failed: %s %s",
                    [s(string(GoalCoverage)), s(string(Detism))]))
        )
    ).

:- func construct_before_after_coverage(coverage_before, coverage_after)
    = coverage_info.

construct_before_after_coverage(Before, After) = Coverage :-
    (
        Before = before_unknown,
        (
            After = after_unknown,
            Coverage = coverage_unknown
        ;
            After = after_known(AfterExecCount),
            Coverage = coverage_known_after(AfterExecCount)
        ;
            After = after_zero,
            Coverage = coverage_known_after(0)
        )
    ;
        Before = before_zero,
        (
            After = after_unknown,
            Coverage = coverage_known_before(0)
        ;
            After = after_zero,
            Coverage = coverage_known_zero
        ;
            After = after_known(AfterExecCount),
            Coverage = coverage_known(0, AfterExecCount)
        )
    ;
        Before = before_known(BeforeExecCount),
        (
            After = after_unknown,
            Coverage = coverage_known_before(BeforeExecCount)
        ;
            After = after_known(AfterExecCount),
            ( if BeforeExecCount = AfterExecCount then
                Coverage = coverage_known_same(BeforeExecCount)
            else
                Coverage = coverage_known(BeforeExecCount, AfterExecCount)
            )
        ;
            After = after_zero,
            Coverage = coverage_known(BeforeExecCount, 0)
        )
    ).

    % Annotate a conjunction with coverage information.
    %
    % The list of goals is the tail of a conjunction, the coverage argument
    % describes the coverage of this list of goals if it were the entire
    % conjunction.  Each goal also has it's own coverage.
    %
:- pred conj_annotate_coverage(list(goal_rep(goal_id))::in,
    coverage_reference_info(Callee)::in,
    coverage_before::in, coverage_after::out,
    goal_attr_array(coverage_info)::gaa_di,
    goal_attr_array(coverage_info)::gaa_uo) is det.

conj_annotate_coverage([], _, Before, After, !Array) :-
    % The empty conjunction is equivalent to 'true' which is deterministic,
    propagate_det_coverage(Before, After).
conj_annotate_coverage([Conj | Conjs], Info, Before, After, !Array) :-
    goal_annotate_coverage(Conj, Info, Before, CoverageAfterHead, !Array),
    after_to_before_coverage(CoverageAfterHead, CoverageBeforeTail),
    conj_annotate_coverage(Conjs, Info, CoverageBeforeTail, After, !Array).

    % Compute the coverage information for a disjunction.
    %
    % Rules:
    %   - The coverage before a disjunction is equal to the coverage before the
    %     first disjunct.
    %   - The coverage after a disjunction is equal to the sum of coverages
    %     after each disjunct.
    %
:- pred disj_annotate_coverage(list(goal_rep(goal_id))::in,
    coverage_reference_info(Callee)::in, solution_count_rep::in,
    coverage_before::in, sum_coverage_after::in, sum_coverage_after::out,
    goal_attr_array(coverage_info)::gaa_di,
    goal_attr_array(coverage_info)::gaa_uo) is det.

disj_annotate_coverage([], _, _, _, !SumAfter, !Array).
disj_annotate_coverage([Disj | Disjs], Info, Solutions, Before0, !SumAfter,
        !Array) :-
    DisjRevGoalPath =
        map.lookup(Info ^ cri_goal_path_map, Disj ^ goal_annotation),
    (
        ( Before0 = before_known(_)
        ; Before0 = before_zero
        ),
        Before = Before0
    ;
        Before0 = before_unknown,
        get_branch_start_coverage(Info, DisjRevGoalPath, Before)
    ),
    goal_annotate_coverage(Disj, Info, Before, After, !Array),
    sum_after_coverage(After, !SumAfter),
    % We don't know how many times the start of the next disjunct is executed
    % unless we have a counter there.
    disj_annotate_coverage(Disjs, Info, Solutions, before_unknown, !SumAfter,
        !Array).

:- pred switch_annotate_coverage(list(case_rep(goal_id))::in,
    coverage_reference_info(Callee)::in, switch_can_fail_rep::in,
    coverage_before::in, coverage_after::out,
    goal_attr_array(coverage_info)::gaa_di,
    goal_attr_array(coverage_info)::gaa_uo) is det.

switch_annotate_coverage(Cases, Info, CanFail, Before, After, !Array) :-
    trace [compile_time(flag("debug_coverage_propagation")), io(!IO)] (
        io.format("Switch: Before0: %s\n", [s(string(Before))], !IO)
    ),

    switch_annotate_coverage_2(Cases, Info, CanFail, Before,
        sum_before_zero, _SumBefore, sum_after_zero, SumAfter, !Array),
    % For can_fail switches, the sum of the exec counts at the starts of the
    % arms may be less than the exec count at the start of the switch. However,
    % even for can_fail switches, the sum of the exec counts at the *ends* of
    % the arms will always equal the exec count at the end of the switch.
    after_count_sum_after_count(SumAfter, After),
    % Note: This code was removed this while simplifying the algorithm, it does
    % not infer any extra coverage information since coverage is known before
    % all goals before goal_annotate_coverage is called, it may be useful if we
    % allow coverage to be incomplete for trivial goals.
    %(
    %    CanFail = switch_can_not_fail_rep,
    %    before_count_from_either_source_sum(SumBefore, !Before)
    %;
    %    CanFail = switch_can_fail_rep
    %),

    trace [compile_time(not flag("no_coverage_propagation_assertions"))] (
        check_switch_coverage(CanFail, Cases, Before, !.Array, Result),
        (
            Result = yes
        ;
            Result = no,
            unexpected($pred,
                string.format(
                    "check_switch_coverage failed\n\t" ++
                    "CanFail: %s\n\tCases: %s\n\tBefore: %s, After: %s\n",
                    [s(string(CanFail)), s(string(Cases)),
                    s(string(Before)), s(string(After))]))
        )
    ).

    % switch_annotate_coverage_2(Info, Detism, RevGoalPathSteps, CaseNum,
    %   !CoverageSum, CoverageBeforeSwitch, !Cases),
    %
    % Perform coverage annotation on cases from the left to the right.
    % The head of the !.Cases list is case number CaseNum, SwitchCoverage
    % is the coverage for the entire switch as known by the caller,
    % !CoverageSum is the sum of the coverage so far.
    %
    % For this goal we use a forwards traversal, since the last goal may not
    % have a coverage point after it, in the expectation that the coverage at
    % the end of the last goal may need to be computed from the coverage of
    % each of the other goals.
    %
:- pred switch_annotate_coverage_2(list(case_rep(goal_id))::in,
    coverage_reference_info(Callee)::in, switch_can_fail_rep::in,
    coverage_before::in, sum_coverage_before::in, sum_coverage_before::out,
    sum_coverage_after::in, sum_coverage_after::out,
    goal_attr_array(coverage_info)::gaa_di,
    goal_attr_array(coverage_info)::gaa_uo) is det.

switch_annotate_coverage_2([], _, _, _, !SumBefore, !SumAfter, !Array).
switch_annotate_coverage_2([Case | Cases], Info, CanFail, SwitchBefore,
        !SumBefore, !SumAfter, !Array) :-
    % If this is the last case in the switch, then its coverage information
    % may be computed from the coverage of other cases and the coverage of the
    % whole switch.  This is only done for the last goal, since only this
    % optimisation is made by the coverage transformation in the compiler.
    %
    % If we cannot calculate this case's coverage information, then try to
    % retrieve the information from a coverage point associated with the case.
    ( if
        Cases = [],
        CanFail = switch_can_not_fail_rep,
        (
            SwitchBefore = before_known(SwitchBeforeExecCount)
        ;
            SwitchBefore = before_zero,
            SwitchBeforeExecCount = 0
        ),
        (
            !.SumBefore = sum_before_known(SumBeforeExecCount)
        ;
            !.SumBefore = sum_before_zero,
            SumBeforeExecCount = 0
        )
    then
        BeforeCase =
            before_coverage(SwitchBeforeExecCount - SumBeforeExecCount)
    else
        % Search for a coverage point for this case.
        RevCaseGoalPath = map.lookup(Info ^ cri_goal_path_map,
            Goal ^ goal_annotation),
        get_branch_start_coverage(Info, RevCaseGoalPath, BeforeCase)
    ),

    % Calculate and annotate the coverage for the case itself.
    Case = case_rep(_ConsID, _OtherConsIDs, Goal),
    goal_annotate_coverage(Goal, Info, BeforeCase, AfterCase, !Array),

    % Keep a sum of the execution counts seen in cases so far.
    sum_before_coverage(BeforeCase, !SumBefore),
    sum_after_coverage(AfterCase, !SumAfter),

    switch_annotate_coverage_2(Cases, Info, CanFail, SwitchBefore,
        !SumBefore, !SumAfter, !Array).

    % Propagate coverage information for if-then-else goals.
    %
:- pred ite_annotate_coverage(goal_rep(goal_id)::in, goal_rep(goal_id)::in,
    goal_rep(goal_id)::in, coverage_reference_info(Callee)::in,
    reverse_goal_path::in, coverage_before::in, coverage_after::out,
    goal_attr_array(coverage_info)::gaa_di,
    goal_attr_array(coverage_info)::gaa_uo) is det.

ite_annotate_coverage(Cond, Then, Else, Info, RevGoalPath, Before, After,
        !Array) :-
    CondDetism = Cond ^ goal_detism_rep,
    GoalPathMap = Info ^ cri_goal_path_map,

    % Step 1:
    %   Call goal_annotate_coverage for the condition goal.
    goal_annotate_coverage(Cond, Info, Before, AfterCond, !Array),
    after_to_before_coverage(AfterCond, BeforeThen0),

    % Step 2:
    %   Lookup coverage information for the starts of the then and else goals.
    (
        ( BeforeThen0 = before_known(_)
        ; BeforeThen0 = before_zero
        ),
        BeforeThen = BeforeThen0
    ;
        BeforeThen0 = before_unknown,
        RevThenGoalPath = map.lookup(GoalPathMap, Then ^ goal_annotation),
        get_branch_start_coverage(Info, RevThenGoalPath, BeforeThen)
    ),

    CondSolns = detism_get_solutions(CondDetism),
    (
        CondSolns = at_most_many_rep,
        RevElseGoalPath = map.lookup(GoalPathMap, Else ^ goal_annotation),
        get_branch_start_coverage(Info, RevElseGoalPath, BeforeElse)
    ;
        ( CondSolns = at_most_one_rep
        ; CondSolns = at_most_zero_rep
        ),
        % Assuming that Cond never throws an exception and we know the
        % coverage before the ite and at the beginning of Then, then the
        % coverage at the beginning of Else can be inferred.
        (
            (
                BeforeThen = before_known(BeforeThenCount)
            ;
                BeforeThen = before_zero,
                BeforeThenCount = 0
            ),
            (
                (
                    Before = before_known(BeforeCount)
                ;
                    Before = before_zero,
                    BeforeCount = 0
                ),
                BeforeElse = before_coverage(BeforeCount - BeforeThenCount)
            ;
                Before = before_unknown,
                RevElseGoalPath = map.lookup(GoalPathMap,
                    Else ^ goal_annotation),
                get_branch_start_coverage(Info, RevElseGoalPath, BeforeElse)
            )
        ;
            BeforeThen = before_unknown,
            RevElseGoalPath = map.lookup(GoalPathMap, Else ^ goal_annotation),
            get_branch_start_coverage(Info, RevElseGoalPath, BeforeElse)
        )
    ),

    trace [compile_time(flag("debug_coverage_propagation")), io(!IO)] (
        io.format("ITE Coverage inferred before then and else branches:\n" ++
            "\tWhole: %s \n\tThen: %s\n\tElse: %s\n" ++
            "\tGoalPath %s\n",
            [s(string(Before)), s(string(BeforeThen)), s(string(BeforeElse)),
            s(rev_goal_path_to_string(RevGoalPath))], !IO)
    ),

    % Step 3:
    %   Call goal_annotate_coverage for the then and else goals.
    goal_annotate_coverage(Then, Info, BeforeThen, AfterThen, !Array),
    goal_annotate_coverage(Else, Info, BeforeElse, AfterElse, !Array),

    % Step 4:
    %   Update what we know about the if-then-else as a whole.
    (
        AfterThen = after_known(AfterThenExecCount),
        (
            AfterElse = after_known(AfterElseExecCount),
            After = after_coverage(AfterThenExecCount + AfterElseExecCount)
        ;
            AfterElse = after_zero,
            After = after_coverage(AfterThenExecCount)
        ;
            AfterElse = after_unknown,
            After = after_unknown
        )
    ;
        AfterThen = after_zero,
        (
            AfterElse = after_known(AfterElseExecCount),
            After = after_coverage(AfterElseExecCount)
        ;
            AfterElse = after_zero,
            After = after_zero
        ;
            AfterElse = after_unknown,
            After = after_unknown
        )
    ;
        AfterThen = after_unknown,
        After = after_unknown
    ),

    trace [compile_time(not flag("no_coverage_propagation_assertions"))] (
        ( if
            check_ite_coverage(Before, After, Before, AfterCond,
                BeforeThen, AfterThen, BeforeElse, AfterElse, CondDetism)
        then
            true
        else
            unexpected($pred,
                string.format(
                    "check_ite_coverage/4 failed\n" ++
                    "\tWhole: %s %s\n" ++
                    "\tCond: %s %s\n\tThen: %s %s\n\tElse: %s %s\n" ++
                    "\tGoalPath: %s\n",
                    [s(string(Before)), s(string(After)),
                    s(string(Before)), s(string(AfterCond)),
                    s(string(BeforeThen)), s(string(AfterThen)),
                    s(string(BeforeElse)), s(string(AfterElse)),
                    s(rev_goal_path_to_string(RevGoalPath))]))
        )
    ).

    % Get the coverage information from a coverage point about the branch
    % referenced by the given goal path.
    %
:- pred get_branch_start_coverage(coverage_reference_info(Callee)::in,
    reverse_goal_path::in, coverage_before::out) is det.

get_branch_start_coverage(Info, RevGoalPath, Before) :-
    ( if map.search(Info ^ cri_branch_coverage_points, RevGoalPath, CP) then
        CP = coverage_point(ExecCount, _, _),
        Before = before_coverage(ExecCount)
    else
        Before = before_unknown
    ).

:- pred negation_annotate_coverage(goal_rep(goal_id)::in,
    coverage_reference_info(Callee)::in,
    coverage_before::in, coverage_after::out,
    goal_attr_array(coverage_info)::gaa_di,
    goal_attr_array(coverage_info)::gaa_uo) is det.

negation_annotate_coverage(Goal, Info, Before, After, !Array) :-
    goal_annotate_coverage(Goal, Info, Before, _CoverageAfter, !Array),
    % The coverage after a negation is always unknown.
    After = after_unknown,
    trace [compile_time(flag("debug_coverage_propagation")), io(!IO)] (
        io.format("Negation: setting negation: before %s, after %s\n",
            [s(string(Before)), s(string(After))], !IO)
    ).

:- pred scope_annotate_coverage(goal_rep(goal_id)::in,
    coverage_reference_info(Callee)::in, maybe_cut::in,
    coverage_before::in, coverage_after::out,
    goal_attr_array(coverage_info)::gaa_di,
    goal_attr_array(coverage_info)::gaa_uo) is det.

scope_annotate_coverage(Goal, Info, MaybeCut, Before, After, !Array) :-
    goal_annotate_coverage(Goal, Info, Before, AfterScopedGoal, !Array),
    (
        MaybeCut = scope_is_cut,
        After = after_unknown
    ;
        MaybeCut = scope_is_no_cut,
        After = AfterScopedGoal
    ).

%---------------------------------------------------------------------------%
%
% These predicates are used to check that computed coverage counts make sense.
%

    % Check that the coverage of a goal makes sense given the determinism of
    % that goal.
    %
:- pred check_coverage_regarding_detism(coverage_info::in, detism_rep::in)
    is semidet.

check_coverage_regarding_detism(Coverage, Detism) :-
    detism_coverage_ok(Coverage, Detism) = yes.

:- func detism_coverage_ok(coverage_info, detism_rep) = bool.

detism_coverage_ok(Coverage, Detism) = OK :-
    (
        ( Detism = det_rep
        ; Detism = cc_multidet_rep
        ),
        (
            ( Coverage = coverage_known_same(_)
            ; Coverage = coverage_known_zero
            ; Coverage = coverage_unknown
            ),
            OK = yes
        ;
            Coverage = coverage_known(Entry, Exit),
            % Execution may leave via the Excp port rather than the exit port.
            % so the exit port count may be smaller than or equal to the entry
            % port count.
            ( if Entry >= Exit then
                OK = yes
            else
                OK = no
            )
        ;
            ( Coverage = coverage_known_before(_)
            ; Coverage = coverage_known_after(_)
            ),
            % If you known coverage at one of these points, you can compute
            % the coverage at the other point.
            OK = no
        )
    ;
        ( Detism = semidet_rep
        ; Detism = cc_nondet_rep
        ),
        (
            ( Coverage = coverage_known_before(_)
            ; Coverage = coverage_known_after(_)
            ; Coverage = coverage_known_same(_)
            ; Coverage = coverage_known_zero
            ; Coverage = coverage_unknown
            ),
            OK = yes
        ;
            Coverage = coverage_known(Entry, Exit),
            ( if Entry >= Exit then
                OK = yes
            else
                OK = no
            )
        )
    ;
        Detism = multidet_rep,
        (
            ( Coverage = coverage_known_before(_)
            ; Coverage = coverage_known_after(_)
            ; Coverage = coverage_known_same(_)
            ; Coverage = coverage_known_zero
            ; Coverage = coverage_unknown
            ),
            OK = yes
        ;
            Coverage = coverage_known(_Entry, _Exit),
            % If the goal throws exceptions no inequalities can be used to
            % check the correctness of the coverage information.
            OK = yes
        )
    ;
        Detism = nondet_rep,
        OK = yes
    ;
        ( Detism = erroneous_rep
        ; Detism = failure_rep
        ),
        (
            % The coverage_known_dert case probably won't occur, but it might.
            ( Coverage = coverage_known(_, Exit)
            ; Coverage = coverage_known_same(Exit)
            ; Coverage = coverage_known_after(Exit)
            ),
            ( if Exit = 0 then
                OK = yes
            else
                OK = no
            )
        ;
            ( Coverage = coverage_known_before(_)
            ; Coverage = coverage_known_zero
            ),
            OK = yes
        ;
            Coverage = coverage_unknown,
            % This shouldn't occur, we should infer at least
            % coverage_known_after(0).
            OK = yes
        )
    ).

    % Check that the coverage on the switch goal and on its cases do not
    % contradict with each other.  This works only for cannot_fail switches.
    %
:- pred check_switch_coverage(switch_can_fail_rep::in,
    list(case_rep(goal_id))::in, coverage_before::in,
    goal_attr_array(coverage_info)::in, bool::out) is det.

check_switch_coverage(CanFail, Cases, Before, Array, Result) :-
    (
        CanFail = switch_can_not_fail_rep,
        list.foldl(sum_switch_case_coverage(Array), Cases, yes(0), MaybeSum),
        (
            MaybeSum = yes(SumA),
            (
                (
                    Before = before_unknown,
                    Result = yes
                ;
                    (
                        Before = before_known(SumB)
                    ;
                        Before = before_zero,
                        SumB = 0
                    ),
                    ( if SumA = SumB then
                        Result = yes
                    else
                        Result = no
                    )
                )
            )
        ;
            MaybeSum = no,
            Result = yes
        )
    ;
        CanFail = switch_can_fail_rep,
        Result = yes
    ).

:- pred sum_switch_case_coverage(goal_attr_array(coverage_info)::in,
    case_rep(goal_id)::in, maybe(int)::in, maybe(int)::out) is det.

sum_switch_case_coverage(Array, case_rep(_, _, Goal), !Acc) :-
    (
        !.Acc = yes(Count),
        Coverage = get_goal_attribute_det(Array, Goal ^ goal_annotation),
        (
            ( Coverage = coverage_known_same(Addend)
            ; Coverage = coverage_known(Addend, _)
            ; Coverage = coverage_known_before(Addend)
            ),
            !:Acc = yes(Count + Addend)
        ;
            Coverage = coverage_known_zero
        ;
            ( Coverage = coverage_unknown
            ; Coverage = coverage_known_after(_)
            ),
            !:Acc = no
        )
    ;
        !.Acc = no
    ).

:- pred check_ite_coverage(coverage_before::in, coverage_after::in,
    coverage_before::in, coverage_after::in,
    coverage_before::in, coverage_after::in,
    coverage_before::in, coverage_after::in,
    detism_rep::in) is semidet.

check_ite_coverage(Before, After, BeforeCond, AfterCond,
        BeforeThen, AfterThen, _BeforeElse, AfterElse, CondDetism) :-
    ( if
        Before = before_known(BeforeExecCount),
        BeforeCond = before_known(BeforeCondExecCount)
    then
        BeforeExecCount = BeforeCondExecCount
    else
        true
    ),
    ( if
        After = after_known(AfterExecCount),
        AfterThen = after_known(AfterThenExecCount),
        AfterElse = after_known(AfterElseExecCount)
    then
        AfterExecCount = AfterThenExecCount + AfterElseExecCount
    else
        true
    ),
    ( if
        AfterCond = after_known(AfterCondExecCount),
        BeforeThen = before_known(BeforeKnownExecCount)
    then
        AfterCondExecCount = BeforeKnownExecCount
    else
        true
    ),
    % Since the condition may throw exceptions and exception count information
    % is not propagated checking the coverage before the else goal based on the
    % coverage before and after the condition goal cannot be done.

    ( if AfterCond = after_known(AfterCondExecCount2) then
        NumSolutions = detism_get_solutions(CondDetism),
        (
            NumSolutions = at_most_zero_rep,
            AfterCondExecCount2 = 0
        ;
            ( NumSolutions = at_most_one_rep
            ; NumSolutions = at_most_many_rep
            )
        )
    else
        true
    ).

:- pred check_coverage_complete(coverage_info::in, goal_expr_rep(T)::in)
    is semidet.

check_coverage_complete(coverage_known(_, _), _GoalExpr).
check_coverage_complete(coverage_known_same(_), _GoalExpr).
check_coverage_complete(coverage_known_zero, _GoalExpr).
% Uncomment this clause if, in the future, we allow coverage to be incomplete
% for trivial goals.
%check_coverage_complete(Coverage, GoalExpr) :-
%    ( Coverage = coverage_known_before(_)
%    ; Coverage = coverage_known_after(_)
%    ; Coverage = coverage_unknown
%    ),
%    goal_expr_is_trivial(GoalExpr).

%---------------------------------------------------------------------------%
%
% Coverage information helper predicates.
%

    % The coverage before a det goal should always equal the coverage after.
    %
:- pred propagate_det_coverage(coverage_before::in, coverage_after::out)
    is det.

propagate_det_coverage(Before, After) :-
    (
        Before = before_unknown,
        After = after_unknown
    ;
        Before = before_known(Count),
        After = after_coverage(Count)
    ;
        Before = before_zero,
        After = after_zero
    ).

    % If the determinism is deterministic or cc_multi use
    % propagate_det_coverage.
    %
    % Note: This predicate must not be called on deterministic call goals or on
    % any deterministic non-atomic goal, since the coverage after the call may
    % be different to the coverage before if the called code throws an
    % exception.
    %
:- pred propagate_detism_coverage(detism_rep::in,
    coverage_before::in, coverage_after::out) is det.

propagate_detism_coverage(Detism, Before, After) :-
    % TODO: Infer that if a goal has a coverage of exactly 0 before it, then it
    % must have a coverage of exactly 0 after it.  And that a goal that cannot
    % fail or throw an exception that has a coverage of 0 after it, must have a
    % coverage of 0 before it - Since the coverage profiling and propagation
    % algorithms are already complete this isn't required.  It should be
    % considered if we choose not to calculate coverage for trivial goals.
    (
        ( Detism = det_rep
        ; Detism = cc_multidet_rep
        ),
        propagate_det_coverage(Before, After)
    ;
        ( Detism = erroneous_rep
        ; Detism = failure_rep
        ),
        % Execution never reaches the end of these goals.
        After = after_zero
    ;
        ( Detism = semidet_rep
        ; Detism = nondet_rep
        ; Detism = multidet_rep
        ; Detism = cc_nondet_rep
        ),
        % We can infer nothing for goals with these determinisms.
        After = after_unknown
    ).

:- pred after_to_before_coverage(coverage_after::in, coverage_before::out)
    is det.

after_to_before_coverage(After, Before) :-
    (
        After = after_unknown,
        Before = before_unknown
    ;
        After = after_known(ExecCount),
        Before = before_known(ExecCount)
    ;
        After = after_zero,
        Before = before_zero
    ).

:- pred after_count_from_either_source(coverage_after::in,
    coverage_after::in, coverage_after::out) is det.

after_count_from_either_source(AfterA, AfterB, After) :-
    (
        AfterA = after_unknown,
        (
            AfterB = after_unknown,
            After = after_unknown
        ;
            AfterB = after_known(AfterCount),
            After = after_coverage(AfterCount)
        ;
            AfterB = after_zero,
            After = after_zero
        )
    ;
        AfterA = after_known(AfterCountA),
        (
            AfterB = after_unknown,
            After = after_coverage(AfterCountA)
        ;
            AfterB = after_known(AfterCountB),
            require(unify(AfterCountA, AfterCountB),
                "after_count_from_either_source: mismatch"),
            After = after_coverage(AfterCountA)
        ;
            AfterB = after_zero,
            require(unify(AfterCountA, 0),
                "after_count_from_either_source: mismatch"),
            After = after_zero
        )
    ;
        AfterA = after_zero,
        (
            AfterB = after_unknown,
            After = after_zero
        ;
            AfterB = after_known(AfterCountB),
            require(unify(0, AfterCountB),
                "after_count_from_either_source: mismatch"),
            After = after_zero
        ;
            AfterB = after_zero,
            After = after_zero
        )
    ).

    % Convert a sum_coverage_after to a coverage_after.
    %
:- pred after_count_sum_after_count(sum_coverage_after::in,
    coverage_after::out) is det.

after_count_sum_after_count(sum_after_unknown, after_unknown).
after_count_sum_after_count(sum_after_zero, after_zero).
after_count_sum_after_count(sum_after_known(C), after_coverage(C)).

:- pred before_count_from_either_source(coverage_before::in,
    coverage_before::in, coverage_before::out) is det.

before_count_from_either_source(BeforeA, BeforeB, Before) :-
    (
        BeforeA = before_unknown,
        (
            BeforeB = before_unknown,
            Before = before_unknown
        ;
            BeforeB = before_known(BeforeCount),
            Before = before_coverage(BeforeCount)
        ;
            BeforeB = before_zero,
            Before = before_zero
        )
    ;
        BeforeA = before_known(BeforeCountA),
        (
            BeforeB = before_unknown,
            Before = before_coverage(BeforeCountA)
        ;
            BeforeB = before_known(BeforeCountB),
            require(unify(BeforeCountA, BeforeCountB),
                "before_count_from_either_source: mismatch"),
            Before = before_coverage(BeforeCountA)
        ;
            BeforeB = before_zero,
            require(unify(BeforeCountA, 0),
                "before_count_from_either_source: mismatch"),
            Before = before_zero
        )
    ;
        BeforeA = before_zero,
        (
            BeforeB = before_unknown,
            Before = before_zero
        ;
            BeforeB = before_known(BeforeCountB),
            require(unify(0, BeforeCountB),
                "before_count_from_either_source: mismatch"),
            Before = before_zero
        ;
            BeforeB = before_zero,
            Before = before_zero
        )
    ).

:- pred before_count_from_either_source_sum(sum_coverage_before::in,
    coverage_before::in, coverage_before::out) is det.

before_count_from_either_source_sum(BeforeA0, BeforeB, Before) :-
    before_count_sum_before_count(BeforeA0, BeforeA),
    before_count_from_either_source(BeforeA, BeforeB, Before).

:- pred sum_before_coverage(coverage_before::in,
    sum_coverage_before::in, sum_coverage_before::out) is det.

sum_before_coverage(Before, !SumBefore) :-
    (
        !.SumBefore = sum_before_known(SumExecCount),
        (
            Before = before_known(ExecCount),
            !:SumBefore = sum_before_known(SumExecCount + ExecCount)
        ;
            Before = before_zero
        ;
            Before = before_unknown,
            !:SumBefore = sum_before_unknown
        )
    ;
        !.SumBefore = sum_before_zero,
        (
            Before = before_known(ExecCount),
            !:SumBefore = sum_before_known(ExecCount)
        ;
            Before = before_zero
        ;
            Before = before_unknown,
            !:SumBefore = sum_before_unknown
        )
    ;
        !.SumBefore = sum_before_unknown
    ).

:- pred sum_after_coverage(coverage_after::in,
    sum_coverage_after::in, sum_coverage_after::out) is det.

sum_after_coverage(After, !SumAfter) :-
    (
        !.SumAfter = sum_after_known(SumExecCount),
        (
            After = after_known(ExecCount),
            !:SumAfter = sum_after_known(SumExecCount + ExecCount)
        ;
            After = after_unknown,
            !:SumAfter = sum_after_unknown
        ;
            After = after_zero
        )
    ;
        !.SumAfter = sum_after_zero,
        (
            After = after_known(ExecCount),
            !:SumAfter = sum_after_known(ExecCount)
        ;
            After = after_zero
        ;
            After = after_unknown,
            !:SumAfter = sum_after_unknown
        )
    ;
        !.SumAfter = sum_after_unknown
    ).

:- pred before_count_sum_before_count(sum_coverage_before::in,
    coverage_before::out) is det.

before_count_sum_before_count(sum_before_unknown, before_unknown).
before_count_sum_before_count(sum_before_known(Num), before_coverage(Num)).
before_count_sum_before_count(sum_before_zero, before_zero).

:- func after_coverage(int) = coverage_after.

after_coverage(Count) =
    ( if Count = 0 then
        after_zero
    else
        after_known(Count)
    ).

:- func before_coverage(int) = coverage_before.

before_coverage(Count) =
    ( if Count = 0 then
        before_zero
    else
        before_known(Count)
    ).

%---------------------------------------------------------------------------%

add_coverage_point_to_map(CoveragePoint, !SolnsMap, !BranchMap) :-
    CoveragePoint = coverage_point(_, GoalPath, CPType),
    (
        CPType = cp_type_coverage_after,
        map.det_insert(GoalPath, CoveragePoint, !SolnsMap)
    ;
        CPType = cp_type_branch_arm,
        map.det_insert(GoalPath, CoveragePoint, !BranchMap)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

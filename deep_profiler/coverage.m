%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: pbone, zs.
%
% This file implements the coverage propagation algorithm, which attaches
% coverage information to the component goals of a procedure body.
%
%-----------------------------------------------------------------------------%

:- module coverage.

:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.
:- import_module measurements.
:- import_module profile.
:- import_module report.

:- import_module map.

:- type coverage_info
    --->    coverage_unknown
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

%----------------------------------------------------------------------------%


    % Annotate the program representation structure with coverage information.
    %
:- pred procrep_annotate_with_coverage(own_prof_info::in,
    map(goal_path, call_site_perf)::in, map(goal_path, coverage_point)::in,
    map(goal_path, coverage_point)::in, proc_rep::in,
    proc_rep(coverage_info)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module program_representation_utils.

:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module unit.

get_coverage_before(coverage_known(Before, _), Before).
get_coverage_before(coverage_known_same(Before), Before).
get_coverage_before(coverage_known_before(Before), Before).

get_coverage_before_and_after(coverage_known(Before, After), Before, After).
get_coverage_before_and_after(coverage_known_same(Count), Count, Count).

%-----------------------------------------------------------------------------%

:- type coverage_before
    --->    before_unknown
    ;       before_known(int).

:- type coverage_after
    --->    after_unknown
    ;       after_known(int).

:- type sum_coverage_before
    --->    sum_before_unknown
    ;       sum_before_known(int).

:- type sum_coverage_after
    --->    sum_after_unknown
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
procrep_annotate_with_coverage(OwnProf, CallSites, SolnsCoveragePoints,
        BranchCoveragePoints, !ProcRep) :-
    some [!ProcDefn, !GoalRep] (
        !:ProcDefn = !.ProcRep ^ pr_defn,
        !:GoalRep = !.ProcDefn ^ pdr_goal,
        ProcLabel = !.ProcRep ^ pr_id,
        Calls = calls(OwnProf),
        Exits = exits(OwnProf),
        Before = before_known(Calls),
        CoverageReference = coverage_reference_info(ProcLabel, CallSites,
            SolnsCoveragePoints, BranchCoveragePoints),
        goal_annotate_coverage(CoverageReference, empty_goal_path,
            Before, After, !GoalRep),
        require(unify(After, after_known(Exits)),
            "Coverage after procedure not equal with exit count of procedure"),
        !:ProcDefn = !.ProcDefn ^ pdr_goal := !.GoalRep,
        !:ProcRep = !.ProcRep ^ pr_defn := !.ProcDefn
    ).

    % These maps are keyed by goal_path, comparing these structures is less
    % efficient than comparing simple structures like the alternative
    % goal_path_string, however, that involves frequently constructing strings
    % from goal paths.  Using goal_path_string may be faster but I'd rather not
    % make this optimisation without first testing it.
    %
:- type coverage_reference_info
    --->    coverage_reference_info(
                cri_proc                    :: string_proc_label,
                cri_call_sites              :: map(goal_path, call_site_perf),
                cri_solns_coverage_points   :: map(goal_path, coverage_point),
                cri_branch_coverage_points  :: map(goal_path, coverage_point)
            ).

    % Annotate a goal and its children with coverage information.
    %
:- pred goal_annotate_coverage(coverage_reference_info::in, goal_path::in,
    coverage_before::in, coverage_after::out,
    goal_rep(unit)::in, goal_rep(coverage_info)::out) is det.

goal_annotate_coverage(Info, GoalPath, Before, After, Goal0, Goal) :-
    Goal0 = goal_rep(GoalExpr0, Detism, _),

    % Calculate coverage of any inner goals.
    (
        GoalExpr0 = conj_rep(Conjuncts0),
        conj_annotate_coverage(Info, GoalPath, Before, After0,
            Conjuncts0, Conjuncts),
        GoalExpr = conj_rep(Conjuncts)
    ;
        GoalExpr0 = disj_rep(Disjuncts0),
        disj_annotate_coverage(Info, Detism, GoalPath, Before, After0,
            Disjuncts0, Disjuncts),
        GoalExpr = disj_rep(Disjuncts)
    ;
        GoalExpr0 = switch_rep(Var, CanFail, Cases0),
        switch_annotate_coverage(Info, CanFail, GoalPath, Before, After0,
            Cases0, Cases),
        GoalExpr = switch_rep(Var, CanFail, Cases)
    ;
        GoalExpr0 = ite_rep(Cond0, Then0, Else0),
        ite_annotate_coverage(Info, GoalPath, Before, After0, Cond0, Cond,
            Then0, Then, Else0, Else),
        GoalExpr = ite_rep(Cond, Then, Else)
    ;
        GoalExpr0 = negation_rep(NegGoal0),
        negation_annotate_coverage(Info, GoalPath, Before, After0,
            NegGoal0, NegGoal),
        GoalExpr = negation_rep(NegGoal)
    ;
        GoalExpr0 = scope_rep(ScopedGoal0, MaybeCut),
        scope_annotate_coverage(Info, GoalPath, MaybeCut,  Before, After0,
            ScopedGoal0, ScopedGoal),
        GoalExpr = scope_rep(ScopedGoal, MaybeCut)
    ;
        GoalExpr0 = atomic_goal_rep(Filename, Line, Vars, AtomicGoal),
        % Note that GoalExpr != GoalExpr0, since they are of different types.
        GoalExpr = atomic_goal_rep(Filename, Line, Vars, AtomicGoal),
        (
            ( AtomicGoal = plain_call_rep(_, _, _)
            ; AtomicGoal = higher_order_call_rep(_, _)
            ; AtomicGoal = method_call_rep(_, _, _)
            ),
            ( map.search(Info ^ cri_call_sites, GoalPath, CallSite) ->
                Summary = CallSite ^ csf_summary_perf,
                % Entry due to redo is not counted at the point before the
                % goal, it is represented when the number of exists is greater
                % than the number of calls. XXX This won't work with nondet
                % code, which should be fixed in the future.
                Calls = Summary ^ perf_row_calls,
                Exits = Summary ^ perf_row_exits,
                require(unify(Before, before_known(Calls)),
                  "Coverage before call doesn't match calls port on call site"),
                After0 = after_known(Exits)
            ;
                error("Couldn't look up call site for port counts GP: " ++
                    goal_path_to_string(GoalPath))
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
    ( map.search(Info ^ cri_solns_coverage_points, GoalPath, CoveragePoint) ->
        CoveragePoint = coverage_point(CoverageAfterCount, _, _),
        after_count_from_either_source(after_known(CoverageAfterCount),
            After0, After)
    ;
        After0 = After
    ),
    GoalCoverage = construct_before_after_coverage(Before, After),
    Goal = goal_rep(GoalExpr, Detism, GoalCoverage),

    trace [compile_time(flag("debug_coverage_propagation")), io(!IO)] (
        io.write_string("goal_annotate_coverage: done\n", !IO),
        io.format("\tGoalPath: %s\n\tDetism %s\n\tCoverage; %s\n",
            [s(goal_path_to_string(GoalPath)),
             s(string(Detism)),
             s(string(GoalCoverage))], !IO)
    ),
    trace [compile_time(not flag("no_coverage_propagation_assertions"))] (
        ( check_coverage_complete(GoalCoverage, GoalExpr) ->
            true
        ;
            error(string.format("check_coverage_complete failed\n" ++
                "\tCoverage: %s\n\tGoalPath: %s\n\tProc: %s\n",
                [s(string(GoalCoverage)), 
                 s(goal_path_to_string(GoalPath)),
                 s(string(Info ^ cri_proc))]))
        ),
        ( check_coverage_regarding_detism(GoalCoverage, Detism) ->
            true
        ;
            error(string.format("check_coverage_regarding_detism failed: %s %s",
                    [s(string(GoalCoverage)), s(string(Detism))]))
        )
    ).

:- func construct_before_after_coverage(coverage_before, coverage_after)
    = coverage_info.

construct_before_after_coverage(Before, After) = Coverage :-
    (
        Before = before_unknown,
        After = after_unknown,
        Coverage = coverage_unknown
    ;
        Before = before_unknown,
        After = after_known(AfterExecCount),
        Coverage = coverage_known_after(AfterExecCount)
    ;
        Before = before_known(BeforeExecCount),
        After = after_unknown,
        Coverage = coverage_known_before(BeforeExecCount)
    ;
        Before = before_known(BeforeExecCount),
        After = after_known(AfterExecCount),
        ( BeforeExecCount = AfterExecCount ->
            Coverage = coverage_known_same(BeforeExecCount)
        ;
            Coverage = coverage_known(BeforeExecCount, AfterExecCount)
        )
    ).

    % Annotate a conjunction with coverage information.
    %
:- pred conj_annotate_coverage(coverage_reference_info::in, goal_path::in,
    coverage_before::in, coverage_after::out,
    list(goal_rep(unit))::in, list(goal_rep(coverage_info))::out) is det.

conj_annotate_coverage(Info, GoalPath, Before, After, Conjs0, Conjs) :-
    conj_annotate_coverage_2(Info, GoalPath, 1, Before, After,
        Conjs0, Conjs).

    % Annotate a conjunction with coverage information.
    %
    % The list of goals is the tail of a conjunction, the coverage argument
    % describes the coverage of this list of goals if it were the entire
    % conjunction.  Each goal also has it's own coverage.
    %
:- pred conj_annotate_coverage_2(coverage_reference_info::in,
    goal_path::in, int::in, coverage_before::in, coverage_after::out,
    list(goal_rep(unit))::in, list(goal_rep(coverage_info))::out) is det.

conj_annotate_coverage_2(_, _, _, Before, After, [], []) :-
    % The empty conjunction is equivalent to 'true' which is deterministic,
    propagate_det_coverage(Before, After).
conj_annotate_coverage_2(Info, GoalPath, ConjunctNum, Before, After,
        [Conj0 | Conjs0], [Conj | Conjs]) :-
    HeadGoalPath = goal_path_add_at_end(GoalPath, step_conj(ConjunctNum)),
    goal_annotate_coverage(Info, HeadGoalPath,
        Before, CoverageAfterHead, Conj0, Conj),
    after_to_before_coverage(CoverageAfterHead, CoverageBeforeTail),
    conj_annotate_coverage_2(Info, GoalPath, ConjunctNum + 1,
        CoverageBeforeTail, After, Conjs0, Conjs).

    % Compute the coverage information for a disjunction.
    %
    % Rules:
    %   - The coverage before a disjunction is equal to the coverage before the
    %     first disjunct.
    %   - The coverage after a disjunction is equal to the sum of coverages
    %     after each disjunct.
    %
:- pred disj_annotate_coverage(coverage_reference_info::in,
    detism_rep::in, goal_path::in, coverage_before::in, coverage_after::out,
    list(goal_rep(unit))::in, list(goal_rep(coverage_info))::out) is det.

disj_annotate_coverage(Info, Detism, GoalPath, Before, After,
        Disjs0, Disjs) :-
    % XXX In theory, we could update Before using information from any counter
    % at the start of the first disjunct, but we don't do that (yet).  This may
    % not be useful for some disjunctions, for example those called from a
    % single solution context or committed-choice.
    Solutions = detism_get_solutions(Detism),
    disj_annotate_coverage_2(Info, GoalPath, 1, Solutions,
        Before, sum_after_known(0), SumAfterDisjuncts, Disjs0, Disjs),
    count_sum_to_count(SumAfterDisjuncts, After).

:- pred disj_annotate_coverage_2(coverage_reference_info::in,
    goal_path::in, int::in, solution_count_rep::in, coverage_before::in,
    sum_coverage_after::in, sum_coverage_after::out,
    list(goal_rep)::in, list(goal_rep(coverage_info))::out) is det.

disj_annotate_coverage_2(_, _, _, _, _, !SumAfter, [], []).
disj_annotate_coverage_2(Info, GoalPath, DisjNum, Solutions,
        Before0, !SumAfter, [Disj0 | Disjs0], [Disj | Disjs]) :-
    DisjGoalPath = goal_path_add_at_end(GoalPath, step_disj(DisjNum)),
    (
        Before0 = before_known(_),
        Before = Before0
    ;
        Before0 = before_unknown,
        get_branch_start_coverage(Info, DisjGoalPath, Before)
    ),
    goal_annotate_coverage(Info, DisjGoalPath,
        Before, After, Disj0, Disj),
    sum_after_coverage(After, !SumAfter),
    % We don't know how many times the start of the next disjunct is executed
    % unless we have a counter there.
    disj_annotate_coverage_2(Info, GoalPath, DisjNum + 1, Solutions,
        before_unknown, !SumAfter, Disjs0, Disjs).

:- pred switch_annotate_coverage(coverage_reference_info::in,
    switch_can_fail_rep::in, goal_path::in,
    coverage_before::in, coverage_after::out,
    list(case_rep(unit))::in, list(case_rep(coverage_info))::out) is det.

switch_annotate_coverage(Info, CanFail, GoalPath, Before, After,
        Cases0, Cases) :-
    trace [compile_time(flag("debug_coverage_propagation")), io(!IO)] (
        io.format("Switch: Before0: %s\n", [s(string(Before))], !IO)
    ),

    switch_annotate_coverage_2(Info, CanFail, GoalPath, 1,
        sum_before_known(0), _SumBefore, sum_after_known(0), SumAfter,
        Before, Cases0, Cases),
    % For can_fail switches, the sum of the exec counts at the starts of the
    % arms may be less than the exec count at the start of the switch. However,
    % even for can_fail switches, the sum of the exec counts at the *ends* of
    % the arms will always equal the exec count at the end of the switch.
    count_sum_to_count(SumAfter, After),
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
        ( check_switch_coverage(CanFail, Cases, Before) ->
            true
        ;
            error(string.format("check_switch_coverage failed\n\t" ++
                "CanFail: %s\n\tCases: %s\n\tBefore: %s, After: %s\n",
                [s(string(CanFail)), s(string(Cases)),
                s(string(Before)), s(string(After))]))
        )
    ).

    % switch_annotate_coverage_2(Info, Detism, GoalPath, CaseNum,
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
:- pred switch_annotate_coverage_2(coverage_reference_info::in,
    switch_can_fail_rep::in, goal_path::in, int::in,
    sum_coverage_before::in, sum_coverage_before::out,
    sum_coverage_after::in, sum_coverage_after::out,
    coverage_before::in,
    list(case_rep(unit))::in, list(case_rep(coverage_info))::out) is det.

switch_annotate_coverage_2(_, _, _, _, !SumBefore, !SumAfter, _, [], []).
switch_annotate_coverage_2(Info, CanFail, GoalPath, CaseNum,
        !SumBefore, !SumAfter, SwitchBefore,
        [Case0 | Cases0], [Case | Cases]) :-
    CaseGoalPath = goal_path_add_at_end(GoalPath, step_switch(CaseNum, no)),

    % If this is the last case in the switch, then its coverage information
    % may be computed from the coverage of other cases and the coverage of the
    % whole switch.  This is only done for the last goal, since only this
    % optimisation is made by the coverage transformation in the compiler.
    %
    % If we cannot calculate this case's coverage information, then try to
    % retrieve the information from a coverage point associated with the case.
    (
        Cases0 = [],
        CanFail = switch_can_not_fail_rep,
        SwitchBefore = before_known(SwitchBeforeExecCount),
        !.SumBefore = sum_before_known(SumBeforeExecCount)
    ->
        BeforeCase = before_known(SwitchBeforeExecCount - SumBeforeExecCount)
    ;
        % Search for a coverage point for this case.
        get_branch_start_coverage(Info, CaseGoalPath, BeforeCase)
    ),

    % Calculate and annotate the coverage for the case itself.
    Case0 = case_rep(ConsID, OtherConsIDs, Goal0),
    goal_annotate_coverage(Info, CaseGoalPath,
        BeforeCase, AfterCase, Goal0, Goal),
    Case = case_rep(ConsID, OtherConsIDs, Goal),

    % Keep a sum of the execution counts seen in cases so far.
    sum_before_coverage(BeforeCase, !SumBefore),
    sum_after_coverage(AfterCase, !SumAfter),

    switch_annotate_coverage_2(Info, CanFail, GoalPath, CaseNum + 1,
        !SumBefore, !SumAfter, SwitchBefore, Cases0, Cases).

    % Propagate coverage information for if-then-else goals.
    %
:- pred ite_annotate_coverage(coverage_reference_info::in, goal_path::in,
    coverage_before::in, coverage_after::out,
    goal_rep::in, goal_rep(coverage_info)::out,
    goal_rep::in, goal_rep(coverage_info)::out,
    goal_rep::in, goal_rep(coverage_info)::out) is det.

ite_annotate_coverage(Info, GoalPath, Before, After,
        Cond0, Cond, Then0, Then, Else0, Else) :-
    CondGoalPath = goal_path_add_at_end(GoalPath, step_ite_cond),
    ThenGoalPath = goal_path_add_at_end(GoalPath, step_ite_then),
    ElseGoalPath = goal_path_add_at_end(GoalPath, step_ite_else),
    CondDetism = Cond0 ^ goal_detism_rep,

    % Step 1:
    %   Call goal_annotate_coverage for the condition goal.
    goal_annotate_coverage(Info, CondGoalPath,
        Before, AfterCond, Cond0, Cond),
    after_to_before_coverage(AfterCond, BeforeThen0),

    % Step 2:
    %   Lookup coverage information for the starts of the then and else goals.
    (
        BeforeThen0 = before_known(_),
        BeforeThen = BeforeThen0
    ;
        BeforeThen0 = before_unknown,
        get_branch_start_coverage(Info, ThenGoalPath, BeforeThen)
    ),
    % XXX It should be possible, if the condition is not at_most_many and does
    % not throw exceptions, to compute BeforeElse as the difference between the
    % counts in the initial value of !.Before and AfterCond, if both are known.
    % check_ite_coverage already knows the relationship.  Using exception
    % counts on call goals and propagating them through the coverage annotation
    % algorithms can solve this.
    get_branch_start_coverage(Info, ElseGoalPath, BeforeElse),

    trace [compile_time(flag("debug_coverage_propagation")), io(!IO)] (
        io.format("ITE Coverage inferred before then and else branches:\n" ++
            "\tWhole: %s \n\tThen: %s\n\tElse: %s\n" ++
            "\tGoalPath %s\n",
            [s(string(Before)), s(string(BeforeThen)), s(string(BeforeElse)),
            s(goal_path_to_string(GoalPath))], !IO)
    ),

    % Step 3:
    %   Call goal_annotate_coverage for the then and else goals.
    goal_annotate_coverage(Info, ThenGoalPath,
        BeforeThen, AfterThen, Then0, Then),
    goal_annotate_coverage(Info, ElseGoalPath,
        BeforeElse, AfterElse, Else0, Else),

    % Step 4:
    %   Update what we know about the if-then-else as a whole.
    (
        AfterThen = after_known(AfterThenExecCount),
        AfterElse = after_known(AfterElseExecCount)
    ->
        After = after_known(AfterThenExecCount + AfterElseExecCount)
    ;
        After = after_unknown
    ),

    trace [compile_time(not flag("no_coverage_propagation_assertions"))] (
        (
            check_ite_coverage(Before, After, Before, AfterCond,
                BeforeThen, AfterThen, BeforeElse, AfterElse, CondDetism)
        ->
            true
        ;
            error(string.format("check_ite_coverage/4 failed\n" ++
                "\tWhole: %s %s\n" ++
                "\tCond: %s %s\n\tThen: %s %s\n\tElse: %s %s\n" ++
                "\tGoalPath: %s\n",
                [s(string(Before)), s(string(After)),
                s(string(Before)), s(string(AfterCond)),
                s(string(BeforeThen)), s(string(AfterThen)),
                s(string(BeforeElse)), s(string(AfterElse)),
                s(goal_path_to_string(GoalPath))]))
        )
    ).

:- pred not_unify(T::in, T::in) is semidet.

not_unify(A, B) :- not unify(A, B).

    % Get the coverage information from a coverage point about the branch
    % referenced by the given goal path.
    %
:- pred get_branch_start_coverage(coverage_reference_info::in, goal_path::in,
    coverage_before::out) is det.

get_branch_start_coverage(Info, GoalPath, Before) :-
    ( map.search(Info ^ cri_branch_coverage_points, GoalPath, CP) ->
        CP = coverage_point(ExecCount, _, _),
        Before = before_known(ExecCount)
    ;
        Before = before_unknown
    ).

:- pred negation_annotate_coverage(coverage_reference_info::in, goal_path::in,
    coverage_before::in, coverage_after::out,
    goal_rep::in, goal_rep(coverage_info)::out) is det.

negation_annotate_coverage(Info, GoalPath, Before, After,
        NegGoal0, NegGoal) :-
    NegGoalPath = goal_path_add_at_end(GoalPath, step_neg),
    goal_annotate_coverage(Info, NegGoalPath,
        Before, _CoverageAfter, NegGoal0, NegGoal),
    % The coverage after a negation is always unknown.
    After = after_unknown,
    trace [compile_time(flag("debug_coverage_propagation")), io(!IO)] (
        io.format("Negation: setting negation: before %s, after %s\n",
            [s(string(Before)), s(string(After))], !IO)
    ).

:- pred scope_annotate_coverage(coverage_reference_info::in,
    goal_path::in, maybe_cut::in, coverage_before::in, coverage_after::out,
    goal_rep::in, goal_rep(coverage_info)::out) is det.

scope_annotate_coverage(Info, GoalPath, MaybeCut, Before, After,
        ScopedGoal0, ScopedGoal) :-
    ScopeGoalPath = goal_path_add_at_end(GoalPath, step_scope(MaybeCut)),
    goal_annotate_coverage(Info, ScopeGoalPath,
        Before, AfterScopedGoal, ScopedGoal0, ScopedGoal),
    (
        MaybeCut = scope_is_cut,
        After = after_unknown
    ;
        MaybeCut = scope_is_no_cut,
        After = AfterScopedGoal
    ).

%----------------------------------------------------------------------------%
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
            ; Coverage = coverage_unknown
            ),
            OK = yes
        ;
            Coverage = coverage_known(Entry, Exit),
            % Execution may leave via the Excp port rather than the exit port.
            % so the exit port count may be smaller than or equal to the entry
            % port count.
            ( Entry >= Exit ->
                OK = yes
            ;
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
            ; Coverage = coverage_unknown
            ),
            OK = yes
        ;
            Coverage = coverage_known(Entry, Exit),
            ( Entry >= Exit ->
                OK = yes
            ;
                OK = no
            )
        )
    ;
        Detism = multidet_rep,
        (
            ( Coverage = coverage_known_before(_)
            ; Coverage = coverage_known_after(_)
            ; Coverage = coverage_known_same(_)
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
            ( Exit = 0 ->
                OK = yes
            ;
                OK = no
            )
        ;
            Coverage = coverage_known_before(_),
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
    list(case_rep(coverage_info))::in, coverage_before::in) is semidet.

check_switch_coverage(CanFail, Cases, Before) :-
    (
        CanFail = switch_can_not_fail_rep,
        list.foldl(sum_switch_case_coverage, Cases, yes(0), MaybeSum),
        (
            MaybeSum = yes(Sum),
            (
                ( Before = before_known(Sum)
                ; Before = before_unknown
                )
            )
        ;
            MaybeSum = no
        )
    ;
        CanFail = switch_can_fail_rep
    ).

:- pred sum_switch_case_coverage(case_rep(coverage_info)::in,
    maybe(int)::in, maybe(int)::out) is det.

sum_switch_case_coverage(case_rep(_, _, Goal), !Acc) :-
    (
        !.Acc = yes(Count),
        Coverage = Goal ^ goal_annotation,
        (
            ( Coverage = coverage_known_same(Addend)
            ; Coverage = coverage_known(Addend, _)
            ; Coverage = coverage_known_before(Addend)
            ),
            !:Acc = yes(Count + Addend)
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
    (
        Before = before_known(BeforeExecCount),
        BeforeCond = before_known(BeforeCondExecCount)
    ->
        BeforeExecCount = BeforeCondExecCount
    ;
        true
    ),
    (
        After = after_known(AfterExecCount),
        AfterThen = after_known(AfterThenExecCount),
        AfterElse = after_known(AfterElseExecCount)
    ->
        AfterExecCount = AfterThenExecCount + AfterElseExecCount
    ;
        true
    ),
    (
        AfterCond = after_known(AfterCondExecCount),
        BeforeThen = before_known(BeforeKnownExecCount)
    ->
        AfterCondExecCount = BeforeKnownExecCount
    ;
        true
    ),
    % Since the condition may throw exceptions and exception count information
    % is not propagated checking the coverage before the else goal based on the
    % coverage before and after the condition goal cannot be done.

    ( AfterCond = after_known(AfterCondExecCount2) ->
        NumSolutions = detism_get_solutions(CondDetism),
        (
            NumSolutions = at_most_zero_rep,
            AfterCondExecCount2 = 0
        ;
            ( NumSolutions = at_most_one_rep
            ; NumSolutions = at_most_many_rep
            )
        )
    ;
        true
    ).

:- pred check_coverage_complete(coverage_info::in, goal_expr_rep(T)::in)
    is semidet.

check_coverage_complete(coverage_known(_, _), _GoalExpr).
check_coverage_complete(coverage_known_same(_), _GoalExpr).
% Uncomment this clause if, in the future, we allow coverage to be incomplete
% for trivial goals.
%check_coverage_complete(Coverage, GoalExpr) :-
%    ( Coverage = coverage_known_before(_)
%    ; Coverage = coverage_known_after(_)
%    ; Coverage = coverage_unknown
%    ),
%    goal_expr_is_trivial(GoalExpr).

:- func goal_is_trivial(goal_rep(T)) = bool.

goal_is_trivial(Goal) = IsTrivial:-
    GoalExpr = Goal ^ goal_expr_rep,
    IsTrivial = goal_expr_is_trivial(GoalExpr).

:- func goal_expr_is_trivial(goal_expr_rep(T)) = bool.

goal_expr_is_trivial(GoalRep) = IsTrivial :-
    (
        (
            GoalRep = conj_rep(SubGoalReps)
        ;
            GoalRep = disj_rep(SubGoalReps)
        ;
            GoalRep = switch_rep(_, _, CaseReps),
            SubGoalReps = list.map(project_case_rep_goal, CaseReps)
        ;
            GoalRep = ite_rep(CondRep, ThenRep, ElseRep),
            SubGoalReps = [CondRep, ThenRep, ElseRep]
        ),
        SubGoalIsTrivials = list.map(goal_is_trivial, SubGoalReps),
        bool.and_list(SubGoalIsTrivials, IsTrivial)
    ;
        ( GoalRep = negation_rep(SubGoalRep)
        ; GoalRep = scope_rep(SubGoalRep, _)
        ),
        IsTrivial = goal_is_trivial(SubGoalRep)
    ;
        GoalRep = atomic_goal_rep(_, _, _, AtomicGoalRep),
        (
            ( AtomicGoalRep = plain_call_rep(_, _, _)
            ; AtomicGoalRep = higher_order_call_rep(_, _)
            ; AtomicGoalRep = method_call_rep(_, _, _)
            ),
            IsTrivial = no
        ;
            ( AtomicGoalRep = unify_construct_rep(_, _, _)
            ; AtomicGoalRep = unify_deconstruct_rep(_, _, _)
            ; AtomicGoalRep = partial_deconstruct_rep(_, _, _)
            ; AtomicGoalRep = partial_construct_rep(_, _, _)
            ; AtomicGoalRep = unify_assign_rep(_, _)
            ; AtomicGoalRep = cast_rep(_, _)
            ; AtomicGoalRep = unify_simple_test_rep(_, _)
            % Built-in calls are cheap enough to consider to be trivial.
            ; AtomicGoalRep = builtin_call_rep(_, _, _)
            ; AtomicGoalRep = pragma_foreign_code_rep(_)
            ; AtomicGoalRep = event_call_rep(_, _)
            ),
            IsTrivial = yes
        )
    ).

%----------------------------------------------------------------------------%
%
% Coverage information helper predicates.
%

    % The coverage before a det goal should always equal the coverage after.
    %
:- pred propagate_det_coverage( coverage_before::in, coverage_after::out)
    is det.

propagate_det_coverage(Before, After) :-
    (
        Before = before_unknown,
        After = after_unknown
    ;
        Before = before_known(Count),
        After = after_known(Count)
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
        After = after_known(0)
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
    ).

:- pred after_count_from_either_source(coverage_after::in,
    coverage_after::in, coverage_after::out) is det.

after_count_from_either_source(AfterA, AfterB, After) :-
    (
        AfterA = after_unknown,
        AfterB = after_unknown,
        After = after_unknown
    ;
        AfterA = after_unknown,
        AfterB = after_known(AfterCount),
        After = after_known(AfterCount)
    ;
        AfterA = after_known(AfterCount),
        AfterB = after_unknown,
        After = after_known(AfterCount)
    ;
        AfterA = after_known(AfterCountA),
        AfterB = after_known(AfterCountB),
        require(unify(AfterCountA, AfterCountB),
            "after_count_from_either_source: mismatch"),
        After = after_known(AfterCountA)
    ).

    % Convert a sum_coverage_after to a coverage_after.
    %
:- pred count_sum_to_count(sum_coverage_after::in, coverage_after::out) is det.

count_sum_to_count(sum_after_unknown, after_unknown).
count_sum_to_count(sum_after_known(C), after_known(C)).

:- pred before_count_from_either_source(coverage_before::in,
    coverage_before::in, coverage_before::out) is det.

before_count_from_either_source(BeforeA, BeforeB, Before) :-
    (
        BeforeA = before_unknown,
        BeforeB = before_unknown,
        Before = before_unknown
    ;
        BeforeA = before_unknown,
        BeforeB = before_known(BeforeCount),
        Before = before_known(BeforeCount)
    ;
        BeforeA = before_known(BeforeCount),
        BeforeB = before_unknown,
        Before = before_known(BeforeCount)
    ;
        BeforeA = before_known(BeforeCountA),
        BeforeB = before_known(BeforeCountB),
        require(unify(BeforeCountA, BeforeCountB),
            "before_count_from_either_source: mismatch"),
        Before = before_known(BeforeCountA)
    ).

:- pred before_count_from_either_source_sum(sum_coverage_before::in,
    coverage_before::in, coverage_before::out) is det.

before_count_from_either_source_sum(BeforeA, BeforeB, Before) :-
    (
        BeforeA = sum_before_unknown,
        BeforeB = before_unknown,
        Before = before_unknown
    ;
        BeforeA = sum_before_unknown,
        BeforeB = before_known(BeforeCount),
        Before = before_known(BeforeCount)
    ;
        BeforeA = sum_before_known(BeforeCount),
        BeforeB = before_unknown,
        Before = before_known(BeforeCount)
    ;
        BeforeA = sum_before_known(BeforeCountA),
        BeforeB = before_known(BeforeCountB),
        require(unify(BeforeCountA, BeforeCountB),
            "before_count_from_either_source: mismatch"),
        Before = before_known(BeforeCountA)
    ).

:- pred sum_before_coverage(coverage_before::in,
    sum_coverage_before::in, sum_coverage_before::out) is det.

sum_before_coverage(Before, !SumBefore) :-
    (
        !.SumBefore = sum_before_known(SumExecCount),
        Before = before_known(ExecCount)
    ->
        !:SumBefore = sum_before_known(SumExecCount + ExecCount)
    ;
        !:SumBefore = sum_before_unknown
    ).

:- pred sum_after_coverage(coverage_after::in,
    sum_coverage_after::in, sum_coverage_after::out) is det.

sum_after_coverage(After, !SumAfter) :-
    (
        !.SumAfter = sum_after_known(SumExecCount),
        After = after_known(ExecCount)
    ->
        !:SumAfter = sum_after_known(SumExecCount + ExecCount)
    ;
        !:SumAfter = sum_after_unknown
    ).

%----------------------------------------------------------------------------%

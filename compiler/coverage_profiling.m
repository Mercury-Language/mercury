%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: coverage_profiling.m.
% Main author: pbone.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.coverage_profiling.

:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.program_representation.
:- import_module parse_tree.
:- import_module parse_tree.var_table.

:- import_module list.
:- import_module maybe.

    % Perform the coverage profiling transformation on the given goal,
    % and return a list of the coverage points created.
    %
:- pred coverage_prof_transform_proc_body(module_info::in, pred_proc_id::in,
    containing_goal_map::in, maybe(deep_recursion_info)::in,
    list(coverage_point_info)::out,
    hlds_goal::in, hlds_goal::out, var_table::in, var_table::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_util.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.deep_profiling.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module counter.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.

    % Information used by the coverage profiling transformation.
    %
:- type proc_coverage_info
    --->    proc_coverage_info(
                % A map from coverage point indexes to coverage point
                % information. Updated as coverage points are inserted.
                ci_coverage_points          :: map(int, coverage_point_info),

                % Used to track the next coverage point index to be allocated.
                ci_cp_index_counter         :: counter,

                % Information about variables, this is updated as variables are
                % introduced.
                ci_var_table                :: var_table,

                % The following fields are static; they are not modified
                % after initialization.
                ci_module_info              :: module_info,
                ci_pred_proc_id             :: pred_proc_id,
                ci_maybe_rec_info           :: maybe(deep_recursion_info),
                ci_coverage_profiling_opts  :: coverage_profiling_options,

                ci_containing_goal_map      :: containing_goal_map
            ).

:- type coverage_data_type
    --->    static_coverage_data
    ;       dynamic_coverage_data.

    % Store what coverage profiling options have been selected.
    %
:- type coverage_profiling_options
    --->    coverage_profiling_options(
                % These fields correspond to coverage profiling options that
                % may be specified on the command line.

                % Use per ProcDynamic coverage rather than per ProcStatic.
                cpo_dynamic_coverage        :: coverage_data_type,

                % Use calls to library code rather than inline foreign code.
                cpo_use_calls               :: bool,

                cpo_coverage_after_goal     :: bool,
                cpo_branch_ite              :: bool,
                cpo_branch_switch           :: bool,
                cpo_branch_disj             :: bool,
                cpo_use_portcounts          :: bool,
                cpo_use_trivial             :: bool,

                % cpo_run_first_pass is true if some information needs to be
                % collected in an initial pass.
                cpo_run_first_pass          :: bool
            ).

:- pred init_coverage_profiling_options(module_info::in,
    coverage_profiling_options::out) is det.

init_coverage_profiling_options(ModuleInfo, CoveragePointOptions) :-
    module_info_get_globals(ModuleInfo, Globals),

    % Options controlling what instrumentation code we generate.
    globals.lookup_bool_option(Globals, coverage_profiling_static,
        Static),
    (
        Static = yes,
        DataType = static_coverage_data
    ;
        Static = no,
        DataType = dynamic_coverage_data
    ),
    globals.lookup_bool_option(Globals, coverage_profiling_via_calls,
        UseCalls),

    % Coverage point types.
    globals.lookup_bool_option(Globals, profile_deep_coverage_after_goal,
        CoverageAfterGoal),
    globals.lookup_bool_option(Globals, profile_deep_coverage_branch_ite,
        BranchIf),
    globals.lookup_bool_option(Globals, profile_deep_coverage_branch_switch,
        BranchSwitch),
    globals.lookup_bool_option(Globals, profile_deep_coverage_branch_disj,
        BranchDisj),

    % Options for tuning the coverage profiling pass.
    globals.lookup_bool_option(Globals, profile_deep_coverage_use_portcounts,
        UsePortCounts),
    globals.lookup_bool_option(Globals, profile_deep_coverage_use_trivial,
        UseTrivial),
    bool.or(UsePortCounts, UseTrivial, RunFirstPass),

    CoveragePointOptions = coverage_profiling_options(DataType, UseCalls,
        CoverageAfterGoal, BranchIf, BranchSwitch, BranchDisj,
        UsePortCounts, UseTrivial, RunFirstPass).

%-----------------------------------------------------------------------------%

coverage_prof_transform_proc_body(ModuleInfo, PredProcId, ContainingGoalMap,
        MaybeRecInfo, CoveragePoints, !Goal, !VarTable) :-
    init_coverage_profiling_options(ModuleInfo, CoverageProfilingOptions),
    CoverageInfo0 = init_proc_coverage_info(!.VarTable, ModuleInfo,
        PredProcId, MaybeRecInfo, CoverageProfilingOptions, ContainingGoalMap),
    RunFirstPass = CoverageProfilingOptions ^ cpo_run_first_pass,
    (
        RunFirstPass = yes,
        coverage_prof_first_pass(CoverageProfilingOptions, !Goal,
            port_counts_give_coverage_after, _)
    ;
        RunFirstPass = no
    ),
    coverage_prof_second_pass_goal(!Goal, coverage_before_known, _,
        CoverageInfo0, CoverageInfo, _),
    CoverageInfo ^ ci_coverage_points = CoveragePointsMap,
    CoverageInfo ^ ci_var_table = !:VarTable,
    coverage_points_map_list(CoveragePointsMap, CoveragePoints).

    % Transform a goal for coverage profiling. This is the second pass of
    % the coverage profiling transformation, and it consists of several steps.
    %
    % Step 1: Apply transformation recursively.
    %
    % Step 2: Decide whether to insert a coverage point after this goal
    % to measure how many times it succeeds.
    %
    % Step 3: Insert the coverage point if we decided to do so.
    %
:- pred coverage_prof_second_pass_goal(hlds_goal::in, hlds_goal::out,
    coverage_before_known::in, coverage_before_known::out,
    proc_coverage_info::in, proc_coverage_info::out, bool::out) is det.

coverage_prof_second_pass_goal(Goal0, Goal,
        CoverageBeforeKnown, NextCoverageBeforeKnown, !Info, AddedImpurity) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    Detism = goal_info_get_determinism(GoalInfo0),
    GoalId = goal_info_get_goal_id(GoalInfo0),
    CPOptions = !.Info ^ ci_coverage_profiling_opts,

    % Currently the first pass is unsupported, we don't make use of the
    % information it provides in IsMDProfInst.
    DPInfo = goal_info_get_dp_info(GoalInfo0),
    DPInfo = dp_goal_info(IsMDProfInst, _MaybeDPCoverageInfo),

    ( if
        IsMDProfInst = goal_is_not_mdprof_inst,
        CoverageBeforeKnown = coverage_before_unknown
    then
        GoalId = goal_id(GoalNum),
        UnknownMsg = string.format(
            "Coverage information is unknown for goal_id %d\n", [i(GoalNum)]),
        unexpected($pred, UnknownMsg)
    else
        true
    ),

    % Step 1.
    %
    % Apply transformation recursively.
    (
        (
            GoalExpr0 = unify(_, _, _, _, _)
        ;
            GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
            % Even though the deep profiler creates a call site when these may
            % call Mercury, the coverage propagation code cannot make use of
            % the call site's port counts, since they measure how often
            % Mercury is re-entered, not how often this call is made.
        ),
        coverage_known_after_goal_with_detism(Detism,
            CoverageBeforeKnown, NextCoverageBeforeKnown0),
        AddedImpurityInner = no,
        GoalExpr1 = GoalExpr0
    ;
        (
            GoalExpr0 = plain_call(_, _, _, BuiltinState, _, _),
            (
                BuiltinState = not_builtin,
                GathersCoverageAfter = yes
            ;
                BuiltinState = inline_builtin,
                GathersCoverageAfter = no
            )
        ;
            GoalExpr0 = generic_call(GenericCall, _, _, _, _),
            (
                ( GenericCall = higher_order(_, _, _, _)
                ; GenericCall = class_method(_, _, _, _)
                ),
                GathersCoverageAfter = yes
            ;
                ( GenericCall = cast(_)
                ; GenericCall = event_call(_)
                ),
                GathersCoverageAfter = no
            )
        ),
        (
            GathersCoverageAfter = yes,
            NextCoverageBeforeKnown0 = coverage_before_known
        ;
            GathersCoverageAfter = no,
            coverage_known_after_goal_with_detism(Detism,
                CoverageBeforeKnown, NextCoverageBeforeKnown0)
        ),
        AddedImpurityInner = no,
        GoalExpr1 = GoalExpr0
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        coverage_prof_second_pass_conj(ConjType, Goals0, Goals,
            CoverageBeforeKnown, NextCoverageBeforeKnown0, !Info,
            AddedImpurityInner),
        GoalExpr1 = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        coverage_prof_second_pass_disj(DPInfo,
            CoverageBeforeKnown, NextCoverageBeforeKnown0,
            Goals0, Goals, !Info, AddedImpurityInner),
        GoalExpr1 = disj(Goals)
    ;
        GoalExpr0 = switch(Var, SwitchCanFail, Cases0),
        coverage_prof_second_pass_switchcase(DPInfo, SwitchCanFail,
            Cases0, Cases, CoverageBeforeKnown, NextCoverageBeforeKnown0,
            !Info, AddedImpurityInner),
        GoalExpr1 = switch(Var, SwitchCanFail, Cases)
    ;
        GoalExpr0 = negation(NegGoal0),
        coverage_prof_second_pass_goal(NegGoal0, NegGoal,
            CoverageBeforeKnown, _, !Info, AddedImpurityInner),
        % The coverage after a negated goal cannot be inferred from its inner
        % goals.
        NextCoverageBeforeKnown0 = coverage_before_unknown,
        GoalExpr1 = negation(NegGoal)
    ;
        GoalExpr0 = scope(Reason, ScopeGoal0),
        % We should special-case the handling of from_ground_term_construct
        % scopes, but that would require special-casing the coverage
        % propagation code in the deep profiler as well.
        coverage_prof_second_pass_goal(ScopeGoal0, ScopeGoal,
            CoverageBeforeKnown, CoverageAfterScopedGoalKnown, !Info,
            AddedImpurityInner),
        % A scope may cut away solutions, if it does we don't know the number
        % of solutions of the scoped goal.
        ScopedGoalDetism = goal_info_get_determinism(ScopeGoal0 ^ hg_info),
        ( if ScopedGoalDetism = Detism then
            NextCoverageBeforeKnown0 = CoverageAfterScopedGoalKnown
        else
            NextCoverageBeforeKnown0 = coverage_before_unknown
        ),
        GoalExpr1 = scope(Reason, ScopeGoal)
    ;
        GoalExpr0 = if_then_else(ITEExistVars, Cond, Then, Else),
        coverage_prof_second_pass_ite(DPInfo, ITEExistVars, Cond, Then, Else,
            GoalExpr1, CoverageBeforeKnown, NextCoverageBeforeKnown0, !Info,
            AddedImpurityInner)
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
    ),

    % Step 2.
    %
    % Decide whether we need to insert a coverage point after this goal
    % to measure how many times execution reaches there.
    ( if
        (
            % Never insert coverage points on goals that are part of the deep
            % profiling instrumentation.
            IsMDProfInst = goal_is_mdprof_inst
        ;
            % We already have execution counts for the program point after this
            % goal; adding a counter would be redundant.
            NextCoverageBeforeKnown0 = coverage_before_known
        )
    then
        MaybeAddCP = no,
        NextCoverageBeforeKnown = NextCoverageBeforeKnown0
    else
        CoverageAfterGoals = CPOptions ^ cpo_coverage_after_goal,
        (
            CoverageAfterGoals  = yes,
            MaybeAddCP = yes(cp_type_coverage_after),
            NextCoverageBeforeKnown = coverage_before_known
        ;
            CoverageAfterGoals = no,
            MaybeAddCP = no,
            NextCoverageBeforeKnown = NextCoverageBeforeKnown0
        )
    ),

    % Step 3.
    %
    % Insert the coverage point if we decided to.
    add_impurity_if_needed(AddedImpurityInner, GoalInfo0, GoalInfo1),
    Goal1 = hlds_goal(GoalExpr1, GoalInfo1),
    (
        MaybeAddCP = yes(CPType),
        ContainingGoalMap = !.Info ^ ci_containing_goal_map,
        RevGoalPath = goal_id_to_reverse_path(ContainingGoalMap, GoalId),
        CPInfo = coverage_point_info(RevGoalPath, CPType),

        make_coverage_point(CPOptions, CPInfo, CPGoals, !Info),
        create_conj_from_list([Goal1 | CPGoals], plain_conj, Goal),

        AddedImpurity = yes
    ;
        MaybeAddCP = no,
        Goal = Goal1,
        AddedImpurity = AddedImpurityInner
    ).

:- pred coverage_known_after_goal_with_detism(determinism::in,
    coverage_before_known::in, coverage_before_known::out) is det.

coverage_known_after_goal_with_detism(Detism, !CoverageKnown) :-
    (
        ( Detism = detism_semi
        ; Detism = detism_multi
        ; Detism = detism_non
        ; Detism = detism_cc_non
        ; Detism = detism_erroneous
        ; Detism = detism_failure
        ),
        !:CoverageKnown = coverage_before_unknown
    ;
        ( Detism = detism_det
        ; Detism = detism_cc_multi
        )
    ).

    % Perform the coverage profiling transformation for conjuncts.
    %
    % The goal list represents the tail of a conjunction. Pos is the position
    % of this list within the entire conjunction, if this is the entire
    % conjunction then Pos should be 1.
    %
:- pred coverage_prof_second_pass_conj(conj_type::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    coverage_before_known::in, coverage_before_known::out,
    proc_coverage_info::in, proc_coverage_info::out, bool::out) is det.

coverage_prof_second_pass_conj(_, [], [], !CoverageBeforeKnown, !Info, no).
coverage_prof_second_pass_conj(ConjType, [HeadGoal0 | TailGoals0], Goals,
        CoverageBeforeKnown, NextCoverageBeforeKnown, !Info, AddedImpurity) :-
    coverage_prof_second_pass_goal(HeadGoal0, HeadGoal,
        CoverageBeforeKnown, CoverageBeforeTailKnown, !Info,
        AddedImpurityHead),
    coverage_prof_second_pass_conj(ConjType, TailGoals0, TailGoals,
        CoverageBeforeTailKnown, NextCoverageBeforeKnown, !Info,
        AddedImpurityTail),
    % Flatten the conjunction. We need to do this if we replaced the head
    % with a goal that is itself a conjunction.
    ( if
        HeadGoal = hlds_goal(conj(plain_conj, HeadConjGoals), _),
        ConjType = plain_conj
    then
        Goals = HeadConjGoals ++ TailGoals
    else
        Goals = [HeadGoal | TailGoals]
    ),
    bool.or(AddedImpurityHead, AddedImpurityTail, AddedImpurity).

    % Perform the coverage profiling transformation over goals within a
    % disjunction.
    %
:- pred coverage_prof_second_pass_disj(dp_goal_info::in,
    coverage_before_known::in, coverage_before_known::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    proc_coverage_info::in, proc_coverage_info::out, bool::out) is det.

coverage_prof_second_pass_disj(DPInfo, CoverageBeforeKnown,
        NextCoverageBeforeKnown, Disjuncts0, Disjuncts, !Info,
        AddedImpurity) :-
    % If the disjunction was introduced by the deep profiling pass, it has two
    % disjuncts and its second disjunct has 'failure' determinism, then
    % perform the coverage profiling pass on the first disjunct as if this
    % is the only goal.
    ( if
        DPInfo = dp_goal_info(goal_is_mdprof_inst, _),
        Disjuncts0 = [FirstDisjunct0, SecondDisjunct],
        goal_info_get_determinism(SecondDisjunct ^ hg_info) = detism_failure
        % XXX: zs: Would this be a better test here?
        % goal_has_feature(SecondDisjunct, feature_preserve_backtrack_into)
        % pbone: I don't think so, the deep profiler doesn't seem to add this
        % feature to disjuncts that end in failure, it is probably a good idea
        % to add this annotation to prevent later compiler passes from breaking
        % the deep profiler.
    then
        coverage_prof_second_pass_goal(FirstDisjunct0, FirstDisjunct,
            CoverageBeforeKnown, NextCoverageBeforeKnown, !Info,
            AddedImpurity),
        Disjuncts = [FirstDisjunct, SecondDisjunct]
    else
        coverage_prof_second_pass_disj_2(DPInfo, CoverageBeforeKnown,
            coverage_before_known, NextCoverageBeforeKnown,
            Disjuncts0, Disjuncts, !Info, AddedImpurity)
    ).

:- pred coverage_prof_second_pass_disj_2(dp_goal_info::in,
    coverage_before_known::in,
    coverage_before_known::in, coverage_before_known::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    proc_coverage_info::in, proc_coverage_info::out, bool::out) is det.

coverage_prof_second_pass_disj_2(_, _, !CoverageKnownAfter, [], [], !Info, no).
coverage_prof_second_pass_disj_2(DPInfo,
        CoverageBeforeKnown0, !CoverageAfterKnown,
        [HeadDisjunct0 | TailDisjuncts0], [HeadDisjunct | TailDisjuncts],
        !Info, AddedImpurity) :-
    % Decide whether we want to insert a branch coverage point at the beginning
    % of the head disjunct.
    CPOptions = !.Info ^ ci_coverage_profiling_opts,
    CPOBranchDisj = CPOptions ^ cpo_branch_disj,
    DPInfo = dp_goal_info(IsMDProfInst, _),
    ( if
        CPOBranchDisj = yes,
        CoverageBeforeKnown0 = coverage_before_unknown,
        IsMDProfInst = goal_is_not_mdprof_inst
    then
        InsertCP = yes,
        CoverageBeforeKnown = coverage_before_known
    else
        InsertCP = no,
        CoverageBeforeKnown = CoverageBeforeKnown0
    ),

    coverage_prof_second_pass_goal(HeadDisjunct0, HeadDisjunct1,
        CoverageBeforeKnown, CoverageAfterDisjKnown, !Info, AddedImpurityHead),
    !:CoverageAfterKnown = coverage_before_known_and(!.CoverageAfterKnown,
        CoverageAfterDisjKnown),
    coverage_prof_second_pass_disj_2(DPInfo, coverage_before_unknown,
        !CoverageAfterKnown, TailDisjuncts0, TailDisjuncts, !Info,
        AddedImpurityTail),

    % Insert the coverage point if we decided to above.
    (
        InsertCP = yes,
        DisjId = goal_info_get_goal_id(HeadDisjunct0 ^ hg_info),
        ContainingGoalMap = !.Info ^ ci_containing_goal_map,
        DisjPath = goal_id_to_reverse_path(ContainingGoalMap, DisjId),
        HeadCoveragePoint = coverage_point_info(DisjPath, cp_type_branch_arm),
        insert_coverage_point_before(CPOptions, HeadCoveragePoint,
            HeadDisjunct1, HeadDisjunct, !Info),
        AddedImpurity = yes
    ;
        InsertCP = no,
        HeadDisjunct = HeadDisjunct1,
        AddedImpurity = bool.or(AddedImpurityHead, AddedImpurityTail)
    ).

    % coverage_prof_second_pass_switchcase(DPInfo, SwitchCanFial, !Cases,
    %   CoverageBeforeSwitch, !Info, AddedImpurity).
    %
    % Preform coverage profiling transformation on switch cases.
    %
:- pred coverage_prof_second_pass_switchcase(dp_goal_info::in, can_fail::in,
    list(case)::in, list(case)::out,
    coverage_before_known::in, coverage_before_known::out,
    proc_coverage_info::in, proc_coverage_info::out, bool::out) is det.

coverage_prof_second_pass_switchcase(DPInfo, CanFail, Cases0, Cases,
        CoverageBeforeSwitchKnown, CoverageAfterSwitchKnown, !Info,
        AddedImpurity) :-
    % If the switch can fail then the coverage after it will be unknown.
    (
        CanFail = can_fail,
        CoverageAfterSwitchKnown0 = coverage_before_unknown
    ;
        CanFail = cannot_fail,
        CoverageAfterSwitchKnown0 = coverage_before_known
    ),
    CoverageBeforeEveryCaseKnown = coverage_before_known,
    coverage_prof_second_pass_switchcase_2(DPInfo, CanFail, Cases0, Cases,
        CoverageBeforeSwitchKnown, CoverageBeforeEveryCaseKnown,
        CoverageAfterSwitchKnown0, CoverageAfterSwitchKnown, !Info,
        AddedImpurity).

:- pred coverage_prof_second_pass_switchcase_2(dp_goal_info::in, can_fail::in,
    list(case)::in, list(case)::out, coverage_before_known::in,
    coverage_before_known::in,
    coverage_before_known::in,  coverage_before_known::out,
    proc_coverage_info::in, proc_coverage_info::out, bool::out) is det.

coverage_prof_second_pass_switchcase_2(_, _, [], [], _, _,
        !CoverageAfterSwitchKnown, !Info, no).
coverage_prof_second_pass_switchcase_2(DPInfo, SwitchCanFail,
        [Case0 | Cases0], [Case | Cases], CoverageBeforeSwitchKnown,
        CoverageBeforeEveryCaseKnown, !CoverageAfterSwitchKnown, !Info,
        AddedImpurity) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),

    % If the switch cannot fail and this is the last case, then the coverage
    % at the beginning of this case can be computed from the coverage before
    % the entire switch and coverage information from each of the other
    % branches of the switch.
    (
        Cases0 = [],
        (
            SwitchCanFail = cannot_fail,
            CoverageBeforeCaseKnown0 = coverage_before_known_and(
                CoverageBeforeSwitchKnown, CoverageBeforeEveryCaseKnown)
        ;
            SwitchCanFail = can_fail,
            CoverageBeforeCaseKnown0 = coverage_before_unknown
        )
    ;
        Cases0 = [_ | _],
        CoverageBeforeCaseKnown0 = coverage_before_unknown
    ),

    % Decide whether to insert a coverage point here.
    CPOptions = !.Info ^ ci_coverage_profiling_opts,
    CPOBranchSwitch = CPOptions ^ cpo_branch_switch,
    DPInfo = dp_goal_info(IsMDProfInst, _),
    ( if
        CPOBranchSwitch = yes,
        CoverageBeforeCaseKnown0 = coverage_before_unknown,
        IsMDProfInst = goal_is_not_mdprof_inst
    then
        InsertCP = yes,
        CoverageBeforeCaseKnown = coverage_before_known
    else
        InsertCP = no,
        CoverageBeforeCaseKnown = CoverageBeforeCaseKnown0
    ),

    coverage_prof_second_pass_goal(Goal0, Goal1,
        CoverageBeforeCaseKnown, CoverageAfterCaseKnown, !Info,
        AddedImpurityHead0),
    !:CoverageAfterSwitchKnown = coverage_before_known_and(
        CoverageAfterCaseKnown, !.CoverageAfterSwitchKnown),

    % Possibly insert coverage point at the start of the case.
    (
        InsertCP = yes,
        CaseId = goal_info_get_goal_id(Goal0 ^ hg_info),
        ContainingGoalMap = !.Info ^ ci_containing_goal_map,
        CasePath = goal_id_to_reverse_path(ContainingGoalMap, CaseId),
        CoveragePoint = coverage_point_info(CasePath, cp_type_branch_arm),
        insert_coverage_point_before(CPOptions, CoveragePoint, Goal1, Goal,
            !Info),
        AddedImpurityHead = yes
    ;
        InsertCP = no,
        Goal = Goal1,
        AddedImpurityHead = AddedImpurityHead0
    ),

    % Handle recursive case and prepare output variables.
    % We cannot optimize away the coverage point at the start of the last case
    % if one of the previous cases does not have coverage information at its
    % start.
    NextCoverageBeforeEveryCaseKnown = coverage_before_known_and(
        CoverageBeforeEveryCaseKnown, CoverageBeforeCaseKnown),
    coverage_prof_second_pass_switchcase_2(DPInfo, SwitchCanFail,
        Cases0, Cases,
        CoverageBeforeSwitchKnown, NextCoverageBeforeEveryCaseKnown,
        !CoverageAfterSwitchKnown, !Info, AddedImpurityTail),
    Case = case(MainConsId, OtherConsIds, Goal),
    bool.or(AddedImpurityHead, AddedImpurityTail, AddedImpurity).

    % Determine if branch coverage points should be inserted in either or
    % both of the then and else branches, insert them and transform the
    % subgoals.
    %
    % This is performed by first transforming the condition, then making
    % decisions about coverage points and inserting them, then transforming
    % the then and else branches and constructing the new ITE goal_expr.
    %
:- pred coverage_prof_second_pass_ite(dp_goal_info::in, list(prog_var)::in,
    hlds_goal::in, hlds_goal::in, hlds_goal::in, hlds_goal_expr::out,
    coverage_before_known::in, coverage_before_known::out,
    proc_coverage_info::in, proc_coverage_info::out, bool::out) is det.

coverage_prof_second_pass_ite(DPInfo, ITEExistVars, Cond0, Then0, Else0,
        GoalExpr, CoverageBeforeITEKnown, NextCoverageBeforeKnown,
        !Info, AddedImpurity) :-
    % Transform the condition.
    coverage_prof_second_pass_goal(Cond0, Cond,
        CoverageBeforeITEKnown, CoverageKnownAfterCond, !Info,
        AddedImpurityCond),

    CoverageKnownBeforeThen0 = CoverageKnownAfterCond,
    CoverageKnownBeforeElse0 = coverage_before_unknown,

    % Gather information and decide what coverage points to insert.
    %
    % Notice that it doesn't matter if any of the goals are trivial or not,
    % we want to know what branch is taken regardless of how inexpensive it
    % may be as different variables may be used in different branches.
    %
    % Whatever we do we will ensure that the coverage will be known at the
    % beginning of each branch.
    CPOptions = !.Info ^ ci_coverage_profiling_opts,
    CPOBranchIf = CPOptions ^ cpo_branch_ite,
    DPInfo = dp_goal_info(IsMDProfInst, _),
    ( if
        CPOBranchIf = yes,
        IsMDProfInst = goal_is_not_mdprof_inst
    then
        ContainingGoalMap = !.Info ^ ci_containing_goal_map,
        (
            CoverageKnownBeforeThen0 = coverage_before_unknown,
            ThenId = goal_info_get_goal_id(Then0 ^ hg_info),
            ThenPath = goal_id_to_reverse_path(ContainingGoalMap, ThenId),
            InsertCPThen = yes(coverage_point_info(ThenPath,
                cp_type_branch_arm))
        ;
            CoverageKnownBeforeThen0 = coverage_before_known,
            InsertCPThen = no
        ),
        CoverageKnownBeforeThen = coverage_before_known,

        ElseId = goal_info_get_goal_id(Else0 ^ hg_info),
        ElsePath = goal_id_to_reverse_path(ContainingGoalMap, ElseId),
        CondDetism = goal_info_get_determinism(Cond ^ hg_info),
        determinism_components(CondDetism, _, CondSolns),
        (
            CondSolns = at_most_many,

            % Always insert a coverage point for the else branch.
            InsertCPElse = yes(coverage_point_info(ElsePath,
                cp_type_branch_arm)),
            CoverageKnownBeforeElse = coverage_before_known
        ;
            ( CondSolns = at_most_zero
            ; CondSolns = at_most_one
            ; CondSolns = at_most_many_cc
            ),

            % Only insert a coverage point if we cannot infer the coverage
            % from before the ITE and before the then branch.
            ( if
                CoverageBeforeITEKnown = coverage_before_known,
                CoverageKnownBeforeThen = coverage_before_known
            then
                InsertCPElse = no,
                CoverageKnownBeforeElse = coverage_before_known
            else
                InsertCPElse = yes(coverage_point_info(ElsePath,
                    cp_type_branch_arm)),
                CoverageKnownBeforeElse = coverage_before_known
            )
        )
    else
        % Don't insert any coverage points.
        InsertCPThen = no,
        InsertCPElse = no,
        CoverageKnownBeforeThen = CoverageKnownBeforeThen0,
        CoverageKnownBeforeElse = CoverageKnownBeforeElse0
    ),

    % Transform Then and Else branches,
    coverage_prof_second_pass_goal(Then0, Then1,
        CoverageKnownBeforeThen, NextCoverageKnownThen, !Info,
        AddedImpurityThenGoal),
    coverage_prof_second_pass_goal(Else0, Else1,
        CoverageKnownBeforeElse, NextCoverageKnownElse, !Info,
        AddedImpurityElseGoal),

    % Insert any coverage points.
    (
        InsertCPThen = yes(CPInfoThen),
        insert_coverage_point_before(CPOptions, CPInfoThen, Then1, Then,
            !Info),
        AddedImpurityThen = yes
    ;
        InsertCPThen = no,
        Then = Then1,
        AddedImpurityThen = AddedImpurityThenGoal
    ),
    (
        InsertCPElse = yes(CPInfoElse),
        insert_coverage_point_before(CPOptions, CPInfoElse, Else1, Else,
            !Info),
        AddedImpurityElse = yes
    ;
        InsertCPElse = no,
        Else = Else1,
        AddedImpurityElse = AddedImpurityElseGoal
    ),

    % Build goal expression and tidy up.
    AddedImpurity = bool.or(AddedImpurityCond,
        bool.or(AddedImpurityThen, AddedImpurityElse)),
    GoalExpr = if_then_else(ITEExistVars, Cond, Then, Else),
    NextCoverageBeforeKnown = coverage_before_known_and(
        NextCoverageKnownThen, NextCoverageKnownElse).

%-----------------------------------------------------------------------------%

    % Create a coverage info struture, initializing some values to sensible
    % defaults.
    %
:- func init_proc_coverage_info(var_table, module_info, pred_proc_id,
    maybe(deep_recursion_info), coverage_profiling_options,
    containing_goal_map) = proc_coverage_info.

init_proc_coverage_info(VarTable, ModuleInfo, PredProcId, MaybeRecInfo,
        CoverageProfilingOptions, ContainingGoalMap) = CoverageInfo :-
    CoverageInfo = proc_coverage_info(map.init, counter.init(0), VarTable,
        ModuleInfo, PredProcId, MaybeRecInfo, CoverageProfilingOptions,
        ContainingGoalMap).

    % Used to describe if coverage information is known at a partiular point
    % within a procedure.
    %
:- type coverage_before_known
    --->    coverage_before_known
    ;       coverage_before_unknown.

    % The logical 'and' of coverage_before_known values.
:- func coverage_before_known_and(coverage_before_known, coverage_before_known)
    = coverage_before_known.

coverage_before_known_and(coverage_before_known, coverage_before_known) =
    coverage_before_known.
coverage_before_known_and(coverage_before_known, coverage_before_unknown) =
    coverage_before_unknown.
coverage_before_known_and(coverage_before_unknown, _) =
    coverage_before_unknown.

    % Boolean AND for the goal_trivial data type.
    %
:- pred goal_trivial_and(goal_trivial::in, goal_trivial::in,
    goal_trivial::out) is det.

goal_trivial_and(A, B, Trivial) :-
    ( if
        A = goal_is_trivial,
        B = goal_is_trivial
    then
        Trivial = goal_is_trivial
    else
        Trivial = goal_is_nontrivial
    ).

:- pred port_counts_give_coverage_after_and(
    port_counts_give_coverage_after::in, port_counts_give_coverage_after::in,
    port_counts_give_coverage_after::out) is det.

port_counts_give_coverage_after_and(A, B, PortCountsCoverageAfter) :-
    ( if
        A = port_counts_give_coverage_after,
        B = port_counts_give_coverage_after
    then
        PortCountsCoverageAfter = port_counts_give_coverage_after
    else
        PortCountsCoverageAfter = no_port_counts_give_coverage_after
    ).

    % Given a goal, whether it has its own port counts and whether port counts
    % are available immediately before it, determine if either set of port
    % counts allows us to determine how often execution reaches the point
    % immediately after the goal.
    %
:- pred has_port_counts_after(hlds_goal::in,
    port_counts_give_coverage_after::in, port_counts_give_coverage_after::in,
    port_counts_give_coverage_after::out) is det.

has_port_counts_after(Goal, PCDirect, PCBefore, PC) :-
    (
        % The trivial case. If port counts are directly available,
        % then they can be used to determine coverage immediately after it.

        PCDirect = port_counts_give_coverage_after,
        PC = port_counts_give_coverage_after
    ;
        PCDirect = no_port_counts_give_coverage_after,

        % If port counts aren't directly available but are before this goal
        % and this goal behaves deterministically (it cannot fail or redo),
        % then they can be used to determine how often execution reaches the
        % point after this goal.

        Detism = goal_info_get_determinism(Goal ^ hg_info),
        has_port_counts_if_det(Detism, PCBefore, PC)
    ).

    % Given the current goal's determinism and whether the next earliest goal
    % has port counts does this goal have port counts
    %
:- pred has_port_counts_if_det(determinism::in,
    port_counts_give_coverage_after::in, port_counts_give_coverage_after::out)
    is det.

has_port_counts_if_det(Detism,
        PortCountsCoverageAfter0, PortCountsCoverageAfter) :-
    ( if
        ( Detism = detism_det
        ; Detism = detism_cc_multi
        )
    then
        PortCountsCoverageAfter = PortCountsCoverageAfter0
    else
        PortCountsCoverageAfter = no_port_counts_give_coverage_after
    ).

    % Used to gather some information about goals before the coverage
    % transformation.
    %
    % This pass gathers the information in the dp_coverage_goal_info structure,
    % namely
    %
    % - whether the goal is trivial (a goal is trivial if neither it
    %   nor any of its subgoals are calls), and
    % - whether a port count is available from the deep profiler from which
    %   the coverage _after_ this goal can be computed.
    %
    % XXX: Currently the first pass is unsupported. The second pass does not
    % use the information it generates.
    %
:- pred coverage_prof_first_pass(coverage_profiling_options::in, hlds_goal::in,
    hlds_goal::out, port_counts_give_coverage_after::in,
    dp_coverage_goal_info::out) is det.

coverage_prof_first_pass(CPOptions, Goal0, Goal, PortCountsCoverageAfterBefore,
        Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        % XXX: Not all call goals have associated call sites, therefore not all
        % of these will have port counts. For, example inline foreign code does
        % not get instrumented by the deep profiler. See above in the
        % deep_profiling transformation.
        %
        % This doesn't matter for the near future, since we are using a single
        % pass coverage profiling algorithm. This will need to be fixed when
        % the two-pass coverage profiling is enabled. Or if a naive assumption
        % in the second pass is corrected, (See the XXX comment at the
        % beginning of coverage_prof_second_pass_goal regarding the defaults
        % that are assumed if the information from the first pass is not
        % available.).
        %
        GoalExpr0 = plain_call(_, _, _, BuiltinState, _, _),
        (
            BuiltinState = not_builtin,
            Trivial0 = goal_is_nontrivial,
            PortCountsCoverageAfterDirect = port_counts_give_coverage_after
        ;
            BuiltinState = inline_builtin,
            Trivial0 = goal_is_trivial,
            PortCountsCoverageAfterDirect = no_port_counts_give_coverage_after
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = generic_call(GenericCall, _, _, _, _),
        (
            ( GenericCall = higher_order(_, _, _, _)
            ; GenericCall = class_method(_, _, _, _)
            ),
            Trivial0 = goal_is_nontrivial,
            PortCountsCoverageAfterDirect = port_counts_give_coverage_after
        ;
            ( GenericCall = cast(_)
            ; GenericCall = event_call(_)
            ),
            Trivial0 = goal_is_trivial,
            PortCountsCoverageAfterDirect = no_port_counts_give_coverage_after
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _),
        % Some foreign proc goals may be trivial., but there is no clear
        % criteria by which we can conclude that here.
        Trivial0 = goal_is_nontrivial,
        PortCountsCoverageAfterDirect = no_port_counts_give_coverage_after,
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = unify(_, _, _, _, _),
        Trivial0 = goal_is_trivial,
        PortCountsCoverageAfterDirect = no_port_counts_give_coverage_after,
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        map_foldl2(coverage_prof_first_pass_conj(CPOptions), Goals0, Goals,
            goal_is_trivial, Trivial0,
            PortCountsCoverageAfterBefore, PortCountsCoverageAfterDirect),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        coverage_prof_first_pass_disj(CPOptions, Goals0, Goals, Trivial0,
            PortCountsCoverageAfterBefore, PortCountsCoverageAfterDirect),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        coverage_prof_first_pass_switchcase(CPOptions, Cases0, Cases, Trivial0,
            PortCountsCoverageAfterCases),
        GoalExpr = switch(Var, CanFail, Cases),
        (
            CanFail = can_fail,
            PortCountsCoverageAfterDirect = no_port_counts_give_coverage_after
        ;
            CanFail = cannot_fail,
            PortCountsCoverageAfterDirect = PortCountsCoverageAfterCases
        )
    ;
        GoalExpr0 = negation(InnerGoal0),
        coverage_prof_first_pass(CPOptions, InnerGoal0, InnerGoal,
            PortCountsCoverageAfterBefore,
            dp_coverage_goal_info(Trivial0, PortCountsCoverageAfterDirect)),
        GoalExpr = negation(InnerGoal)
    ;
        GoalExpr0 = scope(Reason, InnerGoal0),
        % We should special-case the handling of from_ground_term_construct
        % scopes, but that would require special-casing the coverage
        % propagation code in the deep profiler as well.
        coverage_prof_first_pass(CPOptions, InnerGoal0, InnerGoal,
            PortCountsCoverageAfterBefore,
            dp_coverage_goal_info(Trivial0, PortCountsCoverageAfterDirect)),
        GoalExpr = scope(Reason, InnerGoal)
    ;
        GoalExpr0 = if_then_else(Vars, CondGoal0, ThenGoal0, ElseGoal0),

        % The then and else parts of a ITE goal will be able to use the
        % port counts provided by the cond goal if it has them.

        coverage_prof_first_pass(CPOptions, CondGoal0, CondGoal,
            PortCountsCoverageAfterBefore,
            dp_coverage_goal_info(TrivialCond, PortCountsCoverageAfterCond)),

        coverage_prof_first_pass(CPOptions, ThenGoal0, ThenGoal,
            PortCountsCoverageAfterCond,
            dp_coverage_goal_info(TrivialThen, PortCountsCoverageAfterThen)),
        coverage_prof_first_pass(CPOptions, ElseGoal0, ElseGoal,
            PortCountsCoverageAfterCond,
            dp_coverage_goal_info(TrivialElse, PortCountsCoverageAfterElse)),

        GoalExpr = if_then_else(Vars, CondGoal, ThenGoal, ElseGoal),

        % An ITE is trivial iff all of its inner goals are trivial,

        goal_trivial_and(TrivialCond, TrivialThen, TrivialCondThen),
        goal_trivial_and(TrivialCondThen, TrivialElse, Trivial0),

        % And it has port counts iff it will end in a goal with a port count
        % regardless of which of the then and the else branch is taken.

        port_counts_give_coverage_after_and(PortCountsCoverageAfterThen,
            PortCountsCoverageAfterElse, PortCountsCoverageAfterDirect)
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
    ),

    (
        CPOptions ^ cpo_use_portcounts = yes,
        has_port_counts_after(Goal0, PortCountsCoverageAfterDirect,
            PortCountsCoverageAfterBefore, PortCountsCoverageAfter)
    ;
        CPOptions ^ cpo_use_portcounts = no,
        PortCountsCoverageAfter = no_port_counts_give_coverage_after
    ),

    (
        CPOptions ^ cpo_use_trivial = yes,
        Trivial = Trivial0
    ;
        CPOptions ^ cpo_use_trivial = no,
        Trivial = goal_is_nontrivial
    ),

    % Annotate the goal with this new information.
    Info = dp_coverage_goal_info(Trivial, PortCountsCoverageAfter),
    goal_info_get_maybe_dp_info(GoalInfo0) = MaybeDPInfo0,
    (
        MaybeDPInfo0 = yes(dp_goal_info(IsProfilingInstrumentation, _)),
        DPInfo = dp_goal_info(IsProfilingInstrumentation, yes(Info))
    ;
        MaybeDPInfo0 = no,
        unexpected($pred, "goal_dp_info not present")
    ),
    goal_info_set_maybe_dp_info(yes(DPInfo), GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

    % Combine information about goals within a conjunction
    %
:- pred coverage_prof_first_pass_conj(coverage_profiling_options::in,
    hlds_goal::in, hlds_goal::out, goal_trivial::in, goal_trivial::out,
    port_counts_give_coverage_after::in, port_counts_give_coverage_after::out)
    is det.

coverage_prof_first_pass_conj(CPOptions, Goal0, Goal, TrivialAcc, Trivial,
        PortCountsCoverageAfterAcc, PortCountsCoverageAfter) :-
    coverage_prof_first_pass(CPOptions, Goal0, Goal,
        PortCountsCoverageAfterAcc,
        dp_coverage_goal_info(TrivialGoal, PortCountsCoverageAfter)),
    goal_trivial_and(TrivialAcc, TrivialGoal, Trivial).

    % Combine information about goals within a disjunction.
    %
    % A portcount may be available to the goal executed when first entering a
    % disjunction. However it is impractical to determine if any disjuncts
    % other than the first are ever tried. So port counts at the beginning of
    % them are unknown.
    %
:- pred coverage_prof_first_pass_disj(coverage_profiling_options::in,
    list(hlds_goal)::in, list(hlds_goal)::out, goal_trivial::out,
    port_counts_give_coverage_after::in, port_counts_give_coverage_after::out)
    is det.

coverage_prof_first_pass_disj(_, [], [], goal_is_trivial,
        !PortCountsCoverageAfter).
coverage_prof_first_pass_disj(CPOptions, [Goal0 | Goals0], [Goal | Goals],
        Trivial, PortCountsCoverageBeforeDisjunct, PortCountsCoverageAfter) :-
    coverage_prof_first_pass(CPOptions, Goal0, Goal,
        PortCountsCoverageBeforeDisjunct,
        dp_coverage_goal_info(TrivialGoal, PortCountsCoverageAfterGoal)),
    coverage_prof_first_pass_disj(CPOptions, Goals0, Goals, TrivialDisj,
        no_port_counts_give_coverage_after, PortCountsCoverageAfterDisj),
    goal_trivial_and(TrivialGoal, TrivialDisj, Trivial),
    port_counts_give_coverage_after_and(PortCountsCoverageAfterGoal,
        PortCountsCoverageAfterDisj, PortCountsCoverageAfter).

    % A switch is a special type of disjunction. The important difference here
    % is that the coverage of the first case cannot be inferred from the
    % coverage before the switch.
    %
:- pred coverage_prof_first_pass_switchcase(coverage_profiling_options::in,
    list(case)::in, list(case)::out, goal_trivial::out,
    port_counts_give_coverage_after::out) is det.

coverage_prof_first_pass_switchcase(_, [], [],
        goal_is_trivial, port_counts_give_coverage_after).
coverage_prof_first_pass_switchcase(CPOptions,
        [Case0 | Cases0], [Case | Cases], Trivial, PortCountsCoverageAfter) :-
    Case0 = case(FirstFunctor, LaterFunctors, Goal0),

    coverage_prof_first_pass(CPOptions, Goal0, Goal,
        no_port_counts_give_coverage_after,
        dp_coverage_goal_info(TrivialGoal, PortCountsCoverageAfterGoal)),
    coverage_prof_first_pass_switchcase(CPOptions, Cases0, Cases,
        TrivialSwitchcase, PortCountsCoverageAfterSwitchcase),
    goal_trivial_and(TrivialGoal, TrivialSwitchcase, Trivial),
    port_counts_give_coverage_after_and(PortCountsCoverageAfterGoal,
        PortCountsCoverageAfterSwitchcase, PortCountsCoverageAfter),

    Case = case(FirstFunctor, LaterFunctors, Goal).

%-----------------------------------------------------------------------------%

    % Insert a coverage point before the given goal. This returns a flat
    % conjunction consisting of a coverage point followed by the goal.
    %
:- pred insert_coverage_point_before(coverage_profiling_options::in,
    coverage_point_info::in, hlds_goal::in, hlds_goal::out,
    proc_coverage_info::in, proc_coverage_info::out) is det.

insert_coverage_point_before(CPOptions, CPInfo, !Goal, !Info) :-
    make_coverage_point(CPOptions, CPInfo, CPGoals, !Info),
    ( if !.Goal = hlds_goal(conj(plain_conj, InnerGoals), _) then
        Goals = CPGoals ++ InnerGoals
    else
        Goals = CPGoals ++ [!.Goal]
    ),
    create_conj_from_list(Goals, plain_conj, !:Goal).

    % Builds a list of goals (that will form part of a conjunction)
    % for a coverage point.
    %
:- pred make_coverage_point(coverage_profiling_options::in,
    coverage_point_info::in, list(hlds_goal)::out,
    proc_coverage_info::in, proc_coverage_info::out) is det.

make_coverage_point(CPOptions, CoveragePointInfo, Goals, !CoverageInfo) :-
    CoveragePointInfos0 = !.CoverageInfo ^ ci_coverage_points,
    CPIndexCounter0 = !.CoverageInfo ^ ci_cp_index_counter,

    counter.allocate(CPIndex, CPIndexCounter0, CPIndexCounter),
    map.det_insert(CPIndex, CoveragePointInfo,
        CoveragePointInfos0, CoveragePointInfos),
    !CoverageInfo ^ ci_coverage_points := CoveragePointInfos,
    !CoverageInfo ^ ci_cp_index_counter := CPIndexCounter,

    % Build unifications for the coverage point index and the proc static.

    some [!VarTable] (
        !:VarTable = !.CoverageInfo ^ ci_var_table,

        generate_var_int("CPIndex", CPIndexVar, !VarTable),
        generate_deep_const_unify(some_int_const(int_const(CPIndex)),
            CPIndexVar, GoalUnifyIndex),
        % When using dynamic coverage profiling we really on this variable
        % being optimised away later.
        generate_var_c_ptr("ProcLayout", ProcLayoutVar, !VarTable),
        proc_static_cons_id(!.CoverageInfo, ProcStaticConsId),
        generate_deep_const_unify(ProcStaticConsId, ProcLayoutVar,
            GoalUnifyProcLayout),

        !CoverageInfo ^ ci_var_table := !.VarTable
    ),

    % Build a call to the instrumentation code.

    UseCalls = CPOptions ^ cpo_use_calls,
    ModuleInfo = !.CoverageInfo ^ ci_module_info,
    Ground = ground(shared, none_or_default_func),
    DataType = CPOptions ^ cpo_dynamic_coverage,
    FromToGround = from_to_mode(Ground, Ground),
    (
        DataType = dynamic_coverage_data,
        PredName = "increment_dynamic_coverage_point_count",
        ArgVars = [CPIndexVar],
        make_foreign_args(ArgVars,
            [foreign_arg_name_mode_box(
                yes(foreign_arg_name_mode("CPIndex", FromToGround)),
                bp_native_if_possible)],
            [int_type], ForeignArgVars),
        PredArity = 1
    ;
        DataType = static_coverage_data,
        PredName = "increment_static_coverage_point_count",
        ArgVars = [ProcLayoutVar, CPIndexVar],
        make_foreign_args(ArgVars,
            [foreign_arg_name_mode_box(
                yes(foreign_arg_name_mode("ProcLayout", FromToGround)),
                bp_native_if_possible),
            foreign_arg_name_mode_box(
                yes(foreign_arg_name_mode("CPIndex", FromToGround)),
                bp_native_if_possible)],
            [c_pointer_type, int_type], ForeignArgVars),
        PredArity = 2
    ),
    % Note: The body of increment_coverage_point_count includes several
    % assertions. If these are enabled, then bodily including the C code
    % at EVERY coverage point will cause significant code bloat. Generating
    % a call to a predicate with the same code in library/profiling_builtin.m
    % should then yield smaller code, and due to cache effects, it will
    % probably yield faster code as well.
    (
        UseCalls = no,
        get_deep_profile_builtin_ppid(ModuleInfo, PredName, PredArity,
            PredId, ProcId),
        coverage_point_ll_code(DataType, ForeignCallAttrs, ForeignProc),
        CallGoalExpr = call_foreign_proc(ForeignCallAttrs, PredId, ProcId,
            ForeignArgVars, [], no, ForeignProc),
        NonLocals = set_of_var.list_to_set(ArgVars),
        InstMapDelta = instmap_delta_from_assoc_list([]),
        CallGoalInfo = impure_init_goal_info(NonLocals, InstMapDelta,
            detism_det),
        CallGoal = hlds_goal(CallGoalExpr, CallGoalInfo)
    ;
        UseCalls = yes,
        generate_deep_call(ModuleInfo, PredName, PredArity, ArgVars,
            yes([]), detism_det, CallGoal)
    ),

    % Construct complete goal list.
    Goals = [GoalUnifyIndex, GoalUnifyProcLayout, CallGoal].

    % Turn a map of coverage points and their indexes into a list in sorted
    % order.
    %
:- pred coverage_points_map_list(map(int, coverage_point_info)::in,
    list(coverage_point_info)::out) is det.

coverage_points_map_list(Map, List) :-
    map.to_sorted_assoc_list(Map, AssocList),
    assoc_list.values(AssocList, List).

    % Retrieve the pred and proc ids from either the deep_maybe_rec_info or
    % deep_pred_proc_id fields of a deep_info structure.
    %
:- pred pred_proc_id(proc_coverage_info::in, pred_id::out, proc_id::out)
    is det.

pred_proc_id(CoverageInfo, PredId, ProcId) :-
    MaybeRecInfo = CoverageInfo ^ ci_maybe_rec_info,
    PredProcId = CoverageInfo ^ ci_pred_proc_id,
    ( if
        MaybeRecInfo = yes(RecInfo),
        RecInfo ^ dri_role = deep_prof_inner_proc(OuterPredProcId)
    then
        OuterPredProcId = proc(PredId, ProcId)
    else
        PredProcId = proc(PredId, ProcId)
    ).

    % Create a proc static cons_id from the deep recursion info.
    %
:- pred proc_static_cons_id(proc_coverage_info::in, cons_id::out) is det.

proc_static_cons_id(CoverageInfo, ProcStaticConsId) :-
    pred_proc_id(CoverageInfo, PredId, ProcId),
    ShroudedPredProcId = shroud_pred_proc_id(proc(PredId, ProcId)),
    ProcStaticConsId = deep_profiling_proc_layout(ShroudedPredProcId).

    % Returns a string containing the Low Level C code for a coverage point.
    %
:- pred coverage_point_ll_code(coverage_data_type::in,
    pragma_foreign_proc_attributes::out, pragma_foreign_proc_impl::out) is det.

coverage_point_ll_code(CoverageDataType, ForeignProcAttrs, ForeignProcImpl) :-
    some [!ForeignProcAttrs] (
        % XXX When running this code in a parallel grade, the contention for
        % the foreign code mutex may be very expensive. To improve this, we
        % should add a mechanism that, in par grades, allows us to replace
        % the general foreign code mutex with one that guards only the
        % coverage data structure we are updating, since that would yield
        % a LOT less contention.
        !:ForeignProcAttrs = default_attributes(lang_c),
        set_thread_safe(proc_not_thread_safe, !ForeignProcAttrs),
        set_may_call_mercury(proc_will_not_call_mercury, !ForeignProcAttrs),
        set_purity(purity_impure, !ForeignProcAttrs),
        set_terminates(proc_terminates, !ForeignProcAttrs),
        set_may_throw_exception(proc_will_not_throw_exception,
            !ForeignProcAttrs),
        ForeignProcAttrs = !.ForeignProcAttrs
    ),
    ForeignProcImpl = fp_impl_ordinary(Code, no),
    Code = coverage_point_ll_code(CoverageDataType).

:- func coverage_point_ll_code(coverage_data_type) = string.

coverage_point_ll_code(static_coverage_data) =
    % The code of this predicate is duplicated bodily in profiling_builtin.m
    % in the library directory, so any changes here should also be made there.
"
#ifdef MR_DEEP_PROFILING_COVERAGE_STATIC
    const MR_ProcLayout *pl;
    MR_ProcStatic       *ps;

    MR_enter_instrumentation();

  #ifdef MR_DEEP_PROFILING_LOWLEVEL_DEBUG
    if (MR_calldebug && MR_lld_print_enabled) {
        MR_print_deep_prof_vars(stdout, ""increment_coverage_point_count"");
        printf("", ProcLayout: 0x%x, CPIndex: %d\\n"", ProcLayout, CPIndex);
    }
  #endif

    pl = (const MR_ProcLayout *) ProcLayout;

    MR_deep_assert(NULL, NULL, NULL, pl != NULL);
    ps = pl->MR_sle_proc_static;
    MR_deep_assert(NULL, pl, NULL, ps != NULL);

    MR_deep_assert(NULL, pl, ps, CPIndex < ps->MR_ps_num_coverage_points);
    MR_deep_assert(NULL, pl, ps, ps->MR_ps_coverage_points != NULL);

    ps->MR_ps_coverage_points[CPIndex]++;

    MR_leave_instrumentation();
#else
    MR_fatal_error(
        ""increment_static_coverage_point_count:  ""
            ""static coverage profiling not enabled"");
#endif /* MR_DEEP_PROFILING_COVERAGE_STATIC */
".

coverage_point_ll_code(dynamic_coverage_data) =
    % The code of this predicate is duplicated bodily in profiling_builtin.m
    % in the library directory, so any changes here should also be made there.
"
#ifdef MR_DEEP_PROFILING_COVERAGE_DYNAMIC
    const MR_CallSiteDynamic *csd;
    const MR_ProcDynamic *pd;

    MR_enter_instrumentation();

  #ifdef MR_DEEP_PROFILING_LOWLEVEL_DEBUG
    if (MR_calldebug && MR_lld_print_enabled) {
        MR_print_deep_prof_vars(stdout, ""increment_coverage_point_count"");
        printf("", CallSiteDynamic: 0x%x, CPIndex: %d\\n"",
            MR_current_call_site_dynamic, CPIndex);
    }
  #endif

    csd = MR_current_call_site_dynamic;

    MR_deep_assert(NULL, NULL, NULL, csd != NULL);
    pd = csd->MR_csd_callee_ptr;

    MR_deep_assert(csd, NULL, NULL, pd != NULL);

#ifdef MR_DEEP_CHECKS
    /*
    ** Check that CPIndex is within bounds.
    */
    {
        const MR_ProcLayout *pl;
        const MR_ProcStatic *ps;

        pl = pd->MR_pd_proc_layout;
        MR_deep_assert(csd, NULL, NULL, pl != NULL);
        ps = pl->MR_sle_proc_static;
        MR_deep_assert(csd, pl, NULL, ps != NULL);
        MR_deep_assert(csd, pl, ps, CPIndex < ps->MR_ps_num_coverage_points);
    }
#endif

    MR_deep_assert(csd, NULL, NULL, pd->MR_pd_coverage_points != NULL);

    pd->MR_pd_coverage_points[CPIndex]++;

    MR_leave_instrumentation();
#else
    MR_fatal_error(
        ""increment_dynamic_coverage_point_count:  ""
            ""dynamic deep profiling not enabled"");
#endif /* MR_DEEP_PROFILING_COVERAGE_DYNAMIC */
".

%-----------------------------------------------------------------------------%

:- func goal_info_get_dp_info(hlds_goal_info) = dp_goal_info.

goal_info_get_dp_info(GoalInfo) = DPInfo :-
    MaybeDPInfo = goal_info_get_maybe_dp_info(GoalInfo),
    (
        MaybeDPInfo = yes(DPInfo)
    ;
        MaybeDPInfo = no,
        unexpected($pred, "MaybeDPInfo = no")
    ).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.coverage_profiling.
%-----------------------------------------------------------------------------%

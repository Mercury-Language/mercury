%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% The following program causes an abort in rotd-2008-09-19.
% Compile with:  mmc -O0 --deforestation -C bug85.m
%
% This testcase is derived from compiler/deep_profiling.m (+ a few other
% modules.)
%
%---------------------------------------------------------------------------%

:- module bug85.
:- interface.

:- import_module bool.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type coverage_after_known
    --->    coverage_after_known
    ;       coverage_after_unknown.

:- type proc_coverage_info
    --->    proc_coverage_info(
                ci_coverage_profiling_opts :: coverage_profiling_options
            ).

:- type coverage_profiling_options
    --->    coverage_profiling_options(
                cpo_coverage_after_goal :: bool
            ).

:- type determinism
    --->    detism_det
    ;       detism_semi.

:- type coverage_point_info
    --->    coverage_point_info(
                cp_type
            ).

:- type cp_type
    --->    cp_type_coverage_after
    ;       cp_type_branch_arm.

:- type hlds_goal
    --->    hlds_goal(
                hlds_goal_expr :: hlds_goal_expr,
                hlds_goal_info :: hlds_goal_info
            ).

:- type hlds_goal_expr
    --->    conj(conj_type, hlds_goals)
    ;       negation(hlds_goal)
    .

:- type hlds_goals  == list(hlds_goal).

:- type conj_type
    --->    plain_conj
    ;       parallel_conj.

:- type hlds_goal_info ---> hlds_goal_info.

:- type dp_goal_info
    --->    dp_goal_info(
                goal_is_mdprof_inst,
                maybe(dp_coverage_goal_info)
            ).

:- type goal_is_mdprof_inst
    --->    goal_is_mdprof_inst
    ;       goal_is_not_mdprof_inst.

:- type dp_coverage_goal_info
    --->    dp_coverage_goal_info(
                goal_trivial,
                port_counts_give_coverage_after
            ).

:- type goal_trivial
    --->    goal_is_trivial
    ;       goal_is_nontrivial.

:- type port_counts_give_coverage_after
    --->    port_counts_give_coverage_after
    ;       no_port_counts_give_coverage_after.

:- pred coverage_prof_second_pass_goal(dp_goal_info::in,
    hlds_goal::in, hlds_goal::out,
    coverage_after_known::in, coverage_after_known::out,
    proc_coverage_info::in, proc_coverage_info::out, bool::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

coverage_prof_second_pass_goal(DPInfo, Goal0, Goal,
        CoverageAfterKnown0, NextCoverageAfterKnown, !Info, AddedImpurity) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    Detism = GoalInfo0 ^ goal_info_get_determinism,
    CPOptions = !.Info ^ ci_coverage_profiling_opts,
    DPInfo = dp_goal_info(IsMDProfInst, MaybeDPCoverageInfo),
    (
        MaybeDPCoverageInfo =
            yes(dp_coverage_goal_info(GoalTrivial, GoalPortCountsCoverageAfter))
    ;
        MaybeDPCoverageInfo = no,
        GoalTrivial = goal_is_nontrivial,
        GoalPortCountsCoverageAfter = no_port_counts_give_coverage_after
    ),
    (
        GoalPortCountsCoverageAfter = port_counts_give_coverage_after,
        CoverageAfterKnown1 = coverage_after_known
    ;
        GoalPortCountsCoverageAfter = no_port_counts_give_coverage_after,
        CoverageAfterKnown1 = CoverageAfterKnown0
    ),
    ( if
        (
            IsMDProfInst = goal_is_mdprof_inst
        ;
            CoverageAfterKnown1 = coverage_after_known
        ;
            GoalTrivial = goal_is_trivial
        ;
            GoalExpr0 = conj(plain_conj, _)
        )
    then
        MaybeCPType = no
    else
        CoverageAfterGoals = CPOptions ^ cpo_coverage_after_goal,
        (
            CoverageAfterGoals = yes,
            MaybeCPType = yes(cp_type_coverage_after)
        ;
            CoverageAfterGoals = no,
            MaybeCPType = no
        )
    ),
    (
        MaybeCPType = yes(_),
        CoverageAfterKnown2 = coverage_after_known
    ;
        MaybeCPType = no,
        CoverageAfterKnown2 = CoverageAfterKnown1
    ),
    (
        GoalPortCountsCoverageAfter = port_counts_give_coverage_after,
        CoverageAfterKnown = coverage_after_known
    ;
        GoalPortCountsCoverageAfter = no_port_counts_give_coverage_after,
        (
            Detism = detism_semi,
            CoverageAfterKnown = coverage_after_unknown
        ;
            Detism = detism_det,
            CoverageAfterKnown = CoverageAfterKnown2
        )
    ),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        coverage_prof_second_pass_conj(ConjType, Goals0, Goals,
            CoverageAfterKnown, NextCoverageAfterKnown, !Info,
            AddedImpurityInner),
        GoalExpr1 = conj(ConjType, Goals)
    ;
        GoalExpr0 = negation(NegGoal0),
        coverage_prof_second_pass_goal(DPInfo, NegGoal0, NegGoal,
            coverage_after_unknown, NextCoverageAfterKnown, !Info,
            AddedImpurityInner),
        GoalExpr1 = negation(NegGoal)
    ),
    Goal = hlds_goal(GoalExpr1, GoalInfo0),
    AddedImpurity = AddedImpurityInner.

:- pred coverage_prof_second_pass_conj(conj_type::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    coverage_after_known::in, coverage_after_known::out,
    proc_coverage_info::in, proc_coverage_info::out, bool::out) is det.

coverage_prof_second_pass_conj(_, !Goal, !CK, !PCI, yes).

:- func goal_info_get_determinism(hlds_goal_info) = determinism.

goal_info_get_determinism(_) = detism_det.

%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Compiling the following with rotd-2005-05-23 with
% -O0 --common-struct --local-constraint-propagation
% causes the following assertion failure:
%
%   Uncaught Mercury exception:
%   Software Error: nondet model in det/semidet context
%
% NOTE: this is a regression, it didn't occur with rotd-2005-05-17 and before.

:- module constraint_prop_bug.

:- interface.

:- import_module list.
:- import_module pair.

:- type hlds_goal == pair(hlds_goal_expr, hlds_goal_info).

:- type hlds_goal_info ---> hlds_goal_info.
:- type hlds_goal_expr ---> hlds_goal_expr.
:- type proc_info ---> proc_info.

:- pred copy_clauses_to_proc(list(hlds_goal)::in, proc_info::out) is det.

:- implementation.

:- type purity
    --->    pure
    ;       not_pure.

copy_clauses_to_proc(GoalList, Proc) :-
    GoalInfo0 = hlds_goal_info,
    ( if
        list__member(_ - SubGoalInfo, GoalList),
        not goal_info_is_pure(SubGoalInfo)
    then
        list__map(get_purity, GoalList, _PurityList),
        GoalInfo = GoalInfo0
    else
        GoalInfo = GoalInfo0
    ),
    Goal = hlds_goal_expr - GoalInfo,
    proc_info_set_body(Goal, Proc).

:- pred proc_info_set_body(hlds_goal::in, proc_info::out)  is det.

proc_info_set_body(_, proc_info).

:- pred get_purity(hlds_goal::in, purity::out) is det.

get_purity(_, pure).

:- pred goal_info_is_pure(hlds_goal_info::in) is semidet.

goal_info_is_pure(hlds_goal_info) :- semidet_fail.

:- func worst_purity(purity, purity) = purity.

worst_purity(_, _) = pure.

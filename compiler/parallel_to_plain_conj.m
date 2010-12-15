%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008, 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: parallel_to_plain_conj.m.
%
% This module converts parallel conjunction to plain conjunctions,
% for use in grades that do not allow parallelism.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.parallel_to_plain_conj.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

%-----------------------------------------------------------------------------%

    % Transform all parallel conjunctions in the procedure into sequential
    % conjunctions.
    %
:- pred parallel_to_plain_conjs(module_info::in, proc_info::in, proc_info::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_goal.

:- import_module bool.
:- import_module list.
:- import_module require.

%-----------------------------------------------------------------------------%

parallel_to_plain_conjs(_ModuleInfo, !ProcInfo) :-
    proc_info_get_has_parallel_conj(!.ProcInfo, HasParallelConj),
    (
        HasParallelConj = no
    ;
        HasParallelConj = yes,
        proc_info_get_goal(!.ProcInfo, Goal0),
        parallel_to_plain_conjs_goal(Goal0, Goal),
        proc_info_set_goal(Goal, !ProcInfo)
    ).

:- pred parallel_to_plain_conjs_goal(hlds_goal::in, hlds_goal::out) is det.

parallel_to_plain_conjs_goal(Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(_ConjType, Goals0),
        parallel_to_plain_conjs_goals(Goals0, Goals),
        GoalExpr = conj(plain_conj, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        parallel_to_plain_conjs_goals(Goals0, Goals),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        parallel_to_plain_conjs_cases(Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = if_then_else(QuantVars, Cond0, Then0, Else0),
        parallel_to_plain_conjs_goal(Cond0, Cond),
        parallel_to_plain_conjs_goal(Then0, Then),
        parallel_to_plain_conjs_goal(Else0, Else),
        GoalExpr = if_then_else(QuantVars, Cond, Then, Else)
    ;
        GoalExpr0 = negation(SubGoal0),
        parallel_to_plain_conjs_goal(SubGoal0, SubGoal),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( Reason = from_ground_term(_, from_ground_term_construct) ->
            % There cannot be parallel conjunctions inside these scopes.
            SubGoal = SubGoal0
        ;
            parallel_to_plain_conjs_goal(SubGoal0, SubGoal)
        ),
        GoalExpr = scope(Reason, SubGoal)
    ;
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected(this_file, "shorthand")
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo0).

:- pred parallel_to_plain_conjs_goals(
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

parallel_to_plain_conjs_goals([], []).
parallel_to_plain_conjs_goals([Goal0 | Goals0], [Goal | Goals]) :-
    parallel_to_plain_conjs_goal(Goal0, Goal),
    parallel_to_plain_conjs_goals(Goals0, Goals).

:- pred parallel_to_plain_conjs_cases(list(case)::in, list(case)::out) is det.

parallel_to_plain_conjs_cases([], []).
parallel_to_plain_conjs_cases([Case0 | Cases0], [Case | Cases]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    parallel_to_plain_conjs_goal(Goal0, Goal),
    Case = case(MainConsId, OtherConsIds, Goal),
    parallel_to_plain_conjs_cases(Cases0, Cases).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "parallel_to_plain_conj.m".

%-----------------------------------------------------------------------------%
:- end_module parallel_to_plain_conj.
%-----------------------------------------------------------------------------%

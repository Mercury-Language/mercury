%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mark_static_terms.m.
% Main author: fjh.
%
% This module traverses the HLDS, updating the `how_to_construct' field of
% construction unifications. For each construction which can be done
% statically, i.e. whose arguments are all static, it replaces this field with
% `construct_statically'. The main use of information is in the MLDS back-end,
% to determine when we can generate static initialized constants instead of
% calling new_object(). However, other parts of the compiler also use this
% information.
%
%-----------------------------------------------------------------------------%

:- module hlds.mark_static_terms.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

%-----------------------------------------------------------------------------%


:- pred mark_static_terms(module_info::in, proc_info::in, proc_info::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_goal.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module require.
:- import_module set_tree234.

%-----------------------------------------------------------------------------%

    % As we traverse the goal, we keep track of which variables are static at
    % the current program point.
    %
:- type static_info == set_tree234(prog_var).

mark_static_terms(_ModuleInfo, !Proc) :-
    % The ModuleInfo argument is there just for passes_aux.
    proc_info_get_goal(!.Proc, Goal0),
    StaticInfo0 = set_tree234.init,
    goal_mark_static_terms(Goal0, Goal, StaticInfo0, _StaticInfo),
    proc_info_set_goal(Goal, !Proc).

:- pred goal_mark_static_terms(hlds_goal::in, hlds_goal::out,
    static_info::in, static_info::out) is det.

goal_mark_static_terms(Goal0, Goal, !SI) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        % It's OK to treat parallel conjunctions as if they were sequential
        % here, since if we mark any variables as static, the computation
        % of those variables will be done at compile time.
        conj_mark_static_terms(Goals0, Goals, !SI),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        % We revert to the original static_info at the end of branched goals.
        disj_mark_static_terms(Goals0, Goals, !.SI),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        % We revert to the original static_info at the end of branched goals.
        cases_mark_static_terms(Cases0, Cases, !.SI),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = negation(SubGoal0),
        % We revert to the original static_info at the end of the negation.
        goal_mark_static_terms(SubGoal0, SubGoal, !.SI, _SI),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(TermVar, from_ground_term_construct)
        then
            % These scopes already have all their unifications marked
            % as construct_statically.
            set_tree234.insert(TermVar, !SI),
            GoalExpr = GoalExpr0
        else
            goal_mark_static_terms(SubGoal0, SubGoal, !SI),
            GoalExpr = scope(Reason, SubGoal)
        )
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        SI0 = !.SI,
        % We run the Cond and the Then in sequence, and we run the Else
        % in parallel with that, and then we throw away the static_infos
        % we computed and revert to the original static_info at the end,
        % since this was a branched goal.
        goal_mark_static_terms(Cond0, Cond, SI0, SI_Cond),
        goal_mark_static_terms(Then0, Then, SI_Cond, _SI_Then),
        goal_mark_static_terms(Else0, Else, SI0, _SI_Else),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = unify(LHS, RHS, Mode, Unification0, Context),
        unification_mark_static_terms(Unification0, Unification, !SI),
        GoalExpr = unify(LHS, RHS, Mode, Unification, Context)
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

:- pred conj_mark_static_terms(hlds_goals::in, hlds_goals::out,
    static_info::in, static_info::out) is det.

conj_mark_static_terms(Goals0, Goals, !SI) :-
    list.map_foldl(goal_mark_static_terms, Goals0, Goals, !SI).

:- pred disj_mark_static_terms(hlds_goals::in, hlds_goals::out,
    static_info::in) is det.

disj_mark_static_terms([], [], _).
disj_mark_static_terms([Goal0 | Goals0], [Goal | Goals], SI0) :-
    % We throw away the static_info obtained after each branch.
    goal_mark_static_terms(Goal0, Goal, SI0, _SI),
    disj_mark_static_terms(Goals0, Goals, SI0).

:- pred cases_mark_static_terms(list(case)::in, list(case)::out,
    static_info::in) is det.

cases_mark_static_terms([], [], _SI0).
cases_mark_static_terms([Case0 | Cases0], [Case | Cases], SI0) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    % We throw away the static_info obtained after each branch.
    goal_mark_static_terms(Goal0, Goal, SI0, _SI),
    Case = case(MainConsId, OtherConsIds, Goal),
    cases_mark_static_terms(Cases0, Cases, SI0).

:- pred unification_mark_static_terms(unification::in, unification::out,
    static_info::in, static_info::out) is det.

unification_mark_static_terms(Unification0, Unification, !StaticVars) :-
    (
        Unification0 = construct(Var, ConsId, ArgVars, ArgModes,
            HowToConstruct0, Unique, SubInfo),
        % If all the arguments are static, then the newly constructed variable
        % is static too.
        ( if list.all_true(set_tree234.contains(!.StaticVars), ArgVars) then
            HowToConstruct = construct_statically,
            set_tree234.insert(Var, !StaticVars),
            % This is a minor optimization to improve the efficiency of the
            % compiler: don't bother allocating memory if we don't need to.
            ( if HowToConstruct = HowToConstruct0 then
                Unification = Unification0
            else
                Unification = construct(Var, ConsId, ArgVars, ArgModes,
                    HowToConstruct, Unique, SubInfo)
            )
        else
            Unification = Unification0
        )
    ;
        Unification0 = deconstruct(_Var, _ConsId, _ArgVars, _UniModes,
            _CanFail, _CanCGC),
        Unification = Unification0
%       ( if
%           % if the variable being deconstructed is static,
%           % and the deconstruction cannot fail,
%           % then the newly extracted argument variables
%           % are static too
%           % (XXX is the "cannot fail" bit really necessary?)
%           map.search(StaticVars0, Var, Data),
%           CanFail = cannot_fail
%       then
%           XXX insert ArgVars into StaticVars0
%       else
%           true
%       )
    ;
        Unification0 = assign(TargetVar, SourceVar),
        Unification = Unification0,
        % If the variable being assigned from is static, then the variable
        % being assigned to is static too.
        ( if set_tree234.contains(!.StaticVars, SourceVar) then
            set_tree234.insert(TargetVar, !StaticVars)
        else
            true
        )
    ;
        ( Unification0 = simple_test(_, _)
        ; Unification0 = complicated_unify(_, _, _)
        ),
        Unification = Unification0
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.mark_static_terms.
%-----------------------------------------------------------------------------%

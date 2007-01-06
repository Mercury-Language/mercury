%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: follow_code.m.
% Main author: conway.
% Extensive modifications by zs.
% 
% The problem attacked by this module is that sometimes the code generator
% doesn't know where it should put the values of live variables at the end
% of a branched control structure. All branches must put each live variable
% into the same lval, so having each branch leave each live variable where it
% just happens to be is not an option. We currently just put all live variables
% into its own rN register or stack slot, but often is not where the variable
% happens to be at the end of any branch, nor is it where the variable is next
% needed.
%
% The idea used by this module to attack this problem is to try to ensure
% that the branched control structure is followed immediately either by a call
% or by the end of the procedure body, because both have clear rules about
% where every live variable must be. If a branched control structure is
% followed by builtin goals such as unifications, we push those goals into
% each branch.
% 
%-----------------------------------------------------------------------------%

:- module ll_backend.follow_code.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred move_follow_code_in_proc(pred_id::in, proc_id::in, pred_info::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

    % Split a list of goals into the prefix of builtins and the rest.
    %
:- pred move_follow_code_select(list(hlds_goal)::in, list(hlds_goal)::out,
    list(hlds_goal)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module hlds.goal_util.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module pair.

%-----------------------------------------------------------------------------%

move_follow_code_in_proc(_PredId, _ProcId, _PredInfo, !ProcInfo,
        !ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, follow_code, FollowCode),
    globals.lookup_bool_option(Globals, prev_code, PrevCode),
    Flags = FollowCode - PrevCode,
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_varset(!.ProcInfo, Varset0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
    (
        move_follow_code_in_goal(Goal0, Goal1, Flags, no, Res),
        % Did the goal change?
        Res = yes
    ->
        % We need to fix up the goal_info by recalculating the nonlocal vars
        % and the non-atomic instmap deltas.
        proc_info_get_headvars(!.ProcInfo, HeadVars),
        implicitly_quantify_clause_body(HeadVars, _Warnings, Goal1, Goal2,
            Varset0, Varset, VarTypes0, VarTypes, RttiVarMaps0, RttiVarMaps),
        proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InstMap0),
        proc_info_get_inst_varset(!.ProcInfo, InstVarSet),
        recompute_instmap_delta(no, Goal2, Goal, VarTypes, InstVarSet,
            InstMap0, !ModuleInfo)
    ;
        Goal = Goal0,
        Varset = Varset0,
        VarTypes = VarTypes0,
        RttiVarMaps = RttiVarMaps0
    ),
    proc_info_set_goal(Goal, !ProcInfo),
    proc_info_set_varset(Varset, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred move_follow_code_in_goal(hlds_goal::in, hlds_goal::out, pair(bool)::in,
    bool::in, bool::out) is det.

move_follow_code_in_goal(hlds_goal(GoalExpr0, GoalInfo),
        hlds_goal(GoalExpr, GoalInfo), Flags, !R) :-
    move_follow_code_in_goal_2(GoalExpr0, GoalExpr, Flags, !R).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_in_goal_2(hlds_goal_expr::in, hlds_goal_expr::out,
    pair(bool)::in, bool::in, bool::out) is det.

move_follow_code_in_goal_2(conj(ConjType, Goals0), conj(ConjType, Goals),
        Flags, !R) :-
    (
        ConjType = plain_conj,
        move_follow_code_in_conj(Goals0, Goals, Flags, !R)
    ;
        ConjType = parallel_conj,
        move_follow_code_in_independent_goals(Goals0, Goals, Flags, !R)
    ).
move_follow_code_in_goal_2(disj(Goals0), disj(Goals), Flags, !R) :-
    move_follow_code_in_independent_goals(Goals0, Goals, Flags, !R).
move_follow_code_in_goal_2(negation(Goal0), negation(Goal), Flags, !R) :-
    move_follow_code_in_goal(Goal0, Goal, Flags, !R).
move_follow_code_in_goal_2(switch(Var, Det, Cases0),
        switch(Var, Det, Cases), Flags, !R) :-
    move_follow_code_in_cases(Cases0, Cases, Flags, !R).
move_follow_code_in_goal_2(if_then_else(Vars, Cond0, Then0, Else0),
        if_then_else(Vars, Cond, Then, Else), Flags, !R) :-
    move_follow_code_in_goal(Cond0, Cond, Flags, !R),
    move_follow_code_in_goal(Then0, Then, Flags, !R),
    move_follow_code_in_goal(Else0, Else, Flags, !R).
move_follow_code_in_goal_2(scope(Remove, Goal0), scope(Remove, Goal),
        Flags, !R) :-
    move_follow_code_in_goal(Goal0, Goal, Flags, !R).
move_follow_code_in_goal_2(Goal @ generic_call(_, _, _, _), Goal, _, !R).
move_follow_code_in_goal_2(Goal @ plain_call(_, _, _, _, _, _), Goal, _, !R).
move_follow_code_in_goal_2(Goal @ unify(_, _, _, _, _), Goal, _, !R).
move_follow_code_in_goal_2(Goal @ call_foreign_proc(_, _, _, _, _, _, _), Goal,
        _, !R).
move_follow_code_in_goal_2(shorthand(_), _, _, _, _) :-
    % These should have been expanded out by now.
    unexpected(this_file, "move_follow_code_in_goal_2: unexpected shorthand").

%-----------------------------------------------------------------------------%

    % move_follow_code_in_independent_goals is used both for disjunction and
    % parallel conjunction.
    %
:- pred move_follow_code_in_independent_goals(list(hlds_goal)::in,
    list(hlds_goal)::out, pair(bool)::in, bool::in, bool::out) is det.

move_follow_code_in_independent_goals([], [], _, !R).
move_follow_code_in_independent_goals([Goal0|Goals0], [Goal|Goals], Flags,
        !R) :-
    move_follow_code_in_goal(Goal0, Goal, Flags, !R),
    move_follow_code_in_independent_goals(Goals0, Goals, Flags, !R).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_in_cases(list(case)::in, list(case)::out,
    pair(bool)::in, bool::in, bool::out) is det.

move_follow_code_in_cases([], [], _, !R).
move_follow_code_in_cases([case(Cons, Goal0) | Goals0],
        [case(Cons, Goal) | Goals], Flags, !R) :-
    move_follow_code_in_goal(Goal0, Goal, Flags, !R),
    move_follow_code_in_cases(Goals0, Goals, Flags, !R).

%-----------------------------------------------------------------------------%

    % Find the first branched structure, and split the conj into those goals
    % before and after it.
    %
:- pred move_follow_code_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    pair(bool)::in, bool::in, bool::out) is det.

move_follow_code_in_conj(Goals0, Goals, Flags, !R) :-
    move_follow_code_in_conj_2(Goals0, [], RevGoals, Flags, !R),
    list.reverse(RevGoals, Goals).

:- pred move_follow_code_in_conj_2(list(hlds_goal)::in, list(hlds_goal)::in,
    list(hlds_goal)::out, pair(bool)::in, bool::in, bool::out) is det.

move_follow_code_in_conj_2([], !RevPrevGoals, _, !R).
move_follow_code_in_conj_2([Goal0 | Goals0], !RevPrevGoals, Flags, !R) :-
    Flags = PushFollowCode - _PushPrevCode,
    (
        PushFollowCode = yes,
        Goal0 = hlds_goal(GoalExpr0, _),
        goal_util.goal_is_branched(GoalExpr0),
        move_follow_code_select(Goals0, FollowGoals, RestGoalsPrime),
        FollowGoals = [_ | _],
        move_follow_code_move_goals(Goal0, FollowGoals, Goal1Prime)
    ->
        !:R = yes,
        Goal1 = Goal1Prime,
        RestGoals = RestGoalsPrime
    ;
        Goal1 = Goal0,
        RestGoals = Goals0
    ),
    move_follow_code_in_goal(Goal1, Goal, Flags, !R),
    !:RevPrevGoals = [Goal | !.RevPrevGoals],
    move_follow_code_in_conj_2(RestGoals, !RevPrevGoals, Flags, !R).

%-----------------------------------------------------------------------------%

move_follow_code_select([], [], []).
move_follow_code_select([Goal | Goals], FollowGoals, RestGoals) :-
    ( move_follow_code_is_builtin(Goal) ->
        move_follow_code_select(Goals, FollowGoals0, RestGoals),
        FollowGoals = [Goal | FollowGoals0]
    ;
        FollowGoals = [],
        RestGoals = [Goal | Goals]
    ).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_move_goals(hlds_goal::in, list(hlds_goal)::in,
    hlds_goal::out) is semidet.

move_follow_code_move_goals(hlds_goal(GoalExpr0, GoalInfo), FollowGoals,
        hlds_goal(GoalExpr, GoalInfo)) :-
    (
        GoalExpr0 = switch(Var, Det, Cases0),
        move_follow_code_move_goals_cases(Cases0, FollowGoals, Cases),
        GoalExpr = switch(Var, Det, Cases)
    ;
        GoalExpr0 = disj(Goals0),
        move_follow_code_move_goals_disj(Goals0, FollowGoals, Goals),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = if_then_else(Vars, Cond, Then0, Else0),
        follow_code_conjoin_goal_and_goal_list(Then0, FollowGoals, Then),
        follow_code_conjoin_goal_and_goal_list(Else0, FollowGoals, Else),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_move_goals_cases(list(case)::in, list(hlds_goal)::in,
    list(case)::out) is semidet.

move_follow_code_move_goals_cases([], _FollowGoals, []).
move_follow_code_move_goals_cases([Case0|Cases0], FollowGoals, [Case|Cases]) :-
    Case0 = case(Cons, Goal0),
    follow_code_conjoin_goal_and_goal_list(Goal0, FollowGoals, Goal),
    Case = case(Cons, Goal),
    move_follow_code_move_goals_cases(Cases0, FollowGoals, Cases).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_move_goals_disj(list(hlds_goal)::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is semidet.

move_follow_code_move_goals_disj([], _FollowGoals, []).
move_follow_code_move_goals_disj([Goal0|Goals0], FollowGoals, [Goal|Goals]) :-
    follow_code_conjoin_goal_and_goal_list(Goal0, FollowGoals, Goal),
    move_follow_code_move_goals_disj(Goals0, FollowGoals, Goals).

%-----------------------------------------------------------------------------%

    % Takes a goal and a list of goals, and conjoins them (with a potentially
    % blank goal_info), checking that the determinism of the goal is not
    % changed.
    %
:- pred follow_code_conjoin_goal_and_goal_list(hlds_goal::in,
    list(hlds_goal)::in, hlds_goal::out) is semidet.

follow_code_conjoin_goal_and_goal_list(Goal0, FollowGoals, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    goal_info_get_determinism(GoalInfo0, Detism0),
    determinism_components(Detism0, _CanFail0, MaxSolns0),
    ( MaxSolns0 = at_most_zero ->
        Goal = Goal0
    ;
        check_follow_code_detism(FollowGoals, Detism0),
        ( GoalExpr0 = conj(plain_conj, GoalList0) ->
            list.append(GoalList0, FollowGoals, GoalList),
            GoalExpr = conj(plain_conj, GoalList)
        ;
            GoalExpr = conj(plain_conj, [Goal0 | FollowGoals])
        ),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ).

    % This check is necessary to make sure that follow_code doesn't change
    % the determinism of the goal.
    %
:- pred check_follow_code_detism(list(hlds_goal)::in, determinism::in)
    is semidet.

check_follow_code_detism([], _).
check_follow_code_detism([hlds_goal(_, GoalInfo) | Goals], Detism0) :-
    goal_info_get_determinism(GoalInfo, Detism1),
    det_conjunction_detism(Detism0, Detism1, Detism0),
    check_follow_code_detism(Goals, Detism0).

%-----------------------------------------------------------------------------%

:- pred move_follow_code_is_builtin(hlds_goal::in) is semidet.

move_follow_code_is_builtin(hlds_goal(GoalExpr, _)) :-
    (
        GoalExpr = unify(_, _, _, Unification, _),
        Unification \= complicated_unify(_, _, _)
    ;
        GoalExpr = plain_call(_, _, _, inline_builtin, _, _)
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "follow_code.m".

%-----------------------------------------------------------------------------%
:- end_module follow_code.
%-----------------------------------------------------------------------------%

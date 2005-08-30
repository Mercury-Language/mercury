%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main authors: conway, zs.

% This module traverses the goal for every procedure, filling in the
% follow_vars fields of some goals. These fields constitute an advisory
% indication to the code generator as to what location each variable
% should be placed in.
%
% The desired locations of variables are computed by traversing the goal
% BACKWARDS. At the end of the procedure, we want the output variables
% to go into their corresponding registers, so we initialize the follow_vars
% accordingly. At each call or higher order call we reset the follow_vars set
% to reflect where variables should be to make the setting up of the arguments
% of the call as efficient as possible.

% See compiler/notes/allocation.html for a description of the framework that
% this pass operates within, and for a description of which goals have their
% follow_vars field filled in.

%-----------------------------------------------------------------------------%

:- module ll_backend__follow_vars.

:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_llds.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.

:- pred find_final_follow_vars(proc_info::in, abs_follow_vars_map::out,
    int::out) is det.

:- pred find_follow_vars_in_goal(hlds_goal::in, hlds_goal::out,
    vartypes::in, module_info::in,
    abs_follow_vars_map::in, abs_follow_vars_map::out,
    int::in, int::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__mode_util.
:- import_module hlds__arg_info.
:- import_module hlds__code_model.
:- import_module hlds__hlds_data.
:- import_module hlds__quantification.
:- import_module libs__globals.
:- import_module ll_backend__call_gen.
:- import_module ll_backend__code_util.
:- import_module ll_backend__llds.
:- import_module parse_tree__prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module svmap.

%-----------------------------------------------------------------------------%

find_final_follow_vars(ProcInfo, FollowVarsMap, NextNonReserved) :-
    proc_info_arg_info(ProcInfo, ArgInfo),
    proc_info_headvars(ProcInfo, HeadVars),
    assoc_list__from_corresponding_lists(ArgInfo, HeadVars, ArgInfoHeadVars),
    map__init(FollowVarsMap0),
    find_final_follow_vars_2(ArgInfoHeadVars,
        FollowVarsMap0, FollowVarsMap, 1, NextNonReserved).

:- pred find_final_follow_vars_2(assoc_list(arg_info, prog_var)::in,
    abs_follow_vars_map::in, abs_follow_vars_map::out, int::in, int::out)
    is det.

find_final_follow_vars_2([], !FollowMap, !NextNonReserved).
find_final_follow_vars_2([arg_info(RegNum, Mode) - Var | ArgInfoVars],
        !FollowVarsMap, !NextNonReserved) :-
    ( Mode = top_out ->
        Locn = abs_reg(RegNum),
        svmap__det_insert(Var, Locn, !FollowVarsMap),
        int__max(RegNum + 1, !NextNonReserved)
    ;
        true
    ),
    find_final_follow_vars_2(ArgInfoVars, !FollowVarsMap, !NextNonReserved).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

find_follow_vars_in_goal(Goal0 - GoalInfo0, Goal - GoalInfo,
        VarTypes, ModuleInfo, !FollowVarsMap, !NextNonReserved) :-
    find_follow_vars_in_goal_expr(Goal0, Goal, GoalInfo0, GoalInfo,
        VarTypes, ModuleInfo, !FollowVarsMap, !NextNonReserved).

%-----------------------------------------------------------------------------%

:- pred find_follow_vars_in_goal_expr(hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out, vartypes::in, module_info::in,
    abs_follow_vars_map::in, abs_follow_vars_map::out,
    int::in, int::out) is det.

find_follow_vars_in_goal_expr(conj(Goals0), conj(Goals), GoalInfo, GoalInfo,
        VarTypes, ModuleInfo, !FollowVarsMap, !NextNonReserved) :-
    find_follow_vars_in_conj(Goals0, Goals, VarTypes, ModuleInfo,
        no, !FollowVarsMap, !NextNonReserved).

find_follow_vars_in_goal_expr(par_conj(Goals0), par_conj(Goals),
        GoalInfo, GoalInfo, VarTypes, ModuleInfo,
        !FollowVarsMap, !NextNonReserved) :-
        % find_follow_vars_in_disj treats its list of goals as a
        % series of independent goals, so we can use it to process
        % independent parallel conjunction.
    find_follow_vars_in_disj(Goals0, Goals, VarTypes, ModuleInfo,
        !FollowVarsMap, !NextNonReserved).

    % We record that at the end of each disjunct, live variables should
    % be in the locations given by the initial follow_vars, which reflects
    % the requirements of the code following the disjunction.

find_follow_vars_in_goal_expr(disj(Goals0), disj(Goals), GoalInfo0, GoalInfo,
        VarTypes, ModuleInfo, !FollowVarsMap, !NextNonReserved) :-
    goal_info_set_store_map(!.FollowVarsMap, GoalInfo0, GoalInfo),
    find_follow_vars_in_disj(Goals0, Goals, VarTypes, ModuleInfo,
        !FollowVarsMap, !NextNonReserved).

find_follow_vars_in_goal_expr(not(Goal0), not(Goal), GoalInfo, GoalInfo,
        VarTypes, ModuleInfo, !FollowVarsMap, !NextNonReserved) :-
    find_follow_vars_in_goal(Goal0, Goal, VarTypes, ModuleInfo,
        !FollowVarsMap, !NextNonReserved).

    % We record that at the end of each arm of the switch, live variables
    % should be in the locations given by the initial follow_vars, which
    % reflects the requirements of the code following the switch.

find_follow_vars_in_goal_expr(switch(Var, Det, Cases0),
        switch(Var, Det, Cases), GoalInfo0, GoalInfo,
        VarTypes, ModuleInfo,
        !FollowVarsMap, !NextNonReserved) :-
    goal_info_set_store_map(!.FollowVarsMap, GoalInfo0, GoalInfo),
    find_follow_vars_in_cases(Cases0, Cases, VarTypes, ModuleInfo,
        !FollowVarsMap, !NextNonReserved).

    % Set the follow_vars field for the condition, the then-part and the
    % else-part, since in general they have requirements about where
    % variables should be.

    % We use the requirement of the condition as the requirement of
    % the if-then-else itself, since the condition will definitely
    % be entered first. Since part of the condition may fail early,
    % taking into account the preferences of the else part may be
    % worthwhile. The preferences of the then part are already taken
    % into account, since they are an input to the computation of
    % the follow_vars for the condition.

    % We record that at the end of both the then-part and the else-part,
    % live variables should be in the locations given by the initial
    % follow_vars, which reflects the requirements of the code
    % following the if-then-else.

find_follow_vars_in_goal_expr(
        if_then_else(Vars, Cond0, Then0, Else0),
        if_then_else(Vars, Cond, Then, Else),
        GoalInfo0, GoalInfo, VarTypes, ModuleInfo,
        FollowVarsMap0, FollowVarsMapCond,
        NextNonReserved0, NextNonReservedCond) :-
    find_follow_vars_in_goal(Then0, Then1, VarTypes, ModuleInfo,
        FollowVarsMap0, FollowVarsMapThen,
        NextNonReserved0, NextNonReservedThen),
    FollowVarsThen =
        abs_follow_vars(FollowVarsMapThen, NextNonReservedThen),
    goal_set_follow_vars(yes(FollowVarsThen), Then1, Then),

    find_follow_vars_in_goal(Cond0, Cond1, VarTypes, ModuleInfo,
        FollowVarsMapThen, FollowVarsMapCond,
        NextNonReservedThen, NextNonReservedCond),
    FollowVarsCond =
        abs_follow_vars(FollowVarsMapCond, NextNonReservedCond),
    goal_set_follow_vars(yes(FollowVarsCond), Cond1, Cond),

    find_follow_vars_in_goal(Else0, Else1, VarTypes, ModuleInfo,
        FollowVarsMap0, FollowVarsMapElse,
        NextNonReserved0, NextNonReservedElse),
    FollowVarsElse =
        abs_follow_vars(FollowVarsMapElse, NextNonReservedElse),
    goal_set_follow_vars(yes(FollowVarsElse), Else1, Else),

    goal_info_set_store_map(FollowVarsMap0, GoalInfo0, GoalInfo).

find_follow_vars_in_goal_expr(scope(Reason, Goal0), scope(Reason, Goal),
        GoalInfo, GoalInfo, VarTypes, ModuleInfo,
        !FollowVarsMap, !NextNonReserved) :-
    find_follow_vars_in_goal(Goal0, Goal, VarTypes, ModuleInfo,
        !FollowVarsMap, !NextNonReserved).

find_follow_vars_in_goal_expr(Goal @ unify(_, _, _, Unify, _), Goal,
        GoalInfo, GoalInfo, _VarTypes, _ModuleInfo,
        !FollowVarsMap, !NextNonReserved) :-
    (
        Unify = assign(LVar, RVar),
        map__search(!.FollowVarsMap, LVar, DesiredLoc)
    ->
        svmap__set(RVar, DesiredLoc, !FollowVarsMap)
    ;
        true
    ).

find_follow_vars_in_goal_expr(Goal @ foreign_proc(_, _, _, _, _, _), Goal,
        GoalInfo, GoalInfo, _, _, !FollowVarsMap, !NextNonReserved).

find_follow_vars_in_goal_expr(shorthand(_), _, _, _, _, _, _, _, _, _) :-
    % these should have been expanded out by now
    error("find_follow_vars_in_goal_2: unexpected shorthand").

find_follow_vars_in_goal_expr(
        Call @ generic_call(GenericCall, Args, Modes, Det), Call,
        GoalInfo, GoalInfo, VarTypes, ModuleInfo,
        !FollowVarsMap, !NextNonReserved) :-
    % Casts are generated inline.
    ( GenericCall = cast(_) ->
        true
    ;
        determinism_to_code_model(Det, CodeModel),
        map__apply_to_list(Args, VarTypes, Types),
        make_arg_infos(Types, Modes, CodeModel, ModuleInfo, ArgInfos),
        assoc_list__from_corresponding_lists(Args, ArgInfos, ArgsInfos),
        arg_info__partition_args(ArgsInfos, InVarInfos, _),
        assoc_list__keys(InVarInfos, InVars),
        module_info_globals(ModuleInfo, Globals),
		call_gen__generic_call_info(Globals, GenericCall,
			length(InVars), _, SpecifierArgInfos, FirstInput, _),
        find_follow_vars_from_arginfo(SpecifierArgInfos,
            map__init, !:FollowVarsMap, 1, _),
        find_follow_vars_from_sequence(InVars, FirstInput,
            !FollowVarsMap, !:NextNonReserved)
    ).

find_follow_vars_in_goal_expr(call(PredId, ProcId, Args, State, E, F),
        call(PredId, ProcId, Args, State, E, F), GoalInfo, GoalInfo,
        _, ModuleInfo, !FollowVarsMap, !NextNonReserved) :-
    ( State = inline_builtin ->
        true
    ;
        find_follow_vars_in_call(PredId, ProcId, Args, ModuleInfo,
            !:FollowVarsMap, !:NextNonReserved)
    ).

%-----------------------------------------------------------------------------%

:- pred find_follow_vars_in_call(pred_id::in, proc_id::in, list(prog_var)::in,
    module_info::in, abs_follow_vars_map::out, int::out) is det.

find_follow_vars_in_call(PredId, ProcId, Args, ModuleInfo,
        FollowVarsMap, NextNonReserved) :-
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_arg_info(ProcInfo, ArgInfo),
    assoc_list__from_corresponding_lists(Args, ArgInfo, ArgsInfos),
    find_follow_vars_from_arginfo(ArgsInfos, map__init, FollowVarsMap,
        1, NextNonReserved).

:- pred find_follow_vars_from_arginfo(assoc_list(prog_var, arg_info)::in,
    abs_follow_vars_map::in, abs_follow_vars_map::out,
    int::in, int::out) is det.

find_follow_vars_from_arginfo([], !FollowVarsMap, !NextNonReserved).
find_follow_vars_from_arginfo([ArgVar - arg_info(RegNum, Mode) | ArgsInfos],
        !FollowVarsMap, !NextNonReserved) :-
    ( Mode = top_in ->
        Locn = abs_reg(RegNum),
        ( svmap__insert(ArgVar, Locn, !FollowVarsMap) ->
            true    % FollowVarsMap is updated
        ;
            % The call is not in superhomogeneous form: this
            % argument has appeared before. Since the earlier
            % appearance will have given the variable a smaller
            % register number, we prefer that location to the one
            % we would give to this appearance of the variable.
            true    % FollowVarsMap is not updated
        ),
        int__max(RegNum + 1, !NextNonReserved)
    ;
        true
    ),
    find_follow_vars_from_arginfo(ArgsInfos,
        !FollowVarsMap, !NextNonReserved).

%-----------------------------------------------------------------------------%

:- pred find_follow_vars_from_sequence(list(prog_var)::in, int::in,
    abs_follow_vars_map::in, abs_follow_vars_map::out, int::out) is det.

find_follow_vars_from_sequence([], NextRegNum, !FollowVarsMap, NextRegNum).
find_follow_vars_from_sequence([InVar | InVars], NextRegNum, !FollowVarsMap,
        NextNonReserved) :-
    Locn = abs_reg(NextRegNum),
    ( map__insert(!.FollowVarsMap, InVar, Locn, !:FollowVarsMap) ->
        true    % FollowVarsMap is updated
    ;
        % The call is not in superhomogeneous form: this argument has
        % appeared before. Since the earlier appearance will have given
        % the variable a smaller register number, we prefer that
        % location to the one we would give to this appearance of the
        % variable.
        true    % FollowVarsMap is not updated
    ),
    find_follow_vars_from_sequence(InVars, NextRegNum + 1,
        !FollowVarsMap, NextNonReserved).

%-----------------------------------------------------------------------------%

    % We attach a follow_vars to each arm of a switch, since inside
    % each arm the preferred locations for variables will in general
    % be different.

    % For the time being, we return the follow_vars computed from
    % the first arm as the preferred requirements of the switch as
    % a whole. This is close to right, since the first disjunct will
    % definitely be the first to be entered. However, the follow_vars
    % computed for the disjunction as a whole can profitably mention
    % variables that are not live in the first disjunct, but may be
    % needed in the second and later disjuncts. In general, we may
    % wish to take into account the requirements of all disjuncts
    % up to the first non-failing disjunct. (The requirements of
    % later disjuncts are not relevant. For model_non disjunctions,
    % they can only be entered with everything in stack slots; for
    % model_det and model_semi disjunctions, they will never be
    % entered at all.)
    %
    % This code is used both for disjunction and parallel conjunction.

:- pred find_follow_vars_in_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    vartypes::in, module_info::in,
    abs_follow_vars_map::in, abs_follow_vars_map::out,
    int::in, int::out) is det.

find_follow_vars_in_disj([], [], _, _ModuleInfo,
        FollowVarsMap,  FollowVarsMap,
        NextNonReserved, NextNonReserved).
find_follow_vars_in_disj([Goal0 | Goals0], [Goal | Goals],
        VarTypes, ModuleInfo, FollowVarsMap0, FollowVarsMap,
        NextNonReserved0, NextNonReserved) :-
    find_follow_vars_in_goal(Goal0, Goal1, VarTypes, ModuleInfo,
        FollowVarsMap0, FollowVarsMap,
        NextNonReserved0, NextNonReserved),
    FollowVars = abs_follow_vars(FollowVarsMap, NextNonReserved),
    goal_set_follow_vars(yes(FollowVars), Goal1, Goal),
    find_follow_vars_in_disj(Goals0, Goals, VarTypes, ModuleInfo,
        FollowVarsMap0, _FollowVarsMap,
        NextNonReserved0, _NextNonReserved).

%-----------------------------------------------------------------------------%

    % We attach a follow_vars to each arm of a switch, since inside
    % each arm the preferred locations for variables will in general
    % be different.

    % For the time being, we return the follow_vars computed from
    % the first arm as the preferred requirements of the switch as
    % a whole. This can be improved, both to include variables that
    % are not live in that branch (and therefore don't appear in
    % its follow_vars) and to let different branches "vote" on
    % what should be in registers.

:- pred find_follow_vars_in_cases(list(case)::in, list(case)::out,
    vartypes::in, module_info::in,
    abs_follow_vars_map::in, abs_follow_vars_map::out,
    int::in, int::out) is det.

find_follow_vars_in_cases([], [], _, _, !FollowVarsMap, !NextNonReserved).
find_follow_vars_in_cases([case(Cons, Goal0) | Goals0],
        [case(Cons, Goal) | Goals], VarTypes, ModuleInfo,
        FollowVarsMap0, FollowVarsMap,
        NextNonReserved0, NextNonReserved) :-
    find_follow_vars_in_goal(Goal0, Goal1, VarTypes, ModuleInfo,
        FollowVarsMap0, FollowVarsMap,
        NextNonReserved0, NextNonReserved),
    FollowVars = abs_follow_vars(FollowVarsMap, NextNonReserved),
    goal_set_follow_vars(yes(FollowVars), Goal1, Goal),
    find_follow_vars_in_cases(Goals0, Goals, VarTypes, ModuleInfo,
        FollowVarsMap0, _FollowVarsMap,
        NextNonReserved, _NextNonReserved).

%-----------------------------------------------------------------------------%

    % We attach the follow_vars to each goal that follows a goal
    % that is not cachable by the code generator.

:- pred find_follow_vars_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    vartypes::in, module_info::in, bool::in,
    abs_follow_vars_map::in, abs_follow_vars_map::out,
    int::in, int::out) is det.

find_follow_vars_in_conj([], [], _, _ModuleInfo, _AttachToFirst,
        !FollowVarsMap, !NextNonReserved).
find_follow_vars_in_conj([Goal0 | Goals0], [Goal | Goals], VarTypes,
        ModuleInfo, AttachToFirst, !FollowVarsMap, !NextNonReserved) :-
    (
        Goal0 = GoalExpr0 - _,
        (
            GoalExpr0 = call(_, _, _, BuiltinState, _, _),
            BuiltinState = inline_builtin
        ;
            GoalExpr0 = unify(_, _, _, Unification, _),
            Unification \= complicated_unify(_, _, _)
        )
    ->
        AttachToNext = no
    ;
        AttachToNext = yes
    ),
    find_follow_vars_in_conj(Goals0, Goals, VarTypes, ModuleInfo,
        AttachToNext, !FollowVarsMap, !NextNonReserved),
    find_follow_vars_in_goal(Goal0, Goal1, VarTypes, ModuleInfo,
        !FollowVarsMap, !NextNonReserved),
    (
        AttachToFirst = yes,
        FollowVars = abs_follow_vars(!.FollowVarsMap, !.NextNonReserved),
        goal_set_follow_vars(yes(FollowVars), Goal1, Goal)
    ;
        AttachToFirst = no,
        Goal = Goal1
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

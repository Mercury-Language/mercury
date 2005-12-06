%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: saved_vars.m.
% Main author: zs.

% This module traverses the goal for each procedure, looking for and
% exploiting opportunities to reduce the number of variables that have
% to be saved across calls.
%
% At the moment the only opportunity we look for is the assignment of
% constants to variables. These assignments should be delayed until
% the value of the variable is needed. If the variable has several uses,
% we generate a copy for each use, renaming the using goal to refer to the
% variable by its new name.
%
% We thread the SlotInfo structure through the module; this allows us
% to define new variables.

%-----------------------------------------------------------------------------%

:- module ll_backend__saved_vars.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.

:- pred saved_vars_proc(pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module check_hlds.polymorphism.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

saved_vars_proc(PredId, ProcId, ProcInfo0, ProcInfo, !ModuleInfo, !IO) :-
    write_proc_progress_message("% Minimizing saved vars in ",
        PredId, ProcId, !.ModuleInfo, !IO),
    module_info_get_globals(!.ModuleInfo, Globals),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    body_should_use_typeinfo_liveness(PredInfo, Globals, TypeInfoLiveness),
    saved_vars_proc_no_io(TypeInfoLiveness, ProcInfo0, ProcInfo, !ModuleInfo).

:- pred saved_vars_proc_no_io(bool::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out) is det.

saved_vars_proc_no_io(TypeInfoLiveness, !ProcInfo, !ModuleInfo) :-
    proc_info_goal(!.ProcInfo, Goal0),
    proc_info_varset(!.ProcInfo, Varset0),
    proc_info_vartypes(!.ProcInfo, VarTypes0),
    proc_info_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
    init_slot_info(Varset0, VarTypes0, RttiVarMaps0, TypeInfoLiveness,
        SlotInfo0),

    saved_vars_in_goal(Goal0, Goal1, SlotInfo0, SlotInfo),

    final_slot_info(Varset1, VarTypes1, RttiVarMaps, SlotInfo),
    proc_info_headvars(!.ProcInfo, HeadVars),

    % hlds_out__write_goal(Goal1, !.ModuleInfo, Varset1, 0, "\n"),

    % Recompute the nonlocals for each goal.
    implicitly_quantify_clause_body(HeadVars, _Warnings, Goal1, Goal2,
        Varset1, Varset, VarTypes1, VarTypes),
    proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InstMap0),
    proc_info_inst_varset(!.ProcInfo, InstVarSet),
    recompute_instmap_delta(no, Goal2, Goal, VarTypes,
        InstVarSet, InstMap0, !ModuleInfo),

    % hlds_out__write_goal(Goal, !.ModuleInfo, Varset, 0, "\n"),

    proc_info_set_goal(Goal, !ProcInfo),
    proc_info_set_varset(Varset, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo).

%-----------------------------------------------------------------------------%

:- pred saved_vars_in_goal(hlds_goal::in, hlds_goal::out,
    slot_info::in, slot_info::out) is det.

saved_vars_in_goal(GoalExpr0 - GoalInfo0, Goal, !SlotInfo) :-
    (
        GoalExpr0 = conj(Goals0),
        goal_info_get_nonlocals(GoalInfo0, NonLocals),
        saved_vars_in_conj(Goals0, Goals, NonLocals, !SlotInfo),
        conj_list_to_goal(Goals, GoalInfo0, Goal)
    ;
        GoalExpr0 = par_conj(Goals0),
        % saved_vars_in_disj treats its goal list as an independent list
        % of goals, so we can use it to process the list of parallel
        % conjuncts too.
        saved_vars_in_disj(Goals0, Goals, !SlotInfo),
        Goal = par_conj(Goals) - GoalInfo0
    ;
        GoalExpr0 = disj(Goals0),
        saved_vars_in_disj(Goals0, Goals, !SlotInfo),
        Goal = disj(Goals) - GoalInfo0
    ;
        GoalExpr0 = not(NegGoal0),
        saved_vars_in_goal(NegGoal0, NegGoal, !SlotInfo),
        Goal = not(NegGoal) - GoalInfo0
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        saved_vars_in_switch(Cases0, Cases, !SlotInfo),
        Goal = switch(Var, CanFail, Cases) - GoalInfo0
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        saved_vars_in_goal(Cond0, Cond, !SlotInfo),
        saved_vars_in_goal(Then0, Then, !SlotInfo),
        saved_vars_in_goal(Else0, Else, !SlotInfo),
        Goal = if_then_else(Vars, Cond, Then, Else) - GoalInfo0
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        saved_vars_in_goal(SubGoal0, SubGoal, !SlotInfo),
        Goal = scope(Reason, SubGoal) - GoalInfo0
    ;
        GoalExpr0 = generic_call(_, _, _, _),
        Goal = GoalExpr0 - GoalInfo0
    ;
        GoalExpr0 = call(_, _, _, _, _, _),
        Goal = GoalExpr0 - GoalInfo0
    ;
        GoalExpr0 = unify(_, _, _, _, _),
        Goal = GoalExpr0 - GoalInfo0
    ;
        GoalExpr0 = foreign_proc(_, _, _, _, _, _),
        Goal = GoalExpr0 - GoalInfo0
    ;
        GoalExpr0 = shorthand(_),
        % these should have been expanded out by now
        unexpected(this_file,
            "saved_vars_in_goal: unexpected shorthand")
    ).

%-----------------------------------------------------------------------------%

    % If we find a unification that assigns a constant to a variable,
    % attempt to push it into the following code.
    %
    % We cannot push such a unification if the following goal needs the value
    % of the variable. We also avoid attempting to push the unification into
    % any following similar unifications, since the order of such unifications
    % does not matter, and we would like to avoid "pushing contests" between
    % several such unifications about which one should be closest to a
    % following goal that uses all the variables they define.
    %
:- pred saved_vars_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    set(prog_var)::in, slot_info::in, slot_info::out) is det.

saved_vars_in_conj([], [], _, !SlotInfo).
saved_vars_in_conj([Goal0 | Goals0], Goals, NonLocals, !SlotInfo) :-
    (
        Goal0 = unify(_, _, _, Unif, _) - GoalInfo,
        Unif = construct(Var, _, [], _, _, _, _),
        goal_info_get_features(GoalInfo, Features),
        ( all [Feature]
            (
                set__member(Feature, Features)
            =>
                ok_to_duplicate(Feature) = yes
            )
        ),
        \+ slot_info_do_not_duplicate_var(!.SlotInfo, Var),
        skip_constant_constructs(Goals0, Constants, OtherGoals),
        OtherGoals = [First | _Rest],
        can_push(Var, First)
    ->
        set__is_member(Var, NonLocals, IsNonLocal),
        saved_vars_delay_goal(OtherGoals, Goals1, Goal0, Var, IsNonLocal,
            !SlotInfo),
        list__append(Constants, Goals1, Goals2),
        saved_vars_in_conj(Goals2, Goals, NonLocals, !SlotInfo)
    ;
        saved_vars_in_goal(Goal0, Goal1, !SlotInfo),
        saved_vars_in_conj(Goals0, Goals1, NonLocals, !SlotInfo),
        Goals = [Goal1 | Goals1]
    ).

    % ok_to_duplicate returns `no' for features which shouldn't be
    % on construction unifications in the first place as well as for
    % construction unifications that shouldn't be duplicated.
    %
:- func ok_to_duplicate(goal_feature) = bool.

ok_to_duplicate(constraint) = no.
ok_to_duplicate(from_head) = yes.
ok_to_duplicate(impure_goal) = no.
ok_to_duplicate(semipure_goal) = no.
ok_to_duplicate(not_impure_for_determinism) = no.
ok_to_duplicate(stack_opt) = no.
ok_to_duplicate(tuple_opt) = no.
ok_to_duplicate(call_table_gen) = no.
ok_to_duplicate(preserve_backtrack_into) = no.
ok_to_duplicate(hide_debug_event) = no.
ok_to_duplicate(tailcall) = no.
ok_to_duplicate(keep_constant_binding) = no.
ok_to_duplicate(save_deep_excp_vars) = no.
ok_to_duplicate(dont_warn_singleton) = yes.
ok_to_duplicate(mode_check_clauses_goal) = yes.
ok_to_duplicate(will_not_modify_trail) = yes.

    % Divide a list of goals into an initial subsequence of goals
    % that construct constants, and all other goals.
    %
:- pred skip_constant_constructs(list(hlds_goal)::in, list(hlds_goal)::out,
    list(hlds_goal)::out) is det.

skip_constant_constructs([], [], []).
skip_constant_constructs([Goal0 | Goals0], Constants, Others) :-
    (
        Goal0 = unify(_, _, _, Unif, _) - _,
        Unif = construct(_, _, [], _, _, _, _)
    ->
        skip_constant_constructs(Goals0, Constants1, Others),
        Constants = [Goal0 | Constants1]
    ;
        Constants = [],
        Others = [Goal0 | Goals0]
    ).

    % Decide whether the value of the given variable is needed immediately
    % in the goal, or whether the unification that constructs a value for
    % the variable can be usefully pushed into the goal.
    %
    % NOTE: the logic of this predicate must match the logic of
    % saved_vars_delay_goal.
    %
:- pred can_push(prog_var::in, hlds_goal::in) is semidet.

can_push(Var, First) :-
    First = FirstExpr - FirstInfo,
    goal_info_get_nonlocals(FirstInfo, FirstNonLocals),
    ( set__member(Var, FirstNonLocals) ->
        (
            FirstExpr = conj(_)
        ;
            FirstExpr = scope(_, _)
        ;
            FirstExpr = not(_)
        ;
            FirstExpr = disj(_)
        ;
            FirstExpr = switch(SwitchVar, _, _),
            Var \= SwitchVar
        ;
            FirstExpr = if_then_else(_, _, _, _)
        )
    ;
        true
    ).

    % The main inputs of this predicate are a list of goals in a
    % conjunction, and a goal Construct that assigns a constant to a
    % variable Var.
    %
    % When we find an atomic goal in the conjunction that refers to
    % Var, we create a new variable NewVar, rename both this goal
    % and Construct to refer to NewVar instead of Var, and insert
    % the new version of Construct before the new version of the
    % goal.
    %
    % When we find a non-atomic goal in the conjunction that refers
    % to Var, we push Construct into each of its components.
    %
    % If Var is exported from the conjunction, we include Construct
    % at the end of the conjunction to give it its value.
    %
:- pred saved_vars_delay_goal(list(hlds_goal)::in, list(hlds_goal)::out,
    hlds_goal::in, prog_var::in, bool::in, slot_info::in, slot_info::out)
    is det.

saved_vars_delay_goal([], Goals, Construct, _Var, IsNonLocal, !SlotInfo) :-
    (
        IsNonLocal = yes,
        Goals = [Construct]
    ;
        IsNonLocal = no,
        Goals = []
    ).
saved_vars_delay_goal([Goal0 | Goals0], Goals, Construct, Var, IsNonLocal,
        !SlotInfo) :-
    Goal0 = Goal0Expr - Goal0Info,
    goal_info_get_nonlocals(Goal0Info, Goal0NonLocals),
    ( set__member(Var, Goal0NonLocals) ->
        (
            Goal0Expr = unify(_, _, _, _, _),
            rename_var(Var, _NewVar, Subst, !SlotInfo),
            goal_util__rename_vars_in_goal(Subst, Construct, NewConstruct),
            goal_util__rename_vars_in_goal(Subst, Goal0, Goal1),
            saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                IsNonLocal, !SlotInfo),
            Goals = [NewConstruct, Goal1 | Goals1]
        ;
            Goal0Expr = call(_, _, _, _, _, _),
            rename_var(Var, _NewVar, Subst, !SlotInfo),
            goal_util__rename_vars_in_goal(Subst, Construct, NewConstruct),
            goal_util__rename_vars_in_goal(Subst, Goal0, Goal1),
            saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                IsNonLocal, !SlotInfo),
            Goals = [NewConstruct, Goal1 | Goals1]
        ;
            Goal0Expr = generic_call(_, _, _, _),
            rename_var(Var, _NewVar, Subst, !SlotInfo),
            goal_util__rename_vars_in_goal(Subst, Construct, NewConstruct),
            goal_util__rename_vars_in_goal(Subst, Goal0, Goal1),
            saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                IsNonLocal, !SlotInfo),
            Goals = [NewConstruct, Goal1 | Goals1]
        ;
            Goal0Expr = foreign_proc(_, _, _, _, _, _),
            rename_var(Var, _NewVar, Subst, !SlotInfo),
            goal_util__rename_vars_in_goal(Subst, Construct, NewConstruct),
            goal_util__rename_vars_in_goal(Subst, Goal0, Goal1),
            saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                IsNonLocal, !SlotInfo),
            Goals = [NewConstruct, Goal1 | Goals1]
        ;
            Goal0Expr = conj(Conj),
            list__append(Conj, Goals0, Goals1),
            saved_vars_delay_goal(Goals1, Goals, Construct, Var,
                IsNonLocal, !SlotInfo)
        ;
            Goal0Expr = par_conj(_ParConj),
            saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                IsNonLocal, !SlotInfo),
            Goals = [Goal0|Goals1]
        ;
            Goal0Expr = scope(Reason, SomeGoal0),
            rename_var(Var, NewVar, Subst, !SlotInfo),
            goal_util__rename_vars_in_goal(Subst, Construct, NewConstruct),
            goal_util__rename_vars_in_goal(Subst, SomeGoal0, SomeGoal1),
            push_into_goal(SomeGoal1, SomeGoal, NewConstruct, NewVar,
                !SlotInfo),
            Goal1 = scope(Reason, SomeGoal) - Goal0Info,
            saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                IsNonLocal, !SlotInfo),
            Goals = [Goal1 | Goals1]
        ;
            Goal0Expr = not(NegGoal0),
            rename_var(Var, NewVar, Subst, !SlotInfo),
            goal_util__rename_vars_in_goal(Subst, Construct, NewConstruct),
            goal_util__rename_vars_in_goal(Subst, NegGoal0, NegGoal1),
            push_into_goal(NegGoal1, NegGoal, NewConstruct, NewVar,
                !SlotInfo),
            Goal1 = not(NegGoal) - Goal0Info,
            saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                IsNonLocal, !SlotInfo),
            Goals = [Goal1 | Goals1]
        ;
            Goal0Expr = disj(Disjuncts0),
            push_into_goals_rename(Disjuncts0, Disjuncts, Construct, Var,
                !SlotInfo),
            Goal1 = disj(Disjuncts) - Goal0Info,
            saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                IsNonLocal, !SlotInfo),
            Goals = [Goal1 | Goals1]
        ;
            Goal0Expr = switch(SwitchVar, CF, Cases0),
            ( SwitchVar = Var ->
                saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                    IsNonLocal, !SlotInfo),
                Goals = [Construct, Goal0 | Goals1]
            ;
                push_into_cases_rename(Cases0, Cases, Construct, Var,
                    !SlotInfo),
                Goal1 = switch(SwitchVar, CF, Cases) - Goal0Info,
                saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                    IsNonLocal, !SlotInfo),
                Goals = [Goal1 | Goals1]
            )
        ;
            Goal0Expr = if_then_else(V, Cond0, Then0, Else0),
            push_into_goal_rename(Cond0, Cond, Construct, Var, !SlotInfo),
            push_into_goal_rename(Then0, Then, Construct, Var, !SlotInfo),
            push_into_goal_rename(Else0, Else, Construct, Var, !SlotInfo),
            Goal1 = if_then_else(V, Cond, Then, Else) - Goal0Info,
            saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                IsNonLocal, !SlotInfo),
            Goals = [Goal1 | Goals1]
        ;
            Goal0Expr = shorthand(_),
            % These should have been expanded out by now.
            unexpected(this_file,
                "saved_vars_delay_goal: unexpected shorthand")
        )
    ;
        saved_vars_delay_goal(Goals0, Goals1, Construct, Var, IsNonLocal,
            !SlotInfo),
        Goals = [Goal0 | Goals1]
    ).

    % Push a non-renamed version of the given construction into the given goal.
    % Also traverse the goal looking for further opportunities.
    %
:- pred push_into_goal(hlds_goal::in, hlds_goal::out, hlds_goal::in,
    prog_var::in, slot_info::in, slot_info::out) is det.

push_into_goal(Goal0, Goal, Construct, Var, !SlotInfo) :-
    saved_vars_in_goal(Goal0, Goal1, !SlotInfo),
    Goal1 = _ - Goal1Info,
    goal_to_conj_list(Goal1, Conj1),
    saved_vars_delay_goal(Conj1, Conj, Construct, Var, no, !SlotInfo),
    conj_list_to_goal(Conj, Goal1Info, Goal).

    % Push a renamed version of the given construction into the
    % given goal.  If the goal does not refer to the variable bound
    % by the construction, then this would have no effect, so we
    % merely traverse the goal looking for other opportunities.
    %
:- pred push_into_goal_rename(hlds_goal::in, hlds_goal::out, hlds_goal::in,
    prog_var::in, slot_info::in, slot_info::out) is det.

push_into_goal_rename(Goal0, Goal, Construct, Var, !SlotInfo) :-
    Goal0 = _ - GoalInfo0,
    goal_info_get_nonlocals(GoalInfo0, NonLocals),
    ( set__member(Var, NonLocals) ->
        rename_var(Var, NewVar, Subst, !SlotInfo),
        goal_util__rename_vars_in_goal(Subst, Construct, NewConstruct),
        goal_util__rename_vars_in_goal(Subst, Goal0, Goal1),
        push_into_goal(Goal1, Goal, NewConstruct, NewVar, !SlotInfo)
    ;
        saved_vars_in_goal(Goal0, Goal, !SlotInfo)
    ).
    
    % Push renamed versions of the given construction into each of
    % several goals.
    %
:- pred push_into_goals_rename(list(hlds_goal)::in, list(hlds_goal)::out,
    hlds_goal::in, prog_var::in, slot_info::in, slot_info::out) is det.

push_into_goals_rename([], [], _Construct, _Var, !SlotInfo).
push_into_goals_rename([Goal0 | Goals0], [Goal | Goals], Construct, Var,
        !SlotInfo) :-
    push_into_goal_rename(Goal0, Goal, Construct, Var, !SlotInfo),
    push_into_goals_rename(Goals0, Goals, Construct, Var, !SlotInfo).
    
    % Push renamed versions of the given construction into each of
    % several cases.
    %
:- pred push_into_cases_rename(list(case)::in, list(case)::out, hlds_goal::in,
    prog_var::in, slot_info::in, slot_info::out) is det.

push_into_cases_rename([], [], _Construct, _Var, !SlotInfo).
push_into_cases_rename([case(ConsId, Goal0) | Cases0],
        [case(ConsId, Goal) | Cases], Construct, Var, !SlotInfo) :-
    push_into_goal_rename(Goal0, Goal, Construct, Var, !SlotInfo),
    push_into_cases_rename(Cases0, Cases, Construct, Var, !SlotInfo).

%-----------------------------------------------------------------------------%

    % saved_vars_in_disj does a saved_vars_in_goal on an list of
    % independent goals, and is used to process disjunctions and
    % parallel conjunctions.
    %
:- pred saved_vars_in_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    slot_info::in, slot_info::out) is det.

saved_vars_in_disj([], [], !SlotInfo).
saved_vars_in_disj([Goal0 | Goals0], [Goal | Goals], !SlotInfo) :-
    saved_vars_in_goal(Goal0, Goal, !SlotInfo),
    saved_vars_in_disj(Goals0, Goals, !SlotInfo).

:- pred saved_vars_in_switch(list(case)::in, list(case)::out,
    slot_info::in, slot_info::out) is det.

saved_vars_in_switch([], [], !SlotInfo).
saved_vars_in_switch([case(Cons, Goal0) | Cases0],
        [case(Cons, Goal) | Cases], !SlotInfo) :-
    saved_vars_in_goal(Goal0, Goal, !SlotInfo),
    saved_vars_in_switch(Cases0, Cases, !SlotInfo).

%-----------------------------------------------------------------------------%

:- type slot_info
    ---> slot_info(
            prog_varset,
            vartypes,
            rtti_varmaps,
            bool            % TypeInfoLiveness
        ).

:- pred init_slot_info(prog_varset::in, vartypes::in,
    rtti_varmaps::in, bool::in, slot_info::out) is det.

init_slot_info(Varset, VarTypes, RttiVarMaps, TypeInfoLiveness, SlotInfo) :-
    SlotInfo = slot_info(Varset, VarTypes, RttiVarMaps, TypeInfoLiveness).

:- pred final_slot_info(prog_varset::out, vartypes::out, rtti_varmaps::out,
    slot_info::in) is det.

final_slot_info(Varset, VarTypes, RttiVarMaps, SlotInfo) :-
    SlotInfo = slot_info(Varset, VarTypes, RttiVarMaps, _).

:- pred rename_var(prog_var::in, prog_var::out, map(prog_var, prog_var)::out,
    slot_info::in, slot_info::out) is det.

rename_var(Var, NewVar, Substitution, !SlotInfo) :-
    !.SlotInfo = slot_info(Varset0, VarTypes0, RttiVarMaps0, TypeInfoLiveness),
    varset__new_var(Varset0, NewVar, Varset),
    map__from_assoc_list([Var - NewVar], Substitution),
    map__lookup(VarTypes0, Var, Type),
    map__det_insert(VarTypes0, NewVar, Type, VarTypes),
    rtti_var_info_duplicate(Var, NewVar, RttiVarMaps0, RttiVarMaps),
    !:SlotInfo = slot_info(Varset, VarTypes, RttiVarMaps,
        TypeInfoLiveness).

    % Check whether it is ok to duplicate a given variable according
    % to the information in the slot_info.  If TypeInfoLiveness is set,
    % it is possible that liveness.m will want to refer to the
    % rtti_varmaps to calculate which type_infos are live (see the
    % comments at the top of liveness.m).  If we duplicated any
    % type_info variables here then this could cause problems because
    % the rtti_varmaps would not be able to be kept consistent.
    % Therefore we don't allow type_infos to be duplicated when
    % TypeInfoLiveness is set.
    %
:- pred slot_info_do_not_duplicate_var(slot_info::in, prog_var::in) is semidet.

slot_info_do_not_duplicate_var(SlotInfo, Var) :-
    SlotInfo = slot_info(_, VarTypes, _, TypeInfoLiveness),
    TypeInfoLiveness = yes,
    map__lookup(VarTypes, Var, Type),
    polymorphism__type_is_type_info_or_ctor_type(Type).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "saved_vars.m".

%-----------------------------------------------------------------------------%
:- end_module saved_vars.
%-----------------------------------------------------------------------------%

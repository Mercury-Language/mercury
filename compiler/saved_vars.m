%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: saved_vars.m.
% Main author: zs.
%
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
%
%-----------------------------------------------------------------------------%

:- module ll_backend.saved_vars.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

%-----------------------------------------------------------------------------%

:- pred saved_vars_proc(pred_proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_rtti.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

saved_vars_proc(proc(PredId, ProcId), !ProcInfo, !ModuleInfo) :-
    trace [io(!IO)] (
        write_proc_progress_message(!.ModuleInfo,
            "Minimizing saved vars in", PredId, ProcId, !IO)
    ),

    module_info_get_globals(!.ModuleInfo, Globals),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    body_should_use_typeinfo_liveness(PredInfo, Globals, TypeInfoLiveness),

    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_var_table(!.ProcInfo, VarTable0),
    proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
    init_slot_info(VarTable0, RttiVarMaps0, TypeInfoLiveness, SlotInfo0),

    saved_vars_in_goal(Goal0, Goal1, SlotInfo0, SlotInfo),

    final_slot_info(SlotInfo, VarTable1, RttiVarMaps1),
    proc_info_get_headvars(!.ProcInfo, HeadVars),

    % Recompute the nonlocals for each goal.
    implicitly_quantify_clause_body_general(ord_nl_no_lambda,
        HeadVars, _Warnings, Goal1, Goal2,
        VarTable1, VarTable, RttiVarMaps1, RttiVarMaps),
    proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InstMap0),
    proc_info_get_inst_varset(!.ProcInfo, InstVarSet),
    recompute_instmap_delta(no_recomp_atomics, VarTable, InstVarSet,
        InstMap0, Goal2, Goal, !ModuleInfo),

    trace [io(!IO), compile_time(flag("debug_saved_vars"))] (
        OutInfo = hlds_out_util.init_hlds_out_info(Globals, output_debug),
        io.output_stream(Stream, !IO),
        io.write_string(Stream, "initial version:\n", !IO),
        hlds_out_goal.write_goal(OutInfo, Stream, !.ModuleInfo,
            vns_var_table(VarTable0), print_name_and_num, 0, "\n", Goal0, !IO),
        io.write_string(Stream, "after transformation:\n", !IO),
        hlds_out_goal.write_goal(OutInfo, Stream, !.ModuleInfo,
            vns_var_table(VarTable1), print_name_and_num, 0, "\n", Goal1, !IO),
        io.write_string(Stream, "final version:\n", !IO),
        hlds_out_goal.write_goal(OutInfo, Stream, !.ModuleInfo,
            vns_var_table(VarTable), print_name_and_num, 0, "\n", Goal, !IO)
    ),

    proc_info_set_goal(Goal, !ProcInfo),
    proc_info_set_var_table(VarTable, !ProcInfo),
    proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo).

%-----------------------------------------------------------------------------%

:- pred saved_vars_in_goal(hlds_goal::in, hlds_goal::out,
    slot_info::in, slot_info::out) is det.

saved_vars_in_goal(Goal0, Goal, !SlotInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            NonLocals = goal_info_get_nonlocals(GoalInfo0),
            saved_vars_in_conj(Goals0, Goals, NonLocals, !SlotInfo),
            conj_list_to_goal(Goals, GoalInfo0, Goal)
        ;
            ConjType = parallel_conj,
            saved_vars_in_independent_goals(Goals0, Goals, !SlotInfo),
            Goal = hlds_goal(conj(ConjType, Goals), GoalInfo0)
        )
    ;
        GoalExpr0 = disj(Goals0),
        saved_vars_in_independent_goals(Goals0, Goals, !SlotInfo),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(NegGoal0),
        saved_vars_in_goal(NegGoal0, NegGoal, !SlotInfo),
        GoalExpr = negation(NegGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        saved_vars_in_switch(Cases0, Cases, !SlotInfo),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        saved_vars_in_goal(Cond0, Cond, !SlotInfo),
        saved_vars_in_goal(Then0, Then, !SlotInfo),
        saved_vars_in_goal(Else0, Else, !SlotInfo),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            % Moving unifications around inside these scopes is
            % (a) counterproductive, and (b) incorrect, since it would
            % invalidate the invariants required of such scopes.
            SubGoal = SubGoal0
        else
            saved_vars_in_goal(SubGoal0, SubGoal, !SlotInfo)
        ),
        GoalExpr = scope(Reason, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        ( GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = shorthand(_),
        % these should have been expanded out by now
        unexpected($pred, "shorthand")
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
    set_of_progvar::in, slot_info::in, slot_info::out) is det.

saved_vars_in_conj([], [], _, !SlotInfo).
saved_vars_in_conj([Goal0 | Goals0], Goals, NonLocals, !SlotInfo) :-
    ( if
        Goal0 = hlds_goal(unify(_, _, _, Unif, _), GoalInfo),
        Unif = construct(Var, _, [], _, _, _, _),
        Features = goal_info_get_features(GoalInfo),
        ( all [Feature]
            (
                set.member(Feature, Features)
            =>
                ok_to_duplicate(Feature) = yes
            )
        ),
        not slot_info_do_not_duplicate_var(!.SlotInfo, Var),
        skip_constant_constructs(Goals0, Constants, OtherGoals),
        OtherGoals = [First | _Rest],
        can_push(Var, First) = yes
    then
        set_of_var.is_member(NonLocals, Var, IsNonLocal),
        saved_vars_delay_goal(OtherGoals, Goals1, Goal0, Var, IsNonLocal,
            !SlotInfo),
        list.append(Constants, Goals1, Goals2),
        saved_vars_in_conj(Goals2, Goals, NonLocals, !SlotInfo)
    else
        saved_vars_in_goal(Goal0, Goal1, !SlotInfo),
        saved_vars_in_conj(Goals0, Goals1, NonLocals, !SlotInfo),
        Goals = [Goal1 | Goals1]
    ).

    % ok_to_duplicate returns `no' for features which shouldn't be
    % on construction unifications in the first place as well as for
    % construction unifications that shouldn't be duplicated.
    %
:- func ok_to_duplicate(goal_feature) = bool.

ok_to_duplicate(feature_constraint) = no.
ok_to_duplicate(feature_from_head) = yes.
ok_to_duplicate(feature_not_impure_for_determinism) = no.
ok_to_duplicate(feature_stack_opt) = no.
ok_to_duplicate(feature_tuple_opt) = no.
ok_to_duplicate(feature_call_table_gen) = no.
ok_to_duplicate(feature_preserve_backtrack_into) = no.
ok_to_duplicate(feature_hide_debug_event) = no.
ok_to_duplicate(feature_deep_self_tail_rec_call) = no.
ok_to_duplicate(feature_debug_self_tail_rec_call) = no.
ok_to_duplicate(feature_self_or_mutual_tail_rec_call) = no.
ok_to_duplicate(feature_obvious_nontail_rec_call) = no.
ok_to_duplicate(feature_keep_constant_binding) = no.
ok_to_duplicate(feature_save_deep_excp_vars) = no.
ok_to_duplicate(feature_dont_warn_singleton) = yes.
ok_to_duplicate(feature_state_var_copy) = yes.
ok_to_duplicate(feature_duplicated_for_switch) = yes.
ok_to_duplicate(feature_mode_check_clauses_goal) = yes.
ok_to_duplicate(feature_will_not_modify_trail) = yes.
ok_to_duplicate(feature_will_not_call_mm_tabled) = yes.
ok_to_duplicate(feature_contains_trace) = yes.
ok_to_duplicate(feature_pretest_equality) = yes.
ok_to_duplicate(feature_pretest_equality_condition) = yes.
ok_to_duplicate(feature_lambda_undetermined_mode) = yes.
ok_to_duplicate(feature_contains_stm_inner_outer) = yes.
ok_to_duplicate(feature_do_not_tailcall) = no.
ok_to_duplicate(feature_do_not_warn_implicit_stream) = no.
ok_to_duplicate(feature_lifted_by_cse) = no.
ok_to_duplicate(feature_lambda_from_try) = yes.

    % Divide a list of goals into an initial subsequence of goals
    % that construct constants, and all other goals.
    %
:- pred skip_constant_constructs(list(hlds_goal)::in, list(hlds_goal)::out,
    list(hlds_goal)::out) is det.

skip_constant_constructs([], [], []).
skip_constant_constructs([Goal0 | Goals0], Constants, Others) :-
    ( if
        Goal0 = hlds_goal(unify(_, _, _, Unif, _), _),
        Unif = construct(_, _, [], _, _, _, _)
    then
        skip_constant_constructs(Goals0, Constants1, Others),
        Constants = [Goal0 | Constants1]
    else
        Constants = [],
        Others = [Goal0 | Goals0]
    ).

    % Decide whether the value of the given variable is needed immediately
    % in the goal, or whether the unification that constructs a value for
    % the variable can be usefully pushed into the goal.
    %
    % NOTE: the logic of this predicate must match the logic of
    % saved_vars_delay_goal.
    % XXX: I went to update saved_vars_delay_goal after changing this
    % predicate and it already diverges from the algorithm here!
    %
:- func can_push(prog_var, hlds_goal) = bool.

can_push(Var, Goal) = CanPush :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    ( if set_of_var.member(NonLocals, Var) then
        (
            ( GoalExpr = if_then_else(_, _, _, _)
            ; GoalExpr = negation(_)
            ; GoalExpr = disj(_)
            ; GoalExpr = conj(plain_conj, _)
            ),
            CanPush = yes
        ;
            ( GoalExpr = conj(parallel_conj, _)
            ; GoalExpr = unify(_, _, _, _, _)
            ; GoalExpr = plain_call(_, _, _, _, _, _)
            ; GoalExpr = generic_call(_, _, _, _, _)
            ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
            ),
            CanPush = no
        ;
            GoalExpr = scope(Reason, _),
            (
                Reason = disable_warnings(_, _),
                % NOTE: This assumes that compiler passes that generate the
                % warnings that could be disabled by this scope have all
                % been run BEFORE program transformations such as saved vars.
                % If they haven't been, then the transformations can hide
                % warnings about code by moving them into these scopes,
                % or can caused them to be generated when the user does
                % not want them by moving the warned-about code out of
                % such scopes.
                CanPush = yes
            ;
                ( Reason = exist_quant(_)
                ; Reason = from_ground_term(_, from_ground_term_deconstruct)
                ; Reason = from_ground_term(_, from_ground_term_other)
                ),
                CanPush = yes
            ;
                ( Reason = from_ground_term(_, from_ground_term_construct)
                ; Reason = promise_solutions(_, _)
                ; Reason = promise_purity(_)
                ; Reason = commit(_)
                ; Reason = barrier(_)
                ; Reason = trace_goal(_, _, _, _, _)
                ; Reason = loop_control(_, _, _)
                ),
                CanPush = no
            ;
                ( Reason = require_detism(_)
                ; Reason = require_complete_switch(_)
                ; Reason = require_switch_arms_detism(_, _)
                ; Reason = from_ground_term(_, from_ground_term_initial)
                ),
                % These scopes should have been deleted by now.
                unexpected($pred, "unexpected scope")
            )
        ;
            GoalExpr = switch(SwitchVar, _, _),
            ( if Var = SwitchVar then
                CanPush = no
            else
                CanPush = yes
            )
        ;
            GoalExpr = shorthand(_),
            % These should have been expanded out by now.
            unexpected($pred, "shorthand")
        )
    else
        CanPush = yes
    ).

    % The main inputs of this predicate are a list of goals in a conjunction,
    % and a goal Construct that assigns a constant to a variable Var.
    %
    % When we find an atomic goal in the conjunction that refers to Var,
    % we create a new variable NewVar, rename both this goal and Construct
    % to refer to NewVar instead of Var, and insert the new version
    % of Construct before the new version of the goal.
    %
    % When we find a non-atomic goal in the conjunction that refers to Var,
    % we push Construct into each of its components.
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
    Goal0 = hlds_goal(Goal0Expr, Goal0Info),
    Goal0NonLocals = goal_info_get_nonlocals(Goal0Info),
    ( if set_of_var.member(Goal0NonLocals, Var) then
        (
            Goal0Expr = unify(_, _, _, _, _),
            saved_vars_rename_var(Var, _NewVar, Subst, !SlotInfo),
            rename_some_vars_in_goal(Subst, Construct, NewConstruct),
            rename_some_vars_in_goal(Subst, Goal0, Goal1),
            saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                IsNonLocal, !SlotInfo),
            Goals = [NewConstruct, Goal1 | Goals1]
        ;
            Goal0Expr = plain_call(_, _, _, _, _, _),
            saved_vars_rename_var(Var, _NewVar, Subst, !SlotInfo),
            rename_some_vars_in_goal(Subst, Construct, NewConstruct),
            rename_some_vars_in_goal(Subst, Goal0, Goal1),
            saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                IsNonLocal, !SlotInfo),
            Goals = [NewConstruct, Goal1 | Goals1]
        ;
            Goal0Expr = generic_call(_, _, _, _, _),
            saved_vars_rename_var(Var, _NewVar, Subst, !SlotInfo),
            rename_some_vars_in_goal(Subst, Construct, NewConstruct),
            rename_some_vars_in_goal(Subst, Goal0, Goal1),
            saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                IsNonLocal, !SlotInfo),
            Goals = [NewConstruct, Goal1 | Goals1]
        ;
            Goal0Expr = call_foreign_proc(_, _, _, _, _, _, _),
            saved_vars_rename_var(Var, _NewVar, Subst, !SlotInfo),
            rename_some_vars_in_goal(Subst, Construct, NewConstruct),
            rename_some_vars_in_goal(Subst, Goal0, Goal1),
            saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                IsNonLocal, !SlotInfo),
            Goals = [NewConstruct, Goal1 | Goals1]
        ;
            Goal0Expr = conj(ConjType, Conj0),
            (
                ConjType = plain_conj,
                list.append(Conj0, Goals0, Goals1),
                saved_vars_delay_goal(Goals1, Goals, Construct, Var,
                    IsNonLocal, !SlotInfo)
            ;
                ConjType = parallel_conj,
                push_into_goals_rename(Conj0, Conj, Construct, Var,
                    !SlotInfo),
                Goal1 = hlds_goal(conj(ConjType, Conj), Goal0Info),
                saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                    IsNonLocal, !SlotInfo),
                Goals = [Goal1 | Goals1]
            )
        ;
            Goal0Expr = scope(Reason, SomeGoal0),
            saved_vars_rename_var(Var, NewVar, Subst, !SlotInfo),
            rename_some_vars_in_goal(Subst, Construct, NewConstruct),
            rename_some_vars_in_goal(Subst, SomeGoal0, SomeGoal1),
            push_into_goal(SomeGoal1, SomeGoal, NewConstruct, NewVar,
                !SlotInfo),
            Goal1 = hlds_goal(scope(Reason, SomeGoal), Goal0Info),
            saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                IsNonLocal, !SlotInfo),
            Goals = [Goal1 | Goals1]
        ;
            Goal0Expr = negation(NegGoal0),
            saved_vars_rename_var(Var, NewVar, Subst, !SlotInfo),
            rename_some_vars_in_goal(Subst, Construct, NewConstruct),
            rename_some_vars_in_goal(Subst, NegGoal0, NegGoal1),
            push_into_goal(NegGoal1, NegGoal, NewConstruct, NewVar,
                !SlotInfo),
            Goal1 = hlds_goal(negation(NegGoal), Goal0Info),
            saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                IsNonLocal, !SlotInfo),
            Goals = [Goal1 | Goals1]
        ;
            Goal0Expr = disj(Disjuncts0),
            push_into_goals_rename(Disjuncts0, Disjuncts, Construct, Var,
                !SlotInfo),
            Goal1 = hlds_goal(disj(Disjuncts), Goal0Info),
            saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                IsNonLocal, !SlotInfo),
            Goals = [Goal1 | Goals1]
        ;
            Goal0Expr = switch(SwitchVar, CF, Cases0),
            ( if SwitchVar = Var then
                saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                    IsNonLocal, !SlotInfo),
                Goals = [Construct, Goal0 | Goals1]
            else
                push_into_cases_rename(Cases0, Cases, Construct, Var,
                    !SlotInfo),
                Goal1 = hlds_goal(switch(SwitchVar, CF, Cases), Goal0Info),
                saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                    IsNonLocal, !SlotInfo),
                Goals = [Goal1 | Goals1]
            )
        ;
            Goal0Expr = if_then_else(V, Cond0, Then0, Else0),
            push_into_goal_rename(Cond0, Cond, Construct, Var, !SlotInfo),
            push_into_goal_rename(Then0, Then, Construct, Var, !SlotInfo),
            push_into_goal_rename(Else0, Else, Construct, Var, !SlotInfo),
            Goal1 = hlds_goal(if_then_else(V, Cond, Then, Else), Goal0Info),
            saved_vars_delay_goal(Goals0, Goals1, Construct, Var,
                IsNonLocal, !SlotInfo),
            Goals = [Goal1 | Goals1]
        ;
            Goal0Expr = shorthand(_),
            % These should have been expanded out by now.
            unexpected($pred, "shorthand")
        )
    else
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
    Goal1 = hlds_goal(_, GoalInfo1),
    goal_to_conj_list(Goal1, Conj1),
    saved_vars_delay_goal(Conj1, Conj, Construct, Var, no, !SlotInfo),
    conj_list_to_goal(Conj, GoalInfo1, Goal).

    % Push a renamed version of the given construction into the given goal.
    % If the goal does not refer to the variable bound by the construction,
    % then this would have no effect, so we merely traverse the goal
    % looking for other opportunities.
    %
:- pred push_into_goal_rename(hlds_goal::in, hlds_goal::out, hlds_goal::in,
    prog_var::in, slot_info::in, slot_info::out) is det.

push_into_goal_rename(Goal0, Goal, Construct, Var, !SlotInfo) :-
    Goal0 = hlds_goal(_, GoalInfo0),
    NonLocals = goal_info_get_nonlocals(GoalInfo0),
    ( if set_of_var.member(NonLocals, Var) then
        saved_vars_rename_var(Var, NewVar, Subst, !SlotInfo),
        rename_some_vars_in_goal(Subst, Construct, NewConstruct),
        rename_some_vars_in_goal(Subst, Goal0, Goal1),
        push_into_goal(Goal1, Goal, NewConstruct, NewVar, !SlotInfo)
    else
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
push_into_cases_rename([Case0 | Cases0], [Case | Cases], Construct, Var,
        !SlotInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    push_into_goal_rename(Goal0, Goal, Construct, Var, !SlotInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    push_into_cases_rename(Cases0, Cases, Construct, Var, !SlotInfo).

%-----------------------------------------------------------------------------%

    % saved_vars_in_goal does a saved_vars_in_goal on an list of
    % independent goals, and is used to process disjunctions and
    % parallel conjunctions.
    %
:- pred saved_vars_in_independent_goals(list(hlds_goal)::in,
    list(hlds_goal)::out, slot_info::in, slot_info::out) is det.

saved_vars_in_independent_goals([], [], !SlotInfo).
saved_vars_in_independent_goals([Goal0 | Goals0], [Goal | Goals],
        !SlotInfo) :-
    saved_vars_in_goal(Goal0, Goal, !SlotInfo),
    saved_vars_in_independent_goals(Goals0, Goals, !SlotInfo).

:- pred saved_vars_in_switch(list(case)::in, list(case)::out,
    slot_info::in, slot_info::out) is det.

saved_vars_in_switch([], [], !SlotInfo).
saved_vars_in_switch([Case0 | Cases0], [Case | Cases], !SlotInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    saved_vars_in_goal(Goal0, Goal, !SlotInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    saved_vars_in_switch(Cases0, Cases, !SlotInfo).

%-----------------------------------------------------------------------------%

:- type slot_info
    --->    slot_info(
                var_table,
                rtti_varmaps,
                bool            % TypeInfoLiveness
            ).

:- pred init_slot_info(var_table::in, rtti_varmaps::in, bool::in,
    slot_info::out) is det.

init_slot_info(VarTable, RttiVarMaps, TypeInfoLiveness, SlotInfo) :-
    SlotInfo = slot_info(VarTable, RttiVarMaps, TypeInfoLiveness).

:- pred final_slot_info(slot_info::in, var_table::out, rtti_varmaps::out)
    is det.

final_slot_info(SlotInfo, VarTable, RttiVarMaps) :-
    SlotInfo = slot_info(VarTable, RttiVarMaps, _).

:- pred saved_vars_rename_var(prog_var::in, prog_var::out,
    map(prog_var, prog_var)::out, slot_info::in, slot_info::out) is det.

saved_vars_rename_var(Var, NewVar, Substitution, !SlotInfo) :-
    !.SlotInfo = slot_info(VarTable0, RttiVarMaps0, TypeInfoLiveness),
    lookup_var_entry(VarTable0, Var, Entry0),
    Entry0 = vte(_N, Type, IsDummy),
    Entry = vte("", Type, IsDummy),
    add_var_entry(Entry, NewVar, VarTable0, VarTable),
    map.from_assoc_list([Var - NewVar], Substitution),
    rtti_var_info_duplicate(Var, NewVar, RttiVarMaps0, RttiVarMaps),
    !:SlotInfo = slot_info(VarTable, RttiVarMaps, TypeInfoLiveness).

    % Check whether it is ok to duplicate a given variable according
    % to the information in the slot_info. If TypeInfoLiveness is set,
    % it is possible that liveness.m will want to refer to the rtti_varmaps
    % to calculate which type_infos are live (see the comments at the top
    % of liveness.m). If we duplicated any type_info variables here,
    % then this could cause problems because the rtti_varmaps would not
    % be able to be kept consistent. Therefore we don't allow type_infos
    % to be duplicated when TypeInfoLiveness is set.
    %
:- pred slot_info_do_not_duplicate_var(slot_info::in, prog_var::in) is semidet.

slot_info_do_not_duplicate_var(SlotInfo, Var) :-
    SlotInfo = slot_info(VarTable, _, TypeInfoLiveness),
    TypeInfoLiveness = yes,
    lookup_var_type(VarTable, Var, Type),
    type_is_type_info_or_ctor_type(Type).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.saved_vars.
%-----------------------------------------------------------------------------%

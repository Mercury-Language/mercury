%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: add_heap_ops.m.
% Author: fjh.
%
% This module is an HLDS-to-HLDS transformation that inserts code to
% handle heap reclamation on backtracking, by saving and restoring
% the values of the heap pointer.
% The transformation involves adding calls to impure predicates defined in
% library/private_builtin.m, which in turn call the MR_mark_hp() and
% MR_restore_hp() macros defined in runtime/mercury_heap.h.
%
% This pass is currently only used for the MLDS back-end. For historical
% reasons, the LLDS back-end inserts the heap operations as it is generating
% LLDS code, rather than via an HLDS to HLDS transformation.
%
% This module is very similar to add_trail_ops.m.
%
%-----------------------------------------------------------------------------%
%
% XXX check goal_infos for correctness
%
%-----------------------------------------------------------------------------%

:- module ml_backend.add_heap_ops.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- pred add_heap_ops(module_info::in, proc_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.code_model.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % As we traverse the goal, we add new variables to hold the saved values
    % of the heap pointer. So we need the varset and the vartypes map to record
    % the names and types of the new variables.
    %
    % We also keep the module_info around, so that we can use the predicate
    % table that it contains to lookup the pred_ids for the builtin procedures
    % that we insert calls to. We do not update the module_info as we are
    % traversing the goal.
    %
:- type heap_ops_info
    --->    heap_ops_info(
                heap_varset         :: prog_varset,
                heap_var_types      :: vartypes,
                heap_module_info    :: module_info
            ).

add_heap_ops(ModuleInfo0, !Proc) :-
    proc_info_get_goal(!.Proc, Goal0),
    proc_info_get_varset(!.Proc, VarSet0),
    proc_info_get_vartypes(!.Proc, VarTypes0),
    TrailOpsInfo0 = heap_ops_info(VarSet0, VarTypes0, ModuleInfo0),
    goal_add_heap_ops(Goal0, Goal, TrailOpsInfo0, TrailOpsInfo),
    TrailOpsInfo = heap_ops_info(VarSet, VarTypes, _),
    proc_info_set_goal(Goal, !Proc),
    proc_info_set_varset(VarSet, !Proc),
    proc_info_set_vartypes(VarTypes, !Proc),
    % The code below does not maintain the non-local variables,
    % so we need to requantify.
    % XXX it would be more efficient to maintain them rather than recomputing
    % them every time.
    requantify_proc_general(ordinary_nonlocals_no_lambda, !Proc).

:- pred goal_add_heap_ops(hlds_goal::in, hlds_goal::out,
    heap_ops_info::in, heap_ops_info::out) is det.

goal_add_heap_ops(Goal0, Goal, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    goal_expr_add_heap_ops(GoalExpr0, GoalInfo, Goal, !Info).

:- pred goal_expr_add_heap_ops(hlds_goal_expr::in, hlds_goal_info::in,
    hlds_goal::out, heap_ops_info::in, heap_ops_info::out) is det.

goal_expr_add_heap_ops(GoalExpr0, GoalInfo0, Goal, !Info) :-
    (
        GoalExpr0 = conj(ConjType, Goals0),
        conj_add_heap_ops(Goals0, Goals, !Info),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Disjuncts0),
        (
            Disjuncts0 = [],
            GoalExpr = GoalExpr0
        ;
            Disjuncts0 = [FirstDisjunct0 | _],
            Context = goal_info_get_context(GoalInfo0),
            CodeModel = goal_info_get_code_model(GoalInfo0),

            % If necessary, save the heap pointer so that we can restore it
            % on back-tracking. We don't need to do this here if it is a
            % model_det or model_semi disjunction and the first disjunct
            % won't allocate any heap -- in that case, we delay saving the heap
            % pointer until just before the first disjunct that might allocate
            % heap.
            ( if
                ( CodeModel = model_non
                ; goal_may_allocate_heap(FirstDisjunct0)
                )
            then
                new_saved_hp_var(SavedHeapPointerVar, !Info),
                gen_mark_hp(SavedHeapPointerVar, Context, MarkHeapPointerGoal,
                    !Info),
                disj_add_heap_ops(Disjuncts0, Disjuncts, is_first_disjunct,
                    yes(SavedHeapPointerVar), GoalInfo0, !Info),
                DisjGoalExpr = disj(Disjuncts),
                DisjGoal = hlds_goal(DisjGoalExpr, GoalInfo0),
                ConjGoalExpr = conj(plain_conj,
                    [MarkHeapPointerGoal, DisjGoal]),
                ConjGoal = hlds_goal(ConjGoalExpr, GoalInfo0),
                Purity0 = goal_info_get_purity(GoalInfo0),
                GoalExpr = scope(promise_purity(Purity0), ConjGoal)
            else
                disj_add_heap_ops(Disjuncts0, Disjuncts, is_first_disjunct,
                    no, GoalInfo0, !Info),
                GoalExpr = disj(Disjuncts)
            )
        ),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        cases_add_heap_ops(Cases0, Cases, !Info),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(InnerGoal),
        OuterGoalInfo = GoalInfo0,
        % We handle negations by converting them into if-then-elses:
        %   not(G)  ===>  (if G then fail else true)

        Context = goal_info_get_context(OuterGoalInfo),
        InnerGoal = hlds_goal(_, InnerGoalInfo),
        Determinism = goal_info_get_determinism(InnerGoalInfo),
        determinism_components(Determinism, _CanFail, NumSolns),
        True = true_goal_with_context(Context),
        Fail = fail_goal_with_context(Context),
        ModuleInfo = !.Info ^ heap_module_info,
        (
            NumSolns = at_most_zero,
            % The "then" part of the if-then-else will be unreachable, but to
            % preserve the invariants that the MLDS back-end relies on, we
            % need to make sure that it can't fail. So we use a call to
            % `private_builtin.unused' (which will call error/1) rather than
            % `fail' for the "then" part.
            heap_generate_call("unused", detism_det, purity_pure, [],
                instmap_delta_bind_no_var, ModuleInfo, Context, ThenGoal)
        ;
            ( NumSolns = at_most_one
            ; NumSolns = at_most_many
            ; NumSolns = at_most_many_cc
            ),
            ThenGoal = Fail
        ),
        NewOuterGoal = if_then_else([], InnerGoal, ThenGoal, True),
        goal_expr_add_heap_ops(NewOuterGoal, OuterGoalInfo, Goal, !Info)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            SubGoal = SubGoal0
        else
            goal_add_heap_ops(SubGoal0, SubGoal, !Info)
        ),
        GoalExpr = scope(Reason, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, CondGoal0, ThenGoal0, ElseGoal0),
        goal_add_heap_ops(CondGoal0, CondGoal, !Info),
        goal_add_heap_ops(ThenGoal0, ThenGoal, !Info),
        goal_add_heap_ops(ElseGoal0, ElseGoal1, !Info),

        % If the condition can allocate heap space, save the heap pointer
        % so that we can restore it if the condition fails.
        ( if goal_may_allocate_heap(CondGoal0) then
            new_saved_hp_var(SavedHeapPointerVar, !Info),
            Context = goal_info_get_context(GoalInfo0),
            gen_mark_hp(SavedHeapPointerVar, Context, MarkHeapPointerGoal,
                !Info),

            % Generate code to restore the heap pointer, and insert that code
            % at the start of the Else branch.
            gen_restore_hp(SavedHeapPointerVar, Context,
                RestoreHeapPointerGoal, !Info),
            ElseGoal1 = hlds_goal(_, ElseGoal1Info),
            ElseGoalExpr = conj(plain_conj,
                [RestoreHeapPointerGoal, ElseGoal1]),
            ElseGoal = hlds_goal(ElseGoalExpr, ElseGoal1Info),
            ITEGoalExpr = if_then_else(Vars, CondGoal, ThenGoal, ElseGoal),
            ITEGoal = hlds_goal(ITEGoalExpr, GoalInfo0),
            ConjGoalExpr = conj(plain_conj, [MarkHeapPointerGoal, ITEGoal]),
            ConjGoal = hlds_goal(ConjGoalExpr, GoalInfo0),
            Purity0 = goal_info_get_purity(GoalInfo0),
            GoalExpr = scope(promise_purity(Purity0), ConjGoal)
        else
            GoalExpr = if_then_else(Vars, CondGoal, ThenGoal, ElseGoal1)
        ),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ),
        Goal = hlds_goal(GoalExpr0, GoalInfo0)
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _),
        Goal = hlds_goal(GoalExpr0, GoalInfo0)
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

:- pred conj_add_heap_ops(hlds_goals::in, hlds_goals::out,
    heap_ops_info::in, heap_ops_info::out) is det.

conj_add_heap_ops(Goals0, Goals, !Info) :-
    list.map_foldl(goal_add_heap_ops, Goals0, Goals, !Info).

:- pred disj_add_heap_ops(list(hlds_goal)::in, list(hlds_goal)::out,
    is_first_disjunct::in, maybe(prog_var)::in, hlds_goal_info::in,
    heap_ops_info::in, heap_ops_info::out) is det.

disj_add_heap_ops([], [], _, _, _, !Info).
disj_add_heap_ops([Goal0 | Goals0], DisjGoals, IsFirstBranch,
        MaybeSavedHeapPointerVar, DisjGoalInfo, !Info) :-
    goal_add_heap_ops(Goal0, Goal1, !Info),
    Goal1 = hlds_goal(_, GoalInfo),
    Context = goal_info_get_context(GoalInfo),

    % If needed, reset the heap pointer before executing the goal,
    % to reclaim heap space allocated in earlier branches.
    ( if
        IsFirstBranch = is_not_first_disjunct,
        MaybeSavedHeapPointerVar = yes(SavedHeapPointerVar0)
    then
        gen_restore_hp(SavedHeapPointerVar0, Context, RestoreHeapPointerGoal,
            !Info),
        conj_list_to_goal([RestoreHeapPointerGoal, Goal1], GoalInfo, Goal)
    else
        Goal = Goal1
    ),

    % Save the heap pointer,
    % - if we haven't already done so,
    % - if this disjunct might allocate heap space, and
    % - if a next disjunct exists to give us a chance to recover
    %   that heap space.
    ( if
        MaybeSavedHeapPointerVar = no,
        goal_may_allocate_heap(Goal),
        Goals0 = [_ | _]
    then
        % Generate code to save the heap pointer.
        new_saved_hp_var(SavedHeapPointerVar, !Info),
        gen_mark_hp(SavedHeapPointerVar, Context, MarkHeapPointerGoal, !Info),

        % Recursively handle the remaining disjuncts.
        disj_add_heap_ops(Goals0, Goals1, is_not_first_disjunct,
            yes(SavedHeapPointerVar), DisjGoalInfo, !Info),
        % Put this disjunct and the remaining disjuncts in a nested
        % disjunction, so that the heap pointer variable can scope over
        % these disjuncts. (This wouldn't work if Goals0 were [].)
        InnerDisjGoalExpr = disj([Goal | Goals1]),
        InnerDisjGoal = hlds_goal(InnerDisjGoalExpr, DisjGoalInfo),
        ConjGoalExpr = conj(plain_conj, [MarkHeapPointerGoal, InnerDisjGoal]),
        ConjGoal = hlds_goal(ConjGoalExpr, DisjGoalInfo),
        Purity = goal_info_get_purity(DisjGoalInfo),
        ScopeGoalExpr = scope(promise_purity(Purity), ConjGoal),
        ScopeGoal = hlds_goal(ScopeGoalExpr, DisjGoalInfo),
        DisjGoals = [ScopeGoal]
    else
        % Just recursively handle the remaining disjuncts.
        disj_add_heap_ops(Goals0, Goals, is_not_first_disjunct,
            MaybeSavedHeapPointerVar, DisjGoalInfo, !Info),
        DisjGoals = [Goal | Goals]
    ).

:- pred cases_add_heap_ops(list(case)::in, list(case)::out,
    heap_ops_info::in, heap_ops_info::out) is det.

cases_add_heap_ops([], [], !Info).
cases_add_heap_ops([Case0 | Cases0], [Case | Cases], !Info) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    goal_add_heap_ops(Goal0, Goal, !Info),
    Case = case(MainConsId, OtherConsIds, Goal),
    cases_add_heap_ops(Cases0, Cases, !Info).

%-----------------------------------------------------------------------------%

:- pred gen_mark_hp(prog_var::in, prog_context::in, hlds_goal::out,
    heap_ops_info::in, heap_ops_info::out) is det.

gen_mark_hp(SavedHeapPointerVar, Context, MarkHeapPointerGoal, !Info) :-
    heap_generate_call("mark_hp", detism_det, purity_impure,
        [SavedHeapPointerVar], instmap_delta_bind_var(SavedHeapPointerVar),
        !.Info ^ heap_module_info, Context, MarkHeapPointerGoal).

:- pred gen_restore_hp(prog_var::in, prog_context::in, hlds_goal::out,
    heap_ops_info::in, heap_ops_info::out) is det.

gen_restore_hp(SavedHeapPointerVar, Context, RestoreHeapPointerGoal, !Info) :-
    heap_generate_call("restore_hp", detism_det, purity_impure,
        [SavedHeapPointerVar], instmap_delta_bind_no_var,
        !.Info ^ heap_module_info, Context, RestoreHeapPointerGoal).

%-----------------------------------------------------------------------------%

:- pred new_saved_hp_var(prog_var::out,
    heap_ops_info::in, heap_ops_info::out) is det.

new_saved_hp_var(Var, !Info) :-
    new_var("HeapPointer", heap_pointer_type, Var, !Info).

:- pred new_var(string::in, mer_type::in, prog_var::out,
    heap_ops_info::in, heap_ops_info::out) is det.

new_var(Name, Type, Var, !Info) :-
    VarSet0 = !.Info ^ heap_varset,
    VarTypes0 = !.Info ^ heap_var_types,
    varset.new_named_var(Name, Var, VarSet0, VarSet),
    add_var_type(Var, Type, VarTypes0, VarTypes),
    !Info ^ heap_varset := VarSet,
    !Info ^ heap_var_types := VarTypes.

%-----------------------------------------------------------------------------%

:- pred heap_generate_call(string::in, determinism::in, purity::in,
    list(prog_var)::in, instmap_delta::in, module_info::in,
    term.context::in, hlds_goal::out) is det.

heap_generate_call(PredName, Detism, Purity, ArgVars, InstMapDelta, ModuleInfo,
        Context, CallGoal) :-
    generate_simple_call(ModuleInfo, mercury_private_builtin_module,
        PredName, pf_predicate, only_mode, Detism, Purity, ArgVars, [],
        InstMapDelta, Context, CallGoal).

%-----------------------------------------------------------------------------%
:- end_module ml_backend.add_heap_ops.
%-----------------------------------------------------------------------------%

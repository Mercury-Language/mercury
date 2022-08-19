%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This module is intended to help implement the transition
% from the original mode analysis algorithm implemented by modes.m
% and related modules to the new propagation based constraint solver.
% It is intended to represent an interface between mode analysis
% on the one hand, and the rest of the compiler on the other hand,
% that is sufficiently abstract that it could be implemented on top of
% both mode analysis systems. Initially, it will be implemented
% on top of the old mode analysis system. Once the rest of the compiler
% is transitioned to use this interface, we will transition its
% implementation to the propagation based constraint solver.
%
%-----------------------------------------------------------------------------%

:- module hlds.goal_mode.
:- interface.

:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.var_table.

:- import_module list.

%-----------------------------------------------------------------------------%
%
% The goal_mode type, and its initialization.
%

    % The representation of the bindings that a goal makes to the variables
    % of a goal.
    %
:- type goal_mode.

    % Create a dummy goal_mode to put into goals before mode analysis is run.
    %
:- func make_dummy_goal_mode = goal_mode.

%-----------------------------------------------------------------------------%
%
% Tests on goal modes.
%
% A pass can call these modes if it has ensured that the goal_modes
% in goal_infos are up to date. It can do that by invoking
% compute_goal_modes_in_module when it starts.
%

% There are no tests on goal_modes as yet.

%-----------------------------------------------------------------------------%
%
% Misc utility operations on goal modes.
%

    % rename_vars_in_goal_mode(Must, Subn, !GoalMode):
    %
    % Apply the given substitution to !GoalMode.
    %
:- pred rename_vars_in_goal_mode(must_rename::in, prog_var_renaming::in,
    goal_mode::in, goal_mode::out) is det.

    % dump_goal_mode(Prefix, VarTable, GoalMode) = Strs:
    %
    % Return a representation of the given GoalMode that is suitable
    % for use in a HLDS dump. Each Str in Strs should be the contents
    % of a line, including the final newline. Every line should start
    % with Prefix.
    %
:- func dump_goal_mode(string, var_table, goal_mode) = list(string).

%-----------------------------------------------------------------------------%
%
% Filling in the goal mode fields.
%

    % Fill in the goal_mode fields of every goal in every valid procedure
    % in every valid predicate in the module.
    %
    % At the moment, this filling in is done using the information contained
    % in the goals' instmap_deltas, so this predicate may be called
    % only after the original mode checker has been run.
    %
:- pred compute_goal_modes_in_module(module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_test.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.set_of_var.

:- import_module map.
:- import_module require.
:- import_module set_tree234.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type goal_mode
    --->    gm_unreachable
    ;       gm_reachable(
                % The set of variables bound (i.e. further instantiated)
                % by the goal.
                %
                % The values of the _grounded and _not_grounded fields
                % will always partition the value of this field.
                gm_more_bound               :: set_of_progvar,

                % The subset of gm_more_bound whose final instantiation state
                % is an inst representing a ground term (an inst describing
                % a term containing no variables; *not* just the inst `ground'
                % itself).
                gm_more_bound_grounded      :: set_of_progvar,

                % The subset of gm_more_bound whose final instantiation state
                % is an inst that describes terms that *do* contain variables.
                gm_more_bound_not_grounded  :: set_of_progvar
            ).

make_dummy_goal_mode = gm_unreachable.

%-----------------------------------------------------------------------------%

rename_vars_in_goal_mode(Must, Subn, !GoalMode) :-
    (
        !.GoalMode = gm_unreachable
    ;
        !.GoalMode = gm_reachable(Bound0, BoundGrounded0, BoundNonGrounded0),
        rename_vars_in_set_of_var(Must, Subn, Bound0, Bound),
        rename_vars_in_set_of_var(Must, Subn, BoundGrounded0, BoundGrounded),
        rename_vars_in_set_of_var(Must, Subn, BoundNonGrounded0,
            BoundNonGrounded),
        !:GoalMode = gm_reachable(Bound, BoundGrounded, BoundNonGrounded)
    ).

dump_goal_mode(PrefixStr, VarTable, GoalMode) = !:DumpStrs :-
    (
        GoalMode = gm_unreachable,
        !:DumpStrs = [PrefixStr ++ "gm_unreachable\n"]
    ;
        GoalMode = gm_reachable(_Bound, BoundGrounded, BoundNonGrounded),
        % We don't need to dump _Bound, because _Bound is the union of
        % BoundGrounded and BoundNonGrounded.
        BoundGroundedVars = set_of_var.to_sorted_list(BoundGrounded),
        BoundNonGroundedVars = set_of_var.to_sorted_list(BoundNonGrounded),
        ( if BoundGroundedVars = [], BoundNonGroundedVars = [] then
            !:DumpStrs = [PrefixStr ++ "gm_reachable, no vars bound\n"]
        else
            % The gm_reachable part is implicit in the presence of
            % one or both of the following lines.
            (
                BoundNonGroundedVars = [],
                !:DumpStrs = []
            ;
                BoundNonGroundedVars = [_ | _],
                NGVarsStr = mercury_vars_to_string(VarTable,
                    print_name_and_num, BoundNonGroundedVars),
                string.format("%sbound but not grounded:%s\n",
                    [s(PrefixStr), s(NGVarsStr)], NonGroundedStr),
                !:DumpStrs = [NonGroundedStr]
            ),
            (
                BoundGroundedVars = []
            ;
                BoundGroundedVars = [_ | _],
                GVarsStr = mercury_vars_to_string(VarTable,
                    print_name_and_num, BoundNonGroundedVars),
                string.format("%sbound and grounded:%s\n",
                    [s(PrefixStr), s(GVarsStr)], GroundedStr),
                !:DumpStrs = [GroundedStr | !.DumpStrs]
            )
        )
    ).

%-----------------------------------------------------------------------------%

compute_goal_modes_in_module(!ModuleInfo) :-
    module_info_get_valid_pred_id_set(!.ModuleInfo, ValidPredIds),
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    map.map_values(compute_goal_modes_in_pred(!.ModuleInfo, ValidPredIds),
        PredIdTable0, PredIdTable),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo).

:- pred compute_goal_modes_in_pred(module_info::in, set_tree234(pred_id)::in,
    pred_id::in, pred_info::in, pred_info::out) is det.

compute_goal_modes_in_pred(ModuleInfo, ValidPredIds, PredId, !PredInfo) :-
    ( if set_tree234.member(PredId, ValidPredIds) then
        pred_info_get_proc_table(!.PredInfo, ProcTable0),
        map.map_values_only(compute_goal_modes_in_proc(ModuleInfo),
            ProcTable0, ProcTable),
        pred_info_set_proc_table(ProcTable, !PredInfo)
    else
        true
    ).

:- pred compute_goal_modes_in_proc(module_info::in,
    proc_info::in, proc_info::out) is det.

compute_goal_modes_in_proc(ModuleInfo, !ProcInfo) :-
    ( if proc_info_is_valid_mode(!.ProcInfo) then
        proc_info_get_var_table(!.ProcInfo, VarTable),
        proc_info_get_initial_instmap(ModuleInfo, !.ProcInfo, InstMap0),
        proc_info_get_goal(!.ProcInfo, Goal0),
        compute_goal_modes_in_goal(ModuleInfo, VarTable, InstMap0, _InstMap,
            Goal0, Goal),
        proc_info_set_goal(Goal, !ProcInfo)
    else
        true
    ).

:- pred compute_goal_modes_in_goal(module_info::in, var_table::in,
    instmap::in, instmap::out, hlds_goal::in, hlds_goal::out) is det.

compute_goal_modes_in_goal(ModuleInfo, VarTable, InstMap0, InstMap,
        Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo0),
    (
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = conj(ConjType, Conjuncts0),
        compute_goal_modes_in_conj(ModuleInfo, VarTable, InstMap0,
            Conjuncts0, Conjuncts),
        GoalExpr = conj(ConjType, Conjuncts)
    ;
        GoalExpr0 = disj(Disjuncts0),
        compute_goal_modes_in_disj(ModuleInfo, VarTable, InstMap0,
            Disjuncts0, Disjuncts),
        GoalExpr = disj(Disjuncts)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        compute_goal_modes_in_switch(ModuleInfo, VarTable, InstMap0,
            Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = if_then_else(Vars, CondGoal0, ThenGoal0, ElseGoal0),
        compute_goal_modes_in_goal(ModuleInfo, VarTable, InstMap0, InstMapCond,
            CondGoal0, CondGoal),
        compute_goal_modes_in_goal(ModuleInfo, VarTable, InstMapCond, _,
            ThenGoal0, ThenGoal),
        compute_goal_modes_in_goal(ModuleInfo, VarTable, InstMap0, _,
            ElseGoal0, ElseGoal),
        GoalExpr = if_then_else(Vars, CondGoal, ThenGoal, ElseGoal)
    ;
        GoalExpr0 = negation(SubGoal0),
        compute_goal_modes_in_goal(ModuleInfo, VarTable, InstMap0, _,
            SubGoal0, SubGoal),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        compute_goal_modes_in_goal(ModuleInfo, VarTable, InstMap0, _,
            SubGoal0, SubGoal),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = shorthand(Shorthand0),
        (
            Shorthand0 = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        ;
            Shorthand0 = try_goal(MaybeIOVars, ResultVar, SubGoal0),
            compute_goal_modes_in_goal(ModuleInfo, VarTable, InstMap0, _,
                SubGoal0, SubGoal),
            Shorthand = try_goal(MaybeIOVars, ResultVar, SubGoal)
        ;
            Shorthand0 = atomic_goal(AtomicGoalType, OuterVars, InnerVars,
                OutputVars, MainGoal0, OrElseGoals0, OrElseInners),
            compute_goal_modes_in_goal(ModuleInfo, VarTable, InstMap0, _,
                MainGoal0, MainGoal),
            compute_goal_modes_in_disj(ModuleInfo, VarTable, InstMap0,
                OrElseGoals0, OrElseGoals),
            Shorthand = atomic_goal(AtomicGoalType, OuterVars, InnerVars,
                OutputVars, MainGoal, OrElseGoals, OrElseInners)
        ),
        GoalExpr = shorthand(Shorthand)
    ),
    apply_instmap_delta(InstMapDelta, InstMap0, InstMap),
    compute_goal_mode(ModuleInfo, VarTable, InstMapDelta, InstMap0, InstMap,
        GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred compute_goal_modes_in_conj(module_info::in, var_table::in,
    instmap::in, list(hlds_goal)::in, list(hlds_goal)::out) is det.

compute_goal_modes_in_conj(_, _, _, [], []).
compute_goal_modes_in_conj(ModuleInfo, VarTable, !.InstMap,
        [Conjunct0 | Conjuncts0], [Conjunct | Conjuncts]) :-
    compute_goal_modes_in_goal(ModuleInfo, VarTable, !InstMap,
        Conjunct0, Conjunct),
    compute_goal_modes_in_conj(ModuleInfo, VarTable, !.InstMap,
        Conjuncts0, Conjuncts).

:- pred compute_goal_modes_in_disj(module_info::in, var_table::in,
    instmap::in, list(hlds_goal)::in, list(hlds_goal)::out) is det.

compute_goal_modes_in_disj(_, _, _, [], []).
compute_goal_modes_in_disj(ModuleInfo, VarTable, InstMap0,
        [Disjunct0 | Disjuncts0], [Disjunct | Disjuncts]) :-
    compute_goal_modes_in_goal(ModuleInfo, VarTable, InstMap0, _InstMap,
        Disjunct0, Disjunct),
    compute_goal_modes_in_disj(ModuleInfo, VarTable, InstMap0,
        Disjuncts0, Disjuncts).

:- pred compute_goal_modes_in_switch(module_info::in, var_table::in,
    instmap::in, list(case)::in, list(case)::out) is det.

compute_goal_modes_in_switch(_, _, _, [], []).
compute_goal_modes_in_switch(ModuleInfo, VarTable, InstMap0,
        [Case0 | Cases0], [Case | Cases]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    compute_goal_modes_in_goal(ModuleInfo, VarTable, InstMap0, _InstMap,
        Goal0, Goal),
    Case = case(MainConsId, OtherConsIds, Goal),
    compute_goal_modes_in_switch(ModuleInfo, VarTable, InstMap0,
        Cases0, Cases).

%-----------------------------------------------------------------------------%

:- pred compute_goal_mode(module_info::in, var_table::in,
    instmap_delta::in, instmap::in, instmap::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

compute_goal_mode(ModuleInfo, VarTable, InstMapDelta, InstMap0, InstMap,
        !GoalInfo) :-
    ( if instmap_delta_is_reachable(InstMapDelta) then
        instmap_delta_changed_vars(InstMapDelta, Vars),
        BoundVars0 = set_of_var.init,
        BoundGroundedVars0 = set_of_var.init,
        BoundNonGroundedVars0 = set_of_var.init,
        list.foldl3(
            record_var_if_bound(ModuleInfo, VarTable, InstMap0, InstMap),
            set_of_var.to_sorted_list(Vars),
            BoundVars0, BoundVars,
            BoundGroundedVars0, BoundGroundedVars,
            BoundNonGroundedVars0, BoundNonGroundedVars),
        GoalMode = gm_reachable(BoundVars,
            BoundGroundedVars, BoundNonGroundedVars)
    else
        GoalMode = gm_unreachable
    ),
    goal_info_set_goal_mode(GoalMode, !GoalInfo).

:- pred record_var_if_bound(module_info::in, var_table::in,
    instmap::in, instmap::in, prog_var::in,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out,
    set_of_progvar::in, set_of_progvar::out) is det.

record_var_if_bound(ModuleInfo, VarTable, InstMap0, InstMap, Var,
        !BoundVars, !BoundGroundedVars, !BoundNonGroundedVars) :-
    instmap_lookup_var(InstMap0, Var, Inst0),
    instmap_lookup_var(InstMap, Var, Inst),
    lookup_var_type(VarTable, Var, Type),
    ( if inst_matches_final_typed(ModuleInfo, Type, Inst0, Inst) then
        true
    else
        set_of_var.insert(Var, !BoundVars),
        ( if inst_is_ground(ModuleInfo, Inst) then
            set_of_var.insert(Var, !BoundGroundedVars)
        else
            set_of_var.insert(Var, !BoundNonGroundedVars)
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.goal_mode.
%-----------------------------------------------------------------------------%

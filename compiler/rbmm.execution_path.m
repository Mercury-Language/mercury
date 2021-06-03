%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File rbmm.execution_path.m.
% Main author: Quan Phan.
%
% This module collects all execution paths (ExecPath) of procedures.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.rbmm.execution_path.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module transform_hlds.rbmm.region_liveness_info.

    % Collects execution paths for each procedure.
    %
:- pred execution_path_analysis(module_info::in, execution_path_table::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_form.
:- import_module hlds.goal_path.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.smm_common.

:- import_module pair.
:- import_module require.
:- import_module list.
:- import_module map.

%-----------------------------------------------------------------------------%
%
% Execution path analysis
%

execution_path_analysis(ModuleInfo, ExecPathTable) :-
    module_info_get_valid_pred_ids(ModuleInfo, PredIds),
    map.init(ExecPathTable0),
    list.foldl(execution_path_analysis_pred(ModuleInfo), PredIds,
        ExecPathTable0, ExecPathTable).

:- pred execution_path_analysis_pred(module_info::in, pred_id::in,
    execution_path_table::in, execution_path_table::out)
    is det.

execution_path_analysis_pred(ModuleInfo, PredId, !ExecPathTable) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_valid_non_imported_procids(PredInfo),
    list.foldl(execution_path_analysis_proc(ModuleInfo, PredId), ProcIds,
        !ExecPathTable).

:- pred execution_path_analysis_proc(module_info::in, pred_id::in,
    proc_id::in, execution_path_table::in, execution_path_table::out) is det.

execution_path_analysis_proc(ModuleInfo, PredId, ProcId, !ExecPathTable) :-
    PPId = proc(PredId, ProcId),
    ( if some_are_special_preds([PPId], ModuleInfo) then
        true
    else
        module_info_proc_info(ModuleInfo, PPId, ProcInfo),
        compute_execution_paths(ProcInfo, ModuleInfo, ExecPaths),
        map.set(PPId, ExecPaths, !ExecPathTable)
    ).

    % Compute all execution paths in the procedure.
    %
:- pred compute_execution_paths(proc_info::in, module_info::in,
    list(execution_path)::out) is det.

compute_execution_paths(ProcInfo0, ModuleInfo, ExecPaths) :-
    % Fill the goals with program point information.
    fill_goal_path_slots_in_proc(ModuleInfo, ProcInfo0, ProcInfo),
    proc_info_get_goal(ProcInfo, Goal),
    ExecPaths0 = [[]],
    execution_paths_covered_goal(ProcInfo, Goal, ExecPaths0, ExecPaths).

    % Extend the given execution paths to cover this goal.
    %
:- pred execution_paths_covered_goal(proc_info::in, hlds_goal::in,
    list(execution_path)::in, list(execution_path)::out) is det.

execution_paths_covered_goal(ProcInfo, Goal, !ExecPaths) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    HasSubGoals = goal_expr_has_subgoals(GoalExpr),
    (
        HasSubGoals = does_not_have_subgoals,
        ( if
            ( GoalExpr = unify(_, _, _, _, _)
            ; GoalExpr = plain_call(_, _, _, _, _, _)
            ; GoalExpr = conj(_ConjType, [])
            ; GoalExpr = disj([])
            )
        then
            % Retrieve the program point of this goal.
            ProgPoint = program_point_init(GoalInfo),
            append_to_each_execution_path(!.ExecPaths,
                [[pair(ProgPoint, Goal)]], !:ExecPaths)
        else
            % XXX: other kinds of atomic calls (generic_call,
            % foreign_proc), TEMPORARILY ignored their corresponding pps.
            % XXX: handle event_call and unsafe_cast generic_calls
            append_to_each_execution_path(!.ExecPaths, [[]], !:ExecPaths)
        )
    ;
        HasSubGoals = has_subgoals,
        execution_paths_covered_compound_goal(ProcInfo, Goal, !ExecPaths)
    ).

    % Extend current execution paths to cover this compound goal.
    %
:- pred execution_paths_covered_compound_goal(proc_info::in, hlds_goal::in,
    list(execution_path)::in, list(execution_path)::out) is det.

execution_paths_covered_compound_goal(ProcInfo, CompoundGoal, !ExecPaths) :-
    CompoundGoal = hlds_goal(Expr, _),
    (
        Expr = conj(_ConjType, [Conj | Conjs]),
        execution_paths_covered_conj(ProcInfo, [Conj | Conjs], !ExecPaths)
    ;
        Expr = switch(_, _, Cases),
        execution_paths_covered_cases(ProcInfo, CompoundGoal, Cases,
            !ExecPaths)
    ;
        Expr = disj([Disj | Disjs]),
        execution_paths_covered_disj(ProcInfo, [Disj | Disjs],
            !ExecPaths)
    ;
        Expr = negation(Goal),
        execution_paths_covered_goal(ProcInfo, Goal, !ExecPaths)
    ;
        Expr = scope(_, Goal),
        % XXX We should special-case the handling of from_ground_term_construct
        % scopes.
        execution_paths_covered_goal(ProcInfo, Goal, !ExecPaths)
    ;
        Expr = if_then_else(_V, Cond, Then, Else),
        execution_paths_covered_goal(ProcInfo, Cond,
            !.ExecPaths, ExecPathsCond),
        execution_paths_covered_goal(ProcInfo, Then,
            ExecPathsCond, ExecPathsCondThen),
        execution_paths_covered_goal(ProcInfo, Else,
            !.ExecPaths, ExecPathsElse),
        !:ExecPaths = ExecPathsCondThen ++ ExecPathsElse
    ;
        ( Expr = unify(_, _, _, _, _)
        ; Expr = plain_call(_, _, _, _, _, _)
        ; Expr = conj(_, [])
        ; Expr = disj([])
        ; Expr = call_foreign_proc(_, _, _, _, _, _, _)
        ; Expr = generic_call(_, _, _, _, _)
        ; Expr = shorthand(_)
        ),
        unexpected($pred, "encountered atomic or unsupported goal")
    ).

    % Extend execution paths to cover the goals in this conjunction.
    %
:- pred execution_paths_covered_conj(proc_info::in, list(hlds_goal)::in,
    list(execution_path)::in, list(execution_path)::out) is det.

execution_paths_covered_conj(_, [], !ExecPaths).
execution_paths_covered_conj(ProcInfo, [Conj | Conjs], !ExecPaths) :-
    execution_paths_covered_goal(ProcInfo, Conj, !ExecPaths),
    execution_paths_covered_conj(ProcInfo, Conjs, !ExecPaths).

    % Extend execution paths to cover a disjunction.
    % To do this we extend the execution paths from the beginning of the
    % disjunction for each disjunct to obtain a set of execution paths
    % for each disjuct.  At the end of the disjunction we combine these.
    %
:- pred execution_paths_covered_disj(proc_info::in, list(hlds_goal)::in,
    list(execution_path)::in, list(execution_path)::out) is det.

execution_paths_covered_disj(_, [], _, []).
execution_paths_covered_disj(ProcInfo, [Disj | Disjs], !ExecPaths) :-
    execution_paths_covered_goal(ProcInfo, Disj, !.ExecPaths,
        ExecPathsDisj),
    execution_paths_covered_disj(ProcInfo, Disjs, !.ExecPaths,
        ExecPathsDisjs),
    !:ExecPaths = ExecPathsDisj ++ ExecPathsDisjs.

    % Extend execution paths to cover a switch.
    % Switches are handled like disjunctions except that unifications
    % involving the switch var sometimes need special handling.
    % Unifications between the switch vars and a constant or a functor
    % of arity zero are not explicitly present in the goal.  This causes
    % the execution paths to have fewer program points than they should.
    % If this happens we need to add a program point for the removed
    % unification.  The goal corresponding to this introduced program
    % point is the switch goal itself.  This is so that we can get
    % information about the switch var in the live variable analysis.
    %
:- pred execution_paths_covered_cases(proc_info::in, hlds_goal::in,
    list(case)::in, list(execution_path)::in, list(execution_path)::out)
    is det.

execution_paths_covered_cases(_, _, [], _, []).
execution_paths_covered_cases(ProcInfo, Switch, [Case | Cases], !ExecPaths) :-
    Case = case(MainConsId, OtherConsIds, CaseGoal),
    expect(unify(OtherConsIds, []), $pred, "NYI: multi-cons-id cases"),
    Switch = hlds_goal(_SwitchExpr, Info),
    ProgPoint = program_point_init(Info),

    % Handle the unification on the switch var if it has been removed.
    % We add a dummy program point for this unification.
    (
        MainConsId = cons(_SymName, Arity, _),
        ( if Arity = 0 then
            append_to_each_execution_path(!.ExecPaths,
                [[pair(ProgPoint, Switch)]], ExecPathsBeforeCase)
        else
                ExecPathsBeforeCase = !.ExecPaths
        )
    ;
        ( MainConsId = some_int_const(_)
        ; MainConsId = float_const(_Float)
        ; MainConsId = char_const(_Char)
        ; MainConsId = string_const(_String)
        ),
        % need to add a dummy pp
        append_to_each_execution_path(!.ExecPaths,
            [[pair(ProgPoint, Switch)]], ExecPathsBeforeCase)
    ;
        ( MainConsId = tuple_cons(_)
        ; MainConsId = closure_cons(_, _)
        ; MainConsId = impl_defined_const(_)
        ; MainConsId = type_ctor_info_const(_, _, _)
        ; MainConsId = base_typeclass_info_const(_, _, _, _)
        ; MainConsId = type_info_cell_constructor(_)
        ; MainConsId = typeclass_info_cell_constructor
        ; MainConsId = type_info_const(_)
        ; MainConsId = typeclass_info_const(_)
        ; MainConsId = ground_term_const(_, _)
        ; MainConsId = tabling_info_const(_)
        ; MainConsId = table_io_entry_desc(_)
        ; MainConsId = deep_profiling_proc_layout(_)
        ),
        unexpected($pred, "unexpected cons_id")
    ),
    execution_paths_covered_goal(ProcInfo, CaseGoal,
        ExecPathsBeforeCase, ExecPathsCase),
    execution_paths_covered_cases(ProcInfo, Switch, Cases,
        !.ExecPaths, ExecPathsCases),
    !:ExecPaths = ExecPathsCase ++ ExecPathsCases.

    % Extend each execution path in the first list with each in the
    % second list, all the extended execution paths are put in the third list.
    %
:- pred append_to_each_execution_path(list(execution_path)::in,
    list(execution_path)::in, list(execution_path)::out) is det.

append_to_each_execution_path([], _, []).
append_to_each_execution_path([ExecPath | ExecPaths], Extensions,
        ExtendedExecPaths) :-
    extend_exectution_path(ExecPath, Extensions, ExtendedExecPaths0),
    append_to_each_execution_path(ExecPaths, Extensions,
        ExtendedExecPaths1),
    ExtendedExecPaths = ExtendedExecPaths0 ++ ExtendedExecPaths1.

    % extend_exectution_path(ExecPath, Extensions, ExtendedExecPaths):
    %
    % ExtendedExecPaths is the list created by appending each extension
    % in Extensions to ExecPath.
    %
:- pred extend_exectution_path(execution_path::in, list(execution_path)::in,
    list(execution_path)::out) is det.

extend_exectution_path(_, [], []).
extend_exectution_path(ExecPath, [Extension | Extensions],
        ExtendedExecPaths) :-
    ExtendedExecPath = ExecPath ++ Extension,
    extend_exectution_path(ExecPath, Extensions, ExtendedExecPaths0),
    ExtendedExecPaths = [ExtendedExecPath | ExtendedExecPaths0].

%----------------------------------------------------------------------------%
:- end_module transform_hlds.rbmm.execution_path.
%----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: implementation_defined_literals.m.
% Author: wangp.
%
% This module replaces implementation-defined literals such as $file and $line
% by real constants. We transform clauses rather than procedures because
% currently, clauses rather than procedures are written out to `.opt' files,
% and $file and $line must be substituted before being written out.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.implementation_defined_literals.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

:- pred subst_impl_defined_literals(module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_goal.
:- import_module libs.
:- import_module libs.compute_grade.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.
:- import_module term.

:- type subst_literals_info
    --->    subst_literals_info(
                module_info,
                pred_info,
                pred_id
            ).

%-----------------------------------------------------------------------------%

subst_impl_defined_literals(!ModuleInfo) :-
    module_info_get_preds(!.ModuleInfo, Preds0),
    map.map_values(subst_literals_in_pred(!.ModuleInfo), Preds0, Preds),
    module_info_set_preds(Preds, !ModuleInfo).

:- pred subst_literals_in_pred(module_info::in, pred_id::in, pred_info::in,
    pred_info::out)  is det.

subst_literals_in_pred(ModuleInfo, PredId, PredInfo0, PredInfo) :-
    pred_info_get_clauses_info(PredInfo0, ClausesInfo0),
    clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, ItemNumbers),
    get_clause_list_for_replacement(ClausesRep0, Clauses0),
    Info = subst_literals_info(ModuleInfo, PredInfo0, PredId),
    list.map(subst_literals_in_clause(Info), Clauses0, Clauses),
    set_clause_list(Clauses, ClausesRep),
    clauses_info_set_clauses_rep(ClausesRep, ItemNumbers,
        ClausesInfo0, ClausesInfo),
    pred_info_set_clauses_info(ClausesInfo, PredInfo0, PredInfo).

:- pred subst_literals_in_clause(subst_literals_info::in, clause::in,
    clause::out) is det.

subst_literals_in_clause(Info, Clause0, Clause) :-
    Body0 = Clause0 ^ clause_body,
    subst_literals_in_goal(Info, Body0, Body),
    Clause = Clause0 ^ clause_body := Body.

:- pred subst_literals_in_goal(subst_literals_info::in, hlds_goal::in,
    hlds_goal::out) is det.

subst_literals_in_goal(Info, Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = unify(Var, RHS0, Mode, Kind, UnifyContext),
        (
            RHS0 = rhs_functor(ConsId, _, _),
            (
                ConsId = impl_defined_const(IDCKind),
                Context = goal_info_get_context(GoalInfo0),
                make_impl_defined_literal(Var, IDCKind, Context, Info, Goal1),
                Goal1 = hlds_goal(GoalExpr, _),
                Goal = hlds_goal(GoalExpr, GoalInfo0)
            ;
                ( ConsId = cons(_, _, _)
                ; ConsId = tuple_cons(_)
                ; ConsId = closure_cons(_, _)
                ; ConsId = int_const(_)
                ; ConsId = uint_const(_)
                ; ConsId = int8_const(_)
                ; ConsId = uint8_const(_)
                ; ConsId = int16_const(_)
                ; ConsId = uint16_const(_)
                ; ConsId = int32_const(_)
                ; ConsId = uint32_const(_)
                ; ConsId = int64_const(_)
                ; ConsId = uint64_const(_)
                ; ConsId = float_const(_)
                ; ConsId = char_const(_)
                ; ConsId = string_const(_)
                ; ConsId = type_ctor_info_const(_, _, _)
                ; ConsId = base_typeclass_info_const(_, _, _, _)
                ; ConsId = type_info_cell_constructor(_)
                ; ConsId = typeclass_info_cell_constructor
                ; ConsId = type_info_const(_)
                ; ConsId = typeclass_info_const(_)
                ; ConsId = ground_term_const(_, _)
                ; ConsId = tabling_info_const(_)
                ; ConsId = deep_profiling_proc_layout(_)
                ; ConsId = table_io_entry_desc(_)
                ),
                Goal = Goal0
            )
        ;
            RHS0 = rhs_lambda_goal(LambdaPurity, Groundness, PredOrFunc,
                EvalMethod, LambdaNonLocals, LambdaQuantVars,
                LambdaModes, LambdaDetism, LambdaGoal0),
            subst_literals_in_goal(Info, LambdaGoal0, LambdaGoal),
            RHS = rhs_lambda_goal(LambdaPurity, Groundness, PredOrFunc,
                EvalMethod, LambdaNonLocals, LambdaQuantVars,
                LambdaModes, LambdaDetism, LambdaGoal),
            GoalExpr = unify(Var, RHS, Mode, Kind, UnifyContext),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            RHS0 = rhs_var(_),
            Goal = Goal0
        )
    ;
        GoalExpr0 = negation(SubGoal0),
        subst_literals_in_goal(Info, SubGoal0, SubGoal),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        % Implementation-defined literals may appear in
        % from_ground_term_construct scopes.
        subst_literals_in_goal(Info, SubGoal0, SubGoal),
        GoalExpr = scope(Reason, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        list.map(subst_literals_in_goal(Info), Goals0, Goals),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Goals0),
        list.map(subst_literals_in_goal(Info), Goals0, Goals),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        list.map(subst_literals_in_case(Info), Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        subst_literals_in_goal(Info, Cond0, Cond),
        subst_literals_in_goal(Info, Then0, Then),
        subst_literals_in_goal(Info, Else0, Else),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = shorthand(Shorthand0),
        (
            Shorthand0 = bi_implication(A0, B0),
            subst_literals_in_goal(Info, A0, A),
            subst_literals_in_goal(Info, B0, B),
            Shorthand = bi_implication(A, B)
        ;
            Shorthand0 = atomic_goal(GoalType, OuterVars, InnerVars,
                OutputVars, MainGoal0, OrElseGoals0, OrElseInners),
            subst_literals_in_goal(Info, MainGoal0, MainGoal),
            list.map(subst_literals_in_goal(Info), OrElseGoals0, OrElseGoals),
            Shorthand = atomic_goal(GoalType, OuterVars, InnerVars,
                OutputVars, MainGoal, OrElseGoals, OrElseInners)
        ;
            Shorthand0 = try_goal(MaybeIOVars, ResultVar, TryGoal0),
            subst_literals_in_goal(Info, TryGoal0, TryGoal),
            Shorthand = try_goal(MaybeIOVars, ResultVar, TryGoal)
        ),
        GoalExpr = shorthand(Shorthand),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        ( GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ),
        Goal = Goal0
    ).

:- pred subst_literals_in_case(subst_literals_info::in, case::in, case::out)
    is det.

subst_literals_in_case(Info, Case0, Case) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    subst_literals_in_goal(Info, Goal0, Goal),
    Case = case(MainConsId, OtherConsIds, Goal).

:- pred make_impl_defined_literal(prog_var::in, impl_defined_const_kind::in,
    term.context::in, subst_literals_info::in, hlds_goal::out) is det.

make_impl_defined_literal(Var, IDCKind, Context, Info, Goal) :-
    Context = term.context(File, Line),
    Info = subst_literals_info(ModuleInfo, PredInfo, PredId),
    (
        IDCKind = idc_file,
        make_string_const_construction(Context, Var, File, Goal)
    ;
        IDCKind = idc_line,
        make_int_const_construction(Context, Var, Line, Goal)
    ;
        IDCKind = idc_module,
        ModuleName = pred_info_module(PredInfo),
        Str = sym_name_to_string(ModuleName),
        make_string_const_construction(Context, Var, Str, Goal)
    ;
        IDCKind = idc_pred,
        Str = pred_id_to_string(ModuleInfo, PredId),
        make_string_const_construction(Context, Var, Str, Goal)
    ;
        IDCKind = idc_grade,
        module_info_get_globals(ModuleInfo, Globals),
        grade_directory_component(Globals, Grade),
        make_string_const_construction(Context, Var, Grade, Goal)
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.implementation_defined_literals.
%-----------------------------------------------------------------------------%

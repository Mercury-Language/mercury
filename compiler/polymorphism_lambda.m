%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012, 2014 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: polymorphism_lambda.m.
% Main author: fjh (when this code was in polymorphism.m).
%
% XXX Document what the code in this module does, and, more importantly,
% *why* is does what it does.
%
%---------------------------------------------------------------------------%

:- module check_hlds.polymorphism_lambda.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.vartypes.

:- import_module list.
:- import_module term.

%---------------------------------------------------------------------------%

    % Convert a higher order pred term to a lambda goal.
    %
:- pred convert_pred_to_lambda_goal(purity::in, lambda_eval_method::in,
    prog_var::in, pred_id::in, proc_id::in, list(prog_var)::in,
    list(mer_type)::in, unify_context::in, hlds_goal_info::in, context::in,
    module_info::in, maybe1(unify_rhs)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

    % fix_undetermined_mode_lambda_goal(ModuleInfo, ProcId, Functor0, Functor)
    %
    % This is called by mode checking when it figures out which mode that a
    % lambda goal converted from a higher order pred term should call.
    % Functor0 must have been produced by `convert_pred_to_lambda_goal'.
    %
:- pred fix_undetermined_mode_lambda_goal(module_info::in, proc_id::in,
    unify_rhs::in(rhs_lambda_goal),
    maybe1(unify_rhs)::out(maybe1(rhs_lambda_goal))) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.instmap.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module varset.

%---------------------------------------------------------------------------%

convert_pred_to_lambda_goal(Purity, EvalMethod, X0, PredId, ProcId,
        ArgVars0, PredArgTypes, UnifyContext, GoalInfo0, Context,
        ModuleInfo0, MaybeRHS, !VarSet, !VarTypes) :-
    % Create the new lambda-quantified variables.
    create_fresh_vars(PredArgTypes, LambdaVars, !VarSet, !VarTypes),
    Args = ArgVars0 ++ LambdaVars,

    % Build up the hlds_goal_expr for the call that will form the lambda goal.
    module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
        PredInfo, ProcInfo),

    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    QualifiedPName = qualified(PredModule, PredName),

    % The ConsId's type_ctor shouldn't matter in a call_unify_context.
    ConsId = cons(QualifiedPName, list.length(ArgVars0),
        cons_id_dummy_type_ctor),
    RHS0 = rhs_functor(ConsId, is_not_exist_constr, ArgVars0),
    CallUnifyContext = call_unify_context(X0, RHS0, UnifyContext),
    LambdaGoalExpr = plain_call(PredId, ProcId, Args, not_builtin,
        yes(CallUnifyContext), QualifiedPName),

    % Construct a goal_info for the lambda goal, making sure to set up
    % the nonlocals field in the goal_info correctly. The goal_id is needed
    % to compute constraint_ids correctly.

    NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
    set_of_var.insert_list(LambdaVars, NonLocals0, OutsideVars),
    set_of_var.list_to_set(Args, InsideVars),
    set_of_var.intersect(OutsideVars, InsideVars, NonLocals),
    GoalId = goal_info_get_goal_id(GoalInfo0),

    instmap_delta_init_unreachable(DummyInstMapDelta),
    DummyDetism = detism_erroneous,
    goal_info_init(NonLocals, DummyInstMapDelta, DummyDetism, Purity,
        Context, LambdaGoalInfo0),
    goal_info_set_goal_id(GoalId, LambdaGoalInfo0, LambdaGoalInfo),
    LambdaGoal = hlds_goal(LambdaGoalExpr, LambdaGoalInfo),

    % Work out the modes of the introduced lambda variables and the determinism
    % of the lambda goal.
    lambda_modes_and_det(PredInfo, ProcInfo, Context, LambdaVars,
        MaybeLambdaVarsModesDet),
    (
        MaybeLambdaVarsModesDet = ok2(LambdaVarsModes, LambdaDet),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        % Higher-order values created in this fashion are always ground.
        Groundness = ho_ground,
        RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            ArgVars0, LambdaVarsModes, LambdaDet, LambdaGoal),
        MaybeRHS = ok1(RHS)
    ;
        MaybeLambdaVarsModesDet = error2(Specs),
        MaybeRHS = error1(Specs)
    ).

:- pred create_fresh_vars(list(mer_type)::in, list(prog_var)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

create_fresh_vars([], [], !VarSet, !VarTypes).
create_fresh_vars([Type | Types], [Var | Vars], !VarSet, !VarTypes) :-
    varset.new_var(Var, !VarSet),
    add_var_type(Var, Type, !VarTypes),
    create_fresh_vars(Types, Vars, !VarSet, !VarTypes).

fix_undetermined_mode_lambda_goal(ModuleInfo, ProcId, RHS0, MaybeRHS) :-
    RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
        ArgVars0, LambdaVarsModes0, _LambdaDet0, LambdaGoal0),
    assoc_list.keys(LambdaVarsModes0, LambdaVars),
    LambdaGoal0 = hlds_goal(_, LambdaGoalInfo),
    goal_to_conj_list(LambdaGoal0, LambdaGoalList0),
    ( if
        list.split_last(LambdaGoalList0, LambdaGoalButLast0, LastGoal0),
        LastGoal0 = hlds_goal(LastGoalExpr0, LastGoalInfo0),
        LastGoalExpr0 = plain_call(PredId0, _DummyProcId, Args0, not_builtin,
            MaybeCallUnifyContext0, QualifiedPName0)
    then
        PredId = PredId0,
        % Build up LambdaGoal. It is the same as LambdaGoal0, but with the
        % given ProcId.
        LastGoalExpr = plain_call(PredId0, ProcId, Args0, not_builtin,
            MaybeCallUnifyContext0, QualifiedPName0),
        LastGoal = hlds_goal(LastGoalExpr, LastGoalInfo0),
        conj_list_to_goal(LambdaGoalButLast0 ++ [LastGoal], LambdaGoalInfo,
            LambdaGoal),
        Context = goal_info_get_context(LastGoalInfo0)
    else
        unexpected($pred, "unmatched lambda goal")
    ),

    % Work out the modes of the introduced lambda variables and the determinism
    % of the lambda goal.
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
    lambda_modes_and_det(PredInfo, ProcInfo, Context, LambdaVars,
        MaybeLambdaVarsModesDet),
    (
        MaybeLambdaVarsModesDet = ok2(LambdaVarsModes, LambdaDet),
        RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            ArgVars0, LambdaVarsModes, LambdaDet, LambdaGoal),
        MaybeRHS = ok1(RHS)
    ;
        MaybeLambdaVarsModesDet = error2(Specs),
        MaybeRHS = error1(Specs)
    ).

:- pred lambda_modes_and_det(pred_info::in, proc_info::in, prog_context::in,
    list(prog_var)::in,
    maybe2(assoc_list(prog_var, mer_mode), determinism)::out) is det.

lambda_modes_and_det(PredInfo, ProcInfo, Context, LambdaVars, MaybeResult) :-
    proc_info_get_declared_determinism(ProcInfo, MaybeDet),
    (
        MaybeDet = yes(Det),
        proc_info_get_argmodes(ProcInfo, ArgModes),
        list.length(ArgModes, NumArgModes),
        list.length(LambdaVars, NumLambdaVars),
        list.det_drop(NumArgModes - NumLambdaVars, ArgModes, LambdaModes),
        assoc_list.from_corresponding_lists(LambdaVars, LambdaModes,
            LambdaArgModes),
        MaybeResult = ok2(LambdaArgModes, Det)
    ;
        MaybeDet = no,
        pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
        PredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
        pred_info_get_module_name(PredInfo, PredModuleName),
        pred_info_get_name(PredInfo, PredName),
        PredSymName = qualified(PredModuleName, PredName),
        Pieces = [words("Error: the"), words(PredOrFuncStr),
            qual_sym_name(PredSymName), words("has no declared determinism,"),
            words("so a curried call to it"),
            words("may not be used as a lambda expression."), nl],
        Spec = simplest_spec($pred, severity_error, phase_polymorphism,
            Context, Pieces),
        MaybeResult = error2([Spec])
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.polymorphism_lambda.
%---------------------------------------------------------------------------%

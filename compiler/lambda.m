%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: lambda.m.
% Main author: fjh.
%
% This module does lambda expansion, which means that it replaces each
% unification with a lambda expression with the construction of a closure
% whose code address refers to a new predicate that this module creates
% from that lambda expression.
%
% For example, we translate
%
%   :- pred p(int::in) is det.
%   p(X) :-
%       V__1 = (pred(Y::out) is nondet :- q(Y, X)),
%       solutions(V__1, List),
%       ...
%   :- pred q(int::out, int::in) is nondet.
%
% into
%
%   :- pred '__LambdaGoal__1'(int::in, int::out) is nondet.
%   '__LambdaGoal__1'(X, Y) :- q(Y, X).
%
%   p(X) :-
%       V__1 = closure_cons('__LambdaGoal__1')(X)
%       solutions(V__1, List),
%       ...
%
% Note that the mode checker requires that lambda expressions
% not bind any of their non-local variables, such as `X' in the above example.
%
% Similarly, a lambda expression may not bind any of the type_infos for
% those variables; that is, none of the non-local variables should be
% existentially typed (from the perspective of the lambda goal).
% Now that we run the polymorphism.m pass before mode checking,
% and that this is also checked by mode analysis.
%
% It might be OK to allow the parameters of the lambda goal to be
% existentially typed, but currently that is not supported.
% One difficulty is that it is hard to determine here which type variables
% should be existentially quantified. The information is readily
% available during type inference, and really type inference should save
% that information in a field in the lambda_goal struct, but currently it
% does not; it saves the head_type_params field in the pred_info, which
% tells us which type variables were produced by the body, but for
% any given lambda goal, we don't know whether the type variable was
% produced by something outside the lambda goal or by something inside
% the lambda goal (only in the latter case should it be existentially
% quantified).
%
% The other difficulty is that taking the address of a predicate with an
% existential type would require second-order polymorphism: for a predicate
% declared as `:- some [T] pred p(int, T)', the expression `p' must have
% type `some [T] pred(int, T)', which is quite a different thing to saying
% that there is some type `T' for which `p' has type `pred(int, T)' --
% we don't know what `T' is until the predicate is called, and it might
% be different for each call.
%
% Currently we don't support second-order polymorphism, so we cannot support
% existentially typed lambda expressions either.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.lambda.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

%---------------------------------------------------------------------------%

:- pred expand_lambdas_in_module(module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%

% The following are exported for float_reg.m.

:- type reg_wrapper_proc
    --->    reg_wrapper_proc(set_of_progvar)
    ;       not_reg_wrapper_proc.

:- pred expand_lambda(reg_wrapper_proc::in, prog_var::in,
    unify_rhs::in(rhs_lambda_goal), unify_mode::in,
    unification::in(unification_construct), unify_context::in,
    hlds_goal_expr::out, lambda_info::in, lambda_info::out) is det.

:- type lambda_info.

:- pred init_lambda_info(module_info::in, pred_info::in,
    tvarset::in, inst_varset::in, var_table::in,
    rtti_varmaps::in, has_parallel_conj::in, lambda_info::out) is det.

:- type maybe_recompute_nonlocals
    --->    need_not_recompute_nonlocals
    ;       must_recompute_nonlocals.

:- pred lambda_info_get_module_info(lambda_info::in, module_info::out) is det.
:- pred lambda_info_get_pred_info(lambda_info::in, pred_info::out) is det.
:- pred lambda_info_get_tvarset(lambda_info::in, tvarset::out) is det.
:- pred lambda_info_get_inst_varset(lambda_info::in, inst_varset::out) is det.
:- pred lambda_info_get_var_table(lambda_info::in, var_table::out) is det.
:- pred lambda_info_get_rtti_varmaps(lambda_info::in, rtti_varmaps::out)
    is det.
:- pred lambda_info_get_recompute_nonlocals(lambda_info::in,
    maybe_recompute_nonlocals::out) is det.

:- pred lambda_info_set_module_info(module_info::in,
    lambda_info::in, lambda_info::out) is det.
:- pred lambda_info_set_var_table(var_table::in,
    lambda_info::in, lambda_info::out) is det.
:- pred lambda_info_set_recompute_nonlocals(maybe_recompute_nonlocals::in,
    lambda_info::in, lambda_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_test.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module transform_hlds.
:- import_module transform_hlds.direct_arg_in_out.

:- import_module array.
:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% This whole section just traverses the module structure.
%

expand_lambdas_in_module(!ModuleInfo) :-
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    list.foldl(expand_lambdas_in_pred, PredIds, !ModuleInfo),
    % Need update the dependency graph to include the lambda predicates.
    module_info_clobber_dependency_info(!ModuleInfo).

:- pred expand_lambdas_in_pred(pred_id::in, module_info::in, module_info::out)
    is det.

expand_lambdas_in_pred(PredId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_valid_procids(PredInfo),
    list.foldl(expand_lambdas_in_proc(PredId), ProcIds, !ModuleInfo).

:- pred expand_lambdas_in_proc(pred_id::in, proc_id::in,
    module_info::in, module_info::out) is det.

expand_lambdas_in_proc(PredId, ProcId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),

    do_expand_lambdas_in_proc(ProcInfo0, ProcInfo, PredInfo0, PredInfo1,
        !ModuleInfo),

    pred_info_get_proc_table(PredInfo1, ProcTable1),
    map.det_update(ProcId, ProcInfo, ProcTable1, ProcTable),
    pred_info_set_proc_table(ProcTable, PredInfo1, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

:- pred do_expand_lambdas_in_proc(proc_info::in, proc_info::out,
    pred_info::in, pred_info::out, module_info::in, module_info::out) is det.

do_expand_lambdas_in_proc(!ProcInfo, !PredInfo, !ModuleInfo) :-
    % Grab the appropriate fields from the pred_info and proc_info.
    pred_info_get_typevarset(!.PredInfo, TypeVarSet0),
    proc_info_get_headvars(!.ProcInfo, HeadVars),
    proc_info_get_var_table(!.ModuleInfo, !.ProcInfo, VarTable0),
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
    proc_info_get_inst_varset(!.ProcInfo, InstVarSet0),
    proc_info_get_has_parallel_conj(!.ProcInfo, HasParallelConj),

    % Process the goal.
    Info0 = lambda_info(!.ModuleInfo, !.PredInfo, TypeVarSet0, InstVarSet0,
        VarTable0, RttiVarMaps0, HasParallelConj,
        need_not_recompute_nonlocals, have_not_expanded_lambdas),
    expand_lambdas_in_goal(Goal0, Goal1, Info0, Info1),
    Info1 = lambda_info(!:ModuleInfo, _PredInfo, TypeVarSet, _InstVarSet,
        VarTable1, RttiVarMaps1, _HasParallelConj,
        MustRecomputeNonLocals, HaveExpandedLambdas),

    % Check if we need to requantify.
    (
        MustRecomputeNonLocals = must_recompute_nonlocals,
        implicitly_quantify_clause_body_general_vt(
            ordinary_nonlocals_no_lambda, HeadVars, _Warnings,
            Goal1, Goal2, VarTable1, VarTable2, RttiVarMaps1, RttiVarMaps2),
        proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InstMap0),
        recompute_instmap_delta(recompute_atomic_instmap_deltas,
            VarTable2, InstVarSet0, InstMap0, Goal2, Goal, !ModuleInfo)
    ;
        MustRecomputeNonLocals = need_not_recompute_nonlocals,
        Goal = Goal1,
        VarTable2 = VarTable1,
        RttiVarMaps2 = RttiVarMaps1
    ),
    (
        HaveExpandedLambdas = have_expanded_lambdas,
        restrict_var_maps(HeadVars, Goal, VarTable2, VarTable,
            RttiVarMaps2, RttiVarMaps)
    ;
        HaveExpandedLambdas = have_not_expanded_lambdas,
        VarTable = VarTable2,
        RttiVarMaps = RttiVarMaps2
    ),

    % Set the new values of the fields in proc_info and pred_info.
    proc_info_set_goal(Goal, !ProcInfo),
    proc_info_set_var_table(VarTable, !ProcInfo),
    proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),
    pred_info_set_typevarset(TypeVarSet, !PredInfo).

:- pred expand_lambdas_in_goal(hlds_goal::in, hlds_goal::out,
    lambda_info::in, lambda_info::out) is det.

expand_lambdas_in_goal(Goal0, Goal, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    (
        GoalExpr0 = unify(LHS, RHS, Mode, Unification, Context),
        expand_lambdas_in_unify_goal(LHS, RHS, Mode, Unification, Context,
            GoalExpr, !Info)
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        expand_lambdas_in_goal_list(Goals0, Goals, !Info),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        expand_lambdas_in_goal_list(Goals0, Goals, !Info),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        expand_lambdas_in_cases(Cases0, Cases, !Info),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = negation(SubGoal0),
        expand_lambdas_in_goal(SubGoal0, SubGoal, !Info),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % If the scope had any rhs_lambda_goals, modes.m wouldn't have
            % left its kind field as from_ground_term_(de)construct.
            GoalExpr = GoalExpr0
        else
            expand_lambdas_in_goal(SubGoal0, SubGoal, !Info),
            GoalExpr = scope(Reason, SubGoal)
        )
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        expand_lambdas_in_goal(Cond0, Cond, !Info),
        expand_lambdas_in_goal(Then0, Then, !Info),
        expand_lambdas_in_goal(Else0, Else, !Info),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        ( GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            expand_lambdas_in_goal(MainGoal0, MainGoal, !Info),
            expand_lambdas_in_goal_list(OrElseGoals0, OrElseGoals, !Info),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            expand_lambdas_in_goal(SubGoal0, SubGoal, !Info),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        ),
        GoalExpr = shorthand(ShortHand)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred expand_lambdas_in_goal_list(list(hlds_goal)::in, list(hlds_goal)::out,
    lambda_info::in, lambda_info::out) is det.

expand_lambdas_in_goal_list([], [], !Info).
expand_lambdas_in_goal_list([Goal0 | Goals0], [Goal | Goals], !Info) :-
    expand_lambdas_in_goal(Goal0, Goal, !Info),
    expand_lambdas_in_goal_list(Goals0, Goals, !Info).

:- pred expand_lambdas_in_cases(list(case)::in, list(case)::out,
    lambda_info::in, lambda_info::out) is det.

expand_lambdas_in_cases([], [], !Info).
expand_lambdas_in_cases([Case0 | Cases0], [Case | Cases], !Info) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    expand_lambdas_in_goal(Goal0, Goal, !Info),
    Case = case(MainConsId, OtherConsIds, Goal),
    expand_lambdas_in_cases(Cases0, Cases, !Info).

:- pred expand_lambdas_in_unify_goal(prog_var::in, unify_rhs::in,
    unify_mode::in, unification::in, unify_context::in, hlds_goal_expr::out,
    lambda_info::in, lambda_info::out) is det.

expand_lambdas_in_unify_goal(LHSVar, RHS0, UnifyMode, Unification0,
        UnifyContext, GoalExpr, !Info) :-
    (
        RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            NonLocals, ArgVarsModes, Detism, LambdaGoal0),
        % First, process the lambda goal recursively, in case it contains
        % some nested lambda expressions.
        expand_lambdas_in_goal(LambdaGoal0, LambdaGoal, !Info),
        RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            NonLocals, ArgVarsModes, Detism, LambdaGoal),
        (
            Unification0 = construct(_, _, _, _, _, _, _),
            % Then, convert the lambda expression into a new predicate.
            expand_lambda(not_reg_wrapper_proc, LHSVar, RHS, UnifyMode,
                Unification0, UnifyContext, GoalExpr, !Info)
        ;
            ( Unification0 = deconstruct(_, _, _, _, _, _)
            ; Unification0 = assign(_, _)
            ; Unification0 = simple_test(_, _)
            ; Unification0 = complicated_unify(_, _, _)
            ),
            unexpected($pred, "unexpected unification")
        )
    ;
        ( RHS0 = rhs_var(_)
        ; RHS0 = rhs_functor(_, _, _)
        ),
        % We leave ordinary unifications unchanged.
        GoalExpr = unify(LHSVar, RHS0, UnifyMode, Unification0, UnifyContext)
    ).

%---------------------------------------------------------------------------%

expand_lambda(RegWrapperProc, LHSVar, RHS0, UnifyMode,
        Unification0, UnifyContext, GoalExpr, LambdaInfo0, LambdaInfo) :-
    LambdaInfo0 = lambda_info(ModuleInfo0, OrigPredInfo, TVarSet, InstVarSet,
        VarTable, RttiVarMaps, HasParallelConj,
        MustRecomputeNonLocals0, _HaveExpandedLambdas),
    RHS0 = rhs_lambda_goal(_Purity, _Groundness, PredOrFunc, EvalMethod,
        RHSNonLocals, VarsModes, Detism, LambdaGoal),
    assoc_list.keys(VarsModes, Vars),
    Unification0 = construct(Var, _, ArgVars0, ArgUnifyModes0,
        _, _, _),
    trace [compiletime(flag("lambda_sanity_check"))]
    (
        expect(unify(LHSVar, Var), $pred, "LHSVar != Var"),
        list.sort(RHSNonLocals, SortedRHSNonLocals),
        list.sort(ArgVars0, SortedArgVars0),
        expect(unify(SortedRHSNonLocals, SortedArgVars0), $pred,
            "RHSNonLocals != ArgVars0")
    ),
    ( if
        can_we_use_existing_pred(ModuleInfo0, PredOrFunc, Vars, Detism,
            LambdaGoal, PredId0, ProcId0, CalleePredInfo, CalleeProcInfo0,
            CurriedArgVars, ArgUnifyModes1)
    then
        PredId = PredId0,
        ProcId = ProcId0,
        ArgVars = CurriedArgVars,
        ArgUnifyModes = ArgUnifyModes1,
        % We must mark the procedure as having had its address taken.
        proc_info_set_address_taken(address_is_taken,
            CalleeProcInfo0, CalleeProcInfo),
        module_info_set_pred_proc_info(PredId, ProcId,
            CalleePredInfo, CalleeProcInfo, ModuleInfo0, ModuleInfo),
        MustRecomputeNonLocals = MustRecomputeNonLocals0
    else
        create_new_pred_for_lambda(RegWrapperProc, RHS0, ArgVars0, ArgVars,
            PredId, ProcId, ArgUnifyModes0, ArgUnifyModes,
            MustRecomputeNonLocals0, MustRecomputeNonLocals,
            LambdaInfo0, ModuleInfo)
    ),
    ShroudedPredProcId = shroud_pred_proc_id(proc(PredId, ProcId)),
    ConsId = closure_cons(ShroudedPredProcId, EvalMethod),
    RHS = rhs_functor(ConsId, is_not_exist_constr, ArgVars),
    Unification = construct(Var, ConsId, ArgVars, ArgUnifyModes,
        construct_dynamically, cell_is_unique, no_construct_sub_info),
    GoalExpr = unify(LHSVar, RHS, UnifyMode, Unification, UnifyContext),
    LambdaInfo = lambda_info(ModuleInfo, OrigPredInfo, TVarSet, InstVarSet,
        VarTable, RttiVarMaps, HasParallelConj,
        MustRecomputeNonLocals, have_expanded_lambdas).

%---------------------%

        % We would like to optimize a special case: replacing
        %   `(pred(Y1, Y2, ...) is Detism :-
        %       p(X1, X2, ..., Y1, Y2, ...))'
        % where `p' has determinism `Detism', with
        %   `p(X1, X2, ...)'
        %
        % This optimization is only valid if the modes of the Xi are input,
        % since only input arguments can be curried. It is also only valid
        % if all the inputs in the Yi precede the outputs. It is also not valid
        % if any of the Xi are in the Yi.
        %
:- pred can_we_use_existing_pred(module_info::in, pred_or_func::in,
    list(prog_var)::in, determinism::in, hlds_goal::in,
    pred_id::out, proc_id::out, pred_info::out, proc_info::out,
    list(prog_var)::out, list(unify_mode)::out) is semidet.

can_we_use_existing_pred(ModuleInfo, PredOrFunc, Vars, Detism, LambdaGoal,
        PredId, ProcId, CalleePredInfo, CalleeProcInfo,
        CurriedArgVars, CurriedArgUnifyModes) :-
    LambdaGoal = hlds_goal(LambdaGoalExpr, _),
    LambdaGoalExpr = plain_call(PredId, ProcId, CallVars, _, _, _),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
        CalleePredInfo, CalleeProcInfo),
    % If this succeeds, then
    % - Vars are the Yi above;
    % - InitialVars are the Xi; and
    % - CallVars are the Xi followed by the Yi.
    % Thus InitialVars are the variables we put into the curried closure.
    list.remove_suffix(CallVars, Vars, CurriedArgVars),

    % Check that none of the variables that we are trying to use
    % as curried arguments are lambda-bound variables.
    set.list_to_set(CurriedArgVars, CurriedArgVarsSet),
    set.list_to_set(Vars, VarsSet),
    set.intersect(CurriedArgVarsSet, VarsSet, BothSet),
    set.is_empty(BothSet),

    % Check that the curried arguments are all input.
    proc_info_get_argmodes(CalleeProcInfo, CalleeArgModes),
    list.length(CurriedArgVars, NumCurriedArgVars),
    list.take(NumCurriedArgVars, CalleeArgModes, CurriedArgModes),
    list.all_true(mode_is_input(ModuleInfo), CurriedArgModes),
    modes_to_unify_modes(ModuleInfo, CurriedArgModes, CurriedArgModes,
        CurriedArgUnifyModes),

    % Check that the code models are compatible. Note that det is not
    % compatible with semidet, and semidet is not compatible with nondet,
    % since the calling conventions are different. If we are using the LLDS
    % backend, det is compatible with nondet. If we are using the MLDS
    % backend, then predicates and functions have different calling
    % conventions.
    CalleeCodeModel = proc_info_interface_code_model(CalleeProcInfo),
    determinism_to_code_model(Detism, CodeModel),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    (
        HighLevelCode = no,
        (
            CodeModel = CalleeCodeModel
        ;
            CodeModel = model_non,
            CalleeCodeModel = model_det
        )
    ;
        HighLevelCode = yes,
        CalleePredOrFunc = pred_info_is_pred_or_func(CalleePredInfo),
        PredOrFunc = CalleePredOrFunc,
        CodeModel = CalleeCodeModel
    ).

%---------------------%

:- pred create_new_pred_for_lambda(reg_wrapper_proc::in,
    unify_rhs::in(rhs_lambda_goal), list(prog_var)::in, list(prog_var)::out,
    pred_id::out, proc_id::out, list(unify_mode)::in, list(unify_mode)::out,
    maybe_recompute_nonlocals::in, maybe_recompute_nonlocals::out,
    lambda_info::in, module_info::out) is det.

create_new_pred_for_lambda(RegWrapperProc, RHS0, OrigVars, ArgVars,
        PredId, ProcId, ArgUnifyModes0, ArgUnifyModes, !MustRecomputeNonLocals,
        LambdaInfo0, !:ModuleInfo) :-
    LambdaInfo0 = lambda_info(!:ModuleInfo, OrigPredInfo, TVarSet,
        InstVarSet, VarTable, RttiVarMaps, HasParallelConj,
        MustRecomputeNonLocals0, _HaveExpandedLambdas),
    RHS0 = rhs_lambda_goal(Purity, _Groundness, PredOrFunc, _EvalMethod,
        _ClosureVars, LambdaVarsModes, Detism, LambdaGoal),
    assoc_list.keys_and_values(LambdaVarsModes, LambdaVars, LambdaVarModes),
    LambdaGoal = hlds_goal(_, LambdaGoalInfo),

    % Calculate the constraints which apply to this lambda expression.
    % Note currently we only allow lambda expressions to have universally
    % quantified constraints.
    rtti_varmaps_reusable_constraints(RttiVarMaps, AllConstraints),
    lookup_var_types(VarTable, LambdaVars, LambdaVarTypeList),
    set_of_type_vars_in_types(LambdaVarTypeList, LambdaTypeVars),
    list.filter(constraint_contains_only_lambda_tvars(LambdaTypeVars),
        AllConstraints, UnivConstraints),
    Constraints = constraints(UnivConstraints, []),

    % Existentially typed lambda expressions are not yet supported
    % (see the documentation at top of this file).
    ExistQVars = [],
    LambdaGoalNonLocals = goal_info_get_nonlocals(LambdaGoalInfo),
    set_of_var.insert_list(LambdaVars, LambdaGoalNonLocals, LambdaNonLocals),
    extra_nonlocal_typeinfos_typeclass_infos(RttiVarMaps, VarTable,
        ExistQVars, LambdaNonLocals, ExtraTiTcis),

    set_of_var.delete_list(LambdaVars, LambdaGoalNonLocals, NonLocals1),
    % We need all the typeinfos, including the ones that are not used,
    % for the layout structure describing the closure.
    set_of_var.difference(ExtraTiTcis, NonLocals1, NewTiTcis),
    set_of_var.union(NonLocals1, NewTiTcis, NonLocals),
    set_of_var.to_sorted_list(NonLocals, ArgVars1),

    ( if set_of_var.is_empty(NewTiTcis) then
        true
    else
        % If we added variables to the nonlocals of the lambda goal, then
        % we must recompute the nonlocals for the procedure that contains it.
        !:MustRecomputeNonLocals = must_recompute_nonlocals
    ),

    % Prepare to create a new predicate for the lambda expression:
    % work out the arguments, module name, predicate name, arity,
    % arg types, determinism, context, status, etc. for the new predicate.

    ArgVars = put_typeinfo_vars_first(VarTable, ArgVars1),
    AllArgVars = ArgVars ++ LambdaVars,

    module_info_get_name(!.ModuleInfo, ModuleName),
    OrigPredName = pred_info_name(OrigPredInfo),
    OrigContext = goal_info_get_context(LambdaGoalInfo),
    term.context_file(OrigContext, OrigFile),
    term.context_line(OrigContext, OrigLine),
    module_info_next_lambda_count(OrigContext, LambdaCount, !ModuleInfo),
    Transform = tn_lambda(PredOrFunc, lnc(OrigLine, LambdaCount)),
    make_transformed_pred_name(OrigPredName, Transform, TransformedName),
    LambdaContext = goal_info_get_context(LambdaGoalInfo),
    % The TVarSet is a superset of what it really ought be,
    % but that should not matter.
    unify_modes_to_modes(ArgUnifyModes0, OrigArgModes),

    % We have to jump through hoops to work out the mode of the lambda
    % predicate. For introduced type_info arguments, we use the mode "in".
    % For the original non-local vars, we use the modes from `ArgUnifyModes0'.
    % For the lambda var arguments at the end, we use the mode in the
    % lambda expression.
    % XXX The above comment has probably suffered bit-rot.

    list.length(ArgVars, NumArgVars),
    in_mode(In),
    list.duplicate(NumArgVars, In, InModes),
    map.from_corresponding_lists(ArgVars, InModes, ArgModesMap),

    map.from_corresponding_lists(OrigVars, OrigArgModes, OrigArgModesMap),
    map.overlay(ArgModesMap, OrigArgModesMap, ArgModesMap1),
    map.apply_to_list(ArgVars, ArgModesMap1, ArgModes1),
    % Recompute the unify_modes.
    modes_to_unify_modes(!.ModuleInfo, ArgModes1, ArgModes1, ArgUnifyModes),

    AllArgModes = ArgModes1 ++ LambdaVarModes,
    lookup_var_types(VarTable, AllArgVars, ArgTypes),
    list.foldl_corresponding(check_lambda_arg_type_and_mode(!.ModuleInfo),
        ArgTypes, AllArgModes, 0, _),

    purity_to_markers(Purity, PurityMarkers),
    init_markers(LambdaMarkers0),
    add_markers(PurityMarkers, LambdaMarkers0, LambdaMarkers),

    % Now construct the proc_info and pred_info for the new single-mode
    % predicate, using the information computed above.
    map.init(VarNameRemap),
    restrict_var_maps(AllArgVars, LambdaGoal, VarTable, LambdaVarTable,
        RttiVarMaps, LambdaRttiVarMaps),
    some [!ProcInfo] (
        % If the original procedure contained parallel conjunctions,
        % then the one we are creating here may have them as well.
        % If it does not, then the value in the proc_info of the lambda
        % predicate will be an overconservative estimate.
        SeqNum = item_no_seq_num,
        proc_info_create_vt(LambdaContext, SeqNum, LambdaVarTable,
            AllArgVars, InstVarSet, AllArgModes, detism_decl_explicit, Detism,
            LambdaGoal, LambdaRttiVarMaps, address_is_taken,
            HasParallelConj, VarNameRemap, !:ProcInfo),

        % The debugger ignores unnamed variables.
        ensure_all_headvars_are_named(!ProcInfo),

        % If we previously already needed to recompute the nonlocals,
        % then we had better apply that recomputation for the procedure
        % that we just created.
        (
            MustRecomputeNonLocals0 = must_recompute_nonlocals,
            requantify_proc_general(ordinary_nonlocals_maybe_lambda,
                !ProcInfo)
        ;
            MustRecomputeNonLocals0 = need_not_recompute_nonlocals
        ),
        (
            RegWrapperProc = reg_wrapper_proc(RegRHeadVars),
            proc_info_set_reg_r_headvars(RegRHeadVars, !ProcInfo)
        ;
            RegWrapperProc = not_reg_wrapper_proc
        ),
        ProcInfo = !.ProcInfo
    ),
    set.init(Assertions),
    GoalType = goal_not_for_promise(np_goal_type_none),
    pred_info_create(!.ModuleInfo, PredOrFunc, ModuleName, TransformedName,
        LambdaContext, origin_lambda(OrigFile, OrigLine, LambdaCount),
        pred_status(status_local), LambdaMarkers, ArgTypes, TVarSet,
        ExistQVars, Constraints, Assertions, VarNameRemap, GoalType,
        ProcInfo, ProcId, PredInfo),

    % Save the new predicate in the predicate table.
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    predicate_table_insert(PredInfo, PredId, PredicateTable0, PredicateTable),
    module_info_set_predicate_table(PredicateTable, !ModuleInfo),

    find_and_record_any_direct_arg_in_out_posns(PredId, ProcId, LambdaVarTable,
        AllArgVars, AllArgModes, !ModuleInfo).

:- pred constraint_contains_only_lambda_tvars(set(tvar)::in,
    prog_constraint::in) is semidet.

constraint_contains_only_lambda_tvars(LambdaTVars, ClassConstraint) :-
    ClassConstraint = constraint(_, ConstraintTypes),
    set_of_type_vars_in_types(ConstraintTypes, ConstraintTVars),
    set.subset(ConstraintTVars, LambdaTVars).

    % This predicate works out the modes of the original non-local variables
    % of a lambda expression based on the list of unify_mode in the unify_info
    % for the lambda unification.
    %
:- pred unify_modes_to_modes(list(unify_mode)::in, list(mer_mode)::out) is det.

unify_modes_to_modes([], []).
unify_modes_to_modes([UnifyMode | UnifyModes], [Mode | Modes]) :-
    UnifyMode = unify_modes_li_lf_ri_rf(_, _, RHSInit, _RHSFinal),
    Mode = from_to_mode(RHSInit, RHSInit),
    unify_modes_to_modes(UnifyModes, Modes).

    % Make sure the arguments and modes are not misordered. An obvious
    % indicator is if a non-higher order argument is paired a higher order
    % inst.
    %
:- pred check_lambda_arg_type_and_mode(module_info::in, mer_type::in,
    mer_mode::in, int::in, int::out) is det.

check_lambda_arg_type_and_mode(ModuleInfo, Type, Mode, X, X) :-
    Inst = mode_get_initial_inst(ModuleInfo, Mode),
    ( if Inst = ground(_, higher_order(_)) then
        ( if type_is_higher_order(Type) then
            true
        else
            unexpected($pred,
                "non-higher order argument with higher order inst")
        )
    else
        true
    ).

%---------------------------------------------------------------------------%

    % The proc_info has several maps that refer to variables. After lambda
    % expansion, both the newly created procedures and the original procedure
    % that they were carved out of have duplicate copies of these maps.
    % This duplication is a problem because later passes (in particular,
    % the equiv_types_hlds pass) iterate over the entries in these maps,
    % and if an entry is duplicated N times, they have to process it N times.
    % The task of this predicate is to eliminate unnecessary entries
    % from the var_table, and this requires also eliminating them from
    % the rtti_varmaps.
    %
    % We could in theory restrict the tvarset and the inst_varset as well,
    % but since we don't iterate over those sets, there is (as yet) no need
    % for this.
    %
:- pred restrict_var_maps(list(prog_var)::in, hlds_goal::in,
    var_table::in, var_table::out, rtti_varmaps::in, rtti_varmaps::out) is det.

restrict_var_maps(HeadVars, Goal, !VarTable, !RttiVarMaps) :-
    var_table_max_var_num(!.VarTable, MaxVarNum),
    % Variable numbers go from 1 to MaxVarNum. Reserve array slots
    % from 0 to MaxVarNum, since wasting the space of one array element
    % is preferable to having to do a subtraction on every array lookup.
    array.init(MaxVarNum + 1, no, VarUses0),
    mark_vars_as_used(HeadVars, VarUses0, VarUses1),
    find_used_vars_in_goal(Goal, VarUses1, VarUses),

    var_table_to_sorted_assoc_list(!.VarTable, VarTypesList0),
    filter_vartypes(VarTypesList0, VarUses, [], RevVarTypesList),
    var_table_from_rev_sorted_assoc_list(RevVarTypesList, !:VarTable),

    restrict_rtti_varmaps(VarUses, !RttiVarMaps).

:- pred find_used_vars_in_goal(hlds_goal::in,
    array(bool)::array_di, array(bool)::array_uo) is det.

find_used_vars_in_goal(Goal, !VarUses) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = unify(LHSVar, RHS, _, Unif, _),
        mark_var_as_used(LHSVar, !VarUses),
        (
            Unif = construct(_, _, _, _, CellToReuse, _, _),
            ( if CellToReuse = reuse_cell(cell_to_reuse(ReuseVar, _, _)) then
                mark_var_as_used(ReuseVar, !VarUses)
            else
                true
            )
        ;
            ( Unif = deconstruct(_, _, _, _, _, _)
            ; Unif = assign(_, _)
            ; Unif = simple_test(_, _)
            ; Unif = complicated_unify(_, _, _)
            )
        ),
        find_used_vars_in_unify_rhs(RHS, !VarUses)
    ;
        GoalExpr = generic_call(GenericCall, ArgVars, _, _, _),
        (
            GenericCall = higher_order(Var, _, _, _),
            mark_var_as_used(Var, !VarUses)
        ;
            GenericCall = class_method(Var, _, _, _),
            mark_var_as_used(Var, !VarUses)
        ;
            GenericCall = event_call(_)
        ;
            GenericCall = cast(_)
        ),
        mark_vars_as_used(ArgVars, !VarUses)
    ;
        GoalExpr = plain_call(_, _, ArgVars, _, MaybeCallUnifyContext, _),
        mark_vars_as_used(ArgVars, !VarUses),
        (
            MaybeCallUnifyContext = no
        ;
            MaybeCallUnifyContext = yes(CallUnifyContext),
            CallUnifyContext = call_unify_context(Var, RHS, _UC),
            mark_var_as_used(Var, !VarUses),
            find_used_vars_in_unify_rhs(RHS, !VarUses)
        )
    ;
        ( GoalExpr = conj(_, Goals)
        ; GoalExpr = disj(Goals)
        ),
        find_used_vars_in_goals(Goals, !VarUses)
    ;
        GoalExpr = switch(Var, _Det, Cases),
        mark_var_as_used(Var, !VarUses),
        find_used_vars_in_cases(Cases, !VarUses)
    ;
        GoalExpr = scope(Reason, SubGoal),
        (
            Reason = exist_quant(Vars),
            mark_vars_as_used(Vars, !VarUses)
        ;
            Reason = promise_solutions(Vars, _),
            mark_vars_as_used(Vars, !VarUses)
        ;
            Reason = from_ground_term(Var, _),
            mark_var_as_used(Var, !VarUses)
        ;
            Reason = loop_control(LCVar, LCSVar, _),
            mark_var_as_used(LCVar, !VarUses),
            mark_var_as_used(LCSVar, !VarUses)
        ;
            ( Reason = disable_warnings(_, _)
            ; Reason = promise_purity(_)
            ; Reason = barrier(_)
            ; Reason = commit(_)
            ; Reason = trace_goal(_, _, _, _, _)
            )
            % Do nothing.
        ;
            ( Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
            ; Reason = require_switch_arms_detism(_, _)
            ),
            % These scopes should have been deleted by now.
            unexpected($pred, "unexpected scope")
        ),
        find_used_vars_in_goal(SubGoal, !VarUses)
    ;
        GoalExpr = negation(SubGoal),
        find_used_vars_in_goal(SubGoal, !VarUses)
    ;
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        mark_vars_as_used(Vars, !VarUses),
        find_used_vars_in_goal(Cond, !VarUses),
        find_used_vars_in_goal(Then, !VarUses),
        find_used_vars_in_goal(Else, !VarUses)
    ;
        GoalExpr = call_foreign_proc(_, _, _, Args, ExtraArgs, _, _),
        ArgVars = list.map(foreign_arg_var, Args),
        ExtraVars = list.map(foreign_arg_var, ExtraArgs),
        mark_vars_as_used(ArgVars, !VarUses),
        mark_vars_as_used(ExtraVars, !VarUses)
    ;
        GoalExpr = shorthand(Shorthand),
        (
            Shorthand = atomic_goal(_, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, _),
            Outer = atomic_interface_vars(OuterDI, OuterUO),
            mark_var_as_used(OuterDI, !VarUses),
            mark_var_as_used(OuterUO, !VarUses),
            Inner = atomic_interface_vars(InnerDI, InnerUO),
            mark_var_as_used(InnerDI, !VarUses),
            mark_var_as_used(InnerUO, !VarUses),
            (
                MaybeOutputVars = no
            ;
                MaybeOutputVars = yes(OutputVars),
                mark_vars_as_used(OutputVars, !VarUses)
            ),
            find_used_vars_in_goal(MainGoal, !VarUses),
            find_used_vars_in_goals(OrElseGoals, !VarUses)
        ;
            Shorthand = try_goal(_, _, SubGoal),
            % The IO and Result variables would be in SubGoal.
            find_used_vars_in_goal(SubGoal, !VarUses)
        ;
            Shorthand = bi_implication(LeftGoal, RightGoal),
            find_used_vars_in_goal(LeftGoal, !VarUses),
            find_used_vars_in_goal(RightGoal, !VarUses)
        )
    ).

:- pred find_used_vars_in_goals(list(hlds_goal)::in,
    array(bool)::array_di, array(bool)::array_uo) is det.

find_used_vars_in_goals([], !VarUses).
find_used_vars_in_goals([Goal | Goals], !VarUses) :-
    find_used_vars_in_goal(Goal, !VarUses),
    find_used_vars_in_goals(Goals, !VarUses).

:- pred find_used_vars_in_cases(list(case)::in,
    array(bool)::array_di, array(bool)::array_uo) is det.

find_used_vars_in_cases([], !VarUses).
find_used_vars_in_cases([Case | Cases], !VarUses) :-
    Case = case(_, _, Goal),
    find_used_vars_in_goal(Goal, !VarUses),
    find_used_vars_in_cases(Cases, !VarUses).

:- pred find_used_vars_in_unify_rhs(unify_rhs::in,
    array(bool)::array_di, array(bool)::array_uo) is det.

find_used_vars_in_unify_rhs(RHS, !VarUses) :-
    (
        RHS = rhs_var(RHSVar),
        mark_var_as_used(RHSVar, !VarUses)
    ;
        RHS = rhs_functor(_, _, ArgVars),
        mark_vars_as_used(ArgVars, !VarUses)
    ;
        RHS = rhs_lambda_goal(_, _, _, _, NonLocals, ArgVarsModes,
            _, LambdaGoal),
        assoc_list.keys(ArgVarsModes, ArgVars),
        mark_vars_as_used(NonLocals, !VarUses),
        mark_vars_as_used(ArgVars, !VarUses),
        find_used_vars_in_goal(LambdaGoal, !VarUses)
    ).

:- pred mark_var_as_used(prog_var::in,
    array(bool)::array_di, array(bool)::array_uo) is det.
:- pragma inline(pred(mark_var_as_used/3)).

mark_var_as_used(Var, !VarUses) :-
    array.set(var_to_int(Var), yes, !VarUses).

:- pred mark_vars_as_used(list(prog_var)::in,
    array(bool)::array_di, array(bool)::array_uo) is det.

mark_vars_as_used([], !VarUses).
mark_vars_as_used([Var | Vars], !VarUses) :-
    mark_var_as_used(Var, !VarUses),
    mark_vars_as_used(Vars, !VarUses).

:- pred filter_vartypes(assoc_list(prog_var, var_table_entry)::in,
    array(bool)::in,
    assoc_list(prog_var, var_table_entry)::in,
    assoc_list(prog_var, var_table_entry)::out) is det.

filter_vartypes([], _VarUses, !RevVarsEntries).
filter_vartypes([VarEntry | VarsEntries], VarUses, !RevVarsEntries) :-
    VarEntry = Var - _Entry,
    VarNum = var_to_int(Var),
    array.unsafe_lookup(VarUses, VarNum, Used),
    (
        Used = yes,
        !:RevVarsEntries = [VarEntry | !.RevVarsEntries]
    ;
        Used = no
    ),
    filter_vartypes(VarsEntries, VarUses, !RevVarsEntries).

%---------------------------------------------------------------------------%

:- type lambda_info
    --->    lambda_info(
                li_module_info          :: module_info,
                li_pred_info            :: pred_info,

                li_tvarset              :: tvarset,
                li_inst_varset          :: inst_varset,

                li_var_table            :: var_table,
                li_rtti_varmaps         :: rtti_varmaps,
                li_has_parallel_conj    :: has_parallel_conj,

                % Do we need to recompute the nonlocals?
                li_recompute_nonlocals  :: maybe_recompute_nonlocals,

                % Have we expanded some lambda expressions?
                li_have_expanded_lambda :: have_we_expanded_lambdas
            ).

:- type have_we_expanded_lambdas
    --->    have_not_expanded_lambdas
    ;       have_expanded_lambdas.

init_lambda_info(ModuleInfo, PredInfo, TypeVarSet, InstVarSet, VarTable,
        RttiVarMaps, HasParallelConj, Info) :-
    Info = lambda_info(ModuleInfo, PredInfo, TypeVarSet, InstVarSet, VarTable,
        RttiVarMaps, HasParallelConj,
        need_not_recompute_nonlocals, have_not_expanded_lambdas).

lambda_info_get_module_info(Info, X) :-
    X = Info ^ li_module_info.
lambda_info_get_pred_info(Info, X) :-
    X = Info ^ li_pred_info.
lambda_info_get_tvarset(Info, X) :-
    X = Info ^ li_tvarset.
lambda_info_get_inst_varset(Info, X) :-
    X = Info ^ li_inst_varset.
lambda_info_get_var_table(Info, X) :-
    X = Info ^ li_var_table.
lambda_info_get_rtti_varmaps(Info, X) :-
    X = Info ^ li_rtti_varmaps.
lambda_info_get_recompute_nonlocals(Info, X) :-
    X = Info ^ li_recompute_nonlocals.

lambda_info_set_module_info(X, !Info) :-
    !Info ^ li_module_info := X.
lambda_info_set_var_table(X, !Info) :-
    !Info ^ li_var_table := X.
lambda_info_set_recompute_nonlocals(X, !Info) :-
    !Info ^ li_recompute_nonlocals := X.

%---------------------------------------------------------------------------%
:- end_module transform_hlds.lambda.
%---------------------------------------------------------------------------%

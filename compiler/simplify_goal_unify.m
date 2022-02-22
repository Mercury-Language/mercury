%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: simplify_goal_unify.m.
%
% This module handles simplification of unifications.
%
%---------------------------------------------------------------------------%

:- module check_hlds.simplify.simplify_goal_unify.
:- interface.

:- import_module check_hlds.simplify.common.
:- import_module check_hlds.simplify.simplify_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.

:- pred simplify_goal_unify(
    hlds_goal_expr::in(goal_expr_unify), hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.polymorphism_type_info.
:- import_module check_hlds.proc_requests.
:- import_module check_hlds.simplify.simplify_goal.
:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module term.
:- import_module uint.

simplify_goal_unify(GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
        NestedContext0, InstMap0, !Common, !Info) :-
    GoalExpr0 = unify(LHSVar0, RHS0, UnifyMode, Unification0, UnifyContext),
    (
        RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            NonLocals, VarsModes, LambdaDeclaredDetism, LambdaGoal0),
        determinism_to_code_model(LambdaDeclaredDetism, LambdaCodeModel),
        (
            ( LambdaCodeModel = model_det
            ; LambdaCodeModel = model_semi
            ),
            LambdaProcIsModelNon = no
        ;
            LambdaCodeModel = model_non,
            Context = goal_info_get_context(GoalInfo0),
            LambdaProcIsModelNon = yes(imp_lambda(Context))
        ),
        NestedContext0 = simplify_nested_context(InsideDuplForSwitch,
            _ProcModelNon, NumEnclosingBarriers),
        ( if goal_info_has_feature(GoalInfo0, feature_lambda_from_try) then
            LambdaNumEnclosingBarriers = NumEnclosingBarriers
        else
            LambdaNumEnclosingBarriers = NumEnclosingBarriers + 1u
        ),
        LambdaNestedContext = simplify_nested_context(InsideDuplForSwitch,
            LambdaProcIsModelNon, LambdaNumEnclosingBarriers),

        simplify_info_get_module_info(!.Info, ModuleInfo),
        instmap.pre_lambda_update(ModuleInfo, VarsModes,
            InstMap0, LambdaInstMap0),

        % Don't attempt to pass structs into lambda_goals, since that
        % could change the curried non-locals of the lambda_goal, and
        % that would be difficult to fix up.
        simplify_info_get_simplify_tasks(!.Info, SimplifyTasks),
        LambdaCommon0 = common_info_init(SimplifyTasks),

        % Don't attempt to pass structs out of lambda_goals.
        simplify_goal(LambdaGoal0, LambdaGoal, LambdaNestedContext,
            LambdaInstMap0, LambdaCommon0, _, !Info),

        RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            NonLocals, VarsModes, LambdaDeclaredDetism, LambdaGoal),
        GoalExpr = unify(LHSVar0, RHS, UnifyMode, Unification0, UnifyContext),
        GoalInfo = GoalInfo0
    ;
        ( RHS0 = rhs_functor(_, _, _)
        ; RHS0 = rhs_var(_)
        ),
        ( if
            % A unification of the form X = X can be safely optimised away.
            RHS0 = rhs_var(LHSVar0)
        then
            Context = goal_info_get_context(GoalInfo0),
            hlds_goal(GoalExpr, GoalInfo) = true_goal_with_context(Context)
        else if
            Unification0 = complicated_unify(ComplMode, CanFail, TypeInfoVars)
        then
            (
                RHS0 = rhs_var(V),
                process_compl_unify(LHSVar0, V, ComplMode, CanFail,
                    TypeInfoVars, UnifyContext, GoalInfo0, GoalExpr1,
                    NestedContext0, InstMap0, !Common, !Info),
                GoalExpr1 = hlds_goal(GoalExpr, GoalInfo)
            ;
                RHS0 = rhs_functor(_, _, _),
                unexpected($pred, "invalid RHS for complicated unify")
            )
        else
            common_optimise_unification(RHS0, UnifyMode, Unification0,
                UnifyContext, GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
                !Common, !Info)
        )
    ).

:- pred process_compl_unify(prog_var::in, prog_var::in, unify_mode::in,
    can_fail::in, list(prog_var)::in, unify_context::in, hlds_goal_info::in,
    hlds_goal::out, simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

process_compl_unify(XVar, YVar, UnifyMode, CanFail, _OldTypeInfoVars,
        UnifyContext, GoalInfo0, Goal, NestedContext0, InstMap0,
        !Common, !Info) :-
    simplify_info_get_module_info(!.Info, ModuleInfo),
    simplify_info_get_var_types(!.Info, VarTypes),
    lookup_var_type(VarTypes, XVar, Type),
    ( if Type = type_variable(TypeVar, Kind) then
        % Convert polymorphic unifications into calls to `unify/2',
        % the general unification predicate, passing the appropriate type_info:
        %   unify(TypeInfoVar, X, Y)
        % where TypeInfoVar is the type_info variable associated with
        % the type of the variables that are being unified.

        Context = goal_info_get_context(GoalInfo0),
        get_type_info_locn(TypeVar, Kind, Context, TypeInfoVar, ExtraGoals,
            !Info),
        call_generic_unify(TypeInfoVar, XVar, YVar, ModuleInfo, !.Info,
            UnifyContext, GoalInfo0, Call)
    else if type_is_higher_order(Type) then
        % Convert higher-order unifications into calls to
        % builtin_unify_pred (which calls error/1).
        Context = goal_info_get_context(GoalInfo0),
        generate_simple_call(ModuleInfo, mercury_private_builtin_module,
            "builtin_unify_pred", pf_predicate, mode_no(0), detism_semi,
            purity_pure, [], [XVar, YVar], [], instmap_delta_bind_no_var,
            Context, hlds_goal(Call0, _)),
        simplify_goal_expr(Call0, Call1, GoalInfo0, GoalInfo,
            NestedContext0, InstMap0, !Common, !Info),
        Call = hlds_goal(Call1, GoalInfo),
        ExtraGoals = []
    else
        type_to_ctor_and_args_det(Type, TypeCtor, TypeArgs),
        determinism_components(Detism, CanFail, at_most_one),
        lookup_mode_num(ModuleInfo, TypeCtor, UnifyMode, Detism, ProcId),
        ( if
            % On the Erlang backend, it was faster for us to use builtin
            % comparison operators on high level data structures than to
            % deconstruct the data structure and compare the atomic
            % constituents. We can only do this on values of a type
            % if that type does not have user-defined equality.
            %
            % The Erlang backend was the only one on which
            % can_compare_compound_values could ever be "yes".
            %
            % globals.lookup_bool_option(Globals,
            %   can_compare_compound_values, yes),
            semidet_fail,
            hlds_pred.in_in_unification_proc_id(ProcId),
            type_definitely_has_no_user_defined_equality_pred(ModuleInfo, Type)
        then
            ExtraGoals = [],
            Context = goal_info_get_context(GoalInfo0),
            generate_simple_call(ModuleInfo, mercury_private_builtin_module,
                "builtin_compound_eq", pf_predicate, only_mode, detism_semi,
                purity_pure, [], [XVar, YVar], [], instmap_delta_bind_no_var,
                Context, Call)
        else if
            hlds_pred.in_in_unification_proc_id(ProcId),

            % For most imported types, we only generate unification
            % predicate declarations if they are needed for complicated
            % unifications other than proc_id 0. higher_order.m will
            % specialize these cases if possible.
            special_pred_is_generated_lazily(ModuleInfo, TypeCtor)
        then
            make_type_info_vars([Type], TypeInfoVars, ExtraGoals, !Info),
            ( if TypeInfoVars = [TypeInfoVarPrime] then
                TypeInfoVar = TypeInfoVarPrime
            else
                unexpected($pred, "more than one typeinfo for one type var")
            ),
            call_generic_unify(TypeInfoVar, XVar, YVar, ModuleInfo, !.Info,
                UnifyContext, GoalInfo0, Call)
        else
            % Convert other complicated unifications into calls to
            % specific unification predicates, inserting extra typeinfo
            % arguments if necessary.
            make_type_info_vars(TypeArgs, TypeInfoVars, ExtraGoals, !Info),
            call_specific_unify(TypeCtor, TypeInfoVars, XVar, YVar, ProcId,
                ModuleInfo, UnifyContext, GoalInfo0, Call0, CallGoalInfo0),
            simplify_goal_expr(Call0, Call1, CallGoalInfo0, CallGoalInfo1,
                NestedContext0, InstMap0, !Common, !Info),
            Call = hlds_goal(Call1, CallGoalInfo1)
        )
    ),
    Conjuncts = ExtraGoals ++ [Call],
    conj_list_to_goal(Conjuncts, GoalInfo0, Goal).

:- pred call_generic_unify(prog_var::in, prog_var::in,  prog_var::in,
    module_info::in, simplify_info::in, unify_context::in,
    hlds_goal_info::in, hlds_goal::out) is det.

call_generic_unify(TypeInfoVar, XVar, YVar, ModuleInfo, _, _, GoalInfo,
        Call) :-
    Context = goal_info_get_context(GoalInfo),
    generate_simple_call(ModuleInfo, mercury_public_builtin_module, "unify",
        pf_predicate, mode_no(0), detism_semi, purity_pure,
        [TypeInfoVar], [XVar, YVar], [], instmap_delta_bind_no_var,
        Context, Call).

:- pred call_specific_unify(type_ctor::in, list(prog_var)::in,
    prog_var::in, prog_var::in, proc_id::in,
    module_info::in, unify_context::in, hlds_goal_info::in,
    hlds_goal_expr::out, hlds_goal_info::out) is det.

call_specific_unify(TypeCtor, TypeInfoVars, XVar, YVar, ProcId, ModuleInfo,
        Context, GoalInfo0, CallExpr, CallGoalInfo) :-
    % Create the new call goal.
    ArgVars = TypeInfoVars ++ [XVar, YVar],
    module_info_get_special_pred_maps(ModuleInfo, SpecialPredMaps),
    UnifyMap = SpecialPredMaps ^ spm_unify_map,
    map.lookup(UnifyMap, TypeCtor, PredId),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    SymName = qualified(ModuleName, PredName),
    CallContext = call_unify_context(XVar, rhs_var(YVar), Context),
    CallExpr = plain_call(PredId, ProcId, ArgVars, not_builtin,
        yes(CallContext), SymName),

    % Add the extra type_info vars to the nonlocals for the call.
    NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
    set_of_var.insert_list(TypeInfoVars, NonLocals0, NonLocals),
    goal_info_set_nonlocals(NonLocals, GoalInfo0, CallGoalInfo).

%---------------------------------------------------------------------------%

:- pred make_type_info_vars(list(mer_type)::in, list(prog_var)::out,
    list(hlds_goal)::out, simplify_info::in, simplify_info::out) is det.

make_type_info_vars(Types, TypeInfoVars, TypeInfoGoals, !Info) :-
    % Extract the information from simplify_info.
    simplify_info_get_varset(!.Info, VarSet0),
    simplify_info_get_var_types(!.Info, VarTypes0),
    simplify_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    simplify_info_get_module_info(!.Info, ModuleInfo0),
    simplify_info_get_pred_proc_id(!.Info, PredProcId),

    some [!PredInfo, !ProcInfo] (
        % The varset, vartypes and rtti_varmaps get updated by the call to
        % polymorphism_make_type_info_vars_raw_store below, which will get
        % this information from the pred_info and proc_info.
        module_info_pred_proc_info(ModuleInfo0, PredProcId,
            !:PredInfo, !:ProcInfo),
        proc_info_set_vartypes(VarTypes0, !ProcInfo),
        proc_info_set_varset(VarSet0, !ProcInfo),
        proc_info_set_rtti_varmaps(RttiVarMaps0, !ProcInfo),

        % Generate the code that creates the type_infos.
        term.context_init(Context),
        polymorphism_make_type_info_vars_raw(Types, Context,
            TypeInfoVars, TypeInfoGoals, ModuleInfo0, ModuleInfo1,
            !PredInfo, !ProcInfo),

        proc_info_get_vartypes(!.ProcInfo, VarTypes),
        proc_info_get_varset(!.ProcInfo, VarSet),
        proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps),
        simplify_info_set_var_types(VarTypes, !Info),
        simplify_info_set_varset(VarSet, !Info),
        simplify_info_set_rtti_varmaps(RttiVarMaps, !Info),

        % Put the new proc_info and pred_info back in the module_info
        % and put the new module_info back in the simplify_info.
        module_info_set_pred_proc_info(PredProcId, !.PredInfo, !.ProcInfo,
            ModuleInfo1, ModuleInfo),
        simplify_info_set_module_info(ModuleInfo, !Info)
    ).

:- pred get_type_info_locn(tvar::in, kind::in, prog_context::in, prog_var::out,
    list(hlds_goal)::out, simplify_info::in, simplify_info::out) is det.

get_type_info_locn(TypeVar, Kind, Context, TypeInfoVar, Goals, !Info) :-
    simplify_info_get_rtti_varmaps(!.Info, RttiVarMaps),
    rtti_lookup_type_info_locn(RttiVarMaps, TypeVar, TypeInfoLocn),
    (
        % If the typeinfo is available in a variable, just use it.
        TypeInfoLocn = type_info(TypeInfoVar),
        Goals = []
    ;
        % If the typeinfo is in a typeclass_info then we need to extract it.
        TypeInfoLocn = typeclass_info(TypeClassInfoVar, Index),
        extract_type_info(TypeVar, Kind, TypeClassInfoVar, Index, Context,
            Goals, TypeInfoVar, !Info)
    ).

:- pred extract_type_info(tvar::in, kind::in, prog_var::in, int::in,
    prog_context::in, list(hlds_goal)::out, prog_var::out,
    simplify_info::in, simplify_info::out) is det.

extract_type_info(TypeVar, Kind, TypeClassInfoVar, Index, Context,
        Goals, TypeInfoVar, !Info) :-
    simplify_info_get_module_info(!.Info, ModuleInfo),
    simplify_info_get_varset(!.Info, VarSet0),
    simplify_info_get_var_types(!.Info, VarTypes0),
    simplify_info_get_rtti_varmaps(!.Info, RttiVarMaps0),

    polymorphism_type_info.gen_extract_type_info(ModuleInfo, TypeVar, Kind,
        TypeClassInfoVar, iov_int(Index), Context, Goals, TypeInfoVar,
        VarSet0, VarSet, VarTypes0, VarTypes, RttiVarMaps0, RttiVarMaps),

    simplify_info_set_var_types(VarTypes, !Info),
    simplify_info_set_varset(VarSet, !Info),
    simplify_info_set_rtti_varmaps(RttiVarMaps, !Info).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_goal_unify.
%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module transform_hlds.higher_order.specialize_unify_compare.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.higher_order.higher_order_info.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % Succeed if the called pred is "unify" or "compare" and is specializable,
    % returning a specialized goal.
    %
:- pred specialize_call_to_unify_or_compare(pred_id::in, proc_id::in,
    list(prog_var)::in, maybe(call_unify_context)::in,
    hlds_goal_info::in, hlds_goal_expr::out,
    higher_order_info::in, higher_order_info::out) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.add_special_pred.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.special_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_construct.
:- import_module parse_tree.prog_type_test.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.higher_order.higher_order_global_info.

:- import_module bool.
:- import_module map.
:- import_module require.
:- import_module term_context.

%---------------------------------------------------------------------------%

specialize_call_to_unify_or_compare(CalledPred, CalledProc, Args, MaybeContext,
        OrigGoalInfo, Goal, !Info) :-
    ModuleInfo = hogi_get_module_info(hoi_get_global_info(!.Info)),
    ProcInfo0 = hoi_get_proc_info(!.Info),
    KnownVarMap = hoi_get_known_var_map(!.Info),
    proc_info_get_var_table(ProcInfo0, VarTable),
    module_info_pred_info(ModuleInfo, CalledPred, CalledPredInfo),
    mercury_public_builtin_module = pred_info_module(CalledPredInfo),
    PredName = pred_info_name(CalledPredInfo),
    pred_info_get_orig_arity(CalledPredInfo,
        pred_form_arity(PredFormArityInt)),
    special_pred_name_arity(SpecialId, PredName, _, PredFormArityInt),
    special_pred_get_type(SpecialId, Args, Var),
    lookup_var_type(VarTable, Var, Type),
    Type \= type_variable(_, _),
    % Don't specialize tuple types -- the code to unify them only exists
    % in the generic unification routine in the runtime.
    % `private_builtin.builtin_unify_tuple/2' and
    % `private_builtin.builtin_compare_tuple/3' always abort.
    %
    % NOTE It might be worth inlining complicated unifications of small tuples,
    % or of any other small type.
    Type \= tuple_type(_, _),

    Args = [TypeInfoVar | SpecialPredArgs],
    map.search(KnownVarMap, TypeInfoVar,
        known_const(_TypeInfoConsId, TypeInfoVarArgs)),
    type_to_ctor(Type, TypeCtor),
    TypeCtor = type_ctor(_, TypeArity),
    ( if TypeArity = 0 then
        TypeInfoArgs = []
    else
        TypeInfoVarArgs = [_TypeCtorInfo | TypeInfoArgs]
    ),
    ( if
        not type_has_user_defined_equality_pred(ModuleInfo, Type, _),
        proc_id_to_int(CalledProc, CalledProcInt),
        CalledProcInt = 0,
        (
            SpecialId = spec_pred_unify,
            SpecialPredArgs = [Arg1, Arg2],
            MaybeResult = no
        ;
            SpecialId = spec_pred_compare,
            SpecialPredArgs = [Result, Arg1, Arg2],
            MaybeResult = yes(Result)
        )
    then
        ( if
            is_type_a_dummy(ModuleInfo, Type) = is_dummy_type
        then
            specialize_unify_or_compare_pred_for_dummy(MaybeResult, Goal,
                !Info)
        else if
            % Look for unification or comparison applied directly to a
            % builtin or atomic type. This needs to be done separately from
            % the case for user-defined types, for two reasons.
            %
            % First, because we want to specialize such calls even if we are
            % not generating any special preds.
            %
            % Second, because the specialized code is different in the two
            % cases: here it is a call to a builtin predicate, perhaps preceded
            % by casts; there it is a call to a compiler-generated predicate.

            type_is_atomic(ModuleInfo, Type)
        then
            specialize_unify_or_compare_pred_for_atomic(Type, MaybeResult,
                Arg1, Arg2, MaybeContext, OrigGoalInfo, Goal, !Info)
        else if
            % Look for unification or comparison applied to a no-tag type
            % wrapping a builtin or atomic type. This needs to be done to
            % optimize all the map_lookups with keys of type `term.var/1'
            % in the compiler. (:- type var(T) ---> var(int).)
            %
            % This could possibly be better handled by just inlining the
            % unification code, but the compiler doesn't have the code for
            % the comparison or in-in unification procedures for imported
            % types, and unification and comparison may be implemented in
            % C code in the runtime system.

            type_is_no_tag_type(ModuleInfo, Type, Constructor, WrappedType),
            not type_has_user_defined_equality_pred(ModuleInfo,
                WrappedType, _),

            % This could be done for non-atomic types, but it would be a bit
            % more complicated because the type-info for the wrapped type
            % would need to be extracted first.
            type_is_atomic(ModuleInfo, WrappedType)
        then
            WrappedTypeIsDummy = is_type_a_dummy(ModuleInfo, WrappedType),
            specialize_unify_or_compare_pred_for_no_tag(Type, WrappedType,
                WrappedTypeIsDummy, Constructor, MaybeResult, Arg1, Arg2,
                MaybeContext, OrigGoalInfo, Goal, !Info)
        else
            create_goal_to_call_type_specific_unify_or_compare(Type, SpecialId,
                TypeInfoArgs, SpecialPredArgs, MaybeContext, Goal, !Info)
        )
    else
        create_goal_to_call_type_specific_unify_or_compare(Type, SpecialId,
            TypeInfoArgs, SpecialPredArgs, MaybeContext, Goal, !Info)
    ).

:- pred create_goal_to_call_type_specific_unify_or_compare(mer_type::in,
    special_pred_id::in, list(prog_var)::in, list(prog_var)::in,
    maybe(call_unify_context)::in, hlds_goal_expr::out,
    higher_order_info::in, higher_order_info::out) is semidet.

create_goal_to_call_type_specific_unify_or_compare(SpecialPredType, SpecialId,
        TypeInfoArgs, SpecialPredArgs, MaybeContext, Goal, !Info) :-
    % We can only specialize unifications and comparisons to call the
    % type-specific unify or compare predicate if we are generating
    % such predicates.
    type_to_ctor_det(SpecialPredType, SpecialPredTypeCtor),
    find_unify_or_compare_proc(SpecialPredTypeCtor, SpecialId, SymName,
        SpecialPredId, SpecialProcId, !Info),
    ( if type_is_higher_order(SpecialPredType) then
        % Builtin_*_pred are special cases which don't need the type-info
        % arguments.
        CallArgs = SpecialPredArgs
    else
        CallArgs = TypeInfoArgs ++ SpecialPredArgs
    ),
    Goal = plain_call(SpecialPredId, SpecialProcId, CallArgs, not_builtin,
        MaybeContext, SymName).

:- pred specialize_unify_or_compare_pred_for_dummy(maybe(prog_var)::in,
    hlds_goal_expr::out, higher_order_info::in, higher_order_info::out) is det.

specialize_unify_or_compare_pred_for_dummy(MaybeResult, GoalExpr, !Info) :-
    (
        MaybeResult = no,
        GoalExpr = conj(plain_conj, [])     % true
    ;
        MaybeResult = yes(ComparisonResult),
        Builtin = mercury_public_builtin_module,
        TypeCtor = type_ctor(qualified(Builtin, "comparison_result"), 0),
        Eq = cons(qualified(mercury_public_builtin_module, "="), 0, TypeCtor),
        make_const_construction(dummy_context, ComparisonResult, Eq, Goal),
        Goal = hlds_goal(GoalExpr, _)
    ).

:- pred specialize_unify_or_compare_pred_for_atomic(mer_type::in,
    maybe(prog_var)::in, prog_var::in, prog_var::in,
    maybe(call_unify_context)::in, hlds_goal_info::in, hlds_goal_expr::out,
    higher_order_info::in, higher_order_info::out) is det.

specialize_unify_or_compare_pred_for_atomic(SpecialPredType, MaybeResult,
        Arg1, Arg2, MaybeContext, OrigGoalInfo, GoalExpr, !Info) :-
    ModuleInfo = hogi_get_module_info(hoi_get_global_info(!.Info)),
    ProcInfo0 = hoi_get_proc_info(!.Info),
    (
        MaybeResult = no,
        UnifyMode = unify_modes_li_lf_ri_rf(ground_inst, ground_inst,
            ground_inst, ground_inst),
        GoalExpr = unify(Arg1, rhs_var(Arg2), UnifyMode,
            simple_test(Arg1, Arg2), unify_context(umc_explicit, []))
    ;
        MaybeResult = yes(ComparisonResult),
        find_builtin_type_with_equivalent_compare(ModuleInfo,
            SpecialPredType, CompareType, NeedIntCast),
        type_to_ctor_det(CompareType, CompareTypeCtor),
        get_special_proc_det(ModuleInfo, CompareTypeCtor, spec_pred_compare,
            SymName, SpecialPredId, SpecialProcId),
        (
            NeedIntCast = no,
            NewCallArgs = [ComparisonResult, Arg1, Arg2],
            GoalExpr = plain_call(SpecialPredId, SpecialProcId, NewCallArgs,
                not_builtin, MaybeContext, SymName)
        ;
            NeedIntCast = yes,
            Context = goal_info_get_context(OrigGoalInfo),
            generate_unsafe_type_cast(Context, CompareType, is_not_dummy_type,
                Arg1, CastArg1, CastGoal1, ProcInfo0, ProcInfo1),
            generate_unsafe_type_cast(Context, CompareType, is_not_dummy_type,
                Arg2, CastArg2, CastGoal2, ProcInfo1, ProcInfo),
            NewCallArgs = [ComparisonResult, CastArg1, CastArg2],
            Call = plain_call(SpecialPredId, SpecialProcId, NewCallArgs,
                not_builtin, MaybeContext, SymName),
            set_of_var.list_to_set([ComparisonResult, Arg1, Arg2], NonLocals),
            InstMapDelta = instmap_delta_bind_var(ComparisonResult),
            Detism = detism_det,
            goal_info_init(NonLocals, InstMapDelta, Detism, purity_pure,
                Context, GoalInfo),
            GoalExpr = conj(plain_conj,
                [CastGoal1, CastGoal2, hlds_goal(Call, GoalInfo)]),
            hoi_set_proc_info(ProcInfo, !Info)
        )
    ).

:- pred specialize_unify_or_compare_pred_for_no_tag(mer_type::in, mer_type::in,
    is_dummy_type::in, sym_name::in, maybe(prog_var)::in,
    prog_var::in, prog_var::in, maybe(call_unify_context)::in,
    hlds_goal_info::in, hlds_goal_expr::out,
    higher_order_info::in, higher_order_info::out) is det.

specialize_unify_or_compare_pred_for_no_tag(OuterType, WrappedType,
        WrappedTypeIsDummy, Constructor, MaybeResult, Arg1, Arg2,
        MaybeContext, OrigGoalInfo, GoalExpr, !Info) :-
    ModuleInfo = hogi_get_module_info(hoi_get_global_info(!.Info)),
    ProcInfo0 = hoi_get_proc_info(!.Info),
    Context = goal_info_get_context(OrigGoalInfo),
    create_goal_to_unwrap_no_tag_arg(OuterType, WrappedType,
        WrappedTypeIsDummy, Context, Constructor, Arg1, UnwrappedArg1,
        ExtractGoal1, ProcInfo0, ProcInfo1),
    create_goal_to_unwrap_no_tag_arg(OuterType, WrappedType,
        WrappedTypeIsDummy, Context, Constructor, Arg2, UnwrappedArg2,
        ExtractGoal2, ProcInfo1, ProcInfo2),
    set_of_var.list_to_set([UnwrappedArg1, UnwrappedArg2], NonLocals0),
    (
        MaybeResult = no,
        NonLocals = NonLocals0,
        instmap_delta_init_reachable(InstMapDelta),
        Detism = detism_semi,
        UnifyMode = unify_modes_li_lf_ri_rf(ground_inst, ground_inst,
            ground_inst, ground_inst),
        SpecialGoal = unify(UnwrappedArg1, rhs_var(UnwrappedArg2),
            UnifyMode, simple_test(UnwrappedArg1, UnwrappedArg2),
            unify_context(umc_explicit, [])),
        goal_info_init(NonLocals, InstMapDelta, Detism, purity_pure,
            Context, GoalInfo),
        GoalExpr = conj(plain_conj,
            [ExtractGoal1, ExtractGoal2, hlds_goal(SpecialGoal, GoalInfo)]),
        hoi_set_proc_info(ProcInfo2, !Info)
    ;
        MaybeResult = yes(ComparisonResult),
        set_of_var.insert(ComparisonResult, NonLocals0, NonLocals),
        InstMapDelta = instmap_delta_bind_var(ComparisonResult),
        Detism = detism_det,
        % Build a new call with the unwrapped arguments.
        find_builtin_type_with_equivalent_compare(ModuleInfo, WrappedType,
            CompareType, NeedIntCast),
        type_to_ctor_det(CompareType, CompareTypeCtor),
        get_special_proc_det(ModuleInfo, CompareTypeCtor, spec_pred_compare,
            SymName, SpecialPredId, SpecialProcId),
        (
            NeedIntCast = no,
            NewCallArgs = [ComparisonResult, UnwrappedArg1, UnwrappedArg2],
            SpecialGoal = plain_call(SpecialPredId, SpecialProcId, NewCallArgs,
                not_builtin, MaybeContext, SymName),
            goal_info_init(NonLocals, InstMapDelta, Detism, purity_pure,
                Context, GoalInfo),
            GoalExpr = conj(plain_conj, [ExtractGoal1, ExtractGoal2,
                hlds_goal(SpecialGoal, GoalInfo)]),
            hoi_set_proc_info(ProcInfo2, !Info)
        ;
            NeedIntCast = yes,
            generate_unsafe_type_cast(Context, CompareType, is_not_dummy_type,
                UnwrappedArg1, CastArg1, CastGoal1, ProcInfo2, ProcInfo3),
            generate_unsafe_type_cast(Context, CompareType, is_not_dummy_type,
                UnwrappedArg2, CastArg2, CastGoal2, ProcInfo3, ProcInfo4),
            NewCallArgs = [ComparisonResult, CastArg1, CastArg2],
            SpecialGoal = plain_call(SpecialPredId, SpecialProcId, NewCallArgs,
                not_builtin, MaybeContext, SymName),
            goal_info_init(NonLocals, InstMapDelta, Detism, purity_pure,
                Context, GoalInfo),
            GoalExpr = conj(plain_conj,
                [ExtractGoal1, CastGoal1, ExtractGoal2, CastGoal2,
                hlds_goal(SpecialGoal, GoalInfo)]),
            hoi_set_proc_info(ProcInfo4, !Info)
        )
    ).

    % ZZZ HOGI add type unify/compare pred to module_info
:- pred find_unify_or_compare_proc(type_ctor::in, special_pred_id::in,
    sym_name::out, pred_id::out, proc_id::out,
    higher_order_info::in, higher_order_info::out) is semidet.

find_unify_or_compare_proc(TypeCtor, SpecialId, SymName, PredId, ProcId,
        !Info) :-
    ModuleInfo0 = hogi_get_module_info(hoi_get_global_info(!.Info)),
    ( if
        get_special_proc(ModuleInfo0, TypeCtor, SpecialId, SymName0,
            PredId0, ProcId0)
    then
        SymName = SymName0,
        PredId = PredId0,
        ProcId = ProcId0
    else
        special_pred_is_generated_lazily(ModuleInfo0, TypeCtor),
        (
            SpecialId = spec_pred_compare,
            add_lazily_generated_compare_pred_decl(TypeCtor, PredId,
                ModuleInfo0, ModuleInfo),
            ProcId = hlds_pred.initial_proc_id
        ;
            SpecialId = spec_pred_index,
            % This shouldn't happen. The index predicate should only be called
            % from the compare predicate. If it is called, it shouldn't be
            % generated lazily.
            fail
        ;
            SpecialId = spec_pred_unify,
            % XXX We should only add the declaration, not the body, for the
            % unify pred, but that complicates things if mode analysis is rerun
            % after higher_order.m and requests more unification procedures.
            % In particular, it is difficult to run polymorphism on the new
            % clauses if the predicate's arguments have already had type-infos
            % added. This case shouldn't come up unless an optimization does
            % reordering which requires rescheduling a conjunction.
            add_lazily_generated_unify_pred(TypeCtor, PredId,
                ModuleInfo0, ModuleInfo),
            hlds_pred.in_in_unification_proc_id(ProcId)
        ),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        ModuleName = pred_info_module(PredInfo),
        Name = pred_info_name(PredInfo),
        SymName = qualified(ModuleName, Name),
        GlobalInfo1 = hoi_get_global_info(!.Info),
        hogi_set_module_info(ModuleInfo, GlobalInfo1, GlobalInfo),
        hoi_set_global_info(GlobalInfo, !Info)
    ).

:- pred find_builtin_type_with_equivalent_compare(module_info::in,
    mer_type::in, mer_type::out, bool::out) is det.

find_builtin_type_with_equivalent_compare(ModuleInfo, Type, EqvType,
        NeedIntCast) :-
    CtorCat = classify_type(ModuleInfo, Type),
    (
        CtorCat = ctor_cat_builtin(_),
        EqvType = Type,
        NeedIntCast = no
    ;
        CtorCat = ctor_cat_enum(_),
        construct_type(type_ctor(unqualified("int"), 0), [], EqvType),
        NeedIntCast = yes
    ;
        ( CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_user(_)
        ; CtorCat = ctor_cat_system(_)
        ),
        unexpected($pred, "bad type")
    ).

:- pred generate_unsafe_type_cast(prog_context::in,
    mer_type::in, is_dummy_type::in, prog_var::in, prog_var::out,
    hlds_goal::out, proc_info::in, proc_info::out) is det.

generate_unsafe_type_cast(Context, ToType, IsDummy, Arg, CastArg, Goal,
        !ProcInfo) :-
    proc_info_create_var_from_type("", ToType, IsDummy, CastArg, !ProcInfo),
    generate_cast(unsafe_type_cast, Arg, CastArg, Context, Goal).

:- pred create_goal_to_unwrap_no_tag_arg(mer_type::in, mer_type::in,
    is_dummy_type::in, prog_context::in, sym_name::in,
    prog_var::in, prog_var::out, hlds_goal::out,
    proc_info::in, proc_info::out) is det.

create_goal_to_unwrap_no_tag_arg(OuterType, WrappedType, IsDummy, Context,
        Constructor, Arg, UnwrappedArg, Goal, !ProcInfo) :-
    proc_info_create_var_from_type("", WrappedType, IsDummy,
        UnwrappedArg, !ProcInfo),
    type_to_ctor_det(OuterType, OuterTypeCtor),
    ConsId = cons(Constructor, 1, OuterTypeCtor),
    Ground = ground(shared, none_or_default_func),
    UnifyModeInOut = unify_modes_li_lf_ri_rf(Ground, Ground, free, Ground),
    ArgModes = [UnifyModeInOut],
    set_of_var.list_to_set([Arg, UnwrappedArg], NonLocals),
    % This will be recomputed later.
    InstMapDelta = instmap_delta_bind_var(UnwrappedArg),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure, Context,
        GoalInfo),
    Unification = deconstruct(Arg, ConsId, [UnwrappedArg], ArgModes,
        cannot_fail, cannot_cgc),
    GoalExpr = unify(Arg,
        rhs_functor(ConsId, is_not_exist_constr, [UnwrappedArg]),
        UnifyModeInOut, Unification, unify_context(umc_explicit, [])),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.higher_order.specialize_unify_compare.
%---------------------------------------------------------------------------%

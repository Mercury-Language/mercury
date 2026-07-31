%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2021, 2023-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck_coerce.m.
% Main author: wangp.
%
% This module typechecks coerce operations.
%
% This file contains three parts:
%
% - Part1 contains code that is specific to the implementation of
%   the first exported predicate, typecheck_coerce, which is called
%   by the typechecker while it processes procedure bodies.
%
% - Part 2 contains code that is specific to the implementation of
%   the second exported predicate, typecheck_prune_coerce_constraints,
%   which is called by the typechecker when it is finishing up
%   the processing of procedure bodies.
%
% - Part 3 contains the code that is called by both Part 1 and Part 2,
%   which actually does the most meaningful part of the work.
%
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck_coerce.
:- interface.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.typecheck_info.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred typecheck_coerce(typecheck_info::in, prog_context::in,
    list(prog_var)::in, type_assign_set::in, type_assign_set::out) is det.

%---------------------------------------------------------------------------%

    % Check coerce constraints in each type assignment to see if they can be
    % satisfied. If there are one or more type assignments in which all
    % coerce constraints are satisfied, then keep only those type assignments
    % and discard the rest -- we don't need to consider the type assignments
    % with unsatisfiable coerce constraints any more.
    %
:- pred typecheck_prune_coerce_constraints(typecheck_info::in,
    type_assign_set::in, type_assign_set::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.typecheck_util.
:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.type_util.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_type_test.
:- import_module parse_tree.vartypes.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module one_or_more_map.
:- import_module require.
:- import_module term_context.
:- import_module uint.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Part 1.
%

typecheck_coerce(Info, Context, Args, TypeAssignSet0, TypeAssignSet) :-
    ( if Args = [FromVar0, ToVar0] then
        FromVar = FromVar0,
        ToVar = ToVar0
    else
        unexpected($pred, "coerce requires two arguments")
    ),
    list.map(typecheck_coerce_in_type_assign(Info, Context, FromVar, ToVar),
        TypeAssignSet0, TypeAssignSet).

:- pred typecheck_coerce_in_type_assign(typecheck_info::in, prog_context::in,
    prog_var::in, prog_var::in, type_assign::in, type_assign::out) is det.

typecheck_coerce_in_type_assign(Info, Context, FromVar, ToVar,
        TypeAssign0, TypeAssign) :-
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    type_assign_get_typevarset(TypeAssign0, TVarSet0),
    type_assign_get_existq_tvars(TypeAssign0, ExistQTVars0),
    type_assign_get_type_bindings(TypeAssign0, TypeBindings0),

    ( if search_var_type(VarTypes0, FromVar, FromType0) then
        apply_rec_subst_to_type(TypeBindings0, FromType0, FromType),
        TypeAssign1 = TypeAssign0
    else
        type_assign_fresh_type_var(FromVar, FromType,
            TypeAssign0, TypeAssign1)
    ),
    ( if search_var_type(VarTypes0, ToVar, ToType0) then
        apply_rec_subst_to_type(TypeBindings0, ToType0, ToType),
        TypeAssign2 = TypeAssign1
    else
        % Handle X = coerce(X).
        ( if ToVar = FromVar then
            ToType = FromType,
            TypeAssign2 = TypeAssign1
        else
            type_assign_fresh_type_var(ToVar, ToType,
                TypeAssign1, TypeAssign2)
        )
    ),

    ( if
        type_is_ground_except_vars(ExistQTVars0, FromType),
        type_is_ground_except_vars(ExistQTVars0, ToType)
    then
        % We can compare the types on both sides immediately.
        typecheck_info_get_type_table(Info, TypeTable),
        % NOTE The following block of code has a near-duplicate below
        % in check_coerce_constraint_if_ready, though the two places differ
        % in how they handle both resolved and not-yet-resolved constraints.
        % XXX Should we make invariant_tparams_map part of typecheck_info?
        typecheck_coerce_between_types(TypeTable, TVarSet0,
            FromType, ToType, TypeAssign2, TypeAssign3,
            init_invariant_tparams_map, _InvariantTParamsMap, CoerceFails),
        (
            CoerceFails = [],
            type_assign_get_type_bindings(TypeAssign3, TypeBindings1),
            ( if is_same_type_after_subst(TypeBindings1, FromType, ToType) then
                Coercion = coerce_constraint(FromType, ToType, Context,
                    FromVar, satisfied_but_redundant, []),
                add_coerce_constraint(Coercion, TypeAssign3, TypeAssign)
            else
                TypeAssign = TypeAssign3
            )
        ;
            CoerceFails = [_HeadCoerceFail | _TailCoerceFails],
            Coercion = coerce_constraint(FromType, ToType, Context, FromVar,
                unsatisfiable, CoerceFails),
            add_coerce_constraint(Coercion, TypeAssign2, TypeAssign)
        )
    else
        % One or both of the types is not known yet. Add a coercion constraint
        % on the type assignment to be checked after typechecking the clause.
        CoerceFail = nonground_type(ExistQTVars0, FromType, ToType),
        Coercion = coerce_constraint(FromType, ToType, Context, FromVar,
            need_to_check, [CoerceFail]),
        add_coerce_constraint(Coercion, TypeAssign2, TypeAssign)
    ).

:- pred is_same_type_after_subst(tsubst::in, mer_type::in, mer_type::in)
    is semidet.

is_same_type_after_subst(TypeBindings, TypeA0, TypeB0) :-
    apply_rec_subst_to_type(TypeBindings, TypeA0, TypeA),
    apply_rec_subst_to_type(TypeBindings, TypeB0, TypeB),
    strip_kind_annotation(TypeA) = strip_kind_annotation(TypeB).

:- pred add_coerce_constraint(coerce_constraint::in,
    type_assign::in, type_assign::out) is det.

add_coerce_constraint(Coercion, !TypeAssign) :-
    type_assign_get_coerce_constraints(!.TypeAssign, Coercions0),
    Coercions = [Coercion | Coercions0],
    type_assign_set_coerce_constraints(Coercions, !TypeAssign).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Part 2.
%

typecheck_prune_coerce_constraints(Info, TypeAssignSet0, TypeAssignSet) :-
    typecheck_info_get_type_table(Info, TypeTable),
    list.map(type_assign_prune_coerce_constraints(TypeTable),
        TypeAssignSet0, TypeAssignSet1),
    list.filter(type_assign_has_only_satisfied_coerce_constraints,
        TypeAssignSet1, SatisfiedTypeAssignSet, UnsatisfiedTypeAssignSet),
    (
        SatisfiedTypeAssignSet = [_ | _],
        TypeAssignSet = SatisfiedTypeAssignSet
    ;
        SatisfiedTypeAssignSet = [],
        TypeAssignSet = UnsatisfiedTypeAssignSet
    ).

:- pred type_assign_prune_coerce_constraints(type_table::in,
    type_assign::in, type_assign::out) is det.

type_assign_prune_coerce_constraints(TypeTable, !TypeAssign) :-
    type_assign_get_coerce_constraints(!.TypeAssign, Coercions0),
    (
        Coercions0 = []
    ;
        Coercions0 = [_ | _],
        check_pending_coerce_constraints_to_fixpoint(TypeTable,
            Coercions0, Coercions, !TypeAssign),
        type_assign_set_coerce_constraints(Coercions, !TypeAssign)
    ).

:- pred check_pending_coerce_constraints_to_fixpoint(type_table::in,
    list(coerce_constraint)::in, list(coerce_constraint)::out,
    type_assign::in, type_assign::out) is det.

check_pending_coerce_constraints_to_fixpoint(TypeTable, Coercions0, Coercions,
        !TypeAssign) :-
    check_pending_coerce_constraints_loop(TypeTable, Coercions0,
        KeepCoercions, DelayedCoercions, !TypeAssign, no, MadeProgress),
    (
        MadeProgress = no,
        % All coerce constraints were delayed; give up.
        list.map(set_coerce_constraint_to_not_yet_resolved,
            DelayedCoercions, Coercions)
    ;
        MadeProgress = yes,
        check_pending_coerce_constraints_to_fixpoint(TypeTable,
            DelayedCoercions, Coercions1, !TypeAssign),
        Coercions = KeepCoercions ++ Coercions1
    ).

:- pred check_pending_coerce_constraints_loop(type_table::in,
    list(coerce_constraint)::in, list(coerce_constraint)::out,
    list(coerce_constraint)::out, type_assign::in, type_assign::out,
    bool::in, bool::out) is det.

check_pending_coerce_constraints_loop(_TypeTable, [], [], [],
        !TypeAssign, !MadeProgress).
check_pending_coerce_constraints_loop(TypeTable, [Coercion0 | Coercions0],
        KeepCoercions, DelayedCoercions, !TypeAssign, !MadeProgress) :-
    check_coerce_constraint_if_ready(TypeTable, Coercion0, CheckResult,
        !TypeAssign),
    (
        CheckResult = prune,
        !:MadeProgress = yes,
        check_pending_coerce_constraints_loop(TypeTable, Coercions0,
            KeepCoercions, DelayedCoercions, !TypeAssign, !MadeProgress)
    ;
        CheckResult = keep(Coercion),
        !:MadeProgress = yes,
        check_pending_coerce_constraints_loop(TypeTable, Coercions0,
            TailKeepCoercions, DelayedCoercions, !TypeAssign, !MadeProgress),
        KeepCoercions = [Coercion | TailKeepCoercions]
    ;
        CheckResult = delay,
        check_pending_coerce_constraints_loop(TypeTable, Coercions0,
            KeepCoercions, TailDelayedCoercions, !TypeAssign, !MadeProgress),
        DelayedCoercions = [Coercion0 | TailDelayedCoercions]
    ).

:- type check_coerce_constraint_action
    --->    prune
    ;       keep(coerce_constraint)
    ;       delay.

:- pred check_coerce_constraint_if_ready(type_table::in, coerce_constraint::in,
    check_coerce_constraint_action::out, type_assign::in, type_assign::out)
    is det.

check_coerce_constraint_if_ready(TypeTable, Coercion0, Action, !TypeAssign) :-
    Coercion0 = coerce_constraint(FromType0, ToType0, Context, FromVar,
        Status0, _CoerceFails0),
    (
        Status0 = need_to_check,
        TypeAssign0 = !.TypeAssign,
        type_assign_get_typevarset(TypeAssign0, TVarSet0),
        type_assign_get_existq_tvars(TypeAssign0, ExistQTVars0),
        type_assign_get_type_bindings(TypeAssign0, TypeBindings0),
        apply_rec_subst_to_type(TypeBindings0, FromType0, FromType),
        apply_rec_subst_to_type(TypeBindings0, ToType0, ToType),
        ( if type_is_ground_except_vars(ExistQTVars0, FromType) then
            % NOTE The following block of code has a near-duplicate above
            % in typecheck_coerce_in_type_assign, though the two places differ
            % in how they handle both resolved and not-yet-resolved
            % constraints.
            typecheck_coerce_between_types(TypeTable, TVarSet0,
                FromType, ToType, TypeAssign0, TypeAssign1,
                init_invariant_tparams_map, _InvariantTParamsMap, CoerceFails),
            (
                CoerceFails = [],
                type_assign_get_type_bindings(TypeAssign1, TypeBindings1),
                ( if
                    is_same_type_after_subst(TypeBindings1, FromType, ToType)
                then
                    Coercion = coerce_constraint(FromType, ToType, Context,
                        FromVar, satisfied_but_redundant, []),
                    Action = keep(Coercion)
                else
                    Action = prune
                ),
                !:TypeAssign = TypeAssign1
            ;
                CoerceFails = [_HeadCoerceFail | _TailCoerceFails],
                Coercion = coerce_constraint(FromType0, ToType0, Context,
                    FromVar, unsatisfiable, CoerceFails),
                Action = keep(Coercion)
            )
        else
            Action = delay
        )
    ;
        ( Status0 = unsatisfiable
        ; Status0 = not_yet_resolved
        ; Status0 = satisfied_but_redundant
        ),
        Action = keep(Coercion0)
    ).

:- pred set_coerce_constraint_to_not_yet_resolved(
    coerce_constraint::in, coerce_constraint::out) is det.

set_coerce_constraint_to_not_yet_resolved(!Coercion) :-
    !Coercion ^ coerce_status := not_yet_resolved.

:- pred type_assign_has_only_satisfied_coerce_constraints(type_assign::in)
    is semidet.

type_assign_has_only_satisfied_coerce_constraints(TypeAssign) :-
    type_assign_get_coerce_constraints(TypeAssign, Coercions),
    all_true(coerce_constraint_is_satisfied, Coercions).

:- pred coerce_constraint_is_satisfied(coerce_constraint::in) is semidet.

coerce_constraint_is_satisfied(Coercion) :-
    Coercion = coerce_constraint(_FromType, _ToType, _Context, _FromVar,
        Status, _),
    require_complete_switch [Status]
    (
        Status = satisfied_but_redundant
    ;
        ( Status = need_to_check
        ; Status = unsatisfiable
        ; Status = not_yet_resolved
        ),
        fail
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Part 3.
%

    % The type_ctors that we are currently trying to add to
    % invariant_tparams_map.
    %
:- type active_type_ctors == list(type_ctor).

    % The set of type_ctors for which we know which of their
    % type parameters must be invariant.
    %
    % Currently, both typecheck_coerce_in_type_assign and
    % check_coerce_constraint_if_ready start with a fresh
    % invariant_tparams_map, and then throw away the invariant_tparams_map
    % they build up. IF AND WHEN the recomputation of the entries in those
    % throw-away maps becomes a performance problem, we should add a slot
    % to the typecheck_sub_info structure to hold that map. They would
    % require moving this type to typecheck_info.m.
    %
:- type invariant_tparams_map == map(type_ctor, invariant_tvars).

:- func init_invariant_tparams_map = invariant_tparams_map.

init_invariant_tparams_map = map.init.

%---------------------------------------------------------------------------%

:- pred typecheck_coerce_between_types(type_table::in,
    tvarset::in, mer_type::in, mer_type::in, type_assign::in, type_assign::out,
    invariant_tparams_map::in, invariant_tparams_map::out,
    list(coerce_fail)::out) is det.

typecheck_coerce_between_types(TypeTable, TVarSet,
        FromType, ToType, !TypeAssign, !InvariantTParamsMap, CoerceFails) :-
    % Type bindings must have been applied to FromType and ToType already.
    classify_is_du_type(TypeTable, FromType, FromMaybeDuType),
    classify_is_du_type(TypeTable, ToType, ToMaybeDuType),
    are_both_types_du(FromType, ToType, FromMaybeDuType, ToMaybeDuType,
        MaybeBoth),
    (
        MaybeBoth = error2(CoerceFail),
        CoerceFails = [CoerceFail]
    ;
        MaybeBoth = ok2(FromDuTypeInfo, ToDuTypeInfo),
        compute_base_type_of_du_type(TypeTable, TVarSet,
            FromDuTypeInfo, FromBaseTypeInfo),
        compute_base_type_of_du_type(TypeTable, TVarSet,
            ToDuTypeInfo, ToBaseTypeInfo),
        FromBaseTypeInfo = du_type_info(FromBaseTypeCtor, FromBaseTypeArgTypes,
            FromBaseTypeDefn, FromBaseTypeBodyDu),
        ToBaseTypeInfo = du_type_info(ToBaseTypeCtor, ToBaseTypeArgTypes,
            _ToBaseTypeDefn, _ToBaseTypeBodyDu),
        ( if
            % Coercion can work only if the from-type and to-type
            % have the same base type constructor.
            BaseTypeCtor = FromBaseTypeCtor,
            BaseTypeCtor = ToBaseTypeCtor
        then
            % Since FromBaseTypeCtor = ToBaseTypeCtor, the two type
            % definitions and their bodies must be the same as well.
            BaseTypeDefn = FromBaseTypeDefn,
            BaseTypeBodyDu = FromBaseTypeBodyDu,
            hlds_data.get_type_defn_tparams(BaseTypeDefn, BaseTypeCtorParams),
            % Check the variance of type parameters, in the sense of
            % https://en.wikipedia.org/wiki/Type_variance.
            compute_which_type_params_must_be_invariant(TypeTable, [],
                BaseTypeCtor, BaseTypeCtorParams, BaseTypeBodyDu,
                InvariantTVars, !InvariantTParamsMap),
            are_actual_param_type_pairs_as_related_as_needed(TypeTable,
                TVarSet, InvariantTVars, BaseTypeCtor,
                BaseTypeCtorParams, FromBaseTypeArgTypes, ToBaseTypeArgTypes,
                1u, _, !TypeAssign, [], CoerceFails)
        else
            CoerceFail = different_base_types(FromType, FromBaseTypeCtor,
                ToType, ToBaseTypeCtor),
            CoerceFails = [CoerceFail]
        )
    ).

:- pred are_both_types_du(mer_type::in, mer_type::in,
    maybe_du_type::in, maybe_du_type::in,
    maybe2(du_type_info, du_type_info, coerce_fail)::out) is det.

are_both_types_du(FromType, ToType, FromMaybeDuType, ToMaybeDuType,
        MaybeBoth) :-
    (
        FromMaybeDuType = is_not_du_type(FromTypeDesc),
        ToMaybeDuType =   is_not_du_type(ToTypeDesc),
        CoerceFail = non_du_type_ctor(FromType, FromTypeDesc,
            ToType, ToTypeDesc),
        MaybeBoth = error2(CoerceFail)
    ;
        FromMaybeDuType = is_not_du_type(FromTypeDesc),
        ToMaybeDuType =   is_du_type(_),
        CoerceFail = non_du_type_ctor(FromType, FromTypeDesc, ToType, ""),
        MaybeBoth = error2(CoerceFail)
    ;
        FromMaybeDuType = is_du_type(_),
        ToMaybeDuType =   is_not_du_type(ToTypeDesc),
        CoerceFail = non_du_type_ctor(FromType, "", ToType, ToTypeDesc),
        MaybeBoth = error2(CoerceFail)
    ;
        FromMaybeDuType = is_du_type(FromDuTypeInfo),
        ToMaybeDuType =   is_du_type(ToDuTypeInfo),
        MaybeBoth = ok2(FromDuTypeInfo, ToDuTypeInfo)
    ).

%---------------------------------------------------------------------------%

:- pred compute_base_type_of_du_type(type_table::in, tvarset::in,
    du_type_info::in, du_type_info::out) is det.

compute_base_type_of_du_type(TypeTable, TVarSet, DuTypeInfo, BaseDuTypeInfo) :-
    DuTypeInfo = du_type_info(TypeCtor, ArgTypes, TypeDefn, TypeBodyDu),
    MaybeSuperType = TypeBodyDu ^ du_type_supertype,
    (
        MaybeSuperType = not_a_subtype,
        BaseDuTypeInfo = DuTypeInfo
    ;
        MaybeSuperType = subtype_of(SuperType0),
        get_supertype_of_subtype(TVarSet, TypeCtor, ArgTypes, TypeDefn,
            SuperType0, SuperType),
        classify_is_du_type(TypeTable, SuperType, MaybeSuperDuType),
        % The invocations of add_du_ctors_check_subtype_check_foreign_type
        % in make_hlds_passes.m should have already checked that
        % each declared supertype is in fact a du type, and if any
        % of those checks failed, execution should not have been allowed
        % to proceed to the typechecking pass.
        (
            MaybeSuperDuType = is_du_type(SuperDuTypeInfo)
        ;
            MaybeSuperDuType = is_not_du_type(_),
            unexpected($pred, "MaybeSuperDuType != is_du_type")
        ),
        compute_base_type_of_du_type(TypeTable, TVarSet,
            SuperDuTypeInfo, BaseDuTypeInfo)
    ).

%---------------------------------------------------------------------------%

:- type invariant_tvars == one_or_more_map(tvar, ctor_arg_posn).

    % compute_which_type_params_must_be_invariant(TypeTable, ActiveTypeCtors,
    %   BaseTypeCtor, BaseTypeDefn, BaseTypeParams, InvariantTVars,
    %   !InvariantTParamsMap):
    %
    % Our caller has checked that the from-type and the to-type
    % in the coerce operation have the same base type, BaseTypeCtor.
    % After we return, it will compare the arguments of the BaseTypeCtor
    % in the from-type and the to-type. It needs to know which parameters
    % of BaseTypeCtor (which are available here as BaseTypeParams) must be
    % identical in the from-type and the to-type, and which need only be
    % in a supertype/subtype relationship (in either direction.)
    %
    % The elements of BaseTypeParams that we return in InvariantTVars
    % fall into into the first category; the others fall into the second.
    %
:- pred compute_which_type_params_must_be_invariant(type_table::in,
    active_type_ctors::in,
    type_ctor::in, list(tvar)::in, type_body_du::in, invariant_tvars::out,
    invariant_tparams_map::in, invariant_tparams_map::out) is det.

compute_which_type_params_must_be_invariant(TypeTable, ActiveTypeCtors,
        BaseTypeCtor, BaseTypeCtorParams, BaseTypeBodyDu, InvariantTVars,
        !InvariantTParamsMap) :-
    (
        BaseTypeCtorParams = [],
        % The computation in the other branch can return two kinds of tvars
        % in InvariantTVars: tvars that occur in BaseTypeCtorParams, and
        % existentially quantified tvars in data constructors. However,
        % the only thing we ever use InvariantTVars for is to test whether
        % an element of BaseTypeCtorParams occurs in it. This is what
        % justifies returning the empty set here.
        one_or_more_map.init(InvariantTVars)
    ;
        BaseTypeCtorParams = [_ | _],
        BaseTypeBodyDu = type_body_du(OoMCtors, _OoMAlphaSortedCtors,
            _MaybeSuperType, _MaybeCanon, _MaybeTypeRepn, _IsForeignType),
        Ctors = one_or_more_to_list(OoMCtors),
        list.foldl2(
            acc_invariant_tvars_in_ctor(TypeTable, ActiveTypeCtors,
                BaseTypeCtor, BaseTypeCtorParams),
            Ctors, one_or_more_map.init, InvariantTVars, !InvariantTParamsMap)
    ).

:- pred acc_invariant_tvars_in_ctor(type_table::in, active_type_ctors::in,
    type_ctor::in, list(tvar)::in, constructor::in,
    invariant_tvars::in, invariant_tvars::out,
    invariant_tparams_map::in, invariant_tparams_map::out) is det.

acc_invariant_tvars_in_ctor(TypeTable, ActiveTypeCtors,
        BaseTypeCtor, BaseTypeCtorParams, Ctor,
        !InvariantTVars, !InvariantTParamsMap) :-
    Ctor = ctor(_Ordinal, _MaybeExist, CtorSymName, CtorArgs, Arity, _Context),
    DuCtor = du_ctor(CtorSymName, Arity, BaseTypeCtor),
    ConsId = du_data_ctor(DuCtor),
    list.foldl3(
        acc_invariant_tvars_in_ctor_arg(TypeTable, ActiveTypeCtors,
            BaseTypeCtor, BaseTypeCtorParams, ConsId),
        CtorArgs, 1u, _, !InvariantTVars, !InvariantTParamsMap).

:- pred acc_invariant_tvars_in_ctor_arg(type_table::in, active_type_ctors::in,
    type_ctor::in, list(tvar)::in, du_or_tuple_cons_id::in,
    constructor_arg::in, uint::in, uint::out,
    invariant_tvars::in, invariant_tvars::out,
    invariant_tparams_map::in, invariant_tparams_map::out) is det.

acc_invariant_tvars_in_ctor_arg(TypeTable, ActiveTypeCtors,
        BaseTypeCtor, BaseTypeCtorParams, DuCtor, CtorArg,
        !ArgNum, !InvariantTVars, !InvariantTParamsMap) :-
    CtorArg = ctor_arg(_MaybeFieldName, CtorArgType, _Context),
    % Since acc_invariant_tvars_in_ctor_arg_type is recursive,
    % we cannot inline it here.
    acc_invariant_tvars_in_ctor_arg_type(TypeTable, ActiveTypeCtors,
        BaseTypeCtor, BaseTypeCtorParams, DuCtor, CtorArgType,
        !ArgNum, !InvariantTVars, !InvariantTParamsMap).

    % We have to scan pretty much all the types that occur
    % on the right hand side of BaseTypeCtor's definition, whether they occur
    % directly as argument types of a data constructor, or as components
    % of such argument types. The only exceptions are types for which
    % we know either that
    %
    % - they definitely *must* be identical in the from-type and the to-type
    %   (as with higher order types), or that
    %
    % - they definitely *will* be identical (as with recursive types).
    %
:- pred acc_invariant_tvars_in_ctor_arg_type(type_table::in,
    active_type_ctors::in, type_ctor::in, list(tvar)::in,
    du_or_tuple_cons_id::in, mer_type::in,
    uint::in, uint::out, invariant_tvars::in, invariant_tvars::out,
    invariant_tparams_map::in, invariant_tparams_map::out) is det.

acc_invariant_tvars_in_ctor_arg_type(TypeTable, ActiveTypeCtors,
        BaseTypeCtor, BaseTypeCtorParams, ConsId, CtorArgType,
        !ArgNum, !InvariantTVars, !InvariantTParamsMap) :-
    (
        CtorArgType = builtin_type(_)
    ;
        CtorArgType = type_variable(_TypeVar, _Kind)
    ;
        CtorArgType = defined_type(SymName, ArgTypes, _Kind),
        list.length(ArgTypes, NumArgTypes),
        TypeCtor = type_ctor(SymName, NumArgTypes),
        lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        require_complete_switch [TypeBody]
        (
            TypeBody = hlds_du_type(TypeBodyDu),
            % Given a supertype t and a subtype ts, the condition
            % and then-part allows programs to coerce from list(ts)
            % to list(t). However, when trying to coerce from
            % one_or_more(ts) to one_or_more(T), examining the
            % one_or_more function symbol's second arg, whose type
            % is list(ts), the condition fails, and the else-part
            % prevents coercion from list(ts) to list(t).
            %
            % XXX If coercion from list(ts) to list(t) is allowed
            % at the top level, why is it not allowed in an argument?
            %
            % It should be sufficient for TypeCtor and ArgTypes
            % to match ONE of the types among our ancestors;
            % the match shouldn't be restricted to the very top ancestor.
            ( if
                TypeCtor = BaseTypeCtor,
                type_list_to_var_list(ArgTypes, ArgTypeVars),
                ArgTypeVars = BaseTypeCtorParams
            then
                % A type in the RHS that matches exactly the base type
                % does not impose any restrictions on its type params.
                % Any difference that occurs between the from-type and
                % the to-type must by definition occur somewhere else
                % (i.e. outside CtorArgType) as well.
                true
            else
                does_type_ctor_have_invariant_tparams(TypeTable,
                    ActiveTypeCtors, TypeCtor, TypeDefn, TypeBodyDu,
                    MaybeInvariantTParams, !InvariantTParamsMap),
                (
                    MaybeInvariantTParams = known_no_invariant_params
                ;
                    MaybeInvariantTParams = may_have_invariant_params,
                    % XXX Is pir_du_nonrec still an appropriate name
                    % for this coerce_fail? And what about the text of
                    % the diagnostic we generate for it?
                    PosnReason = pir_du_nonrec(BaseTypeCtor, TypeCtor),
                    CtorArgPosn = ctor_arg_posn(ConsId, !.ArgNum, PosnReason),
                    % This is a safe approximation, since we do not know
                    % *which* of TypeCtor's params have to be invariant.
                    type_vars_in_types(ArgTypes, TypeVars),
                    list.foldl(one_or_more_map.reverse_add(CtorArgPosn),
                        TypeVars, !InvariantTVars)
                )
            )
        ;
            ( TypeBody = hlds_foreign_type(_),  PosnReason = pir_foreign
            ; TypeBody = hlds_solver_type(_),   PosnReason = pir_solver
            ; TypeBody = hlds_abstract_type(_), PosnReason = pir_abstract
            ),
            CtorArgPosn = ctor_arg_posn(ConsId, !.ArgNum, PosnReason),
            type_vars_in_types(ArgTypes, TypeVars),
            list.foldl(one_or_more_map.reverse_add(CtorArgPosn), TypeVars,
                !InvariantTVars)
        ;
            TypeBody = hlds_eqv_type(EqvType0),
            % This equivalence type was not expanded out by equiv_type.m,
            % so the source of the equivalence must be outside the set of
            % type definitions that equiv_type.m pays attention to,
            % such as in the implementation section of an imported module.
            %
            % In these cases, expand out the type and process the result
            % as if the equivalence *had* been expanded out.
            hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
            map.from_corresponding_lists(TypeParams, ArgTypes, TSubst),
            apply_subst_to_type(TSubst, EqvType0, EqvType),
            % We ignore the updated !:ArgNum, because we do not want to
            % increment !.ArgNum BOTH here AND at clause end.
            acc_invariant_tvars_in_ctor_arg_type(TypeTable, ActiveTypeCtors,
                BaseTypeCtor, BaseTypeCtorParams, ConsId, EqvType,
                !.ArgNum, _, !InvariantTVars, !InvariantTParamsMap)
        )
    ;
        CtorArgType = tuple_type(ArgTypes, _Kind),
        list.length(ArgTypes, Arity),
        TupleCtor = tuple_cons(Arity),
        list.foldl3(
            acc_invariant_tvars_in_ctor_arg_type(TypeTable, ActiveTypeCtors,
                BaseTypeCtor, BaseTypeCtorParams, TupleCtor),
            ArgTypes, 1u, _, !InvariantTVars, !InvariantTParamsMap)
    ;
        CtorArgType = higher_order_type(_PoF, ArgTypes, _HOInstInfo, _Purity),
        % We do not support any subtyping of higher order types.
        % Therefore the higher order components on the right-hand side of a
        % type definition must be identical in the from-type and the to-type,
        % which means that all type parameters that occur in such
        % higher order types must be bound to the exact same value
        % in the from-type and to-type.
        CtorArgPosn = ctor_arg_posn(ConsId, !.ArgNum, pir_higher_order),
        type_vars_in_types(ArgTypes, TypeVars),
        list.foldl(one_or_more_map.reverse_add(CtorArgPosn), TypeVars,
            !InvariantTVars)
    ;
        CtorArgType = apply_n_type(_, _, _),
        sorry($pred, "apply_n_type")
    ;
        CtorArgType = kinded_type(SubCtorArgType, _Kind),
        acc_invariant_tvars_in_ctor_arg_type(TypeTable, ActiveTypeCtors,
            BaseTypeCtor, BaseTypeCtorParams, ConsId, SubCtorArgType,
            !ArgNum, !InvariantTVars, !InvariantTParamsMap)
    ),
    !:ArgNum = !.ArgNum + 1u.

%---------------------------------------------------------------------------%

:- type maybe_invariant_params
    --->    known_no_invariant_params
    ;       may_have_invariant_params.

:- pred does_type_ctor_have_invariant_tparams(type_table::in,
    active_type_ctors::in, type_ctor::in, hlds_type_defn::in, type_body_du::in,
    maybe_invariant_params::out,
    invariant_tparams_map::in, invariant_tparams_map::out) is det.

does_type_ctor_have_invariant_tparams(TypeTable, ActiveTypeCtors0,
        TypeCtor, TypeDefn, TypeBodyDu, MaybeInvariantParams,
        !InvariantTParamsMap) :-
    ( if list.member(TypeCtor, ActiveTypeCtors0) then
        % This happens in the case of mutually recursive types.
        %
        % We currently do not keep track of type parameter substitutions
        % between mutually recursive types. With this limited machinery,
        % this is the only safe approximation.
        MaybeInvariantParams = may_have_invariant_params
    else
        ( if
            map.search(!.InvariantTParamsMap, TypeCtor, InvariantTVarsPrime)
        then
            InvariantTVars = InvariantTVarsPrime
        else
            ActiveTypeCtors1 = [TypeCtor | ActiveTypeCtors0],
            hlds_data.get_type_defn_tparams(TypeDefn, TypeCtorParams),
            compute_which_type_params_must_be_invariant(TypeTable,
                ActiveTypeCtors1, TypeCtor, TypeCtorParams, TypeBodyDu,
                InvariantTVars, !InvariantTParamsMap),
            map.det_insert(TypeCtor, InvariantTVars, !InvariantTParamsMap)
        ),
        ( if map.is_empty(InvariantTVars) then
            MaybeInvariantParams = known_no_invariant_params
        else
            MaybeInvariantParams = may_have_invariant_params
        )
    ).

%---------------------------------------------------------------------------%

    % are_actual_param_type_pairs_as_related_as_needed(TypeTable, TVarSet,
    %   InvariantTVars, BaseTypeCtor, BaseTypeParams, FromArgTypes, ToArgTypes,
    %   !ArgNume, !TypeAssign, !CoerceFails):
    %
    % FromArgTypes and ToArgTypes are the actual types bound to TypeParams
    % in the from-type and to-type of the coercion respectively.
    % If a given type parameter is in InvariantTVars, then the types bound
    % to that parameter in the from-type and to-type must be identical,
    % while for the type parameters that are not in InvariantTVars,
    % it is enough that one is a subtype of the other (in either direction).
    %
    % If e.g. neither the first nor second TypeParam is in InvariantTVars,
    % we can succeed if the first FromArgType is a subtype of the first
    % ToArgType, but the second ToArgType is a subtype of the second
    % FromArgType. The direction of which is the subtype of the other
    % does NOT need to be consistent. This allows us to support coercion
    % from any subtype of the base type to any other of its subtypes;
    % the from-type and the to-type do not need to be in a subtype-supertype
    % relationship.
    %
:- pred are_actual_param_type_pairs_as_related_as_needed(type_table::in,
    tvarset::in, invariant_tvars::in, type_ctor::in,
    list(tvar)::in, list(mer_type)::in, list(mer_type)::in,
    uint::in, uint::out, type_assign::in, type_assign::out,
    list(coerce_fail)::in, list(coerce_fail)::out) is det.

are_actual_param_type_pairs_as_related_as_needed(TypeTable, TVarSet,
        InvariantTVars, BaseTypeCtor, BaseTypeCtorParams,
        FromArgTypes, ToArgTypes, !ArgNum, !TypeAssign, !CoerceFails) :-
    ( if
        BaseTypeCtorParams = [],
        FromArgTypes = [],
        ToArgTypes = []
    then
        true
    else if
        BaseTypeCtorParams = [HeadBaseTypeCtorParam | TailBaseTypeCtorParams],
        FromArgTypes = [HeadFromArgType | TailFromArgTypes],
        ToArgTypes = [HeadToArgType | TailToArgTypes]
    then
        are_actual_param_type_pair_as_related_as_needed(TypeTable, TVarSet,
            InvariantTVars, BaseTypeCtor,
            HeadBaseTypeCtorParam, HeadFromArgType, HeadToArgType,
            !ArgNum, !TypeAssign, !CoerceFails),
        are_actual_param_type_pairs_as_related_as_needed(TypeTable, TVarSet,
            InvariantTVars, BaseTypeCtor,
            TailBaseTypeCtorParams, TailFromArgTypes, TailToArgTypes,
            !ArgNum, !TypeAssign, !CoerceFails)
    else
        % FromArgTypes and ToArgTypes are the actual types bound to TypeParams
        % in the from-type and to-type of the coercion respectively.
        % If their lengths do not match, then some earlier compiler pass
        % screwed up really badly.
        unexpected($pred, "length mismatch")
    ).

:- pred are_actual_param_type_pair_as_related_as_needed(type_table::in,
    tvarset::in, invariant_tvars::in, type_ctor::in,
    tvar::in, mer_type::in, mer_type::in,
    uint::in, uint::out, type_assign::in, type_assign::out,
    list(coerce_fail)::in, list(coerce_fail)::out) is det.

are_actual_param_type_pair_as_related_as_needed(TypeTable, TVarSet,
        InvariantTVars, BaseTypeCtor, BaseTypeCtorParam, FromType, ToType,
        !ArgNum, !TypeAssign, !CoerceFails) :-
    ( if map.search(InvariantTVars, BaseTypeCtorParam, OoMCtorArgPosn) then
        Comparison = compare_equal(ir_base_type_ctor(OoMCtorArgPosn)),
        types_compare_as_given(TypeTable, TVarSet, BaseTypeCtor, !.ArgNum,
            Comparison, FromType, ToType, !TypeAssign, !CoerceFails)
    else
        types_compare_as_given(TypeTable, TVarSet, BaseTypeCtor, !.ArgNum,
            compare_equal_lt, FromType, ToType,
            !.TypeAssign, FromToTypeAssign, [], FromToCoerceFails),
        (
            FromToCoerceFails = [],
            !:TypeAssign = FromToTypeAssign
        ;
            FromToCoerceFails = [_ | _],
            types_compare_as_given(TypeTable, TVarSet, BaseTypeCtor, !.ArgNum,
                compare_equal_lt, ToType, FromType,
                !.TypeAssign, ToFromTypeAssign, [], ToFromCoerceFails),
            (
                ToFromCoerceFails = [],
                !:TypeAssign = ToFromTypeAssign
            ;
                ToFromCoerceFails = [_ | _],
                % NOTE Adding both FromToCoerceFails and ToFromCoerceFails
                % to !CoerceFails can report the same issue twice, with
                % the roles of coerce-from type and coerce-to type reversed
                % for any symmetrical problem that types_compare_as_given
                % can report. This is ok, because report_invalid_coerce_from_to
                % will ensure that we report just one copy in each such pair.
                !:CoerceFails = FromToCoerceFails ++ ToFromCoerceFails
                    ++ !.CoerceFails
            )
        )
    ),
    !:ArgNum = !.ArgNum + 1u.

%---------------------------------------------------------------------------%

:- type types_comparison
    --->    compare_equal(invariant_reason)
    ;       compare_equal_lt.

    % Succeed if TypeA unifies with TypeB (possibly binding type vars).
    % If Comparison is compare_equal_lt, then also succeed if TypeA =< TypeB
    % by subtype definitions.
    %
    % NOTE We use TypeA and TypeB as names instead of FromType and ToType
    % because are_actual_param_type_pair_as_related_as_needed can pass
    % *either* FromType as TypeA and ToType as TypeB, *or* vice versa.
    %
    % Note: changes here may need to be made also to types_compare_as_given_mc
    % in modecheck_coerce.m.
    %
:- pred types_compare_as_given(type_table::in, tvarset::in,
    type_ctor::in, uint::in, types_comparison::in, mer_type::in, mer_type::in,
    type_assign::in, type_assign::out,
    list(coerce_fail)::in, list(coerce_fail)::out) is det.

types_compare_as_given(TypeTable, TVarSet, BaseTypeCtor, ArgNum,
        Comparison, TypeA, TypeB, !TypeAssign, !CoerceFails) :-
    ( if
        ( TypeA = type_variable(_, _)
        ; TypeB = type_variable(_, _)
        )
    then
        ( if type_assign_unify_type(TypeA, TypeB, !TypeAssign) then
            true
        else
            CoerceFail = cannot_unify_type_vars(TypeA, TypeB),
            !:CoerceFails = [CoerceFail | !.CoerceFails]
        )
    else
        types_compare_as_given_nonvar(TypeTable, TVarSet, BaseTypeCtor, ArgNum,
            Comparison, TypeA, TypeB, !TypeAssign, !CoerceFails)
    ).

:- pred types_compare_as_given_nonvar(type_table::in, tvarset::in,
    type_ctor::in, uint::in, types_comparison::in, mer_type::in, mer_type::in,
    type_assign::in, type_assign::out,
    list(coerce_fail)::in, list(coerce_fail)::out) is det.

types_compare_as_given_nonvar(TypeTable, TVarSet, BaseTypeCtor, ArgNum,
        Comparison, TypeA, TypeB, !TypeAssign, !CoerceFails) :-
    % Several of the kinds of coerce_fails that the code below can generate
    % are NOT TESTED by any test case in the test suite.
    require_complete_switch [TypeA]
    (
        TypeA = builtin_type(BuiltinTypeA),
        ( if TypeB = builtin_type(BuiltinTypeA) then
            true
        else
            ( if TypeB = builtin_type(BuiltinTypeB) then
                CoerceFail =
                    different_builtin_types(BuiltinTypeA, BuiltinTypeB)
            else
                CoerceFail = different_type_categories(TypeTable, TypeA, TypeB)
            ),
            !:CoerceFails = [CoerceFail | !.CoerceFails]
        )
    ;
        TypeA = type_variable(_, _),
        unexpected($pred, "type_variable")
    ;
        TypeA = defined_type(_, _, _),
        ( if TypeB = defined_type(_, _, _) then
            defined_type_to_ctor_and_args(TypeA, TypeCtorA, ArgTypesA),
            defined_type_to_ctor_and_args(TypeB, TypeCtorB, ArgTypesB),
            ( if TypeCtorA = TypeCtorB then
                % Checking for TypeCtorA = TypeCtorB before checking whether
                % TypeA and TypeB are du types allows this code to succeed for
                %
                % - equivalence type
                % - foreign types
                % - solver types
                % - abstract types
                % - undefined type_ctors (ones that are not in the type table)
                %
                % Equivalence types should have been expanded out by now,
                % so they pose no problem. (If they did appear here, we
                % would have to expand them out, because without that,
                % we cannot check for co- versus contra-variance.)
                %
                % The other kinds of types can all occur in the input
                % of this code. Most of the time, their argument lists
                % are the empty list, but they can contain type parameters,
                % such as the ones we use to distinguish e.g. prog_vars
                % from tvars. These are known as "phantom type parameters".
                %
                % I (zs) do not know whether type parameters on solver types
                % (a) are ever useful, or (b) can cause issues with respect to
                % co- versus contra-variance.
                corresponding_types_compare_as_given(TypeTable, TVarSet,
                    BaseTypeCtor, ArgNum, Comparison, ArgTypesA, ArgTypesB,
                    !TypeAssign, !CoerceFails)
            else
                classify_defined_type_is_du_type(TypeTable,
                    TypeCtorA, ArgTypesA, MaybeDuTypeA),
                classify_defined_type_is_du_type(TypeTable,
                    TypeCtorB, ArgTypesB, MaybeDuTypeB),
                are_both_types_du(TypeA, TypeB, MaybeDuTypeA, MaybeDuTypeB,
                    MaybeBoth),
                (
                    MaybeBoth = error2(CoerceFail),
                    % A non-du type constructor cannot be a subtype.
                    !:CoerceFails = [CoerceFail | !.CoerceFails]
                ;
                    MaybeBoth = ok2(DuTypeInfoA, _DuTypeInfoB),
                    (
                        Comparison = compare_equal(Reason),
                        CoerceFail = should_be_invariant_arg(BaseTypeCtor,
                            ArgNum, Reason, TypeA, TypeB),
                        !:CoerceFails = [CoerceFail | !.CoerceFails]
                    ;
                        Comparison = compare_equal_lt,
                        DuTypeInfoA =
                            du_type_info(_, _, TypeDefnA, TypeBodyDuA),
                        MaybeSuperTypeA = TypeBodyDuA ^ du_type_supertype,
                        (
                            MaybeSuperTypeA = subtype_of(SuperTypeA0),
                            get_supertype_of_subtype(TVarSet,
                                TypeCtorA, ArgTypesA, TypeDefnA,
                                SuperTypeA0, SuperTypeA),
                            types_compare_as_given(TypeTable, TVarSet,
                                BaseTypeCtor, ArgNum, Comparison,
                                SuperTypeA, TypeB, !TypeAssign, !CoerceFails)
                        ;
                            MaybeSuperTypeA = not_a_subtype,
                            CoerceFail = du_type_is_not_subtype(TypeCtorA),
                            !:CoerceFails = [CoerceFail | !.CoerceFails]
                        )
                    )
                )
            )
        else
            CoerceFail = different_type_categories(TypeTable, TypeA, TypeB),
            !:CoerceFails = [CoerceFail | !.CoerceFails]
        )
    ;
        TypeA = tuple_type(ArgTypesA, Kind),
        ( if TypeB = tuple_type(ArgTypesB, Kind) then
            list.length(ArgTypesA, NumArgTypesA),
            list.length(ArgTypesB, NumArgTypesB),
            ( if NumArgTypesA = NumArgTypesB then
                corresponding_types_compare_as_given(TypeTable, TVarSet,
                    BaseTypeCtor, ArgNum, Comparison, ArgTypesA, ArgTypesB,
                    !TypeAssign, !CoerceFails)
            else
                CoerceFail =
                    different_tuple_arities(NumArgTypesA, NumArgTypesB),
                !:CoerceFails = [CoerceFail | !.CoerceFails]
            )
        else
            CoerceFail = different_type_categories(TypeTable, TypeA, TypeB),
            !:CoerceFails = [CoerceFail | !.CoerceFails]
        )
    ;
        TypeA = higher_order_type(PredOrFunc, ArgTypesA, _IA, Purity),
        % XXX We should return specific coerce_fails for Purity mismatches.
        % XXX We probably should NOT ignore the higher order inst infos.
        ( if TypeB = higher_order_type(PredOrFunc, ArgTypesB, _IB, Purity) then
            % We do not allow subtyping in higher order argument types, so we
            % pass compare_equal here EVEN IF Comparison is compare_equal_lt.
            SubComparison = compare_equal(ir_higher_order),
            corresponding_types_compare_as_given(TypeTable, TVarSet,
                BaseTypeCtor, ArgNum, SubComparison, ArgTypesA, ArgTypesB,
                !TypeAssign, !CoerceFails)
        else
            CoerceFail = different_type_categories(TypeTable, TypeA, TypeB),
            !:CoerceFails = [CoerceFail | !.CoerceFails]
        )
    ;
        TypeA = apply_n_type(_, _, _),
        sorry($pred, "apply_n_type")
    ;
        TypeA = kinded_type(TypeA1, Kind),
        % We require TypeB to be a kinded type of the SAME KIND as TypeA.
        % XXX We should probably require it to have the same kind as TypeA,
        % *without* requiring it to be a kinded type. However, that will matter
        % only once we start using kinded types.
        ( if TypeB = kinded_type(TypeB1, Kind) then
            types_compare_as_given(TypeTable, TVarSet, BaseTypeCtor, ArgNum,
                Comparison, TypeA1, TypeB1, !TypeAssign, !CoerceFails)
        else
            CoerceFail = different_type_categories(TypeTable, TypeA, TypeB),
            !:CoerceFails = [CoerceFail | !.CoerceFails]
        )
    ).

:- pred corresponding_types_compare_as_given(type_table::in, tvarset::in,
    type_ctor::in, uint::in, types_comparison::in,
    list(mer_type)::in, list(mer_type)::in, type_assign::in, type_assign::out,
    list(coerce_fail)::in, list(coerce_fail)::out) is det.

corresponding_types_compare_as_given(_, _, _, _, _,
        [], [], !TypeAssign, !CoerceFails).
corresponding_types_compare_as_given(TypeTable, TVarSet,
        BaseTypeCtor, ArgNum, Comparison, [TypeA | TypesA], [TypeB | TypesB],
        !TypeAssign, !CoerceFails) :-
    types_compare_as_given(TypeTable, TVarSet,
        BaseTypeCtor, ArgNum, Comparison, TypeA, TypeB,
        !TypeAssign, !CoerceFails),
    corresponding_types_compare_as_given(TypeTable, TVarSet,
        BaseTypeCtor, ArgNum, Comparison, TypesA, TypesB,
        !TypeAssign, !CoerceFails).
corresponding_types_compare_as_given(_, _, _, _, _,
        [_ | _], [], !TypeAssign, !CoerceFails) :-
    unexpected($pred, "length mismatch").
corresponding_types_compare_as_given(_, _, _, _, _,
        [], [_ | _], !TypeAssign, !CoerceFails) :-
    unexpected($pred, "length mismatch").

%---------------------------------------------------------------------------%

    % Succeeds iff the given type contains no type variables except
    % for those in the given list.
    %
:- pred type_is_ground_except_vars(list(tvar)::in, mer_type::in) is semidet.

type_is_ground_except_vars(Except, Type) :-
    all [TVar] (
        type_contains_var(Type, TVar)
    =>
        list.contains(Except, TVar)
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_coerce.
%---------------------------------------------------------------------------%

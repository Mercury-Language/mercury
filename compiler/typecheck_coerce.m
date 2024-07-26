%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2014-2021, 2023-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck_clauses.m.
% Main author: fjh.
%
% This file typechecks coerce operations.
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

:- pred typecheck_coerce(prog_context::in, list(prog_var)::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

    % Check coerce constraints in each type assignment to see if they can be
    % satisfied. If there are one or more type assignments in which all
    % coerce constraints are satisfied, then keep only those type assignments
    % and discard the rest -- we don't need to consider the type assignments
    % with unsatisfiable coerce constraints any more.
    %
:- pred typecheck_prune_coerce_constraints(type_assign_set::in,
    type_assign_set::out, typecheck_info::in, typecheck_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module check_hlds.typecheck_util.
:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_type_test.
:- import_module parse_tree.vartypes.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module require.
:- import_module set.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

typecheck_coerce(Context, Args, TypeAssignSet0, TypeAssignSet, !Info) :-
    ( if Args = [FromVar0, ToVar0] then
        FromVar = FromVar0,
        ToVar = ToVar0
    else
        unexpected($pred, "coerce requires two arguments")
    ),
    list.foldl2(typecheck_coerce_2(Context, FromVar, ToVar),
        TypeAssignSet0, [], TypeAssignSet1, !Info),
    ( if
        TypeAssignSet1 = [],
        TypeAssignSet0 = [_ | _]
    then
        TypeAssignSet = TypeAssignSet0
    else
        TypeAssignSet = TypeAssignSet1
    ).

:- pred typecheck_coerce_2(prog_context::in, prog_var::in, prog_var::in,
    type_assign::in, type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_coerce_2(Context, FromVar, ToVar, TypeAssign0,
        !TypeAssignSet, !Info) :-
    type_assign_get_var_types(TypeAssign0, VarTypes),
    type_assign_get_typevarset(TypeAssign0, TVarSet),
    type_assign_get_existq_tvars(TypeAssign0, ExistQTVars),
    type_assign_get_type_bindings(TypeAssign0, TypeBindings),

    ( if search_var_type(VarTypes, FromVar, FromType0) then
        apply_rec_subst_to_type(TypeBindings, FromType0, FromType1),
        MaybeFromType = yes(FromType1)
    else
        MaybeFromType = no
    ),
    ( if search_var_type(VarTypes, ToVar, ToType0) then
        apply_rec_subst_to_type(TypeBindings, ToType0, ToType1),
        MaybeToType = yes(ToType1)
    else
        MaybeToType = no
    ),

    ( if
        MaybeFromType = yes(FromType),
        MaybeToType = yes(ToType),
        type_is_ground_except_vars(FromType, ExistQTVars),
        type_is_ground_except_vars(ToType, ExistQTVars)
    then
        % We can compare the types on both sides immediately.
        typecheck_info_get_type_table(!.Info, TypeTable),
        ( if
            typecheck_coerce_between_types(TypeTable, TVarSet,
                FromType, ToType, TypeAssign0, TypeAssign1)
        then
            type_assign_get_type_bindings(TypeAssign1, TypeBindings1),
            ( if is_same_type_after_subst(TypeBindings1, FromType, ToType) then
                Coercion = coerce_constraint(FromType, ToType, Context,
                    FromVar, satisfied_but_redundant),
                add_coerce_constraint(Coercion, TypeAssign1, TypeAssign)
            else
                TypeAssign = TypeAssign1
            )
        else
            Coercion = coerce_constraint(FromType, ToType, Context, FromVar,
                unsatisfiable),
            add_coerce_constraint(Coercion, TypeAssign0, TypeAssign)
        ),
        !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
    else
        % One or both of the types is not known yet. Add a coercion constraint
        % on the type assignment to be checked after typechecking the clause.
        (
            MaybeFromType = yes(FromType),
            TypeAssign1 = TypeAssign0
        ;
            MaybeFromType = no,
            type_assign_fresh_type_var(FromVar, FromType,
                TypeAssign0, TypeAssign1)
        ),
        (
            MaybeToType = yes(ToType),
            TypeAssign2 = TypeAssign1
        ;
            MaybeToType = no,
            % Handle X = coerce(X).
            ( if ToVar = FromVar then
                ToType = FromType,
                TypeAssign2 = TypeAssign1
            else
                type_assign_fresh_type_var(ToVar, ToType,
                    TypeAssign1, TypeAssign2)
            )
        ),
        Coercion = coerce_constraint(FromType, ToType, Context, FromVar,
            need_to_check),
        add_coerce_constraint(Coercion, TypeAssign2, TypeAssign),
        !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
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

%-------------%

:- pred typecheck_coerce_between_types(type_table::in, tvarset::in,
    mer_type::in, mer_type::in, type_assign::in, type_assign::out)
    is semidet.

typecheck_coerce_between_types(TypeTable, TVarSet, FromType, ToType,
        !TypeAssign) :-
    % Type bindings must have been applied to FromType and ToType already.
    replace_principal_type_ctor_with_base(TypeTable, TVarSet,
        FromType, FromBaseType),
    replace_principal_type_ctor_with_base(TypeTable, TVarSet,
        ToType, ToBaseType),
    type_to_ctor_and_args(FromBaseType, FromBaseTypeCtor, FromBaseTypeArgs),
    type_to_ctor_and_args(ToBaseType, ToBaseTypeCtor, ToBaseTypeArgs),

    % The input type and result type must share a base type constructor.
    BaseTypeCtor = FromBaseTypeCtor,
    BaseTypeCtor = ToBaseTypeCtor,

    % Check the variance of type arguments.
    hlds_data.search_type_ctor_defn(TypeTable, BaseTypeCtor, BaseTypeDefn),
    hlds_data.get_type_defn_tparams(BaseTypeDefn, BaseTypeParams),
    build_type_param_variance_restrictions(TypeTable, BaseTypeCtor,
        InvariantSet),
    check_coerce_type_params(TypeTable, TVarSet, InvariantSet,
        BaseTypeParams, FromBaseTypeArgs, ToBaseTypeArgs, !TypeAssign).

:- pred replace_principal_type_ctor_with_base(type_table::in, tvarset::in,
    mer_type::in, mer_type::out) is det.

replace_principal_type_ctor_with_base(TypeTable, TVarSet, Type0, Type) :-
    ( if
        type_to_ctor_and_args(Type0, TypeCtor, Args),
        get_supertype(TypeTable, TVarSet, TypeCtor, Args, SuperType)
    then
        replace_principal_type_ctor_with_base(TypeTable, TVarSet,
            SuperType, Type)
    else
        Type = Type0
    ).

%---------------------%

:- type invariant_set == set(tvar).

:- pred build_type_param_variance_restrictions(type_table::in,
    type_ctor::in, invariant_set::out) is det.

build_type_param_variance_restrictions(TypeTable, TypeCtor, InvariantSet) :-
    ( if
        hlds_data.search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        TypeBody = hlds_du_type(TypeBodyDu),
        TypeBodyDu = type_body_du(OoMCtors, _MaybeSuperType, _MaybeCanonical,
            _MaybeTypeRepn, _IsForeignType)
    then
        Ctors = one_or_more_to_list(OoMCtors),
        list.foldl(
            build_type_param_variance_restrictions_in_ctor(TypeTable,
                TypeCtor, TypeParams),
            Ctors, set.init, InvariantSet)
    else
        unexpected($pred, "not du type")
    ).

:- pred build_type_param_variance_restrictions_in_ctor(type_table::in,
    type_ctor::in, list(tvar)::in, constructor::in,
    invariant_set::in, invariant_set::out) is det.

build_type_param_variance_restrictions_in_ctor(TypeTable, CurTypeCtor,
        CurTypeParams, Ctor, !InvariantSet) :-
    Ctor = ctor(_Ordinal, _MaybeExistConstraints, _CtorName, CtorArgs, _Arity,
        _Context),
    list.foldl(
        build_type_param_variance_restrictions_in_ctor_arg(TypeTable,
            CurTypeCtor, CurTypeParams),
        CtorArgs, !InvariantSet).

:- pred build_type_param_variance_restrictions_in_ctor_arg(type_table::in,
    type_ctor::in, list(tvar)::in, constructor_arg::in,
    invariant_set::in, invariant_set::out) is det.

build_type_param_variance_restrictions_in_ctor_arg(TypeTable, CurTypeCtor,
        CurTypeParams, CtorArg, !InvariantSet) :-
    CtorArg = ctor_arg(_MaybeFieldName, CtorArgType, _Context),
    build_type_param_variance_restrictions_in_ctor_arg_type(TypeTable,
        CurTypeCtor, CurTypeParams, CtorArgType, !InvariantSet).

:- pred build_type_param_variance_restrictions_in_ctor_arg_type(type_table::in,
    type_ctor::in, list(tvar)::in, mer_type::in,
    invariant_set::in, invariant_set::out) is det.

build_type_param_variance_restrictions_in_ctor_arg_type(TypeTable, CurTypeCtor,
        CurTypeParams, CtorArgType, !InvariantSet) :-
    (
        CtorArgType = builtin_type(_)
    ;
        CtorArgType = type_variable(_TypeVar, _Kind)
    ;
        CtorArgType = defined_type(_SymName, ArgTypes, _Kind),
        ( if
            type_to_ctor_and_args(CtorArgType, TypeCtor, TypeArgs),
            hlds_data.search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn)
        then
            hlds_data.get_type_defn_body(TypeDefn, TypeBody),
            require_complete_switch [TypeBody]
            (
                TypeBody = hlds_du_type(_),
                ( if
                    TypeCtor = CurTypeCtor,
                    type_list_to_var_list(TypeArgs, CurTypeParams)
                then
                    % A recursive type that matches exactly the current type
                    % head does not impose any restrictions on the type
                    % parameters.
                    true
                else
                    type_vars_in_types(ArgTypes, TypeVars),
                    set.insert_list(TypeVars, !InvariantSet)
                )
            ;
                ( TypeBody = hlds_foreign_type(_)
                ; TypeBody = hlds_abstract_type(_)
                ; TypeBody = hlds_solver_type(_)
                ),
                type_vars_in_types(ArgTypes, TypeVars),
                set.insert_list(TypeVars, !InvariantSet)
            ;
                TypeBody = hlds_eqv_type(_),
                unexpected($pred, "hlds_eqv_type")
            )
        else
            unexpected($pred, "undefined type")
        )
    ;
        CtorArgType = tuple_type(ArgTypes, _Kind),
        list.foldl(
            build_type_param_variance_restrictions_in_ctor_arg_type(TypeTable,
                CurTypeCtor, CurTypeParams),
            ArgTypes, !InvariantSet)
    ;
        CtorArgType = higher_order_type(_PredOrFunc, ArgTypes, _HOInstInfo,
            _Purity),
        type_vars_in_types(ArgTypes, TypeVars),
        set.insert_list(TypeVars, !InvariantSet)
    ;
        CtorArgType = apply_n_type(_, _, _),
        sorry($pred, "apply_n_type")
    ;
        CtorArgType = kinded_type(CtorArgType1, _Kind),
        build_type_param_variance_restrictions_in_ctor_arg_type(TypeTable,
            CurTypeCtor, CurTypeParams, CtorArgType1, !InvariantSet)
    ).

%---------------------%

:- pred check_coerce_type_params(type_table::in, tvarset::in,
    invariant_set::in, list(tvar)::in, list(mer_type)::in, list(mer_type)::in,
    type_assign::in, type_assign::out) is semidet.

check_coerce_type_params(TypeTable, TVarSet, InvariantSet,
        TypeParams, FromTypeArgs, ToTypeArgs, !TypeAssign) :-
    (
        TypeParams = [],
        FromTypeArgs = [],
        ToTypeArgs = []
    ;
        TypeParams = [TypeVar | TailTypeParams],
        FromTypeArgs = [FromType | TailFromTypes],
        ToTypeArgs = [ToType | TailToTypes],
        check_coerce_type_param(TypeTable, TVarSet, InvariantSet,
            TypeVar, FromType, ToType, !TypeAssign),
        check_coerce_type_params(TypeTable, TVarSet, InvariantSet,
            TailTypeParams, TailFromTypes, TailToTypes, !TypeAssign)
    ).

:- pred check_coerce_type_param(type_table::in, tvarset::in, invariant_set::in,
    tvar::in, mer_type::in, mer_type::in, type_assign::in, type_assign::out)
    is semidet.

check_coerce_type_param(TypeTable, TVarSet, InvariantSet,
        TypeVar, FromType, ToType, !TypeAssign) :-
    ( if set.contains(InvariantSet, TypeVar) then
        compare_types(TypeTable, TVarSet, compare_equal, FromType, ToType,
            !TypeAssign)
    else
        ( if
            compare_types(TypeTable, TVarSet, compare_equal_lt,
                FromType, ToType, !TypeAssign)
        then
            true
        else
            compare_types(TypeTable, TVarSet, compare_equal_lt,
                ToType, FromType, !TypeAssign)
        )
    ).

%---------------------%

:- type types_comparison
    --->    compare_equal
    ;       compare_equal_lt.

    % Succeed if TypeA unifies with TypeB (possibly binding type vars).
    % If Comparison is compare_equal_lt, then also succeed if TypeA =< TypeB
    % by subtype definitions.
    %
    % Note: changes here may need to be made to compare_types in
    % modecheck_coerce.m
    %
:- pred compare_types(type_table::in, tvarset::in, types_comparison::in,
    mer_type::in, mer_type::in, type_assign::in, type_assign::out) is semidet.

compare_types(TypeTable, TVarSet, Comparison, TypeA, TypeB,
        !TypeAssign) :-
    ( if
        ( TypeA = type_variable(_, _)
        ; TypeB = type_variable(_, _)
        )
    then
        type_assign_unify_type(TypeA, TypeB, !TypeAssign)
    else
        compare_types_nonvar(TypeTable, TVarSet, Comparison, TypeA, TypeB,
            !TypeAssign)
    ).

:- pred compare_types_nonvar(type_table::in, tvarset::in, types_comparison::in,
    mer_type::in, mer_type::in, type_assign::in, type_assign::out) is semidet.

compare_types_nonvar(TypeTable, TVarSet, Comparison, TypeA, TypeB,
        !TypeAssign) :-
    require_complete_switch [TypeA]
    (
        TypeA = builtin_type(BuiltinType),
        TypeB = builtin_type(BuiltinType)
    ;
        TypeA = type_variable(_, _),
        TypeB = type_variable(_, _),
        unexpected($pred, "type_variable")
    ;
        TypeA = defined_type(_, _, _),
        type_to_ctor_and_args(TypeA, TypeCtorA, ArgsA),
        type_to_ctor_and_args(TypeB, TypeCtorB, ArgsB),
        ( if TypeCtorA = TypeCtorB then
            compare_types_corresponding(TypeTable, TVarSet, Comparison,
                ArgsA, ArgsB, !TypeAssign)
        else
            Comparison = compare_equal_lt,
            get_supertype(TypeTable, TVarSet, TypeCtorA, ArgsA, SuperTypeA),
            compare_types(TypeTable, TVarSet, Comparison, SuperTypeA, TypeB,
                !TypeAssign)
        )
    ;
        TypeA = tuple_type(ArgsA, Kind),
        TypeB = tuple_type(ArgsB, Kind),
        compare_types_corresponding(TypeTable, TVarSet, Comparison,
            ArgsA, ArgsB, !TypeAssign)
    ;
        TypeA = higher_order_type(PredOrFunc, ArgsA, _HOInstInfoA, Purity),
        TypeB = higher_order_type(PredOrFunc, ArgsB, _HOInstInfoB, Purity),
        % We do not allow subtyping in higher order argument types.
        compare_types_corresponding(TypeTable, TVarSet, compare_equal,
            ArgsA, ArgsB, !TypeAssign)
    ;
        TypeA = apply_n_type(_, _, _),
        sorry($pred, "apply_n_type")
    ;
        TypeA = kinded_type(TypeA1, Kind),
        TypeB = kinded_type(TypeB1, Kind),
        compare_types(TypeTable, TVarSet, Comparison, TypeA1, TypeB1,
            !TypeAssign)
    ).

:- pred compare_types_corresponding(type_table::in, tvarset::in,
    types_comparison::in, list(mer_type)::in, list(mer_type)::in,
    type_assign::in, type_assign::out) is semidet.

compare_types_corresponding(_TypeTable, _TVarSet, _Comparison,
        [], [], !TypeAssign).
compare_types_corresponding(TypeTable, TVarSet, Comparison,
        [TypeA | TypesA], [TypeB | TypesB], !TypeAssign) :-
    compare_types(TypeTable, TVarSet, Comparison, TypeA, TypeB, !TypeAssign),
    compare_types_corresponding(TypeTable, TVarSet, Comparison, TypesA, TypesB,
        !TypeAssign).

%---------------------------------------------------------------------------%

typecheck_prune_coerce_constraints(TypeAssignSet0, TypeAssignSet, !Info) :-
    typecheck_info_get_type_table(!.Info, TypeTable),
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
            DelayedCoercions, Coercions2, !TypeAssign),
        Coercions = KeepCoercions ++ Coercions2
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
        Status0),
    (
        Status0 = need_to_check,
        TypeAssign0 = !.TypeAssign,
        type_assign_get_typevarset(TypeAssign0, TVarSet),
        type_assign_get_existq_tvars(TypeAssign0, ExistQTVars),
        type_assign_get_type_bindings(TypeAssign0, TypeBindings0),
        apply_rec_subst_to_type(TypeBindings0, FromType0, FromType),
        apply_rec_subst_to_type(TypeBindings0, ToType0, ToType),
        ( if type_is_ground_except_vars(FromType, ExistQTVars) then
            ( if
                typecheck_coerce_between_types(TypeTable, TVarSet,
                    FromType, ToType, TypeAssign0, TypeAssign1)
            then
                type_assign_get_type_bindings(TypeAssign1, TypeBindings1),
                ( if
                    is_same_type_after_subst(TypeBindings1, FromType, ToType)
                then
                    Coercion = coerce_constraint(FromType, ToType,
                        Context, FromVar, satisfied_but_redundant),
                    Action = keep(Coercion)
                else
                    Action = prune
                ),
                !:TypeAssign = TypeAssign1
            else
                Coercion = coerce_constraint(FromType0, ToType0,
                    Context, FromVar, unsatisfiable),
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
        Status),
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
:- end_module check_hlds.typecheck_coerce.
%---------------------------------------------------------------------------%

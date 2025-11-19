%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2021, 2023-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck_coerce.m.
% Main author: wangp.
%
% This file typechecks coerce operations.
%
% Note that the two exported predicates are completely independent of each
% other; they could easily be in separate modules.
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

typecheck_coerce(Info, Context, Args, TypeAssignSet0, TypeAssignSet) :-
    ( if Args = [FromVar0, ToVar0] then
        FromVar = FromVar0,
        ToVar = ToVar0
    else
        unexpected($pred, "coerce requires two arguments")
    ),
    list.foldl(typecheck_coerce_2(Info, Context, FromVar, ToVar),
        TypeAssignSet0, [], TypeAssignSet1),
    ( if
        TypeAssignSet1 = [],
        TypeAssignSet0 = [_ | _]
    then
        TypeAssignSet = TypeAssignSet0
    else
        TypeAssignSet = TypeAssignSet1
    ).

:- pred typecheck_coerce_2(typecheck_info::in, prog_context::in,
    prog_var::in, prog_var::in, type_assign::in,
    type_assign_set::in, type_assign_set::out) is det.

typecheck_coerce_2(Info, Context, FromVar, ToVar, TypeAssign0,
        !TypeAssignSet) :-
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
        typecheck_info_get_type_table(Info, TypeTable),
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
    compute_base_type(TypeTable, TVarSet, FromType, FromBaseType),
    compute_base_type(TypeTable, TVarSet, ToType, ToBaseType),
    type_to_ctor_and_args(FromBaseType,
        FromBaseTypeCtor, FromBaseTypeArgTypes),
    type_to_ctor_and_args(ToBaseType,
        ToBaseTypeCtor, ToBaseTypeArgTypes),

    % The input type and result type must share a base type constructor.
    BaseTypeCtor = FromBaseTypeCtor,
    BaseTypeCtor = ToBaseTypeCtor,

    % Check the variance of type arguments.
    hlds_data.search_type_ctor_defn(TypeTable, BaseTypeCtor, BaseTypeDefn),
    hlds_data.get_type_defn_body(BaseTypeDefn, BaseTypeBody),
    BaseTypeBody = hlds_du_type(BaseTypeBodyDu),
    hlds_data.get_type_defn_tparams(BaseTypeDefn, BaseTypeParams),
    compute_which_type_params_must_be_invariant(TypeTable, BaseTypeCtor,
        BaseTypeBodyDu, BaseTypeParams, InvariantTVars),
    are_type_params_as_related_as_needed(TypeTable, TVarSet, InvariantTVars,
        BaseTypeParams, FromBaseTypeArgTypes, ToBaseTypeArgTypes, !TypeAssign).

:- pred compute_base_type(type_table::in, tvarset::in,
    mer_type::in, mer_type::out) is det.

compute_base_type(TypeTable, TVarSet, Type, BaseType) :-
    ( if
        type_to_ctor_and_args(Type, TypeCtor, ArgTypes),
        get_supertype(TypeTable, TVarSet, TypeCtor, ArgTypes, SuperType)
    then
        compute_base_type(TypeTable, TVarSet, SuperType, BaseType)
    else
        BaseType = Type
    ).

%---------------------%

:- type invariant_tvars == set(tvar).

    % compute_which_type_params_must_be_invariant(TypeTable,
    %     BaseTypeCtor, BaseTypeDefn, BaseTypeParams, InvariantTVars):
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
    type_ctor::in, type_body_du::in, list(tvar)::in,
    invariant_tvars::out) is det.

compute_which_type_params_must_be_invariant(TypeTable,
        BaseTypeCtor, BaseTypeBodyDu, BaseTypeParams, InvariantTVars) :-
    BaseTypeBodyDu = type_body_du(OoMCtors, _OoMAlphaSortedCtors,
        _MaybeSuperType, _MaybeCanon, _MaybeTypeRepn, _IsForeignType),
    Ctors = one_or_more_to_list(OoMCtors),
    list.foldl(
        acc_invariant_tvars_in_ctor(TypeTable, BaseTypeCtor, BaseTypeParams),
        Ctors, set.init, InvariantTVars).

:- pred acc_invariant_tvars_in_ctor(type_table::in,
    type_ctor::in, list(tvar)::in, constructor::in,
    invariant_tvars::in, invariant_tvars::out) is det.

acc_invariant_tvars_in_ctor(TypeTable, BaseTypeCtor, BaseTypeParams, Ctor,
        !InvariantTVars) :-
    Ctor = ctor(_Ordinal, _MaybeExist, _CtorName, CtorArgs, _Arity, _Context),
    list.foldl(
        acc_invariant_tvars_in_ctor_arg(TypeTable,
            BaseTypeCtor, BaseTypeParams),
        CtorArgs, !InvariantTVars).

:- pred acc_invariant_tvars_in_ctor_arg(type_table::in,
    type_ctor::in, list(tvar)::in, constructor_arg::in,
    invariant_tvars::in, invariant_tvars::out) is det.

acc_invariant_tvars_in_ctor_arg(TypeTable, BaseTypeCtor,
        BaseTypeParams, CtorArg, !InvariantTVars) :-
    CtorArg = ctor_arg(_MaybeFieldName, CtorArgType, _Context),
    % Since acc_invariant_tvars_in_ctor_rhs_type is recursive,
    % we cannot inline it here.
    acc_invariant_tvars_in_ctor_rhs_type(TypeTable, BaseTypeCtor,
        BaseTypeParams, CtorArgType, !InvariantTVars).

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
:- pred acc_invariant_tvars_in_ctor_rhs_type(type_table::in,
    type_ctor::in, list(tvar)::in, mer_type::in,
    invariant_tvars::in, invariant_tvars::out) is det.

acc_invariant_tvars_in_ctor_rhs_type(TypeTable, BaseTypeCtor, BaseTypeParams,
        RhsType, !InvariantTVars) :-
    (
        RhsType = builtin_type(_)
    ;
        RhsType = type_variable(_TypeVar, _Kind)
    ;
        RhsType = defined_type(SymName, ArgTypes, _Kind),
        list.length(ArgTypes, NumArgTypes),
        TypeCtor = type_ctor(SymName, NumArgTypes),
        ( if search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn) then
            hlds_data.get_type_defn_body(TypeDefn, TypeBody),
            require_complete_switch [TypeBody]
            (
                TypeBody = hlds_du_type(_),
                ( if
                    TypeCtor = BaseTypeCtor,
                    type_list_to_var_list(ArgTypes, ArgTypeVars),
                    ArgTypeVars = BaseTypeParams
                then
                    % A type in the RHS that matches exactly the base type
                    % does not impose any restrictions on its type params.
                    % Any difference that occurs between the from-type and
                    % the to-type must by definition occur somewhere else
                    % (i.e. outside RhsType) as well.
                    true
                else
                    type_vars_in_types(ArgTypes, TypeVars),
                    set.insert_list(TypeVars, !InvariantTVars)
                )
            ;
                ( TypeBody = hlds_foreign_type(_)
                ; TypeBody = hlds_abstract_type(_)
                ; TypeBody = hlds_solver_type(_)
                ),
                type_vars_in_types(ArgTypes, TypeVars),
                set.insert_list(TypeVars, !InvariantTVars)
            ;
                TypeBody = hlds_eqv_type(EqvType0),
                % This a equivalence type was not expanded out by
                % equiv_type.m, so the source of the equivalence must be
                % outside the set of type definitions that equiv_type.m
                % pays attention to, such as in the implementation section
                % of an imported module.
                %
                % In these cases, expand out the type and process the result
                % as if the equivalence *had* been expanded out.
                hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
                map.from_corresponding_lists(TypeParams, ArgTypes, TSubst),
                apply_subst_to_type(TSubst, EqvType0, EqvType),
                acc_invariant_tvars_in_ctor_rhs_type(TypeTable,
                    BaseTypeCtor, BaseTypeParams, EqvType, !InvariantTVars)
            )
        else
            unexpected($pred, "undefined type")
        )
    ;
        RhsType = tuple_type(ArgTypes, _Kind),
        list.foldl(
            acc_invariant_tvars_in_ctor_rhs_type(TypeTable,
                BaseTypeCtor, BaseTypeParams),
            ArgTypes, !InvariantTVars)
    ;
        RhsType = higher_order_type(_PoF, ArgTypes, _HOInstInfo, _Purity),
        % We do not support any subtyping of higher order types.
        % Therefore the higher order components on the right-hand side of a
        % type definition must be identical in the from-type and the to-type,
        % which means that all type parameters that occur in such
        % higher order types must be bound to the exact same value
        % in the from-type and to-type.
        type_vars_in_types(ArgTypes, TypeVars),
        set.insert_list(TypeVars, !InvariantTVars)
    ;
        RhsType = apply_n_type(_, _, _),
        sorry($pred, "apply_n_type")
    ;
        RhsType = kinded_type(CtorArgType1, _Kind),
        acc_invariant_tvars_in_ctor_rhs_type(TypeTable,
            BaseTypeCtor, BaseTypeParams, CtorArgType1, !InvariantTVars)
    ).

%---------------------%

    % are_type_params_as_related_as_needed(TypeTable, TVarSet, InvariantTVars,
    %   TypeParams, FromArgTypes, ToArgTypes, !TypeAssign):
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
:- pred are_type_params_as_related_as_needed(type_table::in, tvarset::in,
    invariant_tvars::in, list(tvar)::in,
    list(mer_type)::in, list(mer_type)::in,
    type_assign::in, type_assign::out) is semidet.

are_type_params_as_related_as_needed(TypeTable, TVarSet, InvariantTVars,
        TypeParams, FromArgTypes, ToArgTypes, !TypeAssign) :-
    ( if
        TypeParams = [],
        FromArgTypes = [],
        ToArgTypes = []
    then
        true
    else if
        TypeParams = [HeadTypeParam | TailTypeParams],
        FromArgTypes = [HeadFromArgType | TailFromArgTypes],
        ToArgTypes = [HeadToArgType | TailToArgTypes]
    then
        is_type_param_pair_as_related_as_needed(TypeTable, TVarSet,
            InvariantTVars, HeadTypeParam, HeadFromArgType, HeadToArgType,
            !TypeAssign),
        are_type_params_as_related_as_needed(TypeTable, TVarSet,
            InvariantTVars, TailTypeParams, TailFromArgTypes, TailToArgTypes,
            !TypeAssign)
    else
        % FromArgTypes and ToArgTypes are the actual types bound to TypeParams
        % in the from-type and to-type of the coercion respectively.
        % If their length do not match, then some earlier compiler pass
        % screwed up really badly.
        unexpected($pred, "length mismatch")
    ).

:- pred is_type_param_pair_as_related_as_needed(type_table::in, tvarset::in,
    invariant_tvars::in, tvar::in, mer_type::in, mer_type::in,
    type_assign::in, type_assign::out) is semidet.

is_type_param_pair_as_related_as_needed(TypeTable, TVarSet, InvariantTVars,
        TypeVar, FromType, ToType, !TypeAssign) :-
    ( if set.contains(InvariantTVars, TypeVar) then
        types_compare_as_given(TypeTable, TVarSet, compare_equal,
            FromType, ToType, !TypeAssign)
    else
        ( if
            types_compare_as_given(TypeTable, TVarSet, compare_equal_lt,
                FromType, ToType, !TypeAssign)
        then
            true
        else
            types_compare_as_given(TypeTable, TVarSet, compare_equal_lt,
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
    % Note: changes here may need to be made also to types_compare_as_given_mc
    % in modecheck_coerce.m.
    %
:- pred types_compare_as_given(type_table::in, tvarset::in,
    types_comparison::in, mer_type::in, mer_type::in,
    type_assign::in, type_assign::out) is semidet.

types_compare_as_given(TypeTable, TVarSet, Comparison, TypeA, TypeB,
        !TypeAssign) :-
    ( if
        ( TypeA = type_variable(_, _)
        ; TypeB = type_variable(_, _)
        )
    then
        type_assign_unify_type(TypeA, TypeB, !TypeAssign)
    else
        types_compare_as_given_nonvar(TypeTable, TVarSet, Comparison,
            TypeA, TypeB, !TypeAssign)
    ).

:- pred types_compare_as_given_nonvar(type_table::in, tvarset::in,
    types_comparison::in, mer_type::in, mer_type::in,
    type_assign::in, type_assign::out) is semidet.

types_compare_as_given_nonvar(TypeTable, TVarSet, Comparison,
        TypeA, TypeB, !TypeAssign) :-
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
        type_to_ctor_and_args(TypeA, TypeCtorA, ArgTypesA),
        type_to_ctor_and_args(TypeB, TypeCtorB, ArgTypesB),
        ( if TypeCtorA = TypeCtorB then
            corresponding_types_compare_as_given(TypeTable, TVarSet,
                Comparison, ArgTypesA, ArgTypesB, !TypeAssign)
        else
            Comparison = compare_equal_lt,
            get_supertype(TypeTable, TVarSet, TypeCtorA, ArgTypesA,
                SuperTypeA),
            types_compare_as_given(TypeTable, TVarSet, Comparison,
                SuperTypeA, TypeB, !TypeAssign)
        )
    ;
        TypeA = tuple_type(ArgTypesA, Kind),
        TypeB = tuple_type(ArgTypesB, Kind),
        corresponding_types_compare_as_given(TypeTable, TVarSet, Comparison,
            ArgTypesA, ArgTypesB, !TypeAssign)
    ;
        TypeA = higher_order_type(PredOrFunc, ArgTypesA, _HOInstInfoA, Purity),
        TypeB = higher_order_type(PredOrFunc, ArgTypesB, _HOInstInfoB, Purity),
        % We do not allow subtyping in higher order argument types.
        corresponding_types_compare_as_given(TypeTable, TVarSet, compare_equal,
            ArgTypesA, ArgTypesB, !TypeAssign)
    ;
        TypeA = apply_n_type(_, _, _),
        sorry($pred, "apply_n_type")
    ;
        TypeA = kinded_type(TypeA1, Kind),
        TypeB = kinded_type(TypeB1, Kind),
        types_compare_as_given(TypeTable, TVarSet, Comparison,
            TypeA1, TypeB1, !TypeAssign)
    ).

:- pred corresponding_types_compare_as_given(type_table::in, tvarset::in,
    types_comparison::in, list(mer_type)::in, list(mer_type)::in,
    type_assign::in, type_assign::out) is semidet.

corresponding_types_compare_as_given(_TypeTable, _TVarSet, _Comparison,
        [], [], !TypeAssign).
corresponding_types_compare_as_given(TypeTable, TVarSet, Comparison,
        [TypeA | TypesA], [TypeB | TypesB], !TypeAssign) :-
    types_compare_as_given(TypeTable, TVarSet, Comparison,
        TypeA, TypeB, !TypeAssign),
    corresponding_types_compare_as_given(TypeTable, TVarSet, Comparison,
        TypesA, TypesB, !TypeAssign).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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

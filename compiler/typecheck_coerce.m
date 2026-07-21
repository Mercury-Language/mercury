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
        apply_rec_subst_to_type(TypeBindings0, FromType0, FromType1),
        MaybeFromType = yes(FromType1)
    else
        MaybeFromType = no
    ),
    ( if search_var_type(VarTypes0, ToVar, ToType0) then
        apply_rec_subst_to_type(TypeBindings0, ToType0, ToType1),
        MaybeToType = yes(ToType1)
    else
        MaybeToType = no
    ),

    ( if
        MaybeFromType = yes(FromType),
        MaybeToType = yes(ToType),
        type_is_ground_except_vars(ExistQTVars0, FromType),
        type_is_ground_except_vars(ExistQTVars0, ToType)
    then
        % We can compare the types on both sides immediately.
        typecheck_info_get_type_table(Info, TypeTable),
        % NOTE The following block of code has a near-duplicate below
        % in check_coerce_constraint_if_ready, though the two places differ
        % in how they handle both resolved and not-yet-resolved constraints.
        typecheck_coerce_between_types(TypeTable, TVarSet0,
            FromType, ToType, TypeAssign0, TypeAssign1, CoerceFails),
        (
            CoerceFails = [],
            type_assign_get_type_bindings(TypeAssign1, TypeBindings1),
            ( if is_same_type_after_subst(TypeBindings1, FromType, ToType) then
                Coercion = coerce_constraint(FromType, ToType, Context,
                    FromVar, satisfied_but_redundant, []),
                add_coerce_constraint(Coercion, TypeAssign1, TypeAssign)
            else
                TypeAssign = TypeAssign1
            )
        ;
            CoerceFails = [_HeadCoerceFail | _TailCoerceFails],
            Coercion = coerce_constraint(FromType, ToType, Context, FromVar,
                unsatisfiable, CoerceFails),
            add_coerce_constraint(Coercion, TypeAssign0, TypeAssign)
        )
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
        CoerceFail = unknown_or_nonground_type(ExistQTVars0,
            MaybeFromType, MaybeToType),
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

%---------------------%

:- pred typecheck_coerce_between_types(type_table::in, tvarset::in,
    mer_type::in, mer_type::in, type_assign::in, type_assign::out,
    list(coerce_fail)::out) is det.

typecheck_coerce_between_types(TypeTable, TVarSet, FromType, ToType,
        !TypeAssign, CoerceFails) :-
    % Type bindings must have been applied to FromType and ToType already.
    classify_is_du_type(TypeTable, FromType, FromMaybeDuType),
    classify_is_du_type(TypeTable, ToType, ToMaybeDuType),
    (
        FromMaybeDuType = is_not_du_type(FromTypeDesc),
        ToMaybeDuType =   is_not_du_type(ToTypeDesc),
        CoerceFail = non_du_type_ctor(FromType, FromTypeDesc,
            ToType, ToTypeDesc),
        CoerceFails = [CoerceFail]
    ;
        FromMaybeDuType = is_not_du_type(FromTypeDesc),
        ToMaybeDuType =   is_du_type(_),
        CoerceFail = non_du_type_ctor(FromType, FromTypeDesc,
            ToType, ""),
        CoerceFails = [CoerceFail]
    ;
        FromMaybeDuType = is_du_type(_),
        ToMaybeDuType =   is_not_du_type(ToTypeDesc),
        CoerceFail = non_du_type_ctor(FromType, "",
            ToType, ToTypeDesc),
        CoerceFails = [CoerceFail]
    ;
        FromMaybeDuType = is_du_type(FromDuTypeInfo),
        ToMaybeDuType =   is_du_type(ToDuTypeInfo),
        compute_base_type_if_du_type(TypeTable, TVarSet,
            FromDuTypeInfo, FromBaseTypeInfo),
        compute_base_type_if_du_type(TypeTable, TVarSet,
            ToDuTypeInfo, ToBaseTypeInfo),
        FromBaseTypeInfo = du_type_info(FromBaseTypeCtor, FromBaseTypeArgTypes,
            FromBaseTypeDefn, FromBaseTypeBodyDu),
        ToBaseTypeInfo = du_type_info(ToBaseTypeCtor, ToBaseTypeArgTypes,
            _ToBaseTypeDefn, _ToBaseTypeBodyDu),
        ( if
            % The input type and result type must have
            % the same base type constructor.
            BaseTypeCtor = FromBaseTypeCtor,
            BaseTypeCtor = ToBaseTypeCtor
        then
            % Since FromBaseTypeCtor = ToBaseTypeCtor, the two type
            % definitions and their bodies must be the same as well.
            BaseTypeDefn = FromBaseTypeDefn,
            BaseTypeBodyDu = FromBaseTypeBodyDu,
            % Check the variance of type arguments.
            hlds_data.get_type_defn_tparams(BaseTypeDefn, BaseTypeParams),
            compute_which_type_params_must_be_invariant(TypeTable,
                BaseTypeCtor, BaseTypeBodyDu, BaseTypeParams, InvariantTVars),
            are_type_params_as_related_as_needed(TypeTable, TVarSet,
                InvariantTVars, BaseTypeParams,
                FromBaseTypeArgTypes, ToBaseTypeArgTypes,
                !TypeAssign, [], CoerceFails)
        else
            CoerceFail = different_base_types(FromType, FromBaseTypeCtor,
                ToType, ToBaseTypeCtor),
            CoerceFails = [CoerceFail]
        )
    ).

%---------------------%

:- type maybe_du_type
    --->    is_du_type(du_type_info)
            % The type is a du type, with the given info.
    ;       is_not_du_type(string).
            % The type is not a du type. The string describes
            % what kind of type it is. The description allows code
            % that generates diagnostics to add the article "a" in front
            % of these pieces, and the plural suffix "s" after them.

:- type du_type_info
    --->    du_type_info(type_ctor, list(mer_type),
                hlds_type_defn, type_body_du).
            % This du type has the given type_ctor and argument types.
            % The last two arguments give the whole, and the du body part,
            % of the definition of the type_ctor.

    % If the given type is du type, return the empty list. Otherwise,
    % return a description of what kind of non-du type it is.
    %
:- pred classify_is_du_type(type_table::in, mer_type::in,
    maybe_du_type::out) is det.

classify_is_du_type(TypeTable, Type, MaybeDuType) :-
    (
        Type = type_variable(_, _),
        MaybeDuType = is_not_du_type("type variable")
    ;
        Type = defined_type(SymName, ArgTypes, _Kind),
        list.length(ArgTypes, Arity),
        TypeCtor = type_ctor(SymName, Arity),
        ( if search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn) then
            get_type_defn_body(TypeDefn, TypeBody),
            (
                TypeBody = hlds_du_type(TypeBodyDu),
                DuType = du_type_info(TypeCtor, ArgTypes,
                    TypeDefn, TypeBodyDu),
                MaybeDuType = is_du_type(DuType)
            ;
                TypeBody = hlds_eqv_type(_),
                MaybeDuType = is_not_du_type("equivalence type")
            ;
                TypeBody = hlds_foreign_type(_),
                MaybeDuType = is_not_du_type("foreign type")
            ;
                TypeBody = hlds_solver_type(_),
                MaybeDuType = is_not_du_type("solver type")
            ;
                TypeBody = hlds_abstract_type(_),
                MaybeDuType = is_not_du_type("abstract type")
            )
        else
            MaybeDuType = is_not_du_type("unknown type")
        )
    ;
        Type = builtin_type(_),
        MaybeDuType = is_not_du_type("builtin type")
    ;
        Type = tuple_type(_, _),
        % XXX This code preserves old behavior, but it prevents programs
        % from coercing one tuple type to another, even if the tuple's
        % argument types are coerceable.
        MaybeDuType = is_not_du_type("tuple type")
    ;
        Type = higher_order_type(PorF, _, _, _),
        (
            PorF = pf_function,
            MaybeDuType = is_not_du_type("function type")
        ;
            PorF = pf_predicate,
            MaybeDuType = is_not_du_type("predicate type")
        )
    ;
        Type = apply_n_type(_, _, _),
        MaybeDuType = is_not_du_type("function type")
    ;
        Type = kinded_type(SubType, _),
        classify_is_du_type(TypeTable, SubType, MaybeDuType)
    ).

%---------------------%

:- pred compute_base_type_if_du_type(type_table::in, tvarset::in,
    du_type_info::in, du_type_info::out) is det.

compute_base_type_if_du_type(TypeTable, TVarSet, DuTypeInfo, BaseTypeInfo) :-
    DuTypeInfo = du_type_info(TypeCtor, ArgTypes, TypeDefn, TypeBodyDu),
    MaybeSuperType = TypeBodyDu ^ du_type_supertype,
    (
        MaybeSuperType = not_a_subtype,
        BaseTypeInfo = DuTypeInfo
    ;
        MaybeSuperType = subtype_of(SuperType0),
        get_supertype_of_subtype(TVarSet, TypeCtor, ArgTypes,
            TypeDefn, SuperType0, SuperType),
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
        compute_base_type_if_du_type(TypeTable, TVarSet,
            SuperDuTypeInfo, BaseTypeInfo)
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
    type_assign::in, type_assign::out,
    list(coerce_fail)::in, list(coerce_fail)::out) is det.

are_type_params_as_related_as_needed(TypeTable, TVarSet, InvariantTVars,
        TypeParams, FromArgTypes, ToArgTypes, !TypeAssign, !CoerceFails) :-
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
            !TypeAssign, !CoerceFails),
        are_type_params_as_related_as_needed(TypeTable, TVarSet,
            InvariantTVars, TailTypeParams, TailFromArgTypes, TailToArgTypes,
            !TypeAssign, !CoerceFails)
    else
        % FromArgTypes and ToArgTypes are the actual types bound to TypeParams
        % in the from-type and to-type of the coercion respectively.
        % If their length do not match, then some earlier compiler pass
        % screwed up really badly.
        unexpected($pred, "length mismatch")
    ).

:- pred is_type_param_pair_as_related_as_needed(type_table::in, tvarset::in,
    invariant_tvars::in, tvar::in, mer_type::in, mer_type::in,
    type_assign::in, type_assign::out,
    list(coerce_fail)::in, list(coerce_fail)::out) is det.

is_type_param_pair_as_related_as_needed(TypeTable, TVarSet, InvariantTVars,
        TypeVar, FromType, ToType, !TypeAssign, !CoerceFails) :-
    ( if set.contains(InvariantTVars, TypeVar) then
        types_compare_as_given(TypeTable, TVarSet, compare_equal,
            FromType, ToType, !TypeAssign, !CoerceFails)
    else
        types_compare_as_given(TypeTable, TVarSet, compare_equal_lt,
            FromType, ToType, !.TypeAssign, FromToTypeAssign,
            [], FromToCoerceFails),
        (
            FromToCoerceFails = [],
            !:TypeAssign = FromToTypeAssign
        ;
            FromToCoerceFails = [_ | _],
            types_compare_as_given(TypeTable, TVarSet, compare_equal_lt,
                ToType, FromType, !TypeAssign, !CoerceFails)
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
    type_assign::in, type_assign::out,
    list(coerce_fail)::in, list(coerce_fail)::out) is det.

types_compare_as_given(TypeTable, TVarSet, Comparison, TypeA, TypeB,
        !TypeAssign, !CoerceFails) :-
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
        types_compare_as_given_nonvar(TypeTable, TVarSet, Comparison,
            TypeA, TypeB, !TypeAssign, !CoerceFails)
    ).

:- pred types_compare_as_given_nonvar(type_table::in, tvarset::in,
    types_comparison::in, mer_type::in, mer_type::in,
    type_assign::in, type_assign::out,
    list(coerce_fail)::in, list(coerce_fail)::out) is det.

types_compare_as_given_nonvar(TypeTable, TVarSet, Comparison,
        TypeA, TypeB, !TypeAssign, !CoerceFails) :-
    require_complete_switch [TypeA]
    (
        TypeA = builtin_type(BuiltinType),
        ( if TypeB = builtin_type(BuiltinType) then
            true
        else
            CoerceFail = incompatible_types(TypeA, TypeB),
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
                corresponding_types_compare_as_given(TypeTable, TVarSet,
                    Comparison, ArgTypesA, ArgTypesB,
                    !TypeAssign, !CoerceFails)
            else
                (
                    Comparison = compare_equal,
                    CoerceFail = should_be_invariant_arg(TypeA, TypeB),
                    !:CoerceFails = [CoerceFail | !.CoerceFails]
                ;
                    Comparison = compare_equal_lt,
                    ( if
                        get_supertype(TypeTable, TVarSet, TypeCtorA, ArgTypesA,
                            SuperTypeA)
                    then
                        types_compare_as_given(TypeTable, TVarSet, Comparison,
                            SuperTypeA, TypeB, !TypeAssign, !CoerceFails)
                    else
                        % get_supertype fails only if TypeCtorA's definition
                        % is either
                        % - not a du type definition, or
                        % - it is a du type, but not a subtype type definition.
                        % XXX We should return a differnt fail for each.
                        CoerceFail = incompatible_types(TypeA, TypeB),
                        !:CoerceFails = [CoerceFail | !.CoerceFails]
                    )
                )
            )
        else
            CoerceFail = incompatible_types(TypeA, TypeB),
            !:CoerceFails = [CoerceFail | !.CoerceFails]
        )
    ;
        TypeA = tuple_type(ArgTypesA, Kind),
        ( if TypeB = tuple_type(ArgTypesB, Kind) then
            corresponding_types_compare_as_given(TypeTable, TVarSet,
                Comparison, ArgTypesA, ArgTypesB, !TypeAssign, !CoerceFails)
        else
            CoerceFail = incompatible_types(TypeA, TypeB),
            !:CoerceFails = [CoerceFail | !.CoerceFails]
        )
    ;
        TypeA = higher_order_type(PredOrFunc, ArgTypesA, _HOInstInfoA, Purity),
        ( if TypeB = higher_order_type(PredOrFunc, ArgTypesB, _HO, Purity) then
            % We do not allow subtyping in higher order argument types.
            corresponding_types_compare_as_given(TypeTable, TVarSet,
                compare_equal, ArgTypesA, ArgTypesB, !TypeAssign, !CoerceFails)
        else
            CoerceFail = incompatible_types(TypeA, TypeB),
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
            types_compare_as_given(TypeTable, TVarSet, Comparison,
                TypeA1, TypeB1, !TypeAssign, !CoerceFails)
        else
            CoerceFail = incompatible_types(TypeA, TypeB),
            !:CoerceFails = [CoerceFail | !.CoerceFails]
        )
    ).

:- pred corresponding_types_compare_as_given(type_table::in, tvarset::in,
    types_comparison::in, list(mer_type)::in, list(mer_type)::in,
    type_assign::in, type_assign::out,
    list(coerce_fail)::in, list(coerce_fail)::out) is det.

corresponding_types_compare_as_given(_TypeTable, _TVarSet, _Comparison,
        [], [], !TypeAssign, !CoerceFails).
corresponding_types_compare_as_given(TypeTable, TVarSet, Comparison,
        [TypeA | TypesA], [TypeB | TypesB], !TypeAssign, !CoerceFails) :-
    types_compare_as_given(TypeTable, TVarSet, Comparison,
        TypeA, TypeB, !TypeAssign, !CoerceFails),
    corresponding_types_compare_as_given(TypeTable, TVarSet, Comparison,
        TypesA, TypesB, !TypeAssign, !CoerceFails).
corresponding_types_compare_as_given(_TypeTable, _TVarSet, _Comparison,
        [_ | _], [], !TypeAssign, !CoerceFails) :-
    unexpected($pred, "length mismatch").
corresponding_types_compare_as_given(_TypeTable, _TVarSet, _Comparison,
        [], [_ | _], !TypeAssign, !CoerceFails) :-
    unexpected($pred, "length mismatch").

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
                FromType, ToType, TypeAssign0, TypeAssign1, CoerceFails),
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

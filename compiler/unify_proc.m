%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: unify_proc.m.
%
% This module generates the bodies of the automatically generated
% unify, index and compare predicates.
%
%---------------------------------------------------------------------------%

:- module check_hlds.unify_proc.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

%---------------------------------------------------------------------------%

    % add_lazily_generated_unify_pred(TypeCtor, UnifyPredId_for_Type,
    %   !ModuleInfo):
    %
    % For most imported unification procedures, we delay generating
    % declarations and clauses until we know whether they are actually needed
    % because there is a complicated unification involving the type.
    % This predicate is exported for use by higher_order.m when it is
    % specializing calls to unify/2.
    %
:- pred add_lazily_generated_unify_pred(type_ctor::in, pred_id::out,
    module_info::in, module_info::out) is det.

    % add_lazily_generated_compare_pred_decl(TypeCtor, ComparePredId_for_Type,
    %   !ModuleInfo):
    %
    % Add declarations, but not clauses, for a compare or index predicate.
    %
:- pred add_lazily_generated_compare_pred_decl(type_ctor::in, pred_id::out,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%

    % Generate the clauses for one of the compiler-generated special predicates
    % (compare/3, index/3, unify/2, etc.)
    %
:- pred generate_clause_info(special_pred_id::in, mer_type::in,
    hlds_type_body::in, prog_context::in, module_info::in, clauses_info::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.polymorphism.
:- import_module check_hlds.post_typecheck.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.make_hlds.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.special_pred.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_item.      % undesirable dependency
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

add_lazily_generated_unify_pred(TypeCtor, PredId, !ModuleInfo) :-
    ( if type_ctor_is_tuple(TypeCtor) then
        TypeCtor = type_ctor(_, TupleArity),

        % Build a hlds_type_body for the tuple constructor, which will
        % be used by generate_clause_info.
        varset.init(TVarSet0),
        varset.new_vars(TupleArity, TupleArgTVars, TVarSet0, TVarSet),
        prog_type.var_list_to_type_list(map.init, TupleArgTVars,
            TupleArgTypes),

        % Tuple constructors can't be existentially quantified.
        ExistQVars = [],
        ClassConstraints = [],

        MakeUnamedField = (func(ArgType) =
            ctor_arg(no, ArgType, full_word, Context)),
        CtorArgs = list.map(MakeUnamedField, TupleArgTypes),

        CtorSymName = unqualified("{}"),
        Ctor = ctor(ExistQVars, ClassConstraints, CtorSymName, CtorArgs,
            TupleArity, Context),

        ConsId = tuple_cons(TupleArity),
        map.from_assoc_list([ConsId - single_functor_tag], ConsTagValues),
        UnifyPred = no,
        DirectArgCtors = no,
        DuTypeKind = du_type_kind_general,
        ReservedTag = does_not_use_reserved_tag,
        ReservedAddr = does_not_use_reserved_address,
        IsForeign = no,
        TypeBody = hlds_du_type([Ctor], ConsTagValues, no_cheaper_tag_test,
            DuTypeKind, UnifyPred, DirectArgCtors, ReservedTag, ReservedAddr,
            IsForeign),
        construct_type(TypeCtor, TupleArgTypes, Type),

        term.context_init(Context)
    else
        collect_type_defn(!.ModuleInfo, TypeCtor, Type, TVarSet, TypeBody,
            Context)
    ),
    ( if
        can_generate_special_pred_clauses_for_type(!.ModuleInfo,
            TypeCtor, TypeBody)
    then
        % If the unification predicate has another status it should
        % already have been generated.
        % XXX STATUS this is not an appropriate status for a type.
        TypeStatus = type_status(status_pseudo_imported),
        Item = clauses
    else
        TypeStatus = type_status(status_imported(import_locn_implementation)),
        Item = declaration
    ),
    add_lazily_generated_special_pred(spec_pred_unify, Item, TVarSet, Type,
        TypeCtor, TypeBody, Context, TypeStatus, PredId, !ModuleInfo).

add_lazily_generated_compare_pred_decl(TypeCtor, PredId, !ModuleInfo) :-
    collect_type_defn(!.ModuleInfo, TypeCtor, Type, TVarSet, TypeBody,
        Context),

    % If the compare predicate has another status, it should already have been
    % generated.
    TypeStatus = type_status(status_imported(import_locn_implementation)),
    add_lazily_generated_special_pred(spec_pred_compare, declaration, TVarSet,
        Type, TypeCtor, TypeBody, Context, TypeStatus, PredId, !ModuleInfo).

:- pred add_lazily_generated_special_pred(special_pred_id::in,
    unify_pred_item::in, tvarset::in, mer_type::in, type_ctor::in,
    hlds_type_body::in, context::in, type_status::in, pred_id::out,
    module_info::in, module_info::out) is det.

add_lazily_generated_special_pred(SpecialId, Item, TVarSet, Type, TypeCtor,
        TypeBody, Context, TypeStatus, PredId, !ModuleInfo) :-
    % Add the declaration and maybe clauses.
    (
        Item = clauses,
        add_special_pred_for_real(SpecialId, TVarSet, Type, TypeCtor,
            TypeBody, Context, TypeStatus, !ModuleInfo)
    ;
        Item = declaration,
        add_special_pred_decl_for_real(SpecialId, TVarSet, Type, TypeCtor,
            Context, TypeStatus, !ModuleInfo)
    ),

    module_info_get_special_pred_maps(!.ModuleInfo, SpecialPredMaps),
    lookup_special_pred_maps(SpecialPredMaps, SpecialId, TypeCtor, PredId),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),

    % The clauses are generated with all type information computed,
    % so just go on to post_typecheck.
    (
        Item = clauses,
        PredInfo1 = PredInfo0
    ;
        Item = declaration,
        setup_vartypes_in_clauses_for_imported_pred(PredInfo0, PredInfo1)
    ),
    propagate_types_into_modes(!.ModuleInfo, ErrorProcs, PredInfo1, PredInfo),
    expect(unify(ErrorProcs, []), $pred, "ErrorProcs != []"),

    % Call polymorphism to introduce type_info arguments for polymorphic types.
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo),

    % Note that this will not work if the generated clauses call a polymorphic
    % predicate which requires type_infos to be added. Such calls can be
    % generated by generate_clause_info, but unification predicates which
    % contain such calls are never generated lazily.
    polymorphism_process_generated_pred(PredId, !ModuleInfo).

:- type unify_pred_item
    --->    declaration
    ;       clauses.

:- pred collect_type_defn(module_info::in, type_ctor::in, mer_type::out,
    tvarset::out, hlds_type_body::out, prog_context::out) is det.

collect_type_defn(ModuleInfo, TypeCtor, Type, TVarSet, TypeBody, Context) :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_tvarset(TypeDefn, TVarSet),
    hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
    hlds_data.get_type_defn_kind_map(TypeDefn, KindMap),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
    hlds_data.get_type_defn_context(TypeDefn, Context),

    expect(
        special_pred_is_generated_lazily(ModuleInfo, TypeCtor, TypeBody,
            TypeStatus),
        $pred, "not generated lazily"),
    prog_type.var_list_to_type_list(KindMap, TypeParams, TypeArgs),
    construct_type(TypeCtor, TypeArgs, Type).

%---------------------------------------------------------------------------%

generate_clause_info(SpecialPredId, Type, TypeBody, Context, ModuleInfo,
        ClauseInfo) :-
    special_pred_interface(SpecialPredId, Type, ArgTypes, _Modes, _Det),
    some [!Info] (
        info_init(ModuleInfo, !:Info),
        make_fresh_named_vars_from_types(ArgTypes, "HeadVar__", 1, Args,
            !Info),
        (
            SpecialPredId = spec_pred_unify,
            ( if Args = [X, Y] then
                generate_unify_proc_body(Type, TypeBody, X, Y,
                    Context, Clause, !Info)
            else
                unexpected($pred, "bad unify args")
            )
        ;
            SpecialPredId = spec_pred_index,
            ( if Args = [X, Index] then
                generate_index_proc_body(Type, TypeBody, X, Index,
                    Context, Clause, !Info)
            else
                unexpected($pred, "bad index args")
            )
        ;
            SpecialPredId = spec_pred_compare,
            ( if Args = [Res, X, Y] then
                generate_compare_proc_body(Type, TypeBody, Res, X, Y,
                    Context, Clause, !Info)
            else
                unexpected($pred, "bad compare args")
            )
        ),
        info_extract(!.Info, VarSet, Types)
    ),
    map.init(TVarNameMap),
    ArgVec = proc_arg_vector_init(pf_predicate, Args),
    set_clause_list([Clause], ClausesRep),
    rtti_varmaps_init(RttiVarMaps),
    HasForeignClauses = yes,
    HadSyntaxErrors = no,
    ClauseInfo = clauses_info(VarSet, TVarNameMap, Types, Types, ArgVec,
        ClausesRep, init_clause_item_numbers_comp_gen,
        RttiVarMaps, HasForeignClauses, HadSyntaxErrors).

:- pred generate_unify_proc_body(mer_type::in, hlds_type_body::in,
    prog_var::in, prog_var::in, prog_context::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_unify_proc_body(Type, TypeBody, X, Y, Context, Clause, !Info) :-
    info_get_module_info(!.Info, ModuleInfo),
    type_to_ctor_det(Type, TypeCtor),
    ( if
        check_builtin_dummy_type_ctor(TypeCtor) = is_builtin_dummy_type_ctor
    then
        Goal = true_goal_with_context(Context),
        quantify_clause_body([X, Y], Goal, Context, Clause, !Info)
    else if
        type_body_has_user_defined_equality_pred(ModuleInfo,
            TypeBody, UserEqComp)
    then
        generate_user_defined_unify_proc_body(UserEqComp, X, Y, Context,
            Clause, !Info)
    else
        (
            TypeBody = hlds_du_type(Ctors, _, _, DuTypeKind, _, _, _, _, _),
            (
                ( DuTypeKind = du_type_kind_mercury_enum
                ; DuTypeKind = du_type_kind_foreign_enum(_)
                ),
                make_simple_test(X, Y, umc_explicit, [], Goal),
                quantify_clause_body([X, Y], Goal, Context, Clause, !Info)
            ;
                DuTypeKind = du_type_kind_direct_dummy,
                Goal = true_goal_with_context(Context),
                quantify_clause_body([X, Y], Goal, Context, Clause, !Info)
            ;
                DuTypeKind = du_type_kind_notag(_, ArgType, _),
                IsDummyType = check_dummy_type(ModuleInfo, ArgType),
                (
                    IsDummyType = is_dummy_type,
                    % Treat this type as if it were a dummy type itself.
                    Goal = true_goal_with_context(Context),
                    quantify_clause_body([X, Y], Goal, Context, Clause, !Info)
                ;
                    IsDummyType = is_not_dummy_type,
                    generate_du_unify_proc_body(TypeCtor, Ctors, X, Y, Context,
                        Clause, !Info)
                )
            ;
                DuTypeKind = du_type_kind_general,
                generate_du_unify_proc_body(TypeCtor, Ctors, X, Y, Context,
                    Clause, !Info)
            )
        ;
            TypeBody = hlds_eqv_type(EqvType),
            IsDummyType = check_dummy_type(ModuleInfo, EqvType),
            (
                IsDummyType = is_dummy_type,
                % Treat this type as if it were a dummy type itself.
                Goal = true_goal_with_context(Context),
                quantify_clause_body([X, Y], Goal, Context, Clause, !Info)
            ;
                IsDummyType = is_not_dummy_type,
                generate_eqv_unify_proc_body(EqvType, X, Y, Context,
                    Clause, !Info)
            )
        ;
            TypeBody = hlds_solver_type(_),
            generate_default_solver_type_unify_proc_body(X, Y, Context,
                Clause, !Info)
        ;
            TypeBody = hlds_foreign_type(_),
            % If no user defined equality predicate is given,
            % we treat foreign_type as if they were an equivalent
            % to the builtin type c_pointer.
            generate_eqv_unify_proc_body(c_pointer_type, X, Y, Context,
                Clause, !Info)
        ;
            TypeBody = hlds_abstract_type(_),
            ( if compiler_generated_rtti_for_builtins(ModuleInfo) then
                TypeCategory = classify_type(ModuleInfo, Type),
                generate_builtin_unify(TypeCategory, X, Y, Context, Clause,
                    !Info)
            else
                unexpected($pred,
                    "trying to create unify proc for abstract type")
            )
        )
    ).

:- pred generate_builtin_unify(type_ctor_category::in,
    prog_var::in, prog_var::in, prog_context::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_builtin_unify(CtorCat, X, Y, Context, Clause, !Info) :-
    ArgVars = [X, Y],

    % can_generate_special_pred_clauses_for_type ensures the unexpected
    % cases can never occur.
    (
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int)),
        Name = "builtin_unify_int"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint)),
        Name = "builtin_unify_uint"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int8)),
        Name = "builtin_unify_int8"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint8)),
        Name = "builtin_unify_uint8"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int16)),
        Name = "builtin_unify_int16"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint16)),
        Name = "builtin_unify_uint16"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int32)),
        Name = "builtin_unify_int32"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint32)),
        Name = "builtin_unify_uint32"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_char),
        Name = "builtin_unify_character"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_string),
        Name = "builtin_unify_string"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_float),
        Name = "builtin_unify_float"
    ;
        CtorCat = ctor_cat_higher_order,
        Name = "builtin_unify_pred"
    ;
        ( CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_system(_)
        ; CtorCat = ctor_cat_user(_)
        ),
        unexpected($pred, "bad ctor category")
    ),
    build_call(Name, ArgVars, Context, UnifyGoal, !Info),
    quantify_clause_body(ArgVars, UnifyGoal, Context, Clause, !Info).

:- pred generate_user_defined_unify_proc_body(unify_compare::in,
    prog_var::in, prog_var::in, prog_context::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_user_defined_unify_proc_body(UserEqCompare, _, _, _, _, !Info) :-
    UserEqCompare = abstract_noncanonical_type(_IsSolverType),
    unexpected($pred,
        "trying to create unify proc for abstract noncanonical type").
generate_user_defined_unify_proc_body(UserEqCompare, X, Y, Context, Clause,
        !Info) :-
    UserEqCompare = unify_compare(MaybeUnify, MaybeCompare),
    (
        MaybeUnify = yes(UnifyPredName),
        % Just generate a call to the specified predicate, which is the
        % user-defined equality pred for this type. (The pred_id and proc_id
        % will be figured out by type checking and mode analysis.)

        PredId = invalid_pred_id,
        ModeId = invalid_proc_id,
        Call = plain_call(PredId, ModeId, [X, Y], not_builtin, no,
            UnifyPredName),
        goal_info_init(Context, GoalInfo),
        Goal0 = hlds_goal(Call, GoalInfo)
    ;
        MaybeUnify = no,
        (
            MaybeCompare = yes(ComparePredName),
            % Just generate a call to the specified predicate, which is the
            % user-defined comparison pred for this type, and unify the result
            % with `='. (The pred_id and proc_id will be figured out by type
            % checking and mode analysis.)

            info_new_var(comparison_result_type, ResultVar, !Info),
            PredId = invalid_pred_id,
            ModeId = invalid_proc_id,
            Call = plain_call(PredId, ModeId, [ResultVar, X, Y], not_builtin,
                no, ComparePredName),
            goal_info_init(Context, GoalInfo),
            CallGoal = hlds_goal(Call, GoalInfo),

            create_pure_atomic_complicated_unification(ResultVar,
                compare_functor("="), Context, umc_explicit, [], UnifyGoal),
            Goal0 = hlds_goal(conj(plain_conj, [CallGoal, UnifyGoal]),
                GoalInfo)
        ;
            MaybeCompare = no,
            unexpected($pred, "MaybeCompare = no")
        )
    ),
    maybe_wrap_with_pretest_equality(Context, X, Y, no, Goal0, Goal, !Info),
    quantify_clause_body([X, Y], Goal, Context, Clause, !Info).

:- pred generate_eqv_unify_proc_body(mer_type::in, prog_var::in,
    prog_var::in, prog_context::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_eqv_unify_proc_body(EqvType, X, Y, Context, Clause, !Info) :-
    % We should check whether EqvType is a type variable,
    % an abstract type or a concrete type.
    % If it is type variable, then we should generate the same code
    % we generate now. If it is an abstract type, we should call
    % its unification procedure directly; if it is a concrete type,
    % we should generate the body of its unification procedure
    % inline here.
    make_fresh_named_var_from_type(EqvType, "Cast_HeadVar", 1, CastX, !Info),
    make_fresh_named_var_from_type(EqvType, "Cast_HeadVar", 2, CastY, !Info),
    generate_cast(equiv_type_cast, X, CastX, Context, CastXGoal),
    generate_cast(equiv_type_cast, Y, CastY, Context, CastYGoal),
    create_pure_atomic_complicated_unification(CastX, rhs_var(CastY),
        Context, umc_explicit, [], UnifyGoal),

    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    conj_list_to_goal([CastXGoal, CastYGoal, UnifyGoal], GoalInfo, Goal),
    quantify_clause_body([X, Y], Goal, Context, Clause, !Info).

    % This predicate generates the bodies of index predicates for the
    % types that need index predicates.
    %
    % add_special_preds in make_hlds.m should include index in the list
    % of special preds to define only for the kinds of types which do not
    % lead this predicate to abort.
    %
:- pred generate_index_proc_body(mer_type::in, hlds_type_body::in,
    prog_var::in, prog_var::in, prog_context::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_index_proc_body(Type, TypeBody, X, Index, Context, Clause, !Info) :-
    info_get_module_info(!.Info, ModuleInfo),
    ( if type_body_has_user_defined_equality_pred(ModuleInfo, TypeBody, _) then
        % For non-canonical types, the generated comparison predicate either
        % calls a user-specified comparison predicate or returns an error,
        % and does not call the type's index predicate, so do not generate
        % an index predicate for such types.
        unexpected($pred, "trying to create index proc for non-canonical type")
    else
        (
            TypeBody = hlds_du_type(Ctors, _, _, DuTypeKind, _, _, _, _, _),
            (
                % For enum types, the generated comparison predicate performs
                % an integer comparison, and does not call the type's index
                % predicate, so do not generate an index predicate for such
                % types.
                DuTypeKind = du_type_kind_mercury_enum,
                unexpected($pred, "trying to create index proc for enum type")
            ;
                DuTypeKind = du_type_kind_foreign_enum(_),
                unexpected($pred,
                    "trying to create index proc for foreign enum type")
            ;
                DuTypeKind = du_type_kind_direct_dummy,
                unexpected($pred, "trying to create index proc for dummy type")
            ;
                DuTypeKind = du_type_kind_notag(_, _, _),
                unexpected($pred, "trying to create index proc for notag type")
            ;
                DuTypeKind = du_type_kind_general,
                type_to_ctor_det(Type, TypeCtor),
                generate_du_index_proc_body(TypeCtor, Ctors, X, Index, Context,
                    Clause, !Info)
            )
        ;
            TypeBody = hlds_eqv_type(_Type),
            % The only place that the index predicate for a type can ever
            % be called from is the compare predicate for that type. However,
            % the compare predicate for an equivalence type never calls
            % the index predicate for that type; it calls the compare predicate
            % of the expanded type instead. Therefore the clause body we are
            % being asked to generate should never be invoked.
            unexpected($pred, "trying to create index proc for eqv type")
        ;
            TypeBody = hlds_foreign_type(_),
            unexpected($pred, "trying to create index proc for a foreign type")
        ;
            TypeBody = hlds_solver_type(_),
            unexpected($pred, "trying to create index proc for a solver type")
        ;
            TypeBody = hlds_abstract_type(_),
            unexpected($pred, "trying to create index proc for abstract type")
        )
    ).

:- pred generate_compare_proc_body(mer_type::in, hlds_type_body::in,
    prog_var::in, prog_var::in, prog_var::in, prog_context::in,
    clause::out, unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body(Type, TypeBody, Res, X, Y, Context, Clause,
        !Info) :-
    info_get_module_info(!.Info, ModuleInfo),
    ( if
        type_to_ctor_det(Type, TypeCtor),
        check_builtin_dummy_type_ctor(TypeCtor) = is_builtin_dummy_type_ctor
    then
        generate_dummy_compare_proc_body(Res, X, Y, Context, Clause, !Info)
    else if
        type_body_has_user_defined_equality_pred(ModuleInfo, TypeBody,
            UserEqComp)
    then
        generate_user_defined_compare_proc_body(UserEqComp,
            Res, X, Y, Context, Clause, !Info)
    else
        (
            TypeBody = hlds_du_type(Ctors, _, _, DuTypeKind, _, _, _, _, _),
            (
                ( DuTypeKind = du_type_kind_mercury_enum
                ; DuTypeKind = du_type_kind_foreign_enum(_)
                ),
                generate_enum_compare_proc_body(Res, X, Y, Context, Clause,
                    !Info)
            ;
                DuTypeKind = du_type_kind_direct_dummy,
                generate_dummy_compare_proc_body(Res, X, Y, Context, Clause,
                    !Info)

            ;
                DuTypeKind = du_type_kind_notag(_, ArgType, _),
                IsDummyType = check_dummy_type(ModuleInfo, ArgType),
                (
                    IsDummyType = is_dummy_type,
                    % Treat this type as if it were a dummy type itself.
                    generate_dummy_compare_proc_body(Res, X, Y, Context,
                        Clause, !Info)
                ;
                    IsDummyType = is_not_dummy_type,
                    generate_du_compare_proc_body(Type, Ctors, Res, X, Y,
                        Context, Clause, !Info)
                )
            ;
                DuTypeKind = du_type_kind_general,
                generate_du_compare_proc_body(Type, Ctors, Res, X, Y,
                    Context, Clause, !Info)
            )
        ;
            TypeBody = hlds_eqv_type(EqvType),
            IsDummyType = check_dummy_type(ModuleInfo, EqvType),
            (
                IsDummyType = is_dummy_type,
                % Treat this type as if it were a dummy type itself.
                generate_dummy_compare_proc_body(Res, X, Y, Context, Clause,
                    !Info)
            ;
                IsDummyType = is_not_dummy_type,
                generate_eqv_compare_proc_body(EqvType, Res, X, Y,
                    Context, Clause, !Info)
            )
        ;
            TypeBody = hlds_foreign_type(_),
            generate_eqv_compare_proc_body(c_pointer_type, Res, X, Y,
                Context, Clause, !Info)
        ;
            TypeBody = hlds_solver_type(_),
            generate_default_solver_type_compare_proc_body(Res, X, Y,
                Context, Clause, !Info)
        ;
            TypeBody = hlds_abstract_type(_),
            ( if compiler_generated_rtti_for_builtins(ModuleInfo) then
                TypeCategory = classify_type(ModuleInfo, Type),
                generate_builtin_compare(TypeCategory, Res, X, Y,
                    Context, Clause, !Info)
            else
                unexpected($pred,
                    "trying to create compare proc for abstract type")
            )
        )
    ).

    % This should only be used for the Erlang backend right now.
    % We follow the Erlang order that tuples of smaller arity always precede
    % tuples of larger arity.
    %
:- pred compare_ctors_lexically(constructor::in, constructor::in,
    comparison_result::out) is det.

compare_ctors_lexically(A, B, Res) :-
    list.length(A ^ cons_args, ArityA),
    list.length(B ^ cons_args, ArityB),
    compare(ArityRes, ArityA, ArityB),
    (
        ArityRes = (=),
        % XXX This assumes the string ordering used by the Mercury compiler is
        % the same as that of the target language compiler.
        NameA = unqualify_name(A ^ cons_name),
        NameB = unqualify_name(B ^ cons_name),
        compare(Res, NameA, NameB)
    ;
        ( ArityRes = (<)
        ; ArityRes = (>)
        ),
        Res = ArityRes
    ).

:- pred generate_enum_compare_proc_body(prog_var::in,
    prog_var::in, prog_var::in, prog_context::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_enum_compare_proc_body(Res, X, Y, Context, Clause, !Info) :-
    IntType = int_type,
    make_fresh_named_var_from_type(IntType, "Cast_HeadVar", 1, CastX, !Info),
    make_fresh_named_var_from_type(IntType, "Cast_HeadVar", 2, CastY, !Info),
    generate_cast(unsafe_type_cast, X, CastX, Context, CastXGoal),
    generate_cast(unsafe_type_cast, Y, CastY, Context, CastYGoal),
    build_call("builtin_compare_int", [Res, CastX, CastY], Context,
        CompareGoal, !Info),

    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    conj_list_to_goal([CastXGoal, CastYGoal, CompareGoal], GoalInfo, Goal),
    quantify_clause_body([Res, X, Y], Goal, Context, Clause, !Info).

:- pred generate_dummy_compare_proc_body(prog_var::in, prog_var::in,
    prog_var::in, prog_context::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_dummy_compare_proc_body(Res, X, Y, Context, Clause, !Info) :-
    generate_return_equal(Res, Context, Goal),
    % XXX check me
    quantify_clause_body([Res, X, Y], Goal, Context, Clause, !Info).

:- pred generate_builtin_compare(type_ctor_category::in,
    prog_var::in, prog_var::in, prog_var::in, prog_context::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_builtin_compare(CtorCat, Res, X, Y, Context, Clause, !Info) :-
    ArgVars = [Res, X, Y],

    % can_generate_special_pred_clauses_for_type ensures the unexpected
    % cases can never occur.
    (
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int)),
        Name = "builtin_compare_int"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint)),
        Name = "builtin_compare_uint"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int8)),
        Name = "builtin_compare_int8"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint8)),
        Name = "builtin_compare_uint8"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int16)),
        Name = "builtin_compare_int16"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint16)),
        Name = "builtin_compare_uint16"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int32)),
        Name = "builtin_compare_int32"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint32)),
        Name = "builtin_compare_uint32"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_char),
        Name = "builtin_compare_character"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_string),
        Name = "builtin_compare_string"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_float),
        Name = "builtin_compare_float"
    ;
        CtorCat = ctor_cat_higher_order,
        Name = "builtin_compare_pred"
    ;
        ( CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_system(_)
        ; CtorCat = ctor_cat_user(_)
        ),
        unexpected($pred, "bad ctor category")
    ),
    build_call(Name, ArgVars, Context, CompareGoal, !Info),
    quantify_clause_body(ArgVars, CompareGoal, Context, Clause, !Info).

:- pred generate_default_solver_type_unify_proc_body(prog_var::in,
    prog_var::in, prog_context::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_default_solver_type_unify_proc_body(X, Y, Context, Clause, !Info) :-
    ArgVars = [X, Y],
    build_call("builtin_unify_solver_type", ArgVars, Context, Goal, !Info),
    quantify_clause_body(ArgVars, Goal, Context, Clause, !Info).

:- pred generate_default_solver_type_compare_proc_body(prog_var::in,
    prog_var::in, prog_var::in, prog_context::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_default_solver_type_compare_proc_body(Res, X, Y, Context, Clause,
        !Info) :-
    ArgVars = [Res, X, Y],
    build_call("builtin_compare_solver_type", ArgVars, Context, Goal, !Info),
    quantify_clause_body(ArgVars, Goal, Context, Clause, !Info).

:- pred generate_user_defined_compare_proc_body(unify_compare::in,
    prog_var::in, prog_var::in, prog_var::in, prog_context::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_user_defined_compare_proc_body(UnifyCompare, Res, X, Y,
        Context, Clause, !Info) :-
    (
        UnifyCompare = abstract_noncanonical_type(_),
        unexpected($pred,
            "trying to create compare proc for abstract noncanonical type")
    ;
        UnifyCompare = unify_compare(_, MaybeCompare),
        ArgVars = [Res, X, Y],
        (
            MaybeCompare = yes(ComparePredName),

            % Just generate a call to the specified predicate, which is the
            % user-defined comparison pred for this type.
            % (The pred_id and proc_id will be figured out
            % by type checking and mode analysis.)

            PredId = invalid_pred_id,
            ModeId = invalid_proc_id,
            Call = plain_call(PredId, ModeId, ArgVars, not_builtin, no,
                ComparePredName),
            goal_info_init(Context, GoalInfo),
            Goal0 = hlds_goal(Call, GoalInfo),
            maybe_wrap_with_pretest_equality(Context, X, Y, yes(Res),
                Goal0, Goal, !Info)
        ;
            MaybeCompare = no,
            % Just generate code that will call error/1.
            build_call("builtin_compare_non_canonical_type", ArgVars, Context,
                Goal, !Info)
        ),
        quantify_clause_body(ArgVars, Goal, Context, Clause, !Info)
    ).

:- pred generate_eqv_compare_proc_body(mer_type::in,
    prog_var::in, prog_var::in, prog_var::in, prog_context::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_eqv_compare_proc_body(EqvType, Res, X, Y, Context, Clause, !Info) :-
    % We should check whether EqvType is a type variable, an abstract type
    % or a concrete type. If it is type variable, then we should generate
    % the same code we generate now. If it is an abstract type, we should call
    % its comparison procedure directly; if it is a concrete type, we should
    % generate the body of its comparison procedure inline here.
    make_fresh_named_var_from_type(EqvType, "Cast_HeadVar", 1, CastX, !Info),
    make_fresh_named_var_from_type(EqvType, "Cast_HeadVar", 2, CastY, !Info),
    generate_cast(equiv_type_cast, X, CastX, Context, CastXGoal),
    generate_cast(equiv_type_cast, Y, CastY, Context, CastYGoal),
    build_call("compare", [Res, CastX, CastY], Context, CompareGoal, !Info),

    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    conj_list_to_goal([CastXGoal, CastYGoal, CompareGoal], GoalInfo, Goal),
    quantify_clause_body([Res, X, Y], Goal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

    % For a type such as
    %
    %   :- type t
    %       --->    a1
    %       ;       a2
    %       ;       b(int)
    %       ;       c(float)
    %       ;       d(int, string, t).
    %
    % we want to generate the code
    %
    %   __Unify__(X, Y) :-
    %       (
    %           X = a1,
    %           Y = X
    %           % Actually, to avoid infinite recursion,
    %           % the above unification is done as type int:
    %           %   CastX = unsafe_cast(X) `with_type` int,
    %           %   CastY = unsafe_cast(Y) `with_type` int,
    %           %   CastX = CastY
    %       ;
    %           X = a2,
    %           Y = X   % Likewise, done as type int
    %       ;
    %           X = b(X1),
    %           Y = b(Y2),
    %           X1 = Y2,
    %       ;
    %           X = c(X1),
    %           Y = c(Y1),
    %           X1 = X2,
    %       ;
    %           X = d(X1, X2, X3),
    %           Y = c(Y1, Y2, Y3),
    %           X1 = y1,
    %           X2 = Y2,
    %           X3 = Y3
    %       ).
    %
    % Note that in the disjuncts handling constants, we want to unify Y with
    % X, not with the constant. Doing this allows dupelim to take the code
    % fragments implementing the switch arms for constants and eliminate all
    % but one of them. This can be a significant code size saving for types
    % with lots of constants, which can then lead to significant reductions in
    % C compilation time. The keep_constant_binding feature on the cast goals
    % is there to ask mode analysis to copy any known bound inst on the
    % cast-from variable to the cast-to variable. This is necessary to keep
    % determinism analysis working for modes in which the inputs of the unify
    % predicate are known to be bound to the same constant, modes whose
    % determinism should therefore be inferred to be det.
    % (tests/general/det_complicated_unify2.m tests this case.)
    %
:- pred generate_du_unify_proc_body(type_ctor::in, list(constructor)::in,
    prog_var::in, prog_var::in, prog_context::in,
    clause::out, unify_proc_info::in, unify_proc_info::out) is det.

generate_du_unify_proc_body(TypeCtor, Ctors, X, Y, Context, Clause, !Info) :-
    CanCompareAsInt = can_compare_constants_as_ints(!.Info),
    list.map_foldl(
        generate_du_unify_case(TypeCtor, X, Y, Context, CanCompareAsInt),
        Ctors, Disjuncts, !Info),
    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    Goal0 = hlds_goal(disj(Disjuncts), GoalInfo),
    maybe_wrap_with_pretest_equality(Context, X, Y, no, Goal0, Goal, !Info),
    quantify_clause_body([X, Y], Goal, Context, Clause, !Info).

:- pred generate_du_unify_case(type_ctor::in, prog_var::in, prog_var::in,
    prog_context::in, bool::in, constructor::in, hlds_goal::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_du_unify_case(TypeCtor, X, Y, Context, CanCompareAsInt, Ctor, Goal,
        !Info) :-
    Ctor = ctor(ExistQTVars, _Constraints, FunctorName, ArgTypes,
        FunctorArity, _Ctxt),
    ( if TypeCtor = type_ctor(unqualified("{}"), _) then
        FunctorConsId = tuple_cons(FunctorArity)
    else
        FunctorConsId = cons(FunctorName, FunctorArity, TypeCtor)
    ),
    ( if
        ArgTypes = [],
        CanCompareAsInt = yes
    then
        RHS = rhs_functor(FunctorConsId, is_not_exist_constr, []),
        create_pure_atomic_complicated_unification(X, RHS, Context,
            umc_explicit, [], UnifyX_Goal),
        info_new_named_var(int_type, "CastX", CastX, !Info),
        info_new_named_var(int_type, "CastY", CastY, !Info),
        generate_cast(unsafe_type_cast, X, CastX, Context, CastXGoal0),
        generate_cast(unsafe_type_cast, Y, CastY, Context, CastYGoal0),
        goal_add_feature(feature_keep_constant_binding, CastXGoal0, CastXGoal),
        goal_add_feature(feature_keep_constant_binding, CastYGoal0, CastYGoal),
        create_pure_atomic_complicated_unification(CastY, rhs_var(CastX),
            Context, umc_explicit, [], UnifyY_Goal),
        GoalList = [UnifyX_Goal, CastXGoal, CastYGoal, UnifyY_Goal]
    else
        make_fresh_arg_var_pairs(ExistQTVars, ArgTypes, TypedVarPairs, !Info),
        VarsX = list.map(project_var_x, TypedVarPairs),
        VarsY = list.map(project_var_y, TypedVarPairs),
        RHSX = rhs_functor(FunctorConsId, is_not_exist_constr, VarsX),
        RHSY = rhs_functor(FunctorConsId, is_not_exist_constr, VarsY),
        create_pure_atomic_complicated_unification(X, RHSX, Context,
            umc_explicit, [], UnifyX_Goal),
        create_pure_atomic_complicated_unification(Y, RHSY, Context,
            umc_explicit, [], UnifyY_Goal),
        unify_var_lists(ExistQTVars, TypedVarPairs, UnifyArgs_Goals, !Info),
        GoalList = [UnifyX_Goal, UnifyY_Goal | UnifyArgs_Goals]
    ),
    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    conj_list_to_goal(GoalList, GoalInfo, Goal).

    % Succeed iff the target back end guarantees that comparing two constants
    % for equality can be done by casting them both to integers and comparing
    % the integers for equality.
    %
:- func can_compare_constants_as_ints(unify_proc_info) = bool.

can_compare_constants_as_ints(Info) = CanCompareAsInt :-
    ModuleInfo = Info ^ upi_module_info,
    module_info_get_globals(ModuleInfo, Globals),
    lookup_bool_option(Globals, can_compare_constants_as_ints,
        CanCompareAsInt).

%---------------------------------------------------------------------------%

    % For a type such as
    %
    %   :- type foo
    %       --->    f(a)
    %       ;       g(a, b, c)
    %       ;       h.
    %
    % we want to generate the code
    %
    %   index(X, Index) :-
    %       (
    %           X = f,
    %           Index = 0
    %       ;
    %           X = g(_, _, _),
    %           Index = 1
    %       ;
    %           X = h(_),
    %           Index = 2
    %       ).
    %
:- pred generate_du_index_proc_body(type_ctor::in, list(constructor)::in,
    prog_var::in, prog_var::in, prog_context::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_du_index_proc_body(TypeCtor, Ctors, X, Index, Context, Clause,
        !Info) :-
    list.map_foldl2(generate_du_index_case(TypeCtor, X, Index, Context),
        Ctors, Disjuncts, 0, _, !Info),
    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    Goal = hlds_goal(disj(Disjuncts), GoalInfo),
    quantify_clause_body([X, Index], Goal, Context, Clause, !Info).

:- pred generate_du_index_case(type_ctor::in, prog_var::in, prog_var::in,
    prog_context::in, constructor::in, hlds_goal::out, int::in, int::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_du_index_case(TypeCtor, X, Index, Context, Ctor, Goal, !N, !Info) :-
    Ctor = ctor(ExistQTVars, _Constraints, FunctorName, ArgTypes,
        FunctorArity, _Ctxt),
    FunctorConsId = cons(FunctorName, FunctorArity, TypeCtor),
    make_fresh_vars(ArgTypes, ExistQTVars, ArgVars, !Info),
    create_pure_atomic_complicated_unification(X,
        rhs_functor(FunctorConsId, is_not_exist_constr, ArgVars),
        Context, umc_explicit, [], UnifyX_Goal),
    make_int_const_construction(Index, !.N, UnifyIndex_Goal),
    !:N = !.N + 1,
    GoalList = [UnifyX_Goal, UnifyIndex_Goal],
    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    conj_list_to_goal(GoalList, GoalInfo, Goal).

%---------------------------------------------------------------------------%

:- pred generate_du_compare_proc_body(mer_type::in, list(constructor)::in,
    prog_var::in, prog_var::in, prog_var::in, prog_context::in,
    clause::out, unify_proc_info::in, unify_proc_info::out) is det.

generate_du_compare_proc_body(Type, Ctors0, Res, X, Y, Context, Clause,
        !Info) :-
    info_get_module_info(!.Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, lexically_order_constructors,
        LexicalOrder),
    (
        LexicalOrder = yes,
        list.sort(compare_ctors_lexically, Ctors0, Ctors)
    ;
        LexicalOrder = no,
        Ctors = Ctors0
    ),
    (
        Ctors = [],
        unexpected($pred, "compare for type with no functors")
    ;
        Ctors = [_ | _],
        globals.lookup_int_option(Globals, compare_specialization,
            CompareSpec),
        list.length(Ctors, NumCtors),
        ( if NumCtors =< CompareSpec then
            type_to_ctor_det(Type, TypeCtor),
            generate_du_quad_compare_proc_body(TypeCtor, Ctors, Res, X, Y,
                Context, Goal0, !Info)
        else
            generate_du_linear_compare_proc_body(Type, Ctors, Res, X, Y,
                Context, Goal0, !Info)
        ),
        maybe_wrap_with_pretest_equality(Context, X, Y, yes(Res), Goal0, Goal,
            !Info),
        HeadVars = [Res, X, Y],
        quantify_clause_body(HeadVars, Goal, Context, Clause, !Info)
    ).

%---------------------------------------------------------------------------%

    % For a du type, such as
    %
    %   :- type foo
    %       --->    f(a)
    %       ;       g(a, b, c)
    %       ;       h.
    %
    % the quadratic code we want to generate is
    %
    %   compare(Res, X, Y) :-
    %       (
    %           X = f(X1),
    %           Y = f(Y1),
    %           compare(R, X1, Y1)
    %       ;
    %           X = f(_),
    %           Y = g(_, _, _),
    %           R = (<)
    %       ;
    %           X = f(_),
    %           Y = h,
    %           R = (<)
    %       ;
    %           X = g(_, _, _),
    %           Y = f(_),
    %           R = (>)
    %       ;
    %           X = g(X1, X2, X3),
    %           Y = g(Y1, Y2, Y3),
    %           ( if compare(R1, X1, Y1), R1 \= (=) then
    %               R = R1
    %           else if compare(R2, X2, Y2), R2 \= (=) then
    %               R = R2
    %           else
    %               compare(R, X3, Y3)
    %           )
    %       ;
    %           X = g(_, _, _),
    %           Y = h,
    %           R = (<)
    %       ;
    %           X = f(_),
    %           Y = h,
    %           R = (<)
    %       ;
    %           X = g(_, _, _),
    %           Y = h,
    %           R = (<)
    %       ;
    %           X = h,
    %           Y = h,
    %           R = (<)
    %       ).
    %
    % Note that in the clauses handling two copies of the same constant,
    % we unify Y with the constant, not with X. This is required to get
    % switch_detection and det_analysis to recognize the determinism of the
    % predicate.
    %
:- pred generate_du_quad_compare_proc_body(type_ctor::in,
    list(constructor)::in, prog_var::in, prog_var::in, prog_var::in,
    prog_context::in, hlds_goal::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_du_quad_compare_proc_body(TypeCtor, Ctors, R, X, Y, Context, Goal,
        !Info) :-
    generate_du_quad_compare_switch_on_x(TypeCtor, Ctors, Ctors, R, X, Y,
        Context, [], Cases, !Info),
    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    disj_list_to_goal(Cases, GoalInfo, Goal).

:- pred generate_du_quad_compare_switch_on_x(type_ctor::in,
    list(constructor)::in, list(constructor)::in,
    prog_var::in, prog_var::in, prog_var::in,
    prog_context::in, list(hlds_goal)::in, list(hlds_goal)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_du_quad_compare_switch_on_x(_TypeCtor, [], _RightCtors, _R, _X, _Y,
        _Context, !Cases, !Info).
generate_du_quad_compare_switch_on_x(TypeCtor, [LeftCtor | LeftCtors],
        RightCtors, R, X, Y, Context, !Cases, !Info) :-
    generate_du_quad_compare_switch_on_y(TypeCtor, LeftCtor, RightCtors,
        ">", R, X, Y, Context, !Cases, !Info),
    generate_du_quad_compare_switch_on_x(TypeCtor, LeftCtors, RightCtors,
        R, X, Y, Context, !Cases, !Info).

:- pred generate_du_quad_compare_switch_on_y(type_ctor::in,
    constructor::in, list(constructor)::in, string::in,
    prog_var::in, prog_var::in, prog_var::in, prog_context::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_du_quad_compare_switch_on_y(_TypeCtor, _LeftCtor, [],
        _Cmp, _R, _X, _Y, _Context, !Cases, !Info).
generate_du_quad_compare_switch_on_y(TypeCtor, LeftCtor,
        [RightCtor | RightCtors], Cmp0, R, X, Y, Context, !Cases, !Info) :-
    ( if LeftCtor = RightCtor then
        generate_compare_case(TypeCtor, LeftCtor, R, X, Y, Context, quad, Case,
            !Info),
        Cmp1 = "<"
    else
        generate_asymmetric_compare_case(TypeCtor, LeftCtor, RightCtor,
            Cmp0, R, X, Y, Context, Case, !Info),
        Cmp1 = Cmp0
    ),
    generate_du_quad_compare_switch_on_y(TypeCtor, LeftCtor, RightCtors,
        Cmp1, R, X, Y, Context, [Case | !.Cases], !:Cases, !Info).

%---------------------------------------------------------------------------%

    % For a du type, such as
    %
    %   :- type foo
    %       --->    f
    %       ;       g(a)
    %       ;       h(b, foo).
    %
    % the linear code we want to generate is
    %
    %   compare(Res, X, Y) :-
    %       __Index__(X, X_Index),                  % Call_X_Index
    %       __Index__(Y, Y_Index),                  % Call_Y_Index
    %       ( if X_Index < Y_Index then             % Call_Less_Than
    %           Res = (<)   % Return_Less_Than
    %       else if X_Index > Y_Index then          % Call_Greater_Than
    %           Res = (>)   % Return_Greater_Than
    %       else if
    %           % This disjunction is generated by generate_compare_cases,
    %           % below.
    %           (
    %               X = f
    %               R = (=)
    %           ;
    %               X = g(X1),
    %               Y = g(Y1),
    %               compare(R, X1, Y1)
    %           ;
    %               X = h(X1, X2),
    %               Y = h(Y1, Y2),
    %               ( if compare(R1, X1, Y1), R1 \= (=) then
    %                   R = R1
    %               else
    %                   compare(R, X2, Y2)
    %               )
    %           )
    %       then
    %           Res = R     % Return_R
    %       else
    %           compare_error   % Abort
    %       ).
    %
    % Note that disjuncts covering constants do not test Y, since for constants
    % X_Index = Y_Index implies X = Y.
    %
:- pred generate_du_linear_compare_proc_body(mer_type::in,
    list(constructor)::in, prog_var::in, prog_var::in, prog_var::in,
    prog_context::in, hlds_goal::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_du_linear_compare_proc_body(Type, Ctors, Res, X, Y, Context, Goal,
        !Info) :-
    IntType = int_type,
    info_new_var(IntType, X_Index, !Info),
    info_new_var(IntType, Y_Index, !Info),
    info_new_var(comparison_result_type, R, !Info),

    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),

    X_InstmapDelta = instmap_delta_bind_var(X_Index),
    build_specific_call(Type, spec_pred_index, [X, X_Index],
        X_InstmapDelta, detism_det, Context, Call_X_Index, !Info),
    Y_InstmapDelta = instmap_delta_bind_var(Y_Index),
    build_specific_call(Type, spec_pred_index, [Y, Y_Index],
        Y_InstmapDelta, detism_det, Context, Call_Y_Index, !Info),

    build_call("builtin_int_lt", [X_Index, Y_Index], Context,
        Call_Less_Than, !Info),
    build_call("builtin_int_gt", [X_Index, Y_Index], Context,
        Call_Greater_Than, !Info),

    make_const_construction(Res, compare_cons_id("<"), Return_Less_Than),
    make_const_construction(Res, compare_cons_id(">"), Return_Greater_Than),

    create_pure_atomic_complicated_unification(Res, rhs_var(R), Context,
        umc_explicit, [], Return_R),

    type_to_ctor_det(Type, TypeCtor),
    generate_compare_cases(TypeCtor, Ctors, R, X, Y, Context, Cases, !Info),
    CasesGoal = hlds_goal(disj(Cases), GoalInfo),

    build_call("compare_error", [], Context, Abort, !Info),

    HandleEqualGoal =
        hlds_goal(
            if_then_else([], CasesGoal, Return_R, Abort),
            GoalInfo),
    HandleGreaterEqualGoal =
        hlds_goal(
            if_then_else([], Call_Greater_Than, Return_Greater_Than,
                HandleEqualGoal),
            GoalInfo),
    HandleLessGreaterEqualGoal =
        hlds_goal(
            if_then_else([], Call_Less_Than, Return_Less_Than,
                HandleGreaterEqualGoal),
            GoalInfo),
    Goal =
        hlds_goal(
            conj(plain_conj,
                [Call_X_Index, Call_Y_Index, HandleLessGreaterEqualGoal]),
            GoalInfo).

    % generate_compare_cases: for a type such as
    %
    %   :- type foo
    %       --->    f
    %       ;       g(a)
    %       ;       h(b, foo).
    %
    % we want to generate code
    %
    %   (
    %       X = f,      % UnifyX_Goal
    %       Y = X,      % UnifyY_Goal
    %       R = (=)     % CompareArgs_Goal
    %   ;
    %       X = g(X1),
    %       Y = g(Y1),
    %       compare(R, X1, Y1)
    %   ;
    %       X = h(X1, X2),
    %       Y = h(Y1, Y2),
    %       ( if compare(R1, X1, Y1), R1 \= (=) then
    %           R = R1
    %       else
    %           compare(R, X2, Y2)
    %       )
    %   )
    %
    % Note that in the clauses for constants, we unify Y with X, not with
    % the constant. This is to allow dupelim to eliminate all but one of
    % the code fragments implementing such switch arms.
    %
:- pred generate_compare_cases(type_ctor::in, list(constructor)::in,
    prog_var::in, prog_var::in, prog_var::in, prog_context::in,
    list(hlds_goal)::out, unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_cases(_TypeCtor, [], _R, _X, _Y, _Context, [], !Info).
generate_compare_cases(TypeCtor, [Ctor | Ctors], R, X, Y, Context,
        [Case | Cases], !Info) :-
    generate_compare_case(TypeCtor, Ctor, R, X, Y, Context, linear, Case,
        !Info),
    generate_compare_cases(TypeCtor, Ctors, R, X, Y, Context, Cases, !Info).

:- type linear_or_quad
    --->    linear
    ;       quad.

:- pred generate_compare_case(type_ctor::in, constructor::in, prog_var::in,
    prog_var::in, prog_var::in, prog_context::in, linear_or_quad::in,
    hlds_goal::out, unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_case(TypeCtor, Ctor, R, X, Y, Context, Kind, Case, !Info) :-
    Ctor = ctor(ExistQTVars, _Constraints, FunctorName, ArgTypes,
        FunctorArity, _Ctxt),
    FunctorConsId = cons(FunctorName, FunctorArity, TypeCtor),
    (
        ArgTypes = [],
        RHS = rhs_functor(FunctorConsId, is_not_exist_constr, []),
        create_pure_atomic_complicated_unification(X, RHS, Context,
            umc_explicit, [], UnifyX_Goal),
        generate_return_equal(R, Context, EqualGoal),
        (
            Kind = linear,
            % The disjunct we are generating is executed only if the index
            % values of X and Y are the same, so if X is bound to a constant,
            % Y must also be bound to that same constant.
            GoalList = [UnifyX_Goal, EqualGoal]
        ;
            Kind = quad,
            create_pure_atomic_complicated_unification(Y, RHS, Context,
                umc_explicit, [], UnifyY_Goal),
            GoalList = [UnifyX_Goal, UnifyY_Goal, EqualGoal]
        )
    ;
        ArgTypes = [_ | _],
        make_fresh_arg_var_pairs(ExistQTVars, ArgTypes, TypedVarPairs, !Info),
        VarsX = list.map(project_var_x, TypedVarPairs),
        VarsY = list.map(project_var_y, TypedVarPairs),
        RHSX = rhs_functor(FunctorConsId, is_not_exist_constr, VarsX),
        RHSY = rhs_functor(FunctorConsId, is_not_exist_constr, VarsY),
        create_pure_atomic_complicated_unification(X, RHSX, Context,
            umc_explicit, [], UnifyX_Goal),
        create_pure_atomic_complicated_unification(Y, RHSY, Context,
            umc_explicit, [], UnifyY_Goal),
        compare_args(ExistQTVars, TypedVarPairs, R, Context, CompareArgs_Goal,
            !Info),
        GoalList = [UnifyX_Goal, UnifyY_Goal, CompareArgs_Goal]
    ),
    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    conj_list_to_goal(GoalList, GoalInfo, Case).

:- pred generate_asymmetric_compare_case(type_ctor::in,
    constructor::in, constructor::in,
    string::in, prog_var::in, prog_var::in, prog_var::in, prog_context::in,
    hlds_goal::out, unify_proc_info::in, unify_proc_info::out) is det.

generate_asymmetric_compare_case(TypeCtor, Ctor1, Ctor2, CompareOp, R, X, Y,
        Context, Case, !Info) :-
    Ctor1 = ctor(ExistQTVars1, _Constraints1, FunctorName1, ArgTypes1,
        _Arity1, _Ctxt1),
    Ctor2 = ctor(ExistQTVars2, _Constraints2, FunctorName2, ArgTypes2,
        _Arity2, _Ctxt2),
    list.length(ArgTypes1, FunctorArity1),
    list.length(ArgTypes2, FunctorArity2),
    FunctorConsId1 = cons(FunctorName1, FunctorArity1, TypeCtor),
    FunctorConsId2 = cons(FunctorName2, FunctorArity2, TypeCtor),
    make_fresh_vars(ArgTypes1, ExistQTVars1, Vars1, !Info),
    make_fresh_vars(ArgTypes2, ExistQTVars2, Vars2, !Info),
    RHS1 = rhs_functor(FunctorConsId1, is_not_exist_constr, Vars1),
    RHS2 = rhs_functor(FunctorConsId2, is_not_exist_constr, Vars2),
    create_pure_atomic_complicated_unification(X, RHS1, Context,
        umc_explicit, [], UnifyX_Goal),
    create_pure_atomic_complicated_unification(Y, RHS2, Context,
        umc_explicit, [], UnifyY_Goal),
    make_const_construction(R, compare_cons_id(CompareOp), ReturnResult),
    GoalList = [UnifyX_Goal, UnifyY_Goal, ReturnResult],
    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    conj_list_to_goal(GoalList, GoalInfo, Case).

    % compare_args: for a constructor such as
    %
    %   h(list(int), foo, string)
    %
    % we want to generate code
    %
    %   ( if
    %       compare(R1, X1, Y1),    % Do_Comparison
    %       R1 \= (=)               % Check_Not_Equal
    %   then
    %       R = R1                  % Return_R1
    %   else if
    %       compare(R2, X2, Y2),
    %       R2 \= (=)
    %   then
    %       R = R2
    %   else
    %       compare(R, X3, Y3)      % Return_Comparison
    %   )
    %
    % For a constructor with no arguments, we want to generate code
    %
    %   R = (=)     % Return_Equal
    %
:- pred compare_args(existq_tvars::in, list(typed_var_pair)::in, prog_var::in,
    prog_context::in, hlds_goal::out,
    unify_proc_info::in, unify_proc_info::out) is det.

compare_args(_, [], R, Context, Return_Equal, !Info) :-
    generate_return_equal(R, Context, Return_Equal).
compare_args(ExistQTVars, [TypedVarPair | TypedVarPairs], R, Context, Goal,
        !Info) :-
    TypedVarPair = typed_var_pair(Type, X, Y),
    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),

    % When comparing existentially typed arguments, the arguments may have
    % different types; in that case, rather than just comparing them,
    % which would be a type error, we call `typed_compare', which is a builtin
    % that first compares their types and then compares their values.
    ( if
        some [ExistQTVar] (
            list.member(ExistQTVar, ExistQTVars),
            type_contains_var(Type, ExistQTVar)
        )
    then
        ComparePred = "typed_compare"
    else
        ComparePred = "compare"
    ),
    info_get_module_info(!.Info, ModuleInfo),
    IsDummy = check_dummy_type(ModuleInfo, Type),
    (
        IsDummy = is_dummy_type,
        % X and Y contain dummy values, so there is nothing to compare.
        compare_args(ExistQTVars, TypedVarPairs, R, Context, Goal, !Info)
    ;
        IsDummy = is_not_dummy_type,
        (
            TypedVarPairs = [],
            build_call(ComparePred, [R, X, Y], Context, Goal, !Info)
        ;
            TypedVarPairs = [_ | _],
            info_new_var(comparison_result_type, R1, !Info),
            build_call(ComparePred, [R1, X, Y], Context, Do_Comparison, !Info),

            make_const_construction(R1, compare_cons_id("="), Check_Equal),
            CheckNotEqual = hlds_goal(negation(Check_Equal), GoalInfo),

            create_pure_atomic_complicated_unification(R, rhs_var(R1),
                Context, umc_explicit, [], Return_R1),
            Condition = hlds_goal(
                conj(plain_conj, [Do_Comparison, CheckNotEqual]),
                GoalInfo),
            compare_args(ExistQTVars, TypedVarPairs, R, Context, ElseCase,
                !Info),
            Goal = hlds_goal(
                if_then_else([], Condition, Return_R1, ElseCase),
                GoalInfo)
        )
    ).

:- pred generate_return_equal(prog_var::in, prog_context::in,
    hlds_goal::out) is det.

generate_return_equal(ResultVar, Context, Goal) :-
    make_const_construction(ResultVar, compare_cons_id("="), Goal0),
    goal_set_context(Context, Goal0, Goal).

%---------------------------------------------------------------------------%

:- pred build_call(string::in, list(prog_var)::in, prog_context::in,
    hlds_goal::out, unify_proc_info::in, unify_proc_info::out) is det.

build_call(Name, ArgVars, Context, Goal, !Info) :-
    info_get_module_info(!.Info, ModuleInfo),
    list.length(ArgVars, Arity),

    % We assume that the special preds compare/3, index/2, and unify/2
    % are the only public builtins called by code generated by this module.

    ( if special_pred_name_arity(_, Name, _, Arity) then
        MercuryBuiltin = mercury_public_builtin_module
    else
        MercuryBuiltin = mercury_private_builtin_module
    ),
    generate_simple_call(MercuryBuiltin, Name, pf_predicate,
        mode_no(0), detism_erroneous, purity_pure, ArgVars, [],
        instmap_delta_bind_no_var, ModuleInfo, Context, Goal).

:- pred build_specific_call(mer_type::in, special_pred_id::in,
    list(prog_var)::in, instmap_delta::in, determinism::in,
    prog_context::in, hlds_goal::out,
    unify_proc_info::in, unify_proc_info::out) is det.

build_specific_call(Type, SpecialPredId, ArgVars, InstmapDelta, Detism,
        Context, Goal, !Info) :-
    info_get_module_info(!.Info, ModuleInfo),
    ( if
        polymorphism.get_special_proc(Type, SpecialPredId, ModuleInfo,
            PredName, PredId, ProcId)
    then
        GoalExpr = plain_call(PredId, ProcId, ArgVars, not_builtin, no,
            PredName),
        set_of_var.list_to_set(ArgVars, NonLocals),
        goal_info_init(NonLocals, InstmapDelta, Detism, purity_pure,
            GoalInfo0),
        goal_info_set_context(Context, GoalInfo0, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    else
        % build_specific_call is only ever used to build calls
        % to special preds for a type in the bodies of other special preds
        % for that same type. If the special preds for a type are built in the
        % right order (index before compare), the lookup should never fail.
        unexpected($pred, "lookup failed")
    ).

%---------------------------------------------------------------------------%

:- pred unify_var_lists(existq_tvars::in, list(typed_var_pair)::in,
    list(hlds_goal)::out, unify_proc_info::in, unify_proc_info::out) is det.

unify_var_lists(_, [], [], !Info).
unify_var_lists(ExistQTVars, [TypedVarPair | TypedVarPairs], [Goal | Goals],
        !Info) :-
    TypedVarPair = typed_var_pair(Type, X, Y),
    term.context_init(Context),
    ( if
        info_get_module_info(!.Info, ModuleInfo),
        check_dummy_type(ModuleInfo, Type) = is_dummy_type
    then
        Goal = true_goal
    else if
        % When unifying existentially typed arguments, the arguments may have
        % different types; in that case, rather than just unifying them,
        % which would be a type error, we call `typed_unify', which is
        % a builtin that first checks that their types are equal and then
        % unifies the values.

        some [ExistQTVar] (
            list.member(ExistQTVar, ExistQTVars),
            type_contains_var(Type, ExistQTVar)
        )
    then
        build_call("typed_unify", [X, Y], Context, Goal, !Info)
    else
        create_pure_atomic_complicated_unification(X, rhs_var(Y),
            Context, umc_explicit, [], Goal)
    ),
    unify_var_lists(ExistQTVars, TypedVarPairs, Goals, !Info).

%---------------------------------------------------------------------------%

:- pred maybe_wrap_with_pretest_equality(prog_context::in,
    prog_var::in, prog_var::in, maybe(prog_var)::in,
    hlds_goal::in, hlds_goal::out,
    unify_proc_info::in, unify_proc_info::out) is det.

maybe_wrap_with_pretest_equality(Context, X, Y, MaybeCompareRes, Goal0, Goal,
        !Info) :-
    ShouldPretestEq = should_pretest_equality(!.Info),
    (
        ShouldPretestEq = no,
        Goal = Goal0
    ;
        ShouldPretestEq = yes,
        CastType = get_pretest_equality_cast_type(!.Info),
        info_new_named_var(CastType, "CastX", CastX, !Info),
        info_new_named_var(CastType, "CastY", CastY, !Info),
        generate_cast(unsafe_type_cast, X, CastX, Context, CastXGoal0),
        generate_cast(unsafe_type_cast, Y, CastY, Context, CastYGoal0),
        goal_add_feature(feature_keep_constant_binding, CastXGoal0, CastXGoal),
        goal_add_feature(feature_keep_constant_binding, CastYGoal0, CastYGoal),
        create_pure_atomic_complicated_unification(CastX, rhs_var(CastY),
            Context, umc_explicit, [], EqualityGoal0),
        goal_add_feature(feature_pretest_equality_condition,
            EqualityGoal0, EqualityGoal),
        CondGoalExpr = conj(plain_conj, [CastXGoal, CastYGoal, EqualityGoal]),
        goal_info_init(GoalInfo0),
        goal_info_set_context(Context, GoalInfo0, ContextGoalInfo),
        CondGoal = hlds_goal(CondGoalExpr, ContextGoalInfo),
        (
            MaybeCompareRes = no,
            EqualGoal = true_goal_with_context(Context),
            GoalInfo = ContextGoalInfo
        ;
            MaybeCompareRes = yes(Res),
            make_const_construction(Res, compare_cons_id("="), EqualGoal),
            EqualGoal = hlds_goal(_, EqualGoalInfo),
            InstmapDelta = goal_info_get_instmap_delta(EqualGoalInfo),
            goal_info_set_instmap_delta(InstmapDelta,
                ContextGoalInfo, GoalInfo)
        ),
        GoalExpr = if_then_else([], CondGoal, EqualGoal, Goal0),
        goal_info_add_feature(feature_pretest_equality, GoalInfo,
            FeaturedGoalInfo),
        Goal = hlds_goal(GoalExpr, FeaturedGoalInfo)
    ).

    % We can start unify and compare predicates that may call other predicates
    % with an equality test, since it often succeeds, and when it does, it is
    % faster than executing the rest of the predicate body.
    %
:- func should_pretest_equality(unify_proc_info) = bool.

should_pretest_equality(Info) = ShouldPretestEq :-
    ModuleInfo = Info ^ upi_module_info,
    module_info_get_globals(ModuleInfo, Globals),
    lookup_bool_option(Globals, should_pretest_equality, ShouldPretestEq).

:- func get_pretest_equality_cast_type(unify_proc_info) = mer_type.

get_pretest_equality_cast_type(Info) = CastType :-
    ModuleInfo = Info ^ upi_module_info,
    module_info_get_globals(ModuleInfo, Globals),
    lookup_bool_option(Globals, pretest_equality_cast_pointers, CastPointers),
    (
        CastPointers = yes,
        CastType = c_pointer_type
    ;
        CastPointers = no,
        CastType = int_type
    ).

%---------------------------------------------------------------------------%

:- pred quantify_clause_body(list(prog_var)::in, hlds_goal::in,
    prog_context::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

quantify_clause_body(HeadVars, Goal0, Context, Clause, !Info) :-
    info_get_varset(!.Info, Varset0),
    info_get_types(!.Info, Types0),
    info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    implicitly_quantify_clause_body_general(ordinary_nonlocals_maybe_lambda,
        HeadVars, _Warnings, Goal0, Goal,
        Varset0, Varset, Types0, Types, RttiVarMaps0, RttiVarMaps),
    info_set_varset(Varset, !Info),
    info_set_types(Types, !Info),
    info_set_rtti_varmaps(RttiVarMaps, !Info),
    Clause = clause(all_modes, Goal, impl_lang_mercury, Context, []).

%---------------------------------------------------------------------------%

:- func compare_type_ctor = type_ctor.

compare_type_ctor = TypeCtor :-
    Builtin = mercury_public_builtin_module,
    TypeCtor = type_ctor(qualified(Builtin, "comparison_result"), 0).

:- func compare_cons_id(string) = cons_id.

compare_cons_id(Name) = cons(SymName, 0, compare_type_ctor) :-
    SymName = qualified(mercury_public_builtin_module, Name).

:- func compare_functor(string) = unify_rhs.

compare_functor(Name) =
    rhs_functor(compare_cons_id(Name), is_not_exist_constr, []).

%---------------------------------------------------------------------------%

:- pred make_fresh_vars(list(constructor_arg)::in, existq_tvars::in,
    list(prog_var)::out, unify_proc_info::in, unify_proc_info::out) is det.

make_fresh_vars(CtorArgs, ExistQTVars, Vars, !Info) :-
    (
        ExistQTVars = [],
        ArgTypes = list.map(func(C) = C ^ arg_type, CtorArgs),
        make_fresh_vars_from_types(ArgTypes, Vars, !Info)
    ;
        ExistQTVars = [_ | _],
        % If there are existential types involved, then it is too hard to get
        % the types right here (it would require allocating new type variables)
        % -- instead, typecheck.m will typecheck the clause to figure out
        % the correct types. So we just allocate the variables and leave it
        % up to typecheck.m to infer their types.
        info_get_varset(!.Info, VarSet0),
        list.length(CtorArgs, NumVars),
        varset.new_vars(NumVars, Vars, VarSet0, VarSet),
        info_set_varset(VarSet, !Info)
    ).

:- pred make_fresh_vars_from_types(list(mer_type)::in, list(prog_var)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

make_fresh_vars_from_types([], [], !Info).
make_fresh_vars_from_types([Type | Types], [Var | Vars], !Info) :-
    info_new_var(Type, Var, !Info),
    make_fresh_vars_from_types(Types, Vars, !Info).

%---------------------------------------------------------------------------%

:- type typed_var_pair
    --->    typed_var_pair(mer_type, prog_var, prog_var).

:- func project_var_x(typed_var_pair) = prog_var.

project_var_x(typed_var_pair(_ArgType, VarX, _VarY)) = VarX.

:- func project_var_y(typed_var_pair) = prog_var.

project_var_y(typed_var_pair(_ArgType, _VarX, VarY)) = VarY.

:- pred make_fresh_arg_var_pairs(existq_tvars::in,
    list(constructor_arg)::in, list(typed_var_pair)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

make_fresh_arg_var_pairs(ExistQTVars, CtorArgs, TypedVarPairs, !Info) :-
    (
        ExistQTVars = [],
        GiveFreshVarsTypes = yes
    ;
        ExistQTVars = [_ | _],
        % If there are existential types involved, then it is too hard to get
        % the types right here (it would require allocating new type variables)
        % -- instead, typecheck.m will typecheck the clause to figure out
        % the correct types. So we just allocate the variables and leave it
        % up to typecheck.m to infer their types.
        GiveFreshVarsTypes = no
    ),
    VarSet0 = !.Info ^ upi_varset,
    VarTypes0 = !.Info ^ upi_vartypes,
    make_fresh_arg_var_pairs(GiveFreshVarsTypes, 1, CtorArgs, TypedVarPairs,
        VarSet0, VarSet, VarTypes0, VarTypes),
    !Info ^ upi_varset := VarSet,
    !Info ^ upi_vartypes := VarTypes.

:- pred make_fresh_arg_var_pairs(bool::in, int::in,
    list(constructor_arg)::in, list(typed_var_pair)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

make_fresh_arg_var_pairs(_GiveFreshVarsTypes, _ArgNum, [], [],
        !VarSet, !VarTypes).
make_fresh_arg_var_pairs(GiveFreshVarsTypes, ArgNum, [CtorArg | CtorArgs],
        [TypedVarPair | TypedVarPairs], !VarSet, !VarTypes) :-
    make_fresh_arg_var_pair(GiveFreshVarsTypes, ArgNum, CtorArg,
        TypedVarPair, !VarSet, !VarTypes),
    make_fresh_arg_var_pairs(GiveFreshVarsTypes, ArgNum + 1, CtorArgs,
        TypedVarPairs, !VarSet, !VarTypes).

:- pred make_fresh_arg_var_pair(bool::in, int::in,
    constructor_arg::in, typed_var_pair::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

make_fresh_arg_var_pair(GiveFreshVarsTypes, ArgNum, CtorArg, TypedVarPair,
        !VarSet, !VarTypes) :-
    ArgType = CtorArg ^ arg_type,
    ArgNumStr = string.int_to_string(ArgNum),
    varset.new_named_var("ArgX" ++ ArgNumStr, VarX, !VarSet),
    varset.new_named_var("ArgY" ++ ArgNumStr, VarY, !VarSet),
    (
        GiveFreshVarsTypes = no
    ;
        GiveFreshVarsTypes = yes,
        add_var_type(VarX, ArgType, !VarTypes),
        add_var_type(VarY, ArgType, !VarTypes)
    ),
    TypedVarPair = typed_var_pair(ArgType, VarX, VarY).

%---------------------------------------------------------------------------%

:- pred make_fresh_named_vars_from_types(list(mer_type)::in, string::in,
    int::in, list(prog_var)::out, unify_proc_info::in, unify_proc_info::out)
    is det.

make_fresh_named_vars_from_types([], _, _, [], !Info).
make_fresh_named_vars_from_types([Type | Types], BaseName, Num,
        [Var | Vars], !Info) :-
    make_fresh_named_var_from_type(Type, BaseName, Num, Var, !Info),
    make_fresh_named_vars_from_types(Types, BaseName, Num + 1, Vars, !Info).

:- pred make_fresh_named_var_from_type(mer_type::in, string::in, int::in,
    prog_var::out, unify_proc_info::in, unify_proc_info::out) is det.

make_fresh_named_var_from_type(Type, BaseName, Num, Var, !Info) :-
    string.int_to_string(Num, NumStr),
    string.append(BaseName, NumStr, Name),
    info_new_named_var(Type, Name, Var, !Info).

%---------------------------------------------------------------------------%

:- type unify_proc_info
    --->    unify_proc_info(
                upi_module_info     ::  module_info,
                upi_varset          ::  prog_varset,
                upi_vartypes        ::  vartypes,
                upi_rtti_varmaps    ::  rtti_varmaps
            ).

:- pred info_init(module_info::in, unify_proc_info::out) is det.

info_init(ModuleInfo, UPI) :-
    varset.init(VarSet),
    init_vartypes(VarTypes),
    rtti_varmaps_init(RttiVarMaps),
    UPI = unify_proc_info(ModuleInfo, VarSet, VarTypes, RttiVarMaps).

:- pred info_get_module_info(unify_proc_info::in, module_info::out) is det.
:- pred info_get_varset(unify_proc_info::in, prog_varset::out) is det.
:- pred info_get_types(unify_proc_info::in, vartypes::out) is det.
:- pred info_get_rtti_varmaps(unify_proc_info::in, rtti_varmaps::out) is det.

info_get_module_info(UPI, X) :-
    X = UPI ^ upi_module_info.
info_get_varset(UPI, X) :-
    X = UPI ^ upi_varset.
info_get_types(UPI, X) :-
    X = UPI ^ upi_vartypes.
info_get_rtti_varmaps(UPI, X) :-
    X = UPI ^ upi_rtti_varmaps.

:- pred info_set_varset(prog_varset::in,
    unify_proc_info::in, unify_proc_info::out) is det.
:- pred info_set_types(vartypes::in,
    unify_proc_info::in, unify_proc_info::out) is det.
:- pred info_set_rtti_varmaps(rtti_varmaps::in,
    unify_proc_info::in, unify_proc_info::out) is det.

info_set_varset(X, !UPI) :-
    !UPI ^ upi_varset := X.
info_set_types(X, !UPI) :-
    !UPI ^ upi_vartypes := X.
info_set_rtti_varmaps(X, !UPI) :-
    !UPI ^ upi_rtti_varmaps := X.

%---------------------%

:- pred info_new_var(mer_type::in, prog_var::out,
    unify_proc_info::in, unify_proc_info::out) is det.

info_new_var(Type, Var, !UPI) :-
    VarSet0 = !.UPI ^ upi_varset,
    VarTypes0 = !.UPI ^ upi_vartypes,
    varset.new_var(Var, VarSet0, VarSet),
    add_var_type(Var, Type, VarTypes0, VarTypes),
    !UPI ^ upi_varset := VarSet,
    !UPI ^ upi_vartypes := VarTypes.

:- pred info_new_named_var(mer_type::in, string::in, prog_var::out,
    unify_proc_info::in, unify_proc_info::out) is det.

info_new_named_var(Type, Name, Var, !UPI) :-
    VarSet0 = !.UPI ^ upi_varset,
    VarTypes0 = !.UPI ^ upi_vartypes,
    varset.new_named_var(Name, Var, VarSet0, VarSet),
    add_var_type(Var, Type, VarTypes0, VarTypes),
    !UPI ^ upi_varset := VarSet,
    !UPI ^ upi_vartypes := VarTypes.

:- pred info_extract(unify_proc_info::in,
    prog_varset::out, vartypes::out) is det.

info_extract(UPI, UPI ^ upi_varset, UPI ^ upi_vartypes).

%---------------------------------------------------------------------------%
:- end_module check_hlds.unify_proc.
%---------------------------------------------------------------------------%

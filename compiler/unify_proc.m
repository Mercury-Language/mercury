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
% This module generates the bodies of the unify, compare and indexpredicates
% that the compiler automatically creates for each type definition.
%
% We can sometimes do this without knowing the representation of the type.
% For example, if the type has user has specified the predicates by which
% two values of the type should be unified or compared, then the automatically
% generated clauses need only call the specified predicates.
%
% However, in many cases, we *do* need to know the representation of the type.
% For example, we need that information
%
% - to decide whether an eqv type is equivalent to a dummy type;
% - to decide whether arguments of a functor of a du type are dummies; and
% - to decide what code to generate for a unify or compare pred for du
%
%---------------------------------------------------------------------------%
%
% This module has five main sections.
%
% The first section distributes the work between the next three sections.
%
% The middle three sections generate the definitions of unify, compare
% and index predicates respectively. Each of these sections handles
% the various kinds of type definitions in the same order.
%
% The last section contains utility predicates.
% 
%---------------------------------------------------------------------------%

:- module check_hlds.unify_proc.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

%---------------------------------------------------------------------------%

:- type spec_pred_defn_info
    --->    spec_pred_defn_info(
                spdi_spec_pred_id       :: special_pred_id,
                spdi_pred_id            :: pred_id,
                spdi_tvarset            :: tvarset,
                spdi_type               :: mer_type,
                spdi_type_ctor          :: type_ctor,
                spdi_type_body          :: hlds_type_body,
                spdi_orig_status        :: type_status,
                spdi_context            :: prog_context
            ).

    % Generate the clauses for one of the compiler-generated special predicates
    % (unify2, compare/3 and index/2) for a type constructor.
    %
    % We will return a modified module_info if the code we generate for
    % the given special_pred requires the definition of another special_pred
    % that is not defined by default.
    %
    % Right now, this will happen only if a comparison predicate needs
    % the use of an index predicate.
    %
:- pred generate_clauses_for_special_pred(spec_pred_defn_info::in,
    clauses_info::out, module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module hlds.add_special_pred.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.special_pred.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_clauses_for_special_pred(SpecDefnInfo, ClauseInfo, !ModuleInfo) :-
    SpecialPredId = SpecDefnInfo ^ spdi_spec_pred_id,
    Type = SpecDefnInfo ^ spdi_type,
    special_pred_interface(SpecialPredId, Type, ArgTypes, _Modes, _Det),
    some [!Info] (
        info_init(!.ModuleInfo, !:Info),
        make_fresh_named_vars_from_types(ArgTypes, "HeadVar__", 1, Args,
            !Info),
        (
            SpecialPredId = spec_pred_unify,
            ( if Args = [X, Y] then
                generate_unify_proc_body(SpecDefnInfo, X, Y, Clause, !Info)
            else
                unexpected($pred, "bad unify args")
            )
        ;
            SpecialPredId = spec_pred_index,
            ( if Args = [X, Index] then
                generate_index_proc_body(SpecDefnInfo, X, Index, Clause, !Info)
            else
                unexpected($pred, "bad index args")
            )
        ;
            SpecialPredId = spec_pred_compare,
            ( if Args = [Res, X, Y] then
                generate_compare_proc_body(SpecDefnInfo,
                    Res, X, Y, Clause, !Info)
            else
                unexpected($pred, "bad compare args")
            )
        ),
        info_extract(!.Info, !:ModuleInfo, VarSet, Types)
    ),
    map.init(TVarNameMap),
    ArgVec = proc_arg_vector_init(pf_predicate, Args),
    set_clause_list([Clause], ClausesRep),
    rtti_varmaps_init(RttiVarMaps),
    % XXX TYPE_REPN Should be HasForeignClauses = no
    HasForeignClauses = yes,
    HadSyntaxErrors = no,
    ClauseInfo = clauses_info(VarSet, TVarNameMap, Types, Types, ArgVec,
        ClausesRep, init_clause_item_numbers_comp_gen,
        RttiVarMaps, HasForeignClauses, HadSyntaxErrors).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Generate clauses for unify predicates.
%

:- pred generate_unify_proc_body(spec_pred_defn_info::in,
    prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_unify_proc_body(SpecDefnInfo, X, Y, Clause, !Info) :-
    TypeCtor = SpecDefnInfo ^ spdi_type_ctor,
    Context = SpecDefnInfo ^ spdi_context,
    IsDummy = is_type_ctor_a_builtin_dummy(TypeCtor),
    (
        IsDummy = is_builtin_dummy_type_ctor,
        generate_unify_proc_body_dummy(Context, X, Y, Clause, !Info)
    ;
        IsDummy = is_not_builtin_dummy_type_ctor,
        info_get_module_info(!.Info, ModuleInfo),
        TypeBody = SpecDefnInfo ^ spdi_type_body,
        ( if
            type_body_has_user_defined_equality_pred(ModuleInfo,
                TypeBody, UserEqComp)
        then
            generate_unify_proc_body_user(UserEqComp, X, Y, Context,
                Clause, !Info)
        else
            (
                TypeBody = hlds_abstract_type(_),
                % There is no way we can generate unify, index or compare
                % predicates for actual abstract types. Having our ancestor
                % pass hlds_abstract_type here is a special in-band signal
                % that the type is actually a builtin type.
                ( if compiler_generated_rtti_for_builtins(ModuleInfo) then
                    generate_unify_proc_body_builtin(SpecDefnInfo, X, Y,
                        Clause, !Info)
                else
                    unexpected($pred,
                        "trying to create unify proc for abstract type")
                )
            ;
                TypeBody = hlds_eqv_type(EqvType),
                EqvIsDummy = is_type_a_dummy(ModuleInfo, EqvType),
                (
                    EqvIsDummy = is_dummy_type,
                    % Treat this type as if it were a dummy type itself.
                    generate_unify_proc_body_dummy(Context, X, Y,
                        Clause, !Info)
                ;
                    EqvIsDummy = is_not_dummy_type,
                    generate_unify_proc_body_eqv(Context, EqvType, X, Y,
                        Clause, !Info)
                )
            ;
                TypeBody = hlds_foreign_type(_),
                % If no user defined equality predicate is given,
                % we treat foreign_types as if they were equivalent
                % to the builtin type c_pointer.
                generate_unify_proc_body_eqv(Context, c_pointer_type, X, Y,
                    Clause, !Info)
            ;
                TypeBody = hlds_solver_type(_),
                generate_unify_proc_body_solver(Context, X, Y,
                    Clause, !Info)
            ;
                TypeBody = hlds_du_type(Ctors, _, MaybeRepn, _),
                (
                    MaybeRepn = no,
                    unexpected($pred, "MaybeRepn = no")
                ;
                    MaybeRepn = yes(Repn)
                ),
                DuTypeKind = Repn ^ dur_kind,
                (
                    ( DuTypeKind = du_type_kind_mercury_enum
                    ; DuTypeKind = du_type_kind_foreign_enum(_)
                    ),
                    generate_unify_proc_body_enum(Context, X, Y,
                        Clause, !Info)
                ;
                    DuTypeKind = du_type_kind_direct_dummy,
                    generate_unify_proc_body_dummy(Context, X, Y,
                        Clause, !Info)
                ;
                    DuTypeKind = du_type_kind_notag(_, ArgType, _),
                    ArgIsDummy = is_type_a_dummy(ModuleInfo, ArgType),
                    (
                        ArgIsDummy = is_dummy_type,
                        % Treat this type as if it were a dummy type
                        % itself.
                        generate_unify_proc_body_dummy(Context, X, Y,
                            Clause, !Info)
                    ;
                        ArgIsDummy = is_not_dummy_type,
                        generate_unify_proc_body_du(SpecDefnInfo,
                            Ctors, X, Y, Clause, !Info)
                    )
                ;
                    DuTypeKind = du_type_kind_general,
                    generate_unify_proc_body_du(SpecDefnInfo,
                        Ctors, X, Y, Clause, !Info)
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

:- pred generate_unify_proc_body_dummy(prog_context::in,
    prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_unify_proc_body_dummy(Context, X, Y, Clause, !Info) :-
    Goal = true_goal_with_context(Context),
    quantify_clause_body([X, Y], Goal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

:- pred generate_unify_proc_body_user(noncanonical::in,
    prog_var::in, prog_var::in, prog_context::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_unify_proc_body_user(NonCanonical, X, Y, Context, Clause, !Info) :-
    (
        NonCanonical = noncanon_abstract(_IsSolverType),
        unexpected($pred,
            "trying to create unify proc for abstract noncanonical type")
    ;
        ( NonCanonical = noncanon_uni_cmp(UnifyPredName, _)
        ; NonCanonical = noncanon_uni_only(UnifyPredName)
        ),
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
        NonCanonical = noncanon_cmp_only(ComparePredName),
        % Just generate a call to the specified predicate, which is the
        % user-defined comparison pred for this type, and unify the result
        % with `='. (The pred_id and proc_id will be figured out by type
        % checking and mode analysis.)

        info_new_var(comparison_result_type, ResultVar, !Info),
        PredId = invalid_pred_id,
        ModeId = invalid_proc_id,
        Call = plain_call(PredId, ModeId, [ResultVar, X, Y], not_builtin, no,
            ComparePredName),
        goal_info_init(Context, GoalInfo),
        CallGoal = hlds_goal(Call, GoalInfo),

        create_pure_atomic_complicated_unification(ResultVar,
            compare_functor("="), Context, umc_explicit, [], UnifyGoal),
        Goal0 = hlds_goal(conj(plain_conj, [CallGoal, UnifyGoal]), GoalInfo)
    ),
    maybe_wrap_with_pretest_equality(Context, X, Y, no, Goal0, Goal, !Info),
    quantify_clause_body([X, Y], Goal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

:- pred generate_unify_proc_body_builtin(spec_pred_defn_info::in,
    prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_unify_proc_body_builtin(SpecDefnInfo, X, Y, Clause, !Info) :-
    info_get_module_info(!.Info, ModuleInfo),
    TypeCtor = SpecDefnInfo ^ spdi_type_ctor,
    CtorCat = classify_type_ctor(ModuleInfo, TypeCtor),
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
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int64)),
        Name = "builtin_unify_int64"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint64)),
        Name = "builtin_unify_uint64"
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
    Context = SpecDefnInfo ^ spdi_context,
    build_call(Name, ArgVars, Context, UnifyGoal, !Info),
    quantify_clause_body(ArgVars, UnifyGoal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

:- pred generate_unify_proc_body_eqv(prog_context::in, mer_type::in,
    prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_unify_proc_body_eqv(Context, EqvType, X, Y, Clause, !Info) :-
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

%---------------------------------------------------------------------------%

:- pred generate_unify_proc_body_solver(prog_context::in,
    prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_unify_proc_body_solver(Context, X, Y, Clause, !Info) :-
    ArgVars = [X, Y],
    build_call("builtin_unify_solver_type", ArgVars, Context, Goal, !Info),
    quantify_clause_body(ArgVars, Goal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

:- pred generate_unify_proc_body_enum(prog_context::in,
    prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_unify_proc_body_enum(Context, X, Y, Clause, !Info) :-
    make_simple_test(X, Y, umc_explicit, [], Goal),
    quantify_clause_body([X, Y], Goal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

    % generate_unify_proc_body_du: for a type such as
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
:- pred generate_unify_proc_body_du(spec_pred_defn_info::in,
    list(constructor)::in, prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_unify_proc_body_du(SpecDefnInfo, Ctors, X, Y, Clause, !Info) :-
    CanCompareAsInt = can_compare_constants_as_ints(!.Info),
    list.map_foldl(generate_du_unify_case(SpecDefnInfo, X, Y, CanCompareAsInt),
        Ctors, Disjuncts, !Info),
    Context = SpecDefnInfo ^ spdi_context,
    goal_info_init(Context, GoalInfo),
    Goal0 = hlds_goal(disj(Disjuncts), GoalInfo),
    maybe_wrap_with_pretest_equality(Context, X, Y, no, Goal0, Goal, !Info),
    quantify_clause_body([X, Y], Goal, Context, Clause, !Info).

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

:- pred generate_du_unify_case(spec_pred_defn_info::in,
    prog_var::in, prog_var::in, bool::in, constructor::in, hlds_goal::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_du_unify_case(SpecDefnInfo, X, Y, CanCompareAsInt, Ctor, Goal,
        !Info) :-
    Ctor = ctor(MaybeExistConstraints, FunctorName, ArgTypes, FunctorArity,
        _Ctxt),
    TypeCtor = SpecDefnInfo ^ spdi_type_ctor,
    ( if TypeCtor = type_ctor(unqualified("{}"), _) then
        FunctorConsId = tuple_cons(FunctorArity)
    else
        FunctorConsId = cons(FunctorName, FunctorArity, TypeCtor)
    ),
    Context = SpecDefnInfo ^ spdi_context,
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
        (
            MaybeExistConstraints = no_exist_constraints,
            ExistQTVars = []
        ;
            MaybeExistConstraints = exist_constraints(ExistConstraints),
            ExistConstraints = cons_exist_constraints(ExistQTVars,
                _Constraints, _UnconstrainedQTVars, _ConstrainedQTVars)
        ),
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
    goal_info_init(Context, GoalInfo),
    conj_list_to_goal(GoalList, GoalInfo, Goal).

:- pred unify_var_lists(existq_tvars::in, list(typed_var_pair)::in,
    list(hlds_goal)::out, unify_proc_info::in, unify_proc_info::out) is det.

unify_var_lists(_, [], [], !Info).
unify_var_lists(ExistQTVars, [TypedVarPair | TypedVarPairs], [Goal | Goals],
        !Info) :-
    TypedVarPair = typed_var_pair(Type, X, Y),
    term.context_init(Context),
    ( if
        info_get_module_info(!.Info, ModuleInfo),
        is_type_a_dummy(ModuleInfo, Type) = is_dummy_type
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
%---------------------------------------------------------------------------%
%
% Generate clauses for compare predicates.
%

:- pred generate_compare_proc_body(spec_pred_defn_info::in,
    prog_var::in, prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body(SpecDefnInfo, Res, X, Y, Clause, !Info) :-
    TypeCtor = SpecDefnInfo ^ spdi_type_ctor,
    Context = SpecDefnInfo ^ spdi_context,
    IsDummy = is_type_ctor_a_builtin_dummy(TypeCtor),
    (
        IsDummy = is_builtin_dummy_type_ctor,
        generate_compare_proc_body_dummy(Context, Res, X, Y, Clause, !Info)
    ;
        IsDummy = is_not_builtin_dummy_type_ctor,
        info_get_module_info(!.Info, ModuleInfo),
        TypeBody = SpecDefnInfo ^ spdi_type_body,
        ( if
            type_body_has_user_defined_equality_pred(ModuleInfo, TypeBody,
                UserEqComp)
        then
            generate_compare_proc_body_user(Context, UserEqComp,
                Res, X, Y, Clause, !Info)
        else
            (
                TypeBody = hlds_abstract_type(_),
                % There is no way we can generate unify, index or compare
                % predicates for actual abstract types. Having our ancestor
                % pass hlds_abstract_type here is a special in-band signal
                % that the type is actually a builtin type.
                ( if compiler_generated_rtti_for_builtins(ModuleInfo) then
                    generate_compare_proc_body_builtin(SpecDefnInfo,
                        Res, X, Y, Clause, !Info)
                else
                    unexpected($pred,
                        "trying to create compare proc for abstract type")
                )
            ;
                TypeBody = hlds_eqv_type(EqvType),
                EqvIsDummy = is_type_a_dummy(ModuleInfo, EqvType),
                (
                    EqvIsDummy = is_dummy_type,
                    % Treat this type as if it were a dummy type itself.
                    generate_compare_proc_body_dummy(Context, Res, X, Y,
                        Clause, !Info)
                ;
                    EqvIsDummy = is_not_dummy_type,
                    generate_compare_proc_body_eqv(Context, EqvType,
                        Res, X, Y, Clause, !Info)
                )
            ;
                TypeBody = hlds_foreign_type(_),
                % If no user defined equality predicate is given,
                % we treat foreign_types as if they were equivalent
                % to the builtin type c_pointer.
                generate_compare_proc_body_eqv(Context, c_pointer_type,
                    Res, X, Y, Clause, !Info)
            ;
                TypeBody = hlds_solver_type(_),
                generate_compare_proc_body_solver(Context,
                    Res, X, Y, Clause, !Info)
            ;
                TypeBody = hlds_du_type(Ctors, _, MaybeRepn, _),
                (
                    MaybeRepn = no,
                    unexpected($pred, "MaybeRepn = no")
                ;
                    MaybeRepn = yes(Repn)
                ),
                DuTypeKind = Repn ^ dur_kind,
                (
                    ( DuTypeKind = du_type_kind_mercury_enum
                    ; DuTypeKind = du_type_kind_foreign_enum(_)
                    ),
                    generate_compare_proc_body_enum(Context,
                        Res, X, Y, Clause, !Info)
                ;
                    DuTypeKind = du_type_kind_direct_dummy,
                    generate_compare_proc_body_dummy(Context,
                        Res, X, Y, Clause, !Info)
                ;
                    DuTypeKind = du_type_kind_notag(_, ArgType, _),
                    ArgIsDummy = is_type_a_dummy(ModuleInfo, ArgType),
                    (
                        ArgIsDummy = is_dummy_type,
                        % Treat this type as if it were a dummy type
                        % itself.
                        generate_compare_proc_body_dummy(Context,
                            Res, X, Y, Clause, !Info)
                    ;
                        ArgIsDummy = is_not_dummy_type,
                        generate_compare_proc_body_du(SpecDefnInfo,
                            Ctors, Res, X, Y, Clause, !Info)
                    )
                ;
                    DuTypeKind = du_type_kind_general,
                    generate_compare_proc_body_du(SpecDefnInfo,
                        Ctors, Res, X, Y, Clause, !Info)
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

:- pred generate_compare_proc_body_dummy(prog_context::in,
    prog_var::in, prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body_dummy(Context, Res, X, Y, Clause, !Info) :-
    generate_return_equal(Res, Context, Goal),
    quantify_clause_body([Res, X, Y], Goal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

:- pred generate_compare_proc_body_user(prog_context::in,
    noncanonical::in, prog_var::in, prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body_user(Context, NonCanonical, Res, X, Y,
        Clause, !Info) :-
    (
        NonCanonical = noncanon_abstract(_),
        unexpected($pred,
            "trying to create compare proc for abstract noncanonical type")
    ;
        NonCanonical = noncanon_uni_only(_),
        % Just generate code that will call error/1.
        ArgVars = [Res, X, Y],
        build_call("builtin_compare_non_canonical_type", ArgVars, Context,
            Goal, !Info)
    ;
        ( NonCanonical = noncanon_uni_cmp(_, ComparePredName)
        ; NonCanonical = noncanon_cmp_only(ComparePredName)
        ),
        % Just generate a call to the specified predicate, which is the
        % user-defined comparison pred for this type.
        % (The pred_id and proc_id will be figured out
        % by type checking and mode analysis.)
        PredId = invalid_pred_id,
        ModeId = invalid_proc_id,
        ArgVars = [Res, X, Y],
        Call = plain_call(PredId, ModeId, ArgVars, not_builtin, no,
            ComparePredName),
        goal_info_init(Context, GoalInfo),
        Goal0 = hlds_goal(Call, GoalInfo),
        maybe_wrap_with_pretest_equality(Context, X, Y, yes(Res),
            Goal0, Goal, !Info)
    ),
    quantify_clause_body(ArgVars, Goal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

:- pred generate_compare_proc_body_builtin(spec_pred_defn_info::in,
    prog_var::in, prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body_builtin(SpecDefnInfo, Res, X, Y, Clause, !Info) :-
    info_get_module_info(!.Info, ModuleInfo),
    TypeCtor = SpecDefnInfo ^ spdi_type_ctor,
    CtorCat = classify_type_ctor(ModuleInfo, TypeCtor),
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
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int64)),
        Name = "builtin_compare_int64"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint64)),
        Name = "builtin_compare_uint64"
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
    Context = SpecDefnInfo ^ spdi_context,
    build_call(Name, ArgVars, Context, CompareGoal, !Info),
    quantify_clause_body(ArgVars, CompareGoal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

:- pred generate_compare_proc_body_eqv(prog_context::in, mer_type::in,
    prog_var::in, prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body_eqv(Context, EqvType, Res, X, Y, Clause, !Info) :-
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

:- pred generate_compare_proc_body_solver(prog_context::in,
    prog_var::in, prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body_solver(Context, Res, X, Y, Clause, !Info) :-
    ArgVars = [Res, X, Y],
    build_call("builtin_compare_solver_type", ArgVars, Context, Goal, !Info),
    quantify_clause_body(ArgVars, Goal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

:- pred generate_compare_proc_body_enum(prog_context::in,
    prog_var::in, prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body_enum(Context, Res, X, Y, Clause, !Info) :-
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

%---------------------------------------------------------------------------%

:- pred generate_compare_proc_body_du(spec_pred_defn_info::in,
    list(constructor)::in, prog_var::in, prog_var::in, prog_var::in,
    clause::out, unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body_du(SpecDefnInfo, Ctors0, Res, X, Y, Clause,
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
            generate_compare_proc_body_du_quad(SpecDefnInfo,
                Ctors, Res, X, Y, Goal0, !Info)
        else
            generate_compare_proc_body_du_linear(SpecDefnInfo,
                Ctors, Res, X, Y, Goal0, !Info)
        ),
        Context = SpecDefnInfo ^ spdi_context,
        maybe_wrap_with_pretest_equality(Context, X, Y, yes(Res), Goal0, Goal,
            !Info),
        HeadVars = [Res, X, Y],
        quantify_clause_body(HeadVars, Goal, Context, Clause, !Info)
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

%---------------------%

    % generate_compare_proc_body_du_quad: for a du type, such as
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
:- pred generate_compare_proc_body_du_quad(spec_pred_defn_info::in,
    list(constructor)::in, prog_var::in, prog_var::in, prog_var::in,
    hlds_goal::out, unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body_du_quad(SpecDefnInfo, Ctors, R, X, Y, Goal,
        !Info) :-
    generate_compare_du_quad_switch_on_x(SpecDefnInfo, Ctors, Ctors, R, X, Y,
        [], Cases, !Info),
    Context = SpecDefnInfo ^ spdi_context,
    goal_info_init(Context, GoalInfo),
    disj_list_to_goal(Cases, GoalInfo, Goal).

:- pred generate_compare_du_quad_switch_on_x(spec_pred_defn_info::in,
    list(constructor)::in, list(constructor)::in,
    prog_var::in, prog_var::in, prog_var::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_du_quad_switch_on_x(_SpecDefnInfo, [],
        _RightCtors, _R, _X, _Y, !Cases, !Info).
generate_compare_du_quad_switch_on_x(SpecDefnInfo, [LeftCtor | LeftCtors],
        RightCtors, R, X, Y, !Cases, !Info) :-
    generate_compare_du_quad_switch_on_y(SpecDefnInfo, LeftCtor, RightCtors,
        ">", R, X, Y, !Cases, !Info),
    generate_compare_du_quad_switch_on_x(SpecDefnInfo, LeftCtors, RightCtors,
        R, X, Y, !Cases, !Info).

:- pred generate_compare_du_quad_switch_on_y(spec_pred_defn_info::in,
    constructor::in, list(constructor)::in, string::in,
    prog_var::in, prog_var::in, prog_var::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_du_quad_switch_on_y(_SpecDefnInfo, _LeftCtor,
        [], _Cmp, _R, _X, _Y, !Cases, !Info).
generate_compare_du_quad_switch_on_y(SpecDefnInfo, LeftCtor,
        [RightCtor | RightCtors], Cmp0, R, X, Y, !Cases, !Info) :-
    ( if LeftCtor = RightCtor then
        generate_compare_case(SpecDefnInfo, LeftCtor,
            R, X, Y, quad, Case, !Info),
        Cmp1 = "<"
    else
        generate_compare_du_quad_compare_asymmetric(SpecDefnInfo,
            LeftCtor, RightCtor, Cmp0, R, X, Y, Case, !Info),
        Cmp1 = Cmp0
    ),
    generate_compare_du_quad_switch_on_y(SpecDefnInfo, LeftCtor, RightCtors,
        Cmp1, R, X, Y, [Case | !.Cases], !:Cases, !Info).

:- pred generate_compare_du_quad_compare_asymmetric(spec_pred_defn_info::in,
    constructor::in, constructor::in,
    string::in, prog_var::in, prog_var::in, prog_var::in,
    hlds_goal::out, unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_du_quad_compare_asymmetric(SpecDefnInfo, Ctor1, Ctor2,
        CompareOp, R, X, Y, Case, !Info) :-
    Ctor1 = ctor(MaybeExistConstraints1, FunctorName1, ArgTypes1,
        _Arity1, _Ctxt1),
    Ctor2 = ctor(MaybeExistConstraints2, FunctorName2, ArgTypes2,
        _Arity2, _Ctxt2),
    list.length(ArgTypes1, FunctorArity1),
    list.length(ArgTypes2, FunctorArity2),
    TypeCtor = SpecDefnInfo ^ spdi_type_ctor,
    FunctorConsId1 = cons(FunctorName1, FunctorArity1, TypeCtor),
    FunctorConsId2 = cons(FunctorName2, FunctorArity2, TypeCtor),
    make_fresh_vars_for_cons_args(ArgTypes1, MaybeExistConstraints1, Vars1,
        !Info),
    make_fresh_vars_for_cons_args(ArgTypes2, MaybeExistConstraints2, Vars2,
        !Info),
    RHS1 = rhs_functor(FunctorConsId1, is_not_exist_constr, Vars1),
    RHS2 = rhs_functor(FunctorConsId2, is_not_exist_constr, Vars2),
    Context = SpecDefnInfo ^ spdi_context,
    create_pure_atomic_complicated_unification(X, RHS1, Context,
        umc_explicit, [], UnifyX_Goal),
    create_pure_atomic_complicated_unification(Y, RHS2, Context,
        umc_explicit, [], UnifyY_Goal),
    make_const_construction(Context, R,
        compare_cons_id(CompareOp), ReturnResult),
    GoalList = [UnifyX_Goal, UnifyY_Goal, ReturnResult],
    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    conj_list_to_goal(GoalList, GoalInfo, Case).

%---------------------%

    % generate_compare_proc_body_du_linear: for a du type, such as
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
    %           % The disjuncts of this disjunction are generated by
    %           % the predicate generate_linear_compare_cases below.
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
    %           Res = R         % Return_R
    %       else
    %           compare_error   % Abort
    %       ).
    %
    % Note that disjuncts covering constants do not test Y, since for constants
    % X_Index = Y_Index implies X = Y.
    %
:- pred generate_compare_proc_body_du_linear(spec_pred_defn_info::in,
    list(constructor)::in, prog_var::in, prog_var::in, prog_var::in,
    hlds_goal::out, unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body_du_linear(SpecDefnInfo, Ctors, Res, X, Y, Goal,
        !Info) :-
    IntType = int_type,
    info_new_var(IntType, X_Index, !Info),
    info_new_var(IntType, Y_Index, !Info),
    info_new_var(comparison_result_type, R, !Info),

    goal_info_init(Context, GoalInfo),

    SpecDefnInfo = spec_pred_defn_info(_SpecialPredId, _ThisPredId,
        TVarSet, Type, TypeCtor, TypeBody, TypeStatus0, Context),
    info_get_module_info(!.Info, ModuleInfo0),
    add_special_pred_decl_defn(spec_pred_index, TVarSet, Type, TypeCtor,
        TypeBody, TypeStatus0, Context, ModuleInfo0, ModuleInfo),
    info_set_module_info(ModuleInfo, !Info),

    X_InstmapDelta = instmap_delta_bind_var(X_Index),
    build_spec_pred_call(TypeCtor, spec_pred_index, [X, X_Index],
        X_InstmapDelta, detism_det, Context, Call_X_Index, !Info),
    Y_InstmapDelta = instmap_delta_bind_var(Y_Index),
    build_spec_pred_call(TypeCtor, spec_pred_index, [Y, Y_Index],
        Y_InstmapDelta, detism_det, Context, Call_Y_Index, !Info),

    build_call("builtin_int_lt", [X_Index, Y_Index], Context,
        Call_Less_Than, !Info),
    build_call("builtin_int_gt", [X_Index, Y_Index], Context,
        Call_Greater_Than, !Info),

    make_const_construction(Context, Res,
        compare_cons_id("<"), Return_Less_Than),
    make_const_construction(Context, Res,
        compare_cons_id(">"), Return_Greater_Than),

    create_pure_atomic_complicated_unification(Res, rhs_var(R), Context,
        umc_explicit, [], Return_R),

    generate_compare_du_linear_cases(SpecDefnInfo, Ctors, R, X, Y,
        Cases, !Info),
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

    % generate_linear_compare_cases: for a type such as
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
:- pred generate_compare_du_linear_cases(spec_pred_defn_info::in,
    list(constructor)::in, prog_var::in, prog_var::in, prog_var::in,
    list(hlds_goal)::out, unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_du_linear_cases(_SpecDefnInfo, [], _R, _X, _Y,
        [], !Info).
generate_compare_du_linear_cases(SpecDefnInfo, [Ctor | Ctors], R, X, Y,
        [Case | Cases], !Info) :-
    generate_compare_case(SpecDefnInfo, Ctor, R, X, Y, linear, Case, !Info),
    generate_compare_du_linear_cases(SpecDefnInfo, Ctors, R, X, Y, Cases,
        !Info).

%---------------------%

:- type linear_or_quad
    --->    linear
    ;       quad.

:- pred generate_compare_case(spec_pred_defn_info::in,
    constructor::in, prog_var::in, prog_var::in, prog_var::in,
    linear_or_quad::in, hlds_goal::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_case(SpecDefnInfo, Ctor, R, X, Y, LinearOrQuad, Case, !Info) :-
    Ctor = ctor(MaybeExistConstraints, FunctorName, ArgTypes, FunctorArity,
        _Ctxt),
    TypeCtor = SpecDefnInfo ^ spdi_type_ctor,
    FunctorConsId = cons(FunctorName, FunctorArity, TypeCtor),
    Context = SpecDefnInfo ^ spdi_context,
    (
        ArgTypes = [],
        RHS = rhs_functor(FunctorConsId, is_not_exist_constr, []),
        create_pure_atomic_complicated_unification(X, RHS, Context,
            umc_explicit, [], UnifyX_Goal),
        generate_return_equal(R, Context, EqualGoal),
        (
            LinearOrQuad = linear,
            % The disjunct we are generating is executed only if the index
            % values of X and Y are the same, so if X is bound to a constant,
            % Y must also be bound to that same constant.
            GoalList = [UnifyX_Goal, EqualGoal]
        ;
            LinearOrQuad = quad,
            create_pure_atomic_complicated_unification(Y, RHS, Context,
                umc_explicit, [], UnifyY_Goal),
            GoalList = [UnifyX_Goal, UnifyY_Goal, EqualGoal]
        )
    ;
        ArgTypes = [_ | _],
        (
            MaybeExistConstraints = no_exist_constraints,
            ExistQTVars = []
        ;
            MaybeExistConstraints = exist_constraints(ExistConstraints),
            ExistConstraints = cons_exist_constraints(ExistQTVars,
                _Constraints, _UnconstrainedQTVars, _ConstrainedQTVars)
        ),
        make_fresh_arg_var_pairs(ExistQTVars, ArgTypes, TypedVarPairs, !Info),
        VarsX = list.map(project_var_x, TypedVarPairs),
        VarsY = list.map(project_var_y, TypedVarPairs),
        RHSX = rhs_functor(FunctorConsId, is_not_exist_constr, VarsX),
        RHSY = rhs_functor(FunctorConsId, is_not_exist_constr, VarsY),
        create_pure_atomic_complicated_unification(X, RHSX, Context,
            umc_explicit, [], UnifyX_Goal),
        create_pure_atomic_complicated_unification(Y, RHSY, Context,
            umc_explicit, [], UnifyY_Goal),
        generate_compare_args(ExistQTVars, TypedVarPairs, R, Context,
            CompareArgs_Goal, !Info),
        GoalList = [UnifyX_Goal, UnifyY_Goal, CompareArgs_Goal]
    ),
    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    conj_list_to_goal(GoalList, GoalInfo, Case).

    % generate_compare_args: for a constructor such as
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
:- pred generate_compare_args(existq_tvars::in, list(typed_var_pair)::in,
    prog_var::in, prog_context::in, hlds_goal::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_args(_, [], R, Context, Goal, !Info) :-
    generate_return_equal(R, Context, Goal).
generate_compare_args(ExistQTVars, [TypedVarPair | TypedVarPairs], R, Context,
        Goal, !Info) :-
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

    IsDummy = is_type_a_dummy(ModuleInfo, Type),
    (
        IsDummy = is_dummy_type,
        % X and Y contain dummy values, so there is nothing to compare.
        generate_compare_args(ExistQTVars, TypedVarPairs, R, Context,
            Goal, !Info)
    ;
        IsDummy = is_not_dummy_type,
        (
            TypedVarPairs = [],
            build_call(ComparePred, [R, X, Y], Context, Goal, !Info)
        ;
            TypedVarPairs = [_ | _],
            info_new_var(comparison_result_type, R1, !Info),
            build_call(ComparePred, [R1, X, Y], Context, Do_Comparison, !Info),

            make_const_construction(Context, R1,
                compare_cons_id("="), Check_Equal),
            CheckNotEqual = hlds_goal(negation(Check_Equal), GoalInfo),

            create_pure_atomic_complicated_unification(R, rhs_var(R1),
                Context, umc_explicit, [], Return_R1),
            Condition = hlds_goal(
                conj(plain_conj, [Do_Comparison, CheckNotEqual]),
                GoalInfo),
            generate_compare_args(ExistQTVars, TypedVarPairs, R, Context,
                ElseCase, !Info),
            Goal = hlds_goal(
                if_then_else([], Condition, Return_R1, ElseCase),
                GoalInfo)
        )
    ).

:- pred generate_return_equal(prog_var::in, prog_context::in,
    hlds_goal::out) is det.

generate_return_equal(ResultVar, Context, Goal) :-
    make_const_construction(Context, ResultVar, compare_cons_id("="), Goal).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Generate clauses for index predicates.
%

:- pred generate_index_proc_body(spec_pred_defn_info::in,
    prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_index_proc_body(SpecDefnInfo, X, Index, Clause, !Info) :-
    % The only kind of type for which generate_compare_proc_body ends up
    % creating an index predicate is one which is
    %
    % - does not have a user-defined equality or comparison predicate,
    % - is a general du type, i.e. not dummy, not foreign, not enum, not notag.
    %
    TypeBody = SpecDefnInfo ^ spdi_type_body,
    (
        TypeBody = hlds_abstract_type(_),
        unexpected($pred, "trying to create index proc for abstract type")
    ;
        TypeBody = hlds_eqv_type(_Type),
        unexpected($pred, "trying to create index proc for eqv type")
    ;
        TypeBody = hlds_foreign_type(_),
        unexpected($pred, "trying to create index proc for a foreign type")
    ;
        TypeBody = hlds_solver_type(_),
        unexpected($pred, "trying to create index proc for a solver type")
    ;
        TypeBody = hlds_du_type(Ctors, _, MaybeRepn, _),
        (
            MaybeRepn = no,
            unexpected($pred, "MaybeRepn = no")
        ;
            MaybeRepn = yes(Repn)
        ),
        DuTypeKind = Repn ^ dur_kind,
        (
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
            generate_index_proc_body_du(SpecDefnInfo, Ctors, X, Index,
                Clause, !Info)
        )
    ).

%---------------------------------------------------------------------------%

    % generate_index_proc_body_du: for a type such as
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
:- pred generate_index_proc_body_du(spec_pred_defn_info::in,
    list(constructor)::in, prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_index_proc_body_du(SpecDefnInfo, Ctors, X, Index, Clause, !Info) :-
    list.map_foldl2(generate_index_du_case(SpecDefnInfo, X, Index),
        Ctors, Disjuncts, 0, _, !Info),
    Context = SpecDefnInfo ^ spdi_context,
    goal_info_init(Context, GoalInfo),
    Goal = hlds_goal(disj(Disjuncts), GoalInfo),
    quantify_clause_body([X, Index], Goal, Context, Clause, !Info).

:- pred generate_index_du_case(spec_pred_defn_info::in,
    prog_var::in, prog_var::in, constructor::in, hlds_goal::out,
    int::in, int::out, unify_proc_info::in, unify_proc_info::out) is det.

generate_index_du_case(SpecDefnInfo, X, Index, Ctor, Goal, !N, !Info) :-
    Ctor = ctor(MaybeExistConstraints, FunctorName, ArgTypes, FunctorArity,
        _Ctxt),
    TypeCtor = SpecDefnInfo ^ spdi_type_ctor,
    FunctorConsId = cons(FunctorName, FunctorArity, TypeCtor),
    make_fresh_vars_for_cons_args(ArgTypes, MaybeExistConstraints, ArgVars,
        !Info),
    Context = SpecDefnInfo ^ spdi_context,
    create_pure_atomic_complicated_unification(X,
        rhs_functor(FunctorConsId, is_not_exist_constr, ArgVars),
        Context, umc_explicit, [], UnifyX_Goal),
    make_int_const_construction(Context, Index, !.N, UnifyIndex_Goal),
    !:N = !.N + 1,
    GoalList = [UnifyX_Goal, UnifyIndex_Goal],
    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    conj_list_to_goal(GoalList, GoalInfo, Goal).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Utility predicates.
%

:- pred build_call(string::in, list(prog_var)::in, prog_context::in,
    hlds_goal::out, unify_proc_info::in, unify_proc_info::out) is det.

build_call(PredName, ArgVars, Context, Goal, !Info) :-
    % We assume that the special preds compare/3, index/2, and unify/2
    % are the only public builtins called by code generated by this module.
    list.length(ArgVars, Arity),
    ( if special_pred_name_arity(_, PredName, _, Arity) then
        ModuleName = mercury_public_builtin_module
    else
        ModuleName = mercury_private_builtin_module
    ),
    info_get_module_info(!.Info, ModuleInfo),
    generate_simple_call(ModuleName, PredName, pf_predicate,
        mode_no(0), detism_erroneous, purity_pure, ArgVars, [],
        instmap_delta_bind_no_var, ModuleInfo, Context, Goal).

:- pred build_spec_pred_call(type_ctor::in, special_pred_id::in,
    list(prog_var)::in, instmap_delta::in, determinism::in,
    prog_context::in, hlds_goal::out,
    unify_proc_info::in, unify_proc_info::out) is det.

build_spec_pred_call(TypeCtor, SpecialPredId, ArgVars, InstmapDelta, Detism,
        Context, Goal, !Info) :-
    info_get_module_info(!.Info, ModuleInfo),
    get_special_proc_det(ModuleInfo, TypeCtor, SpecialPredId,
        PredName, PredId, ProcId),
    GoalExpr = plain_call(PredId, ProcId, ArgVars, not_builtin, no, PredName),
    set_of_var.list_to_set(ArgVars, NonLocals),
    goal_info_init(NonLocals, InstmapDelta, Detism, purity_pure, GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

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
        goal_info_init(Context, ContextGoalInfo),
        CondGoal = hlds_goal(CondGoalExpr, ContextGoalInfo),
        (
            MaybeCompareRes = no,
            EqualGoal = true_goal_with_context(Context),
            GoalInfo = ContextGoalInfo
        ;
            MaybeCompareRes = yes(Res),
            make_const_construction(Context, Res,
                compare_cons_id("="), EqualGoal),
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

:- type typed_var_pair
    --->    typed_var_pair(mer_type, prog_var, prog_var).

:- func project_var_x(typed_var_pair) = prog_var.

project_var_x(typed_var_pair(_ArgType, VarX, _VarY)) = VarX.

:- func project_var_y(typed_var_pair) = prog_var.

project_var_y(typed_var_pair(_ArgType, _VarX, VarY)) = VarY.

%---------------------%

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

%---------------------%

:- pred make_fresh_vars_for_cons_args(list(constructor_arg)::in,
    maybe_cons_exist_constraints::in, list(prog_var)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

make_fresh_vars_for_cons_args(CtorArgs, MaybeExistConstraints, Vars, !Info) :-
    (
        MaybeExistConstraints = no_exist_constraints,
        ArgTypes = list.map(func(C) = C ^ arg_type, CtorArgs),
        make_fresh_vars_from_types(ArgTypes, Vars, !Info)
    ;
        MaybeExistConstraints = exist_constraints(_ExistConstraints),
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

%---------------------%

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

:- pred info_set_module_info(module_info::in,
    unify_proc_info::in, unify_proc_info::out) is det.
:- pred info_set_varset(prog_varset::in,
    unify_proc_info::in, unify_proc_info::out) is det.
:- pred info_set_types(vartypes::in,
    unify_proc_info::in, unify_proc_info::out) is det.
:- pred info_set_rtti_varmaps(rtti_varmaps::in,
    unify_proc_info::in, unify_proc_info::out) is det.

info_set_module_info(X, !UPI) :-
    !UPI ^ upi_module_info := X.
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
    module_info::out, prog_varset::out, vartypes::out) is det.

info_extract(UPI, ModuleInfo, VarSet, VarTypes) :-
    ModuleInfo = UPI ^ upi_module_info,
    VarSet = UPI ^ upi_varset,
    VarTypes = UPI ^ upi_vartypes.

%---------------------------------------------------------------------------%
:- end_module check_hlds.unify_proc.
%---------------------------------------------------------------------------%

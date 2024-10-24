%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2015-2018, 2020-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: unify_proc.m.
%
% This module generates the bodies of the unify, compare and index predicates
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
:- import_module hlds.var_table_hlds.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_type_test.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module parse_tree.vartypes.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set_tree234.
:- import_module string.
:- import_module term.
:- import_module uint8.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_clauses_for_special_pred(SpecDefnInfo, ClauseInfo, !ModuleInfo) :-
    SpecialPredId = SpecDefnInfo ^ spdi_spec_pred_id,
    Type = SpecDefnInfo ^ spdi_type,
    special_pred_interface(SpecialPredId, Type, ArgTypes, _Modes, _Det),
    some [!Info] (
        unify_proc_info_init(!.ModuleInfo, !:Info),
        make_fresh_named_vars_from_types(ArgTypes, "HeadVar__", 1, ArgVars,
            !Info),
        (
            SpecialPredId = spec_pred_unify,
            ( if ArgVars = [X, Y] then
                generate_unify_proc_body(SpecDefnInfo, X, Y, Clauses, !Info)
            else
                unexpected($pred, "bad unify args")
            )
        ;
            SpecialPredId = spec_pred_index,
            ( if ArgVars = [X, Index] then
                generate_index_proc_body(SpecDefnInfo, X, Index, Clause, !Info)
            else
                unexpected($pred, "bad index args")
            ),
            Clauses = [Clause]
        ;
            SpecialPredId = spec_pred_compare,
            ( if ArgVars = [Res, X, Y] then
                generate_compare_proc_body(SpecDefnInfo,
                    Res, X, Y, Clause, !Info)
            else
                unexpected($pred, "bad compare args")
            ),
            Clauses = [Clause]
        ),
        unify_proc_info_extract(!.Info, !:ModuleInfo, VarTable)
    ),
    split_var_table(VarTable, VarSet, VarTypes0),
    vartypes_to_sorted_assoc_list(VarTypes0, VarTypesAL0),
    get_explicitly_typed_vars(VarTypesAL0, [], RevExplicitVarTypesAL),
    vartypes_from_rev_sorted_assoc_list(RevExplicitVarTypesAL,
        ExplicitVarTypes),
    rtti_varmaps_init(RttiVarMaps),
    map.init(TVarNameMap),
    ArgVec = proc_arg_vector_init(pf_predicate, ArgVars),
    set_clause_list(Clauses, ClausesRep),
    % We fill in the VarSet and ExplicitVarTypes fields because typechecking
    % will need them.
    % XXX TYPE_REPN Should be no_foreign_lang_clauses
    ClauseInfo = clauses_info(VarSet, ExplicitVarTypes, VarTable, RttiVarMaps,
        TVarNameMap, ArgVec, ClausesRep, init_clause_item_numbers_comp_gen,
        some_foreign_lang_clauses, no_clause_syntax_errors).

:- pred get_explicitly_typed_vars(assoc_list(prog_var, mer_type)::in,
    assoc_list(prog_var, mer_type)::in,
    assoc_list(prog_var, mer_type)::out) is det.

get_explicitly_typed_vars([], !RevVarsTypes).
get_explicitly_typed_vars([Var - Type | VarsTypes], !RevVarsTypes) :-
    ( if Type = void_type then
        true
    else
        !:RevVarsTypes = [Var - Type | !.RevVarsTypes]
    ),
    get_explicitly_typed_vars(VarsTypes, !RevVarsTypes).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Generate clauses for unify predicates.
%

:- pred generate_unify_proc_body(spec_pred_defn_info::in,
    prog_var::in, prog_var::in, list(clause)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_unify_proc_body(SpecDefnInfo, X, Y, Clauses, !Info) :-
    unify_proc_info_get_module_info(!.Info, ModuleInfo),
    TypeBody = SpecDefnInfo ^ spdi_type_body,
    Context = SpecDefnInfo ^ spdi_context,
    ( if
        TypeBody = hlds_du_type(TypeBodyDu),
        TypeBodyDu = type_body_du(_, subtype_of(SuperType), _, _, _)
    then
        % Unify subtype terms after casting to base type.
        % This is necessary in high-level data grades,
        % and saves some code in low-level data grades.
        TVarSet = SpecDefnInfo ^ spdi_tvarset,
        get_du_base_type(ModuleInfo, TVarSet, SuperType, BaseType),
        generate_unify_proc_body_eqv(Context, BaseType, X, Y, Clause, !Info),
        Clauses = [Clause]
    else if
        % We used to special-case the type_ctors for which
        % is_type_ctor_a_builtin_dummy(TypeCtor) = is_builtin_dummy_type_ctor,
        % but both those types now have user-defined unify and compare preds.
        type_body_has_user_defined_equality_pred(ModuleInfo,
            TypeBody, UserEqComp)
    then
        generate_unify_proc_body_user(UserEqComp, X, Y, Context,
            Clause, !Info),
        Clauses = [Clause]
    else
        (
            TypeBody = hlds_abstract_type(_),
            % There is no way we can generate unify, index or compare
            % predicates for actual abstract types. Having our ancestor
            % pass hlds_abstract_type here is a special in-band signal
            % that the type is actually a builtin type.
            ( if compiler_generated_rtti_for_builtins(ModuleInfo) then
                TypeCtor = SpecDefnInfo ^ spdi_type_ctor,
                CtorCat = classify_type_ctor(ModuleInfo, TypeCtor),
                generate_unify_proc_body_builtin(CtorCat, Context, X, Y,
                    Clause, !Info)
            else
                unexpected($pred,
                    "trying to create unify proc for abstract type")
            ),
            Clauses = [Clause]
        ;
            TypeBody = hlds_eqv_type(EqvType),
            ( if type_is_higher_order(EqvType) then
                generate_unify_proc_body_builtin(ctor_cat_higher_order,
                    Context, X, Y, Clause, !Info)
            else
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
            ),
            Clauses = [Clause]
        ;
            TypeBody = hlds_foreign_type(_),
            % If no user defined equality predicate is given,
            % we treat foreign_types as if they were equivalent
            % to the builtin type c_pointer.
            generate_unify_proc_body_eqv(Context, c_pointer_type, X, Y,
                Clause, !Info),
            Clauses = [Clause]
        ;
            TypeBody = hlds_solver_type(_),
            generate_unify_proc_body_solver(Context, X, Y,
                Clause, !Info),
            Clauses = [Clause]
        ;
            TypeBody = hlds_du_type(TypeBodyDu),
            TypeBodyDu = type_body_du(_, MaybeSuperType, _, MaybeRepn, _),
            expect(unify(MaybeSuperType, not_a_subtype), $pred,
                "MaybeSuperType != not_a_subtype"),
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
                    Clause, !Info),
                Clauses = [Clause]
            ;
                DuTypeKind = du_type_kind_direct_dummy,
                generate_unify_proc_body_dummy(Context, X, Y,
                    Clause, !Info),
                Clauses = [Clause]
            ;
                DuTypeKind = du_type_kind_notag(_, ArgType, _),
                ArgIsDummy = is_type_a_dummy(ModuleInfo, ArgType),
                (
                    ArgIsDummy = is_dummy_type,
                    % Treat this type as if it were a dummy type
                    % itself.
                    generate_unify_proc_body_dummy(Context, X, Y,
                        Clause, !Info),
                    Clauses = [Clause]
                ;
                    ArgIsDummy = is_not_dummy_type,
                    CtorRepns = Repn ^ dur_ctor_repns,
                    generate_unify_proc_body_du(SpecDefnInfo,
                        CtorRepns, X, Y, Clauses, !Info)
                )
            ;
                DuTypeKind = du_type_kind_general,
                CtorRepns = Repn ^ dur_ctor_repns,
                generate_unify_proc_body_du(SpecDefnInfo,
                    CtorRepns, X, Y, Clauses, !Info)
            )
        )
    ).

%---------------------------------------------------------------------------%

:- pred generate_unify_proc_body_dummy(prog_context::in,
    prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_unify_proc_body_dummy(Context, X, Y, Clause, !Info) :-
    Goal = true_goal_with_context(Context),
    quantify_clause_body(all_modes, [X, Y], Goal, Context, Clause, !Info).

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
        NonCanonical = noncanon_subtype,
        unexpected($pred, "trying to create unify proc for subtype")
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

        unify_proc_info_new_var("Result", comparison_result_type,
            ResultVar, !Info),
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
    % XXX If the user-specified unify (or compare) predicate always aborts,
    % we should avoid a pretest, since if it accidentally happens to succeed,
    % it avoids the requested abort.
    maybe_wrap_with_pretest_equality(Context, X, Y, no, Goal0, Goal, !Info),
    quantify_clause_body(all_modes, [X, Y], Goal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

:- pred generate_unify_proc_body_builtin(type_ctor_category::in,
    prog_context::in, prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_unify_proc_body_builtin(CtorCat, Context, X, Y, Clause, !Info) :-
    unify_proc_info_get_module_info(!.Info, ModuleInfo),
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
    build_simple_call(ModuleInfo, mercury_private_builtin_module,
        Name, ArgVars, Context, UnifyGoal),
    quantify_clause_body(all_modes, ArgVars, UnifyGoal, Context, Clause,
        !Info).

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
    quantify_clause_body(all_modes, [X, Y], Goal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

:- pred generate_unify_proc_body_solver(prog_context::in,
    prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_unify_proc_body_solver(Context, X, Y, Clause, !Info) :-
    ArgVars = [X, Y],
    unify_proc_info_get_module_info(!.Info, ModuleInfo),
    build_simple_call(ModuleInfo, mercury_private_builtin_module,
        "builtin_unify_solver_type", ArgVars, Context, Goal),
    quantify_clause_body(all_modes, ArgVars, Goal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

:- pred generate_unify_proc_body_enum(prog_context::in,
    prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_unify_proc_body_enum(Context, X, Y, Clause, !Info) :-
    make_simple_test(X, Y, umc_explicit, [], Goal),
    quantify_clause_body(all_modes, [X, Y], Goal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

:- type maybe_compare_constants_as_ints
    --->    do_not_compare_constants_as_ints
    ;       compare_constants_as_ints.

:- type maybe_allow_packed_unify_compare
    --->    do_not_allow_packed_unify_compare
    ;       allow_packed_unify_compare.

:- type uc_options
    --->    uc_options(
                uco_constants_as_ints       :: maybe_compare_constants_as_ints,
                uco_packed_unify_compare    :: maybe_allow_packed_unify_compare
            ).

    % Succeed iff the target back end guarantees that comparing two constants
    % for equality can be done by casting them both to integers and comparing
    % the integers for equality.
    %
:- func lookup_unify_compare_options(unify_proc_info) = uc_options.

lookup_unify_compare_options(Info) = UCOptions :-
    unify_proc_info_get_module_info(Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, can_compare_constants_as_ints,
        BoolCanCompareAsInt),
    (
        BoolCanCompareAsInt = no,
        CanCompareAsInt = do_not_compare_constants_as_ints
    ;
        BoolCanCompareAsInt = yes,
        CanCompareAsInt = compare_constants_as_ints
    ),
    globals.lookup_bool_option(Globals, allow_packed_unify_compare,
        BoolAllowPackedUC),
    globals.get_target(Globals, Target),
    ( if
        BoolAllowPackedUC = yes,
        % The foreign_procs we generate are all in C.
        Target = target_c
    then
        AllowPackedUC = allow_packed_unify_compare
    else
        AllowPackedUC = do_not_allow_packed_unify_compare
    ),
    UCOptions = uc_options(CanCompareAsInt, AllowPackedUC).

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
    list(constructor_repn)::in, prog_var::in, prog_var::in, list(clause)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_unify_proc_body_du(SpecDefnInfo, CtorRepns, X, Y, Clauses, !Info) :-
    UCOptions = lookup_unify_compare_options(!.Info),
    Context = SpecDefnInfo ^ spdi_context,
    ( if
        UCOptions ^ uco_constants_as_ints = compare_constants_as_ints,
        UCOptions ^ uco_packed_unify_compare = allow_packed_unify_compare,
        MayUnifyCtorAsWhole =
            ( pred(CtorRepn::in) is semidet :-
                CtorRepn = ctor_repn(_, _, _, ConsTag, CtorArgRepns, _, _),
                (
                    CtorArgRepns = []
                ;
                    CtorArgRepns = [_ | _],
                    ConsTag = local_args_tag(_)
                )
            ),
        list.all_true(MayUnifyCtorAsWhole, CtorRepns)
    then
        CastType = get_pretest_equality_cast_type(!.Info),
        unify_proc_info_new_var("CastX", CastType, CastX, !Info),
        unify_proc_info_new_var("CastY", CastType, CastY, !Info),
        generate_cast(unsafe_type_cast, X, CastX, Context, CastXGoal),
        generate_cast(unsafe_type_cast, Y, CastY, Context, CastYGoal),
        create_pure_atomic_complicated_unification(CastX, rhs_var(CastY),
            Context, umc_explicit, [], EqualityGoal),
        GoalExpr = conj(plain_conj, [CastXGoal, CastYGoal, EqualityGoal]),
        goal_info_init(Context, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo),
        % Casting X and Y to e.g. an integer when they contain arguments
        % is a kind of packed word operation.
        PackedOps = used_some_packed_word_ops
    else
        generate_du_unify_cases(SpecDefnInfo, UCOptions, X, Y, CtorRepns,
            Goal0, !Info),
        maybe_wrap_with_pretest_equality(Context, X, Y, no,
            Goal0, Goal, !Info),
        PackedOps = !.Info ^ upi_packed_ops
    ),

    % Did the clause we just generate use any bulk operations?
    (
        PackedOps = used_no_packed_word_ops,
        % No: mark the clause as suitable for all modes.
        quantify_clause_body(all_modes, [X, Y], Goal, Context, Clause, !Info),
        Clauses = [Clause]
    ;
        PackedOps = used_some_packed_word_ops,
        % Yes: mark the clause as suitable only for <in,in> modes, and ...
        quantify_clause_body(unify_in_in_modes, [X, Y], Goal, Context,
            InInClause, !Info),

        % ... generate another clause for non-<in,in> modes for which
        % the generation of bulk comparisons is disabled.
        NonPackedUCOptions = UCOptions ^ uco_packed_unify_compare :=
            do_not_allow_packed_unify_compare,
        !Info ^ upi_packed_ops := used_no_packed_word_ops,
        generate_du_unify_cases(SpecDefnInfo, NonPackedUCOptions, X, Y,
            CtorRepns, NonPackedGoal0, !Info),
        expect(unify(!.Info ^ upi_packed_ops, used_no_packed_word_ops), $pred,
            "packed word ops show up after being disabled"),
        maybe_wrap_with_pretest_equality(Context, X, Y, no,
            NonPackedGoal0, NonPackedGoal, !Info),
        quantify_clause_body(unify_non_in_in_modes, [X, Y], NonPackedGoal,
            Context, NonInInClause, !Info),

        % The order of the clauses does not matter; clause_to_proc.m
        % will always pick or the other, never both.
        Clauses = [InInClause, NonInInClause]
    ).

:- pred generate_du_unify_cases(spec_pred_defn_info::in, uc_options::in,
    prog_var::in, prog_var::in, list(constructor_repn)::in, hlds_goal::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_du_unify_cases(SpecDefnInfo, UCOptions, X, Y, CtorRepns,
        SwitchGoal, !Info) :-
    list.foldl3(generate_du_unify_case(SpecDefnInfo, UCOptions, X, Y),
        CtorRepns, [], IntEqConsIds, [], NonIntEqCases, !Info),
    list.sort(IntEqConsIds, SortedIntEqConsIds),
    Context = SpecDefnInfo ^ spdi_context,
    goal_info_init(Context, GoalInfo),
    (
        SortedIntEqConsIds = [],
        Cases = NonIntEqCases
    ;
        SortedIntEqConsIds = [HeadIntEqConsId | TailIntEqConsIds],
        unify_proc_info_new_var("CastX", int_type, CastX, !Info),
        unify_proc_info_new_var("CastY", int_type, CastY, !Info),
        generate_cast(unsafe_type_cast, X, CastX, Context, CastXGoal0),
        generate_cast(unsafe_type_cast, Y, CastY, Context, CastYGoal0),
        goal_add_feature(feature_keep_constant_binding,
            CastXGoal0, CastXGoal),
        goal_add_feature(feature_keep_constant_binding,
            CastYGoal0, CastYGoal),
        create_pure_atomic_complicated_unification(CastY, rhs_var(CastX),
            Context, umc_explicit, [], GoalUnifyCastXY),
        GoalList = [CastXGoal, CastYGoal, GoalUnifyCastXY],
        conj_list_to_goal(GoalList, GoalInfo, Goal),
        IntEqCase = case(HeadIntEqConsId, TailIntEqConsIds, Goal),
        Cases = [IntEqCase | NonIntEqCases]
    ),
    list.sort(Cases, SortedCases),
    SwitchGoal = hlds_goal(switch(X, cannot_fail, SortedCases), GoalInfo).

:- pred generate_du_unify_case(spec_pred_defn_info::in, uc_options::in,
    prog_var::in, prog_var::in, constructor_repn::in,
    list(cons_id)::in, list(cons_id)::out, list(case)::in, list(case)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_du_unify_case(SpecDefnInfo, UCOptions, X, Y, CtorRepn,
        !IntEqConsIds, !Cases, !Info) :-
    CtorRepn = ctor_repn(_Ordinal, MaybeExistConstraints, FunctorName,
        ConsTag, CtorArgRepns, FunctorArity, _Ctxt),
    TypeCtor = SpecDefnInfo ^ spdi_type_ctor,
    ( if TypeCtor = type_ctor(unqualified("{}"), _) then
        FunctorConsId = tuple_cons(FunctorArity)
    else
        FunctorConsId =
            du_data_ctor(du_ctor(FunctorName, FunctorArity, TypeCtor))
    ),
    Context = SpecDefnInfo ^ spdi_context,
    compute_exist_constraint_implications(MaybeExistConstraints, ExistQTVars,
        GiveVarsTypes),
    ( if
        (
            CtorArgRepns = [],
            UCOptions ^ uco_constants_as_ints = compare_constants_as_ints
        ;
            CtorArgRepns = [_ | _],
            ConsTag = local_args_tag(_),
            UCOptions ^ uco_packed_unify_compare = allow_packed_unify_compare,
            % There are arguments to compare, but they are stored
            % in the same word as the ptag and the sectag (if any).
            !Info ^ upi_packed_ops := used_some_packed_word_ops
        )
    then
        !:IntEqConsIds = [FunctorConsId | !.IntEqConsIds]
    else
        MaybePackableArgsLocn = compute_maybe_packable_args_locn(ConsTag),
        unify_proc_info_get_module_info(!.Info, ModuleInfo),
        UCParams = uc_params(ModuleInfo, Context, ExistQTVars,
            MaybePackableArgsLocn, GiveVarsTypes,
            UCOptions ^ uco_constants_as_ints,
            UCOptions ^ uco_packed_unify_compare),
        unify_proc_info_get_var_table(!.Info, VarTable0),
        lookup_var_type(VarTable0, X, TermType),
        FirstArgNum = 1,
        generate_arg_unify_goals(UCParams, TermType, X, Y,
            FirstArgNum, CtorArgRepns, UnifyArgsGoals, VarsX, VarsY, !Info),

        RHSX = rhs_functor(FunctorConsId, is_not_exist_constr, VarsX),
        RHSY = rhs_functor(FunctorConsId, is_not_exist_constr, VarsY),
        create_pure_atomic_complicated_unification(X, RHSX, Context,
            umc_explicit, [], GoalUnifyX),
        create_pure_atomic_complicated_unification(Y, RHSY, Context,
            umc_explicit, [], GoalUnifyY),
        GoalList = [GoalUnifyX, GoalUnifyY | UnifyArgsGoals],
        goal_info_init(Context, GoalInfo),
        conj_list_to_goal(GoalList, GoalInfo, Goal),
        Case = case(FunctorConsId, [], Goal),
        !:Cases = [Case | !.Cases]
    ).

:- pred generate_arg_unify_goals(uc_params::in,
    mer_type::in, prog_var::in, prog_var::in,
    int::in, list(constructor_arg_repn)::in, list(hlds_goal)::out,
    list(prog_var)::out, list(prog_var)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_arg_unify_goals(_, _, _, _, _, [], [], [], [], !Info).
generate_arg_unify_goals(UCParams, TermType, TermVarX, TermVarY,
        ArgNum, [CtorArgRepn | CtorArgRepns], Goals, VarsX, VarsY, !Info) :-
    may_we_start_packing_at_this_arg_unify(UCParams, CtorArgRepn, UnifyHow),
    GiveVarsTypes = UCParams ^ ucp_give_vars_types,
    (
        UnifyHow = unify_unpacked,
        Type = CtorArgRepn ^ car_type,
        ModuleInfo = UCParams ^ ucp_module_info,
        IsDummy = is_type_a_dummy(ModuleInfo, Type),
        (
            IsDummy = is_dummy_type,
            make_fresh_var_pair(GiveVarsTypes, "_ArgX", "_ArgY", ArgNum,
                Type, HeadVarX, HeadVarY, !Info),
            generate_arg_unify_goals(UCParams, TermType, TermVarX, TermVarY,
                ArgNum + 1, CtorArgRepns, Goals, TailVarsX, TailVarsY, !Info)
        ;
            IsDummy = is_not_dummy_type,
            make_fresh_var_pair(GiveVarsTypes, "ArgX", "ArgY", ArgNum,
                Type, HeadVarX, HeadVarY, !Info),
            % When unifying existentially typed arguments, the arguments
            % may have different types; in that case, rather than just
            % unifying them, which would be a type error, we call
            % `typed_unify', which is a builtin that first checks that
            % their types are equal and then unifies the values.
            Context = UCParams ^ ucp_context,
            ( if type_contains_existq_tvar(UCParams, Type) then
                build_simple_call(ModuleInfo, mercury_private_builtin_module,
                    "typed_unify", [HeadVarX, HeadVarY], Context, HeadGoal)
            else
                create_pure_atomic_complicated_unification(HeadVarX,
                    rhs_var(HeadVarY), Context, umc_explicit, [], HeadGoal)
            ),
            generate_arg_unify_goals(UCParams, TermType, TermVarX, TermVarY,
                ArgNum + 1, CtorArgRepns, TailGoals, TailVarsX, TailVarsY,
                !Info),
            Goals = [HeadGoal | TailGoals]
        ),
        VarsX = [HeadVarX | TailVarsX],
        VarsY = [HeadVarY | TailVarsY]
    ;
        UnifyHow = unify_packed(ArgsLocn, CellOffset),
        (
            ArgsLocn = args_local,
            % If ArgsLocn = args_local, then all the arguments fit into
            % one word, and we can compare X and Y by casting both to ints
            % and comparing the ints. And we should have done just that above,
            % which means that execution should never get here.
            unexpected($pred, "args_local")
        ;
            ArgsLocn = args_remote(Ptag)
        ),
        unify_proc_info_set_packed_ops(used_some_packed_word_ops, !Info),

        Type = CtorArgRepn ^ car_type,
        Context = UCParams ^ ucp_context,
        expect_not(type_contains_existq_tvar(UCParams, Type), $pred,
            "sub-word-size argument of existential type"),
        make_fresh_var_pair(GiveVarsTypes, "_ArgX", "_ArgY", ArgNum,
            Type, HeadVarX, HeadVarY, !Info),
        get_rest_of_word(UCParams, CellOffset,
            ArgNum, LeftOverArgNum, CtorArgRepns, LeftOverCtorArgRepns,
            RestOfWordVarsX, RestOfWordVarsY, !Info),
        ModuleInfo = UCParams ^ ucp_module_info,
        build_bulk_unify_foreign_proc(ModuleInfo, Ptag, TermType,
            TermVarX, TermVarY, ArgNum, CellOffset, Context, HeadGoals, !Info),

        generate_arg_unify_goals(UCParams, TermType, TermVarX, TermVarY,
            LeftOverArgNum, LeftOverCtorArgRepns, TailGoals,
            TailVarsX, TailVarsY, !Info),
        Goals = HeadGoals ++ TailGoals,
        VarsX = [HeadVarX | RestOfWordVarsX] ++ TailVarsX,
        VarsY = [HeadVarY | RestOfWordVarsY] ++ TailVarsY
    ).

:- pred build_bulk_unify_foreign_proc(module_info::in, ptag::in,
    mer_type::in, prog_var::in, prog_var::in,
    int::in, cell_offset::in, prog_context::in, list(hlds_goal)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

build_bulk_unify_foreign_proc(ModuleInfo, Ptag, TermType, TermVarX, TermVarY,
        ArgNum, CellOffset, Context, Goals, !Info) :-
    % Keep the predicates
    %   build_bulk_unify_foreign_proc
    %   select_and_build_signed_comparison_foreign_proc
    %   select_and_build_bulk_comparison_foreign_proc
    % in sync where relevant.
    TermVarArgX = foreign_arg(TermVarX,
        yes(foreign_arg_name_mode("TermVarX", in_mode)),
        TermType, bp_native_if_possible),
    TermVarArgY = foreign_arg(TermVarY,
        yes(foreign_arg_name_mode("TermVarY", in_mode)),
        TermType, bp_native_if_possible),

    ForeignCode = "
        MR_Unsigned *cell_x;
        MR_Unsigned *cell_y;
        MR_Unsigned word_x;
        MR_Unsigned word_y;

        cell_x = (MR_Unsigned *)
            (((MR_Unsigned) TermVarX) - (MR_Unsigned) Ptag);
        cell_y = (MR_Unsigned *)
            (((MR_Unsigned) TermVarY) - (MR_Unsigned) Ptag);
        word_x = cell_x[CellOffsetVar];
        word_y = cell_y[CellOffsetVar];

        SUCCESS_INDICATOR = (word_x == word_y);
    ",

    PredName = "unify_remote_arg_words",
    make_ptag_and_cell_offset_args(ArgNum, Ptag, CellOffset, Context,
        WordsArgs, WordsGoals, !Info),

    ForeignArgs = [TermVarArgX, TermVarArgY] ++ WordsArgs,
    generate_call_foreign_proc(ModuleInfo, pf_predicate,
        mercury_private_builtin_module, PredName,
        [], ForeignArgs, [], instmap_delta_bind_no_var, only_mode,
        detism_semi, purity_pure, [], pure_proc_foreign_attributes,
        no, ForeignCode, Context, UnifyRemoteArgWordGoal),
    Goals = WordsGoals ++ [UnifyRemoteArgWordGoal].

:- func pure_proc_foreign_attributes = foreign_proc_attributes.

pure_proc_foreign_attributes = !:Attrs :-
    !:Attrs = default_attributes(lang_c),
    set_may_call_mercury(proc_will_not_call_mercury, !Attrs),
    set_thread_safe(proc_thread_safe, !Attrs),
    set_purity(purity_pure, !Attrs),
    set_terminates(proc_terminates, !Attrs),
    set_may_throw_exception(proc_will_not_throw_exception, !Attrs),
    set_may_modify_trail(proc_will_not_modify_trail, !Attrs),
    set_may_call_mm_tabled(proc_will_not_call_mm_tabled, !Attrs),
    set_affects_liveness(proc_does_not_affect_liveness, !Attrs),
    set_allocates_memory(proc_does_not_allocate_memory, !Attrs),
    set_registers_roots(proc_does_not_register_roots, !Attrs).

:- pred get_rest_of_word(uc_params::in, cell_offset::in, int::in, int::out,
    list(constructor_arg_repn)::in, list(constructor_arg_repn)::out,
    list(prog_var)::out, list(prog_var)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

get_rest_of_word(_UCParams, _CellOffset, !ArgNum, [], [], [], [], !Info).
get_rest_of_word(UCParams, CellOffset, !ArgNum, [CtorArgRepn | CtorArgRepns],
        LeftOverCtorArgRepns, VarsX, VarsY, !Info) :-
    ArgPosWidth = CtorArgRepn ^ car_pos_width,
    (
        ( ArgPosWidth = apw_partial_shifted(_, ArgCellOffset, _, _, _, _)
        ; ArgPosWidth = apw_none_shifted(_, ArgCellOffset)
        ),
        Type = CtorArgRepn ^ car_type,
        expect_not(type_contains_existq_tvar(UCParams, Type), $pred,
            "sub-word-size argument of existential type"),
        expect(unify(CellOffset, ArgCellOffset), $pred,
            "apw_{partial,none}_shifted offset != CellOffset"),
        GiveVarsTypes = UCParams ^ ucp_give_vars_types,
        make_fresh_var_pair(GiveVarsTypes, "_ArgX", "_ArgY", !.ArgNum,
            Type, HeadVarX, HeadVarY, !Info),
        !:ArgNum = !.ArgNum + 1,
        get_rest_of_word(UCParams, CellOffset, !ArgNum,
            CtorArgRepns, LeftOverCtorArgRepns,
            TailVarsX, TailVarsY, !Info),
        VarsX = [HeadVarX | TailVarsX],
        VarsY = [HeadVarY | TailVarsY]
    ;
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _, _)
        ),
        LeftOverCtorArgRepns = [CtorArgRepn | CtorArgRepns],
        VarsX = [],
        VarsY = []
    ).

:- pred type_contains_existq_tvar(uc_params::in, mer_type::in) is semidet.

type_contains_existq_tvar(UCParams, Type) :-
    ExistQTVars = UCParams ^ ucp_existq_tvars,
    some [ExistQTVar] (
        list.member(ExistQTVar, ExistQTVars),
        type_contains_var(Type, ExistQTVar)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Generate clauses for compare predicates.
%

:- pred generate_compare_proc_body(spec_pred_defn_info::in,
    prog_var::in, prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body(SpecDefnInfo, Res, X, Y, Clause, !Info) :-
    unify_proc_info_get_module_info(!.Info, ModuleInfo),
    TypeBody = SpecDefnInfo ^ spdi_type_body,
    Context = SpecDefnInfo ^ spdi_context,
    ( if
        TypeBody = hlds_du_type(TypeBodyDu),
        TypeBodyDu = type_body_du(_, subtype_of(SuperType), _, _, _)
    then
        % Compare subtype terms after casting to base type.
        TVarSet = SpecDefnInfo ^ spdi_tvarset,
        get_du_base_type(ModuleInfo, TVarSet, SuperType, BaseType),
        generate_compare_proc_body_eqv(Context, BaseType, Res, X, Y,
            Clause, !Info)
    else if
        % We used to special-case the type_ctors for which
        % is_type_ctor_a_builtin_dummy(TypeCtor) = is_builtin_dummy_type_ctor,
        % but both those types now have user-defined unify and compare preds.
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
                TypeCtor = SpecDefnInfo ^ spdi_type_ctor,
                CtorCat = classify_type_ctor(ModuleInfo, TypeCtor),
                generate_compare_proc_body_builtin(CtorCat, Context,
                    Res, X, Y, Clause, !Info)
            else
                unexpected($pred,
                    "trying to create compare proc for abstract type")
            )
        ;
            TypeBody = hlds_eqv_type(EqvType),
            ( if type_is_higher_order(EqvType) then
                generate_compare_proc_body_builtin(ctor_cat_higher_order,
                    Context, Res, X, Y, Clause, !Info)
            else
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
            TypeBody = hlds_du_type(TypeBodyDu),
            TypeBodyDu = type_body_du(_, MaybeSuperType, _, MaybeRepn, _),
            expect(unify(MaybeSuperType, not_a_subtype), $pred,
                "MaybeSuperType != not_a_subtype"),
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
                    CtorRepns = Repn ^ dur_ctor_repns,
                    generate_compare_proc_body_du(SpecDefnInfo,
                        CtorRepns, Res, X, Y, Clause, !Info)
                )
            ;
                DuTypeKind = du_type_kind_general,
                CtorRepns = Repn ^ dur_ctor_repns,
                generate_compare_proc_body_du(SpecDefnInfo,
                    CtorRepns, Res, X, Y, Clause, !Info)
            )
        )
    ).

%---------------------------------------------------------------------------%

:- pred generate_compare_proc_body_dummy(prog_context::in,
    prog_var::in, prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body_dummy(Context, Res, X, Y, Clause, !Info) :-
    generate_return_equal(Res, Context, Goal),
    quantify_clause_body(all_modes, [Res, X, Y], Goal, Context, Clause, !Info).

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
        NonCanonical = noncanon_subtype,
        unexpected($pred, "trying to create compare proc for subtype")
    ;
        NonCanonical = noncanon_uni_only(_),
        % Just generate code that will call error/1.
        unify_proc_info_get_module_info(!.Info, ModuleInfo),
        ArgVars = [Res, X, Y],
        build_simple_call(ModuleInfo, mercury_private_builtin_module,
            "builtin_compare_non_canonical_type", ArgVars, Context, Goal)
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
        % XXX If the user-specified compare predicate always aborts,
        % we should avoid a pretest, since if it accidentally happens
        % to succeed, it avoids the requested abort.
        maybe_wrap_with_pretest_equality(Context, X, Y, yes(Res),
            Goal0, Goal, !Info)
    ),
    quantify_clause_body(all_modes, ArgVars, Goal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

:- pred generate_compare_proc_body_builtin(type_ctor_category::in,
    prog_context::in, prog_var::in, prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body_builtin(CtorCat, Context, Res, X, Y, Clause,
        !Info) :-
    unify_proc_info_get_module_info(!.Info, ModuleInfo),
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
    build_simple_call(ModuleInfo, mercury_private_builtin_module,
        Name, ArgVars, Context, CompareGoal),
    quantify_clause_body(all_modes, ArgVars, CompareGoal, Context, Clause,
        !Info).

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
    unify_proc_info_get_module_info(!.Info, ModuleInfo),
    make_fresh_named_var_from_type(EqvType, "Cast_HeadVar", 1, CastX, !Info),
    make_fresh_named_var_from_type(EqvType, "Cast_HeadVar", 2, CastY, !Info),
    generate_cast(equiv_type_cast, X, CastX, Context, CastXGoal),
    generate_cast(equiv_type_cast, Y, CastY, Context, CastYGoal),
    build_simple_call(ModuleInfo, mercury_public_builtin_module,
        "compare", [Res, CastX, CastY], Context, CompareGoal),

    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    conj_list_to_goal([CastXGoal, CastYGoal, CompareGoal], GoalInfo, Goal),
    quantify_clause_body(all_modes, [Res, X, Y], Goal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

:- pred generate_compare_proc_body_solver(prog_context::in,
    prog_var::in, prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body_solver(Context, Res, X, Y, Clause, !Info) :-
    unify_proc_info_get_module_info(!.Info, ModuleInfo),
    ArgVars = [Res, X, Y],
    build_simple_call(ModuleInfo, mercury_private_builtin_module,
        "builtin_compare_solver_type", ArgVars, Context, Goal),
    quantify_clause_body(all_modes, ArgVars, Goal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

:- pred generate_compare_proc_body_enum(prog_context::in,
    prog_var::in, prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body_enum(Context, Res, X, Y, Clause, !Info) :-
    unify_proc_info_get_module_info(!.Info, ModuleInfo),
    IntType = int_type,
    make_fresh_named_var_from_type(IntType, "Cast_HeadVar", 1, CastX, !Info),
    make_fresh_named_var_from_type(IntType, "Cast_HeadVar", 2, CastY, !Info),
    generate_cast(unsafe_type_cast, X, CastX, Context, CastXGoal),
    generate_cast(unsafe_type_cast, Y, CastY, Context, CastYGoal),
    build_simple_call(ModuleInfo, mercury_private_builtin_module,
        "builtin_compare_int", [Res, CastX, CastY], Context, CompareGoal),
    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    conj_list_to_goal([CastXGoal, CastYGoal, CompareGoal], GoalInfo, Goal),
    quantify_clause_body(all_modes, [Res, X, Y], Goal, Context, Clause, !Info).

%---------------------------------------------------------------------------%

:- pred generate_compare_proc_body_du(spec_pred_defn_info::in,
    list(constructor_repn)::in, prog_var::in, prog_var::in, prog_var::in,
    clause::out, unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body_du(SpecDefnInfo, CtorRepns, Res, X, Y, Clause,
        !Info) :-
    unify_proc_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    expect_not(unify(CtorRepns, []), $pred,
        "compare for type with no functors"),
    UCOptions = lookup_unify_compare_options(!.Info),
    Context = SpecDefnInfo ^ spdi_context,
    ( if
        UCOptions ^ uco_constants_as_ints = compare_constants_as_ints,
        UCOptions ^ uco_packed_unify_compare = allow_packed_unify_compare,
        % Can we compare two values of this type by casting both values
        % to unsigned and comparing the results?
        (
            CtorRepns = [CtorRepnA],
            % If all the arguments of functor A are stored next to the ptag,
            % and if they are all comparable as unsigned, two conditions
            % that is_ctor_with_all_locally_packed_unsigned_args will test,
            % and if they are arranged earlier-args-in-more-significant-bits,
            % which is always guaranteed for packed arguments by
            % du_type_layout.m, then yes, cast-to-unsigned-and-compare
            % will work.
            is_ctor_with_all_locally_packed_unsigned_args(CtorRepnA, _)
        ;
            CtorRepns = [CtorRepnA, CtorRepnB],
            % If functor A comes before functor B, and
            %
            % - the value of a term whose functor is A *must* be all zeroes
            % - the value of a term whose functor is B *cannot* be all zeroes
            %
            % then comparing two terms, one bound to A and one bound to B,
            % by casting both to unsigned and comparing the results, will yield
            % the correct result.
            %
            % If A has arity zero, then there is only one possible term whose
            % functor is A, so cast-to-unsigned-and-compare works when
            % both terms are bound to A. And if B's arguments meet the
            % conditions laid out in the first switch arm above, then
            % cast-to-unsigned-and-compare also works when both terms
            % are bound to B.
            CtorRepnA = ctor_repn(_OrdinalA, _MaybeExistConstraintsA,
                _FunctorNameA, ConsTagA, _CtorArgRepnsA, ArityA, _CtxtA),
            ArityA = 0,
            ConsTagA = local_args_tag(LocalArgsTagInfoA),
            LocalArgsTagInfoA =
                local_args_not_only_functor(PtagA, LocalSecTagA),
            PtagA = ptag(0u8),
            LocalSecTagA = local_sectag(0u, _, _),

            is_ctor_with_all_locally_packed_unsigned_args(CtorRepnB,
                PtagBUint8),
            PtagBUint8 > 0u8
        )
        % If the type has three or more functors, then cast-to-unsigned-and-
        % compare will not work.
        %
        % If the third functor is a constant, then one of the constants
        % will have a nonzero local sectag value. If the arguments of the
        % non-constant functor happen to be all zeros, then this nonzero
        % local sectag can cause the unsigned comparison to report that
        % the constant is greater then the non-constant, even if the constant
        % comes earlier in the list of functors. And if this constant comes
        % later in the list of functors, then the unsigned comparison will
        % yield the wrong result if the value of some argument in the
        % nonconstant causes a bit to be set that is more significant
        % than the bit(s) set in the local sectag. So regardless of the
        % relative order of these two functors, correctness would not be
        % guaranteed.
        %
        % If the third functor is a non-constant, then comparisons between
        % terms bound to different non-constants should be decided by the
        % local sectags of the two terms regardless of the values of their
        % arguments, but the local sectag is stored in *less* significant bits
        % than the arguments. We cannot move the position of the local sectag
        % without incurring significant costs in RTTI complexity.
    then
        CastType = uint_type,
        unify_proc_info_new_var("CastX", CastType, CastX, !Info),
        unify_proc_info_new_var("CastY", CastType, CastY, !Info),
        generate_cast(unsafe_type_cast, X, CastX, Context, CastXGoal),
        generate_cast(unsafe_type_cast, Y, CastY, Context, CastYGoal),
        build_simple_call(ModuleInfo, mercury_public_builtin_module, "compare",
            [Res, CastX, CastY], Context, CompareGoal),
        GoalExpr = conj(plain_conj, [CastXGoal, CastYGoal, CompareGoal]),
        goal_info_init(Context, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    else
        globals.lookup_int_option(Globals, compare_specialization,
            CompareSpec),
        list.length(CtorRepns, NumCtors),
        ( if NumCtors =< CompareSpec then
            generate_compare_proc_body_du_quad(SpecDefnInfo, UCOptions,
                CtorRepns, Res, X, Y, Goal0, !Info)
        else
            generate_compare_proc_body_du_linear(SpecDefnInfo, UCOptions,
                CtorRepns, Res, X, Y, Goal0, !Info)
        ),
        maybe_wrap_with_pretest_equality(Context, X, Y, yes(Res), Goal0, Goal,
            !Info)
    ),
    HeadVars = [Res, X, Y],
    quantify_clause_body(all_modes, HeadVars, Goal, Context, Clause, !Info).

:- pred is_ctor_with_all_locally_packed_unsigned_args(constructor_repn::in,
    uint8::out) is semidet.

is_ctor_with_all_locally_packed_unsigned_args(CtorRepn, PtagUint8) :-
    CtorRepn = ctor_repn(_Ordinal, _MaybeExistConstraints,
        _FunctorName, ConsTag, CtorArgRepns, Arity, _Ctxt),
    Arity > 0,
    ConsTag = local_args_tag(LocalArgsTagInfo),
    (
        LocalArgsTagInfo = local_args_only_functor,
        PtagUint8 = 0u8
    ;
        LocalArgsTagInfo = local_args_not_only_functor(Ptag, LocalSecTag),
        Ptag = ptag(PtagUint8),
        LocalSecTag = local_sectag(0u, _, _)
    ),
    IsArgUnsignedComparable =
        ( pred(CtorArgRepn::in) is semidet :-
            ArgPosWidth = CtorArgRepn ^ car_pos_width,
            (
                ( ArgPosWidth = apw_partial_first(_, _, _, _, _, Fill)
                ; ArgPosWidth = apw_partial_shifted(_, _, _, _, _, Fill)
                ),
                fill_bulk_comparability(Fill) = bulk_comparable_unsigned
            ;
                ArgPosWidth = apw_none_shifted(_, _)
            )
        ),
    list.all_true(IsArgUnsignedComparable, CtorArgRepns).

%---------------------%

    % generate_compare_proc_body_du_quad: for a du type, such as
    %
    %   :- type foo
    %       --->    f(a)
    %       ;       g(a, b, c)
    %       ;       h.
    %
    % the quadratic code we *used* to generate was
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
    %           X = h,
    %           Y = f(_),
    %           R = (>)
    %       ;
    %           X = h,
    %           Y = g(_, _, _),
    %           R = (>)
    %       ;
    %           X = h,
    %           Y = h,
    %           R = (=)
    %       ).
    %
    % Note that in the clauses handling two copies of the same constant,
    % we unify Y with the constant, not with X. This is required to get
    % switch_detection and det_analysis to recognize the determinism of the
    % predicate.
    %
    % However, we now generate switches like this directly (show using
    % pseudo-code, since Mercury's user syntax has no way to express
    % switches *directly*):
    %
    %   compare(Res, X, Y) :-
    %       % switch on X
    %       (
    %           % X has function symbol f/1
    %           % switch on Y
    %           (
    %               % Y has function symbol f/1
    %               X = f(X1),
    %               Y = f(Y1),
    %               compare(R, X1, Y1)
    %           ;
    %               % Y has function symbol g/3 or h/0
    %               R = (<)
    %           )
    %       ;
    %           % X has function symbol g/3
    %           % switch on Y
    %           (
    %               % Y has function symbol f/1
    %               R = (>)
    %           ;
    %               % Y has function symbol g/3
    %               X = g(X1, X2, X3),
    %               Y = g(Y1, Y2, Y3),
    %               ( if compare(R1, X1, Y1), R1 \= (=) then
    %                   R = R1
    %               else if compare(R2, X2, Y2), R2 \= (=) then
    %                   R = R2
    %               else
    %                   compare(R, X3, Y3)
    %               )
    %           ;
    %               % Y has function symbol h/0
    %               R = (<)
    %           )
    %       ;
    %           % X has function symbol h/0
    %           % switch on Y
    %           (
    %               % Y has function symbol f/1 or g/3
    %               R = (>)
    %           ;
    %               % Y has function symbol h/0
    %               R = (=)
    %           )
    %       ).
    %
    % The general structure is an outer switch on X, with one arm for each
    % symbol in the type. Each arm of this switch is an inner switch on Y.
    % This switch will have two or three arms. It will have
    %
    % - one arm for the case where X and Y are bound to the same function
    %   symbol, where R = (=) is a possible outcome, provided that the
    %   arguments of the function symbol in X and Y are equal
    %
    % - one arm for the case where Y is bound to one of the function symbols
    %   that precede X's function symbol in the type's order, if there are
    %   any such symbols, which always returns R = (>), and
    %
    % - one arm for the case where Y is bound to one of the function symbols
    %   that follow X's function symbol in the type's order, if there are
    %   any such symbols, which always returns R = (<).
    %
    % This is possible because the typechecker no longer throws an exception
    % when given a switch goal as input. (It used to do so, because it *always*
    % runs before the switch detection pass.) However, it "handles" switches
    % by *ignoring* the unifications implicit in the switch arms. This is OK
    % *only* because the only switches it sees are the ones we generate here,
    % and these have the properties that
    %
    % - the variable being switched on is always X or Y;
    %
    % - since X and Y are arguments with declared types (which will be
    %   the same type), the types of X and Y are already fully known
    %   before the typechecker arrives arrives at the switch; and
    %
    % - we derive the cons_ids in the switch case arms from the definition
    %   of that type, meaning that type errors are impossible by construction.
    %
:- pred generate_compare_proc_body_du_quad(spec_pred_defn_info::in,
    uc_options::in, list(constructor_repn)::in,
    prog_var::in, prog_var::in, prog_var::in,
    hlds_goal::out, unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body_du_quad(SpecDefnInfo, UCOptions, CtorRepns,
        R, X, Y, Goal, !Info) :-
    % XXX Consider returning switches, not disjunctions, both here
    % and everywhere else.
    % XXX Done here, not yet everywhere else.
    (
        CtorRepns = [],
        % The definition of a type constructor must contain at least one
        % data constructor.
        unexpected($pred, "CtorRepns = []")
    ;
        CtorRepns = [CtorRepn],
        % If the type has only one data constructor, then the data constructors
        % of X and Y cannot be different.
        generate_compare_goal(SpecDefnInfo, UCOptions,
            cons_ids_known_to_match, CtorRepn, R, X, Y, _ConsId, Goal, !Info)
    ;
        CtorRepns = [_, _ | _],
        Context = SpecDefnInfo ^ spdi_context,
        make_const_construction(Context, R, compare_cons_id("<"), ReturnLt),
        make_const_construction(Context, R, compare_cons_id("="), ReturnEq),
        make_const_construction(Context, R, compare_cons_id(">"), ReturnGt),
        TypeCtor = SpecDefnInfo ^ spdi_type_ctor,
        ConsIds = list.map(ctor_repn_to_cons_id(TypeCtor), CtorRepns),
        ConsIdSet = set_tree234.list_to_set(ConsIds),
        generate_compare_du_quad_outer_switch_arms(SpecDefnInfo, UCOptions,
            CtorRepns, R, X, Y, ReturnLt, ReturnEq, ReturnGt,
            set_tree234.init, ConsIdSet, [], Cases, !Info),
        % Reversing Cases before putting them into GoalExpr can help
        % make the initial HLDS we generate be a bit more readable,
        % but for any purpose other than that, the order should not matter.
        % (Technically, the order can matter for performance if we implement
        % the switch using an if-then-else chain, but
        %
        % - that should happen only if disable smart indexing, which we
        %   just about never do, and
        %
        % - even then, we don't how frequently each cons_id in the type
        %   occurs at the program point just before the switch, so we
        %   don't know what order of the if-then-else chain would be best.
        GoalExpr = switch(X, cannot_fail, Cases),
        goal_info_init(Context, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

    % generate_compare_du_quad_outer_switch_arms(SpecDefnInfo, UCOptions,
    %   CtorRepns, R, X, Y, !.ConsIdsLt, !.ConsIdsEqGt, !Cases, !Info):
    %
    % Generate one arm of the outer switch (on X), which is itself a complete
    % inner switch (on Y) for each constructor in CtorRepns. X and Y are
    % the terms being compared, and R is the result. !.ConsIdsLt are all
    % the cons_ids that should compare as less-than the first constructor
    % in CtorRepns, while !.ConsIdsEqGt are all the cons_ids that should
    % compare as greater then or equal to that same constructor.
    %
    % Invariant: the union of !.ConsIdsLt and !.ConsIdsEqGt will always equal
    % the set of all cons_ids in the type constructor's definition.
    %
:- pred generate_compare_du_quad_outer_switch_arms(spec_pred_defn_info::in,
    uc_options::in, list(constructor_repn)::in,
    prog_var::in, prog_var::in, prog_var::in,
    hlds_goal::in, hlds_goal::in, hlds_goal::in,
    set_tree234(cons_id)::in, set_tree234(cons_id)::in,
    list(case)::in, list(case)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_du_quad_outer_switch_arms(_SpecDefnInfo, _UCOptions,
        [], _R, _X, _Y, _ReturnLt, _ReturnEq, _ReturnGt,
        _ConsIdsLt, _ConsIdsEqGt, !Cases, !Info).
generate_compare_du_quad_outer_switch_arms(SpecDefnInfo, UCOptions,
        [CtorRepn | CtorRepns], R, X, Y, ReturnLt, ReturnEq, ReturnGt,
        !.ConsIdsLt, !.ConsIdsEqGt, !Cases, !Info) :-
    TypeCtor = SpecDefnInfo ^ spdi_type_ctor,
    CtorRepnConsId = ctor_repn_to_cons_id(TypeCtor, CtorRepn),

    CtorArity = CtorRepn ^ cr_num_args,
    ( if CtorArity = 0 then
        EqGoal = ReturnEq
    else
        generate_compare_goal(SpecDefnInfo, UCOptions,
            cons_ids_not_known_to_match, CtorRepn, R, X, Y,
            _ConsId, EqGoal, !Info)
    ),
    EqCase = case(CtorRepnConsId, [], EqGoal),

    LtConsIds = set_tree234.to_sorted_list(!.ConsIdsLt),
    set_tree234.det_remove(CtorRepnConsId, !.ConsIdsEqGt, ConsIdsGt),
    GtConsIds = set_tree234.to_sorted_list(ConsIdsGt),

    (
        LtConsIds = [],
        LtCases = []
    ;
        LtConsIds = [HeadLtConsId | TailLtConsIds],
        % CtorRepnConsId is greater than all the LtConsIds.
        LtCase = case(HeadLtConsId, TailLtConsIds, ReturnGt),
        LtCases = [LtCase]
    ),

    (
        GtConsIds = [],
        GtCases = []
    ;
        GtConsIds = [HeadGtConsId | TailGtConsIds],
        % CtorRepnConsId is less than all the GtConsIds.
        GtCase = case(HeadGtConsId, TailGtConsIds, ReturnLt),
        GtCases = [GtCase]
    ),

    InnerSwitchCases = LtCases ++ [EqCase | GtCases],
    InnerSwitchGoalExpr = switch(Y, cannot_fail, InnerSwitchCases),
    Context = SpecDefnInfo ^ spdi_context,
    goal_info_init(Context, InnerSwitchGoalInfo),
    InnerSwitchGoal = hlds_goal(InnerSwitchGoalExpr, InnerSwitchGoalInfo),
    Case = case(CtorRepnConsId, [], InnerSwitchGoal),
    !:Cases = [Case | !.Cases],

    set_tree234.insert(CtorRepnConsId, !ConsIdsLt),
    !:ConsIdsEqGt = ConsIdsGt,
    generate_compare_du_quad_outer_switch_arms(SpecDefnInfo, UCOptions,
        CtorRepns, R, X, Y, ReturnLt, ReturnEq, ReturnGt,
        !.ConsIdsLt, !.ConsIdsEqGt, !Cases, !Info).

:- func ctor_repn_to_cons_id(type_ctor, constructor_repn) = cons_id.

ctor_repn_to_cons_id(TypeCtor, CtorRepn) = ConsId :-
    CtorRepn = ctor_repn(_Ordinal, _MaybeExistConstraints, FunctorName,
        _ConsTag, _ArgRepns, FunctorArity, _Ctxt),
    ConsId = du_data_ctor(du_ctor(FunctorName, FunctorArity, TypeCtor)).

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
    %       __Index__(X, IndexX),                   % GoalIndexX
    %       __Index__(Y, IndexY),                   % GoalIndexY
    %       ( if IndexX < IndexY then               % GoalCallLessThan
    %           Res = (<)                           % GoalReturnLessThan
    %       else if IndexX > IndexY then            % GoalCallGreaterThan
    %           Res = (>)                           % GoalReturnGreaterThan
    %       else if
    %           % The disjuncts of this disjunction are generated by
    %           % the predicate generate_compare_du_linear_cases below.
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
    %           Res = R         % ReturnResultGoal
    %       else
    %           compare_error   % AbortGoal
    %       ).
    %
    % Note that disjuncts covering constants do not test Y,
    % since for constants, IndexX = IndexY implies X = Y.
    %
:- pred generate_compare_proc_body_du_linear(spec_pred_defn_info::in,
    uc_options::in, list(constructor_repn)::in,
    prog_var::in, prog_var::in, prog_var::in,
    hlds_goal::out, unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_proc_body_du_linear(SpecDefnInfo, UCOptions, CtorRepns,
        Res, X, Y, Goal, !Info) :-
    IntType = int_type,
    unify_proc_info_new_var("IndexX", IntType, IndexX, !Info),
    unify_proc_info_new_var("IndexY", IntType, IndexY, !Info),
    unify_proc_info_new_var("CompareResult", comparison_result_type, R, !Info),

    goal_info_init(Context, GoalInfo),

    SpecDefnInfo = spec_pred_defn_info(_SpecialPredId, _ThisPredId,
        TVarSet, Type, TypeCtor, TypeBody, TypeStatus0, Context),
    unify_proc_info_get_module_info(!.Info, ModuleInfo0),
    add_special_pred_decl_defn(spec_pred_index, TVarSet, Type, TypeCtor,
        TypeBody, TypeStatus0, Context, ModuleInfo0, ModuleInfo),
    unify_proc_info_set_module_info(ModuleInfo, !Info),

    X_InstmapDelta = instmap_delta_bind_var(IndexX),
    build_spec_pred_call(TypeCtor, spec_pred_index, [X, IndexX],
        X_InstmapDelta, detism_det, Context, GoalIndexX, !Info),
    Y_InstmapDelta = instmap_delta_bind_var(IndexY),
    build_spec_pred_call(TypeCtor, spec_pred_index, [Y, IndexY],
        Y_InstmapDelta, detism_det, Context, GoalIndexY, !Info),

    build_simple_call(ModuleInfo, mercury_private_builtin_module,
        "builtin_int_lt", [IndexX, IndexY], Context,
        GoalCallLessThan),
    build_simple_call(ModuleInfo, mercury_private_builtin_module,
        "builtin_int_gt", [IndexX, IndexY], Context, GoalCallGreaterThan),

    make_const_construction(Context, Res,
        compare_cons_id("<"), GoalReturnLessThan),
    make_const_construction(Context, Res,
        compare_cons_id(">"), GoalReturnGreaterThan),

    create_pure_atomic_complicated_unification(Res, rhs_var(R), Context,
        umc_explicit, [], ReturnResultGoal),

    generate_compare_du_linear_cases(SpecDefnInfo, UCOptions, CtorRepns,
        R, X, Y, [], EqConsIds, [], NonEqCases, !Info),
    list.sort(EqConsIds, SortedEqConsIds),
    (
        SortedEqConsIds = [],
        Cases = NonEqCases
    ;
        SortedEqConsIds = [HeadEqConsId | TailEqConsIds],
        generate_return_equal(R, Context, EqGoal),
        EqCase = case(HeadEqConsId, TailEqConsIds, EqGoal),
        Cases = [EqCase | NonEqCases]
    ),
    list.sort(Cases, SortedCases),
    CasesGoal = hlds_goal(switch(X, cannot_fail, SortedCases), GoalInfo),

    build_simple_call(ModuleInfo, mercury_private_builtin_module,
        "compare_error", [], Context, AbortGoal),

    HandleEqualGoal =
        hlds_goal(
            if_then_else([], CasesGoal, ReturnResultGoal, AbortGoal),
            GoalInfo),
    HandleGreaterEqualGoal =
        hlds_goal(
            if_then_else([], GoalCallGreaterThan, GoalReturnGreaterThan,
                HandleEqualGoal),
            GoalInfo),
    HandleLessGreaterEqualGoal =
        hlds_goal(
            if_then_else([], GoalCallLessThan, GoalReturnLessThan,
                HandleGreaterEqualGoal),
            GoalInfo),
    Goal =
        hlds_goal(
            conj(plain_conj,
                [GoalIndexX, GoalIndexY, HandleLessGreaterEqualGoal]),
            GoalInfo).

    % generate_compare_du_linear_cases does a part of the job assigned to
    % generate_compare_proc_body_du_linear. Specifically, for a type such as
    %
    %   :- type foo
    %       --->    f
    %       ;       g(a)
    %       ;       h(b, foo).
    %
    % we generate
    %
    %   (
    %       X = f,      % GoalUnifyX
    %       R = (=)     % CompareArgsGoal
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
:- pred generate_compare_du_linear_cases(spec_pred_defn_info::in,
    uc_options::in, list(constructor_repn)::in,
    prog_var::in, prog_var::in, prog_var::in,
    list(cons_id)::in, list(cons_id)::out, list(case)::in, list(case)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_du_linear_cases(_SpecDefnInfo, _UCOptions,
        [], _R, _X, _Y, !EqConsIds, !Cases, !Info).
generate_compare_du_linear_cases(SpecDefnInfo, UCOptions,
        [CtorRepn | CtorRepns], R, X, Y, !EqConsIds, !Cases, !Info) :-
    CtorRepn = ctor_repn(_Ordinal, _MaybeExistConstraints, FunctorName,
        _ConsTag, ArgRepns, FunctorArity, _Ctxt),
    (
        ArgRepns = [],
        TypeCtor = SpecDefnInfo ^ spdi_type_ctor,
        ConsId = du_data_ctor(du_ctor(FunctorName, FunctorArity, TypeCtor)),
        !:EqConsIds = [ConsId | !.EqConsIds]
    ;
        ArgRepns = [_ | _],
        generate_compare_goal(SpecDefnInfo, UCOptions, cons_ids_known_to_match,
            CtorRepn, R, X, Y, ConsId, Goal, !Info),
        Case = case(ConsId, [], Goal),
        !:Cases = [Case | !.Cases]
    ),
    generate_compare_du_linear_cases(SpecDefnInfo, UCOptions,
        CtorRepns, R, X, Y, !EqConsIds, !Cases, !Info).

%---------------------%

:- type cons_ids_match
    --->    cons_ids_not_known_to_match
    ;       cons_ids_known_to_match.

:- pred generate_compare_goal(spec_pred_defn_info::in,
    uc_options::in, cons_ids_match::in, constructor_repn::in,
    prog_var::in, prog_var::in, prog_var::in, cons_id::out, hlds_goal::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_compare_goal(SpecDefnInfo, UCOptions, ConsIdsMatch, CtorRepn,
        R, X, Y, FunctorConsId, Goal, !Info) :-
    CtorRepn = ctor_repn(_Ordinal, MaybeExistConstraints, FunctorName,
        ConsTag, ArgRepns, FunctorArity, _Ctxt),
    TypeCtor = SpecDefnInfo ^ spdi_type_ctor,
    FunctorConsId = du_data_ctor(du_ctor(FunctorName, FunctorArity, TypeCtor)),
    Context = SpecDefnInfo ^ spdi_context,
    (
        ArgRepns = [],
        RHS = rhs_functor(FunctorConsId, is_not_exist_constr, []),
        create_pure_atomic_complicated_unification(X, RHS, Context,
            umc_explicit, [], GoalUnifyX),
        generate_return_equal(R, Context, EqualGoal),
        (
            ConsIdsMatch = cons_ids_known_to_match,
            % We get called with cons_ids_known_to_match in two cases.
            %
            % - If we are generating the body of the compare predicate
            %   using the linear method. With that method, we compare
            %   the two input terms only if calls to the type's index predicate
            %   say that they have the same cons_id. This test is done
            %   at runtime.
            %
            % - If we are generating the body of the compare predicate
            %   using the quadratic method, but the type consists of
            %   only one data constructor. In that case, *all* values
            %   of the type have that data constructor. This test is done
            %   at compile time.
            GoalList = [GoalUnifyX, EqualGoal]
        ;
            ConsIdsMatch = cons_ids_not_known_to_match,
            create_pure_atomic_complicated_unification(Y, RHS, Context,
                umc_explicit, [], GoalUnifyY),
            GoalList = [GoalUnifyX, GoalUnifyY, EqualGoal]
        )
    ;
        ArgRepns = [_ | _],
        compute_exist_constraint_implications(MaybeExistConstraints,
            ExistQTVars, GiveVarsTypes),
        MaybePackableArgsLocn = compute_maybe_packable_args_locn(ConsTag),
        unify_proc_info_get_module_info(!.Info, ModuleInfo),
        UCParams = uc_params(ModuleInfo, Context, ExistQTVars,
            MaybePackableArgsLocn, GiveVarsTypes,
            UCOptions ^ uco_constants_as_ints,
            UCOptions ^ uco_packed_unify_compare),
        unify_proc_info_get_var_table(!.Info, VarTable0),
        lookup_var_type(VarTable0, X, TermType),
        generate_arg_compare_goals(UCParams, TermType, X, Y, R,
            all_args_in_word_so_far, 1, ArgRepns, CompareArgsGoal,
            VarsX, VarsY, !Info),
        RHSX = rhs_functor(FunctorConsId, is_not_exist_constr, VarsX),
        RHSY = rhs_functor(FunctorConsId, is_not_exist_constr, VarsY),
        create_pure_atomic_complicated_unification(X, RHSX, Context,
            umc_explicit, [], GoalUnifyX),
        create_pure_atomic_complicated_unification(Y, RHSY, Context,
            umc_explicit, [], GoalUnifyY),
        GoalList = [GoalUnifyX, GoalUnifyY, CompareArgsGoal]
    ),
    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    conj_list_to_goal(GoalList, GoalInfo, Goal).

%---------------------%

    % generate_arg_compare_goals: for a constructor such as
    %
    %   h(list(int), foo, string)
    %
    % none of whose arguments are special in any way, we want to generate
    % code such as:
    %
    %   ( if
    %       compare(SubResult1, X1, Y1),
    %       SubResult1 \= (=)
    %   then
    %       Result = SubResult1
    %   else if
    %       compare(SubResult2, X2, Y2),
    %       SubResult2 \= (=)
    %   then
    %       Result = SubResult2
    %   else
    %       compare(Result, X3, Y3)
    %   )
    %
    % The idea is that for each non-last argument, we compare the
    % corresponding arguments, and then
    %
    % - if they are not equal, we return the result of their comparison, while
    % - if they *are* equal, then we proceed to compare the later arguments.
    %
    % For the last argument, we always return the result of the argument
    % comparison.
    %
    % This results in the if-then-else chain above. This chain is built by
    % prepare_for_conjoining_arg_comparisons and conjoin_arg_comparisons below.
    %
    % However, we handle some arguments differently.
    %
    % - Arguments whose type is a dummy type don't need to be compared
    %   at all. Since a dummy type consists of only one value, two values
    %   of the same dummy type are always equal.
    %
    % - Any consecutive sequence of sub-word-sized arguments of unsigned types
    %   that are packed together into one word can be compared by a single
    %   unsigned compare operation of all their bits, because the arg packing
    %   algorithm puts the earlier arguments into higher value bit positions
    %   specifically to make this possible. Since this bulk comparison
    %   cannot be expressed in Mercury, the code we generate for it is in C,
    %   wrapped up in a foreign_proc. (We enable argument packing only
    %   when targeting C.)
    %
    % - We also generate C code inside a foreign_proc for any sub-word-sized
    %   argument of a signed type (int8, int16 or int32) that is packed
    %   together with other sub-word-sized arguments. We *could* simply let
    %   the initial deconstruction of the two terms being compared
    %   pick up the values of these arguments, and compare them by calling
    %   builtin.compare, but that approach has a lot more overhead than
    %   the simple, direct code in the foreign_proc we generate.
    %
:- pred generate_arg_compare_goals(uc_params::in,
    mer_type::in, prog_var::in, prog_var::in, prog_var::in,
    maybe_all_args_in_word_so_far::in, int::in,
    list(constructor_arg_repn)::in, hlds_goal::out,
    list(prog_var)::out, list(prog_var)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_arg_compare_goals(UCParams, _, _, _, ResultVar,
        _, _, [], Goal, [], [], !Info) :-
    generate_return_equal(ResultVar, UCParams ^ ucp_context, Goal).
generate_arg_compare_goals(UCParams, TermType, TermVarX, TermVarY, ResultVar,
        !.MaybeAllArgs, ArgNum, [CtorArgRepn | CtorArgRepns], Goal,
        VarsX, VarsY, !Info) :-
    GiveVarsTypes = UCParams ^ ucp_give_vars_types,
    ModuleInfo = UCParams ^ ucp_module_info,
    Type = CtorArgRepn ^ car_type,
    IsDummy = is_type_a_dummy(ModuleInfo, Type),
    (
        IsDummy = is_dummy_type,
        % X and Y contain dummy values, so there is nothing to compare.
        make_fresh_var_pair(GiveVarsTypes, "_ArgX", "_ArgY", ArgNum,
            Type, HeadVarX, HeadVarY, !Info),
        generate_arg_compare_goals(UCParams,
            TermType, TermVarX, TermVarY, ResultVar,
            !.MaybeAllArgs, ArgNum + 1, CtorArgRepns, Goal,
            TailVarsX, TailVarsY, !Info),
        VarsX = [HeadVarX | TailVarsX],
        VarsY = [HeadVarY | TailVarsY]
    ;
        IsDummy = is_not_dummy_type,
        Context = UCParams ^ ucp_context,
        may_we_start_packing_at_this_arg_compare(UCParams, CtorArgRepn,
            CompareHow, !MaybeAllArgs),
        (
            CompareHow = compare_noop,
            make_fresh_var_pair(GiveVarsTypes, "_ArgX", "_ArgY", ArgNum,
                Type, HeadVarX, HeadVarY, !Info),
            generate_arg_compare_goals(UCParams,
                TermType, TermVarX, TermVarY, ResultVar,
                !.MaybeAllArgs, ArgNum + 1, CtorArgRepns, Goal,
                TailVarsX, TailVarsY, !Info),
            VarsX = [HeadVarX | TailVarsX],
            VarsY = [HeadVarY | TailVarsY]
        ;
            CompareHow = compare_unpacked,
            make_fresh_var_pair(GiveVarsTypes, "ArgX", "ArgY", ArgNum,
                Type, HeadVarX, HeadVarY, !Info),
            % When comparing existentially typed arguments, the arguments
            % may have different types; in that case, rather than just
            % comparing them, which would be a type error, we call
            % `typed_compare', which is a builtin that first compares
            % their types, and compares their values only if the types match.
            ( if type_contains_existq_tvar(UCParams, Type) then
                ComparePred = "typed_compare",
                ComparePredModule = mercury_private_builtin_module
            else
                ComparePred = "compare",
                ComparePredModule = mercury_public_builtin_module
            ),
            prepare_for_conjoining_arg_comparisons(CtorArgRepns,
                ArgNum, ResultVar, CurCompareResultVar, ConjoinKind, !Info),
            build_simple_call(ModuleInfo, ComparePredModule,
                ComparePred, [CurCompareResultVar, HeadVarX, HeadVarY],
                Context, SubCompareGoal),
            conjoin_arg_comparisons(UCParams, ConjoinKind,
                TermType, TermVarX, TermVarY, ResultVar,
                not_all_args_in_word_so_far, ArgNum + 1, SubCompareGoal, Goal,
                TailVarsX, TailVarsY, !Info),
            VarsX = [HeadVarX | TailVarsX],
            VarsY = [HeadVarY | TailVarsY]
        ;
            CompareHow = compare_subword_signed(ArgsLocn, CellOffset, Shift,
                SignedIntSize),
            make_fresh_var_pair(GiveVarsTypes, "_ArgX", "_ArgY", ArgNum,
                Type, HeadVarX, HeadVarY, !Info),
            prepare_for_conjoining_arg_comparisons(CtorArgRepns,
                ArgNum, ResultVar, CurCompareResultVar, ConjoinKind, !Info),
            select_and_build_signed_comparison_foreign_proc(ModuleInfo,
                ArgsLocn, TermType, TermVarX, TermVarY, CurCompareResultVar,
                ArgNum, CellOffset, Shift, SignedIntSize,
                Context, SubCompareGoal, !Info),
            conjoin_arg_comparisons(UCParams, ConjoinKind,
                TermType, TermVarX, TermVarY, ResultVar,
                not_all_args_in_word_so_far, ArgNum + 1, SubCompareGoal, Goal,
                TailVarsX, TailVarsY, !Info),
            VarsX = [HeadVarX | TailVarsX],
            VarsY = [HeadVarY | TailVarsY]
        ;
            CompareHow = compare_packed(ArgsLocn, CellOffset,
                Shift0, NumBits0),
            expect_not(type_contains_existq_tvar(UCParams, Type), $pred,
                "sub-word-size argument of existential type"),
            make_fresh_var_pair(GiveVarsTypes, "_ArgX", "_ArgY", ArgNum,
                Type, HeadVarX, HeadVarY, !Info),
            get_bulk_comparable_packed_args(UCParams, CellOffset,
                ArgNum, LeftOverArgNum, Shift0, Shift, NumBits0, NumBits,
                CtorArgRepns, LeftOverCtorArgRepns, !MaybeAllArgs,
                TailBulkVarsX, TailBulkVarsY, !Info),
            prepare_for_conjoining_arg_comparisons(LeftOverCtorArgRepns,
                ArgNum, ResultVar, CurCompareResultVar, ConjoinKind, !Info),
            select_and_build_bulk_comparison_foreign_proc(ModuleInfo, ArgsLocn,
                TermType, TermVarX, TermVarY, CurCompareResultVar,
                !.MaybeAllArgs, ArgNum, CellOffset, Shift, NumBits,
                Context, SubCompareGoal, !Info),
            conjoin_arg_comparisons(UCParams, ConjoinKind,
                TermType, TermVarX, TermVarY, ResultVar,
                !.MaybeAllArgs, LeftOverArgNum, SubCompareGoal, Goal,
                TailVarsX, TailVarsY, !Info),
            VarsX = [HeadVarX | TailBulkVarsX] ++ TailVarsX,
            VarsY = [HeadVarY | TailBulkVarsY] ++ TailVarsY
        )
    ).

:- type compare_conjoin_kind
    --->    no_more_comparisons
    ;       more_comparisons(
                constructor_arg_repn,
                list(constructor_arg_repn),
                prog_var
            ).

:- pred prepare_for_conjoining_arg_comparisons(list(constructor_arg_repn)::in,
    int::in, prog_var::in, prog_var::out, compare_conjoin_kind::out,
    unify_proc_info::in, unify_proc_info::out) is det.

prepare_for_conjoining_arg_comparisons(CtorArgRepns, ArgNum,
        ResultVar, CurCompareResultVar, ConjoinKind, !Info) :-
    (
        CtorArgRepns = [],
        CurCompareResultVar = ResultVar,
        ConjoinKind = no_more_comparisons
    ;
        CtorArgRepns = [HeadCtorArgRepn | TailCtorArgRepns],
        make_fresh_var(give_vars_types, "SubResult", ArgNum,
            comparison_result_type, CurCompareResultVar, !Info),
        ConjoinKind = more_comparisons(HeadCtorArgRepn, TailCtorArgRepns,
            CurCompareResultVar)
    ).

:- pred conjoin_arg_comparisons(uc_params::in, compare_conjoin_kind::in,
    mer_type::in, prog_var::in, prog_var::in, prog_var::in,
    maybe_all_args_in_word_so_far::in, int::in, hlds_goal::in, hlds_goal::out,
    list(prog_var)::out, list(prog_var)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

conjoin_arg_comparisons(UCParams, ConjoinKind,
        TermType, TermVarX, TermVarY, ResultVar,
        MaybeAllArgs, NextArgNum, SubCompareGoal, Goal,
        TailVarsX, TailVarsY, !Info) :-
    (
        ConjoinKind = no_more_comparisons,
        Goal = SubCompareGoal,
        TailVarsX = [],
        TailVarsY = []
    ;
        ConjoinKind = more_comparisons(HeadCtorArgRepn, TailCtorArgRepns,
            SubResultVar),
        Context = UCParams ^ ucp_context,
        goal_info_init(Context, GoalInfo),

        make_const_construction(Context, SubResultVar, compare_cons_id("="),
            CheckEqualGoal),
        CheckNotEqualGoal = hlds_goal(negation(CheckEqualGoal), GoalInfo),

        SubResultRHS = rhs_var(SubResultVar),
        create_pure_atomic_complicated_unification(ResultVar, SubResultRHS,
            Context, umc_explicit, [], ReturnSubResultGoal),
        CondGoalExpr = conj(plain_conj, [SubCompareGoal, CheckNotEqualGoal]),
        CondGoal = hlds_goal(CondGoalExpr, GoalInfo),
        generate_arg_compare_goals(UCParams, TermType, TermVarX, TermVarY,
            ResultVar, MaybeAllArgs, NextArgNum,
            [HeadCtorArgRepn | TailCtorArgRepns], ElseGoal,
            TailVarsX, TailVarsY, !Info),
        GoalExpr = if_then_else([], CondGoal, ReturnSubResultGoal, ElseGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

%---------------------%

:- pred select_and_build_signed_comparison_foreign_proc(module_info::in,
    args_locn::in, mer_type::in, prog_var::in, prog_var::in, prog_var::in,
    int::in, cell_offset::in, arg_shift::in, string::in,
    prog_context::in, hlds_goal::out,
    unify_proc_info::in, unify_proc_info::out) is det.

select_and_build_signed_comparison_foreign_proc(ModuleInfo, ArgsLocn,
        TermType, TermVarX, TermVarY, CompareResultVar, ArgNum, CellOffset,
        Shift, SizeStr, Context, CompareConjGoal, !Info) :-
    % Keep the predicates
    %   build_bulk_unify_foreign_proc
    %   select_and_build_signed_comparison_foreign_proc
    %   select_and_build_bulk_comparison_foreign_proc
    % in sync where relevant.
    TermVarXForeignArg = foreign_arg(TermVarX,
        yes(foreign_arg_name_mode("TermVarX", in_mode)),
        TermType, bp_native_if_possible),
    TermVarYForeignArg = foreign_arg(TermVarY,
        yes(foreign_arg_name_mode("TermVarY", in_mode)),
        TermType, bp_native_if_possible),
    CompareResultForeignArg = foreign_arg(CompareResultVar,
        yes(foreign_arg_name_mode("ResultVar", out_mode)),
        comparison_result_type, bp_native_if_possible),

    % Keep the Remote versions in sync with generate_arg_unify_goals.
    LocalWordsDecl = "
        MR_Unsigned word_x;
        MR_Unsigned word_y;
    ",
    LocalWordsCode = "
        word_x = (MR_Unsigned) TermVarX;
        word_y = (MR_Unsigned) TermVarY;
    ",
    RemoteWordsDecl = "
        MR_Unsigned *cell_x;
        MR_Unsigned *cell_y;
        MR_Unsigned word_x;
        MR_Unsigned word_y;
    ",
    RemoteWordsCode = "
        cell_x = (MR_Unsigned *)
            (((MR_Unsigned) TermVarX) - (MR_Unsigned) Ptag);
        cell_y = (MR_Unsigned *)
            (((MR_Unsigned) TermVarY) - (MR_Unsigned) Ptag);
        word_x = cell_x[CellOffsetVar];
        word_y = cell_y[CellOffsetVar];
    ",
    ValuesDecl = string.format("
        MR_Unsigned mask;
        int%s_t     value_x;
        int%s_t     value_y;
    ", [s(SizeStr), s(SizeStr)]),
    ValuesCode = string.format("
        mask = (MR_Unsigned) ((UINT64_C(1) << %s) - 1);
        value_x = (int%s_t) (word_x >> ShiftVar) & mask;
        value_y = (int%s_t) (word_y >> ShiftVar) & mask;
    ", [s(SizeStr), s(SizeStr), s(SizeStr)]),
    CompareValuesCode = "
        if (value_x < value_y) {
            ResultVar = MR_COMPARE_LESS;
        } else if (value_x > value_y) {
            ResultVar = MR_COMPARE_GREATER;
        } else {
            ResultVar = MR_COMPARE_EQUAL;
        }
    ",

    Shift = arg_shift(ShiftInt),
    make_fresh_int_var_and_arg(Context, "ShiftVar", ArgNum, ShiftInt,
        ShiftForeignArg, MakeShiftGoal, !Info),

    (
        ArgsLocn = args_local,
        ComparePredName =
            string.format("compare_local_int%s_bitfields", [s(SizeStr)]),
        MaybeWordsArgs = [],
        MaybeWordsGoals = [],
        WordsDecl = LocalWordsDecl,
        WordsCode = LocalWordsCode
    ;
        ArgsLocn = args_remote(Ptag),
        ComparePredName =
            string.format("compare_remote_int%s_bitfields", [s(SizeStr)]),
        make_ptag_and_cell_offset_args(ArgNum, Ptag, CellOffset, Context,
            MaybeWordsArgs, MaybeWordsGoals, !Info),
        WordsDecl = RemoteWordsDecl,
        WordsCode = RemoteWordsCode
    ),

    ForeignArgs = [TermVarXForeignArg, TermVarYForeignArg] ++
        MaybeWordsArgs ++ [ShiftForeignArg, CompareResultForeignArg],
    ForeignCode = WordsDecl ++ ValuesDecl ++
        WordsCode ++ ValuesCode ++ CompareValuesCode,

    generate_call_foreign_proc(ModuleInfo, pf_predicate,
        mercury_private_builtin_module, ComparePredName,
        [], ForeignArgs, [], instmap_delta_bind_var(CompareResultVar),
        only_mode, detism_semi, purity_pure, [], pure_proc_foreign_attributes,
        no, ForeignCode, Context, CompareGoal),
    CompareConjGoalExpr = conj(plain_conj,
        MaybeWordsGoals ++ [MakeShiftGoal, CompareGoal]),
    goal_info_init(Context, ContextGoalInfo),
    CompareConjGoal = hlds_goal(CompareConjGoalExpr, ContextGoalInfo).

:- pred select_and_build_bulk_comparison_foreign_proc(module_info::in,
    args_locn::in, mer_type::in, prog_var::in, prog_var::in, prog_var::in,
    maybe_all_args_in_word_so_far::in, int::in, cell_offset::in,
    arg_shift::in, arg_num_bits::in, prog_context::in, hlds_goal::out,
    unify_proc_info::in, unify_proc_info::out) is det.

select_and_build_bulk_comparison_foreign_proc(ModuleInfo, ArgsLocn,
        TermType, TermVarX, TermVarY, CompareResultVar,
        MaybeAllArgs, ArgNum, CellOffset, Shift, NumBits,
        Context, CompareConjGoal, !Info) :-
    % Keep the predicates
    %   build_bulk_unify_foreign_proc
    %   select_and_build_signed_comparison_foreign_proc
    %   select_and_build_bulk_comparison_foreign_proc
    % in sync where relevant.
    TermVarXForeignArg = foreign_arg(TermVarX,
        yes(foreign_arg_name_mode("TermVarX", in_mode)),
        TermType, bp_native_if_possible),
    TermVarYForeignArg = foreign_arg(TermVarY,
        yes(foreign_arg_name_mode("TermVarY", in_mode)),
        TermType, bp_native_if_possible),
    CompareResultForeignArg = foreign_arg(CompareResultVar,
        yes(foreign_arg_name_mode("ResultVar", out_mode)),
        comparison_result_type, bp_native_if_possible),

    LocalWordsDecl = "
        MR_Unsigned word_x;
        MR_Unsigned word_y;
    ",
    LocalWordsCode = "
        word_x = (MR_Unsigned) TermVarX;
        word_y = (MR_Unsigned) TermVarY;
    ",
    RemoteWordsDecl = "
        MR_Unsigned *cell_x;
        MR_Unsigned *cell_y;
        MR_Unsigned word_x;
        MR_Unsigned word_y;
    ",
    RemoteWordsCode = "
        cell_x = (MR_Unsigned *)
            (((MR_Unsigned) TermVarX) - (MR_Unsigned) Ptag);
        cell_y = (MR_Unsigned *)
            (((MR_Unsigned) TermVarY) - (MR_Unsigned) Ptag);
        word_x = cell_x[CellOffsetVar];
        word_y = cell_y[CellOffsetVar];
    ",
    ValuesFromWordsDecl = "
        MR_Unsigned value_x;
        MR_Unsigned value_y;
    ",
    ValuesFromWordsCode = "
        value_x = word_x;
        value_y = word_y;
    ",
    ValuesFromShiftMaskDecl = "
        MR_Unsigned mask;
        MR_Unsigned value_x;
        MR_Unsigned value_y;
    ",
    ValuesFromShiftMaskCode = "
        mask = (MR_Unsigned) ((UINT64_C(1) << NumBitsVar) - 1);
        value_x = (word_x >> ShiftVar) & mask;
        value_y = (word_y >> ShiftVar) & mask;
    ",
    CompareValuesCode = "
        if (value_x < value_y) {
            ResultVar = MR_COMPARE_LESS;
        } else if (value_x > value_y) {
            ResultVar = MR_COMPARE_GREATER;
        } else {
            ResultVar = MR_COMPARE_EQUAL;
        }
    ",
    (
        MaybeAllArgs = all_args_in_word_so_far,
        ComparePredNameSuffix = "words",
        ValuesDecl = ValuesFromWordsDecl,
        ValuesCode = ValuesFromWordsCode,
        MaybeShiftMaskArgs = [],
        MaybeShiftMaskGoals = []
    ;
        MaybeAllArgs = not_all_args_in_word_so_far,
        ComparePredNameSuffix = "bitfields",
        Shift = arg_shift(ShiftInt),
        make_fresh_int_var_and_arg(Context, "ShiftVar", ArgNum, ShiftInt,
            ShiftForeignArg, MakeShiftGoal, !Info),
        NumBits = arg_num_bits(NumBitsInt),
        make_fresh_int_var_and_arg(Context, "NumBitsVar", ArgNum, NumBitsInt,
            NumBitsForeignArg, MakeNumBitsGoal, !Info),
        ValuesDecl = ValuesFromShiftMaskDecl,
        ValuesCode = ValuesFromShiftMaskCode,
        MaybeShiftMaskArgs = [ShiftForeignArg, NumBitsForeignArg],
        MaybeShiftMaskGoals = [MakeShiftGoal, MakeNumBitsGoal]
    ),

    (
        ArgsLocn = args_local,
        ComparePredName = "compare_local_uint_" ++ ComparePredNameSuffix,
        MaybeWordsArgs = [],
        MaybeWordsGoals = [],
        WordsDecl = LocalWordsDecl,
        WordsCode = LocalWordsCode
    ;
        ArgsLocn = args_remote(Ptag),
        ComparePredName = "compare_remote_uint_" ++ ComparePredNameSuffix,
        make_ptag_and_cell_offset_args(ArgNum, Ptag, CellOffset, Context,
            MaybeWordsArgs, MaybeWordsGoals, !Info),
        WordsDecl = RemoteWordsDecl,
        WordsCode = RemoteWordsCode
    ),

    ForeignArgs = [TermVarXForeignArg, TermVarYForeignArg] ++
        MaybeWordsArgs ++ MaybeShiftMaskArgs ++ [CompareResultForeignArg],
    ForeignCode = WordsDecl ++ ValuesDecl ++
        WordsCode ++ ValuesCode ++ CompareValuesCode,

    generate_call_foreign_proc(ModuleInfo, pf_predicate,
        mercury_private_builtin_module, ComparePredName,
        [], ForeignArgs, [], instmap_delta_bind_var(CompareResultVar),
        only_mode, detism_semi, purity_pure, [], pure_proc_foreign_attributes,
        no, ForeignCode, Context, CompareGoal),
    CompareConjGoalExpr = conj(plain_conj,
        MaybeWordsGoals ++ MaybeShiftMaskGoals ++ [CompareGoal]),
    goal_info_init(Context, ContextGoalInfo),
    CompareConjGoal = hlds_goal(CompareConjGoalExpr, ContextGoalInfo).

:- pred get_bulk_comparable_packed_args(uc_params::in, cell_offset::in,
    int::in, int::out,
    arg_shift::in, arg_shift::out, arg_num_bits::in, arg_num_bits::out,
    list(constructor_arg_repn)::in, list(constructor_arg_repn)::out,
    maybe_all_args_in_word_so_far::in, maybe_all_args_in_word_so_far::out,
    list(prog_var)::out, list(prog_var)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

get_bulk_comparable_packed_args(_UCParams, _CellOffset, !ArgNum,
        !Shift, !NumBits, [], [], !MaybeAllArgs, [], [], !Info).
get_bulk_comparable_packed_args(UCParams, CellOffset, !ArgNum,
        !Shift, !NumBits, [CtorArgRepn | CtorArgRepns], LeftOverCtorArgRepns,
        !MaybeAllArgs, VarsX, VarsY, !Info) :-
    ArgPosWidth = CtorArgRepn ^ car_pos_width,
    (
        (
            ArgPosWidth = apw_partial_shifted(_, _, _, _, _, Fill),
            BulkComparability = fill_bulk_comparability(Fill)
        ;
            ArgPosWidth = apw_none_shifted(_, _),
            BulkComparability = bulk_comparable_unsigned
        ),
        (
            BulkComparability = bulk_comparable_unsigned,
            Type = CtorArgRepn ^ car_type,
            expect_not(type_contains_existq_tvar(UCParams, Type), $pred,
                "sub-word-size argument of existential type"),
            (
                ArgPosWidth = apw_partial_shifted(_, ArgCellOffset,
                    ArgShift, ArgNumBits, _, _),
                !.Shift = arg_shift(ShiftInt0),
                !.NumBits = arg_num_bits(NumBitsInt0),
                ArgShift = arg_shift(ArgShiftInt),
                ArgNumBits = arg_num_bits(ArgNumBitsInt),
                ShiftInt = ArgShiftInt,
                NumBitsInt = NumBitsInt0 + ArgNumBitsInt,
                expect(unify(ArgShiftInt + ArgNumBitsInt, ShiftInt0), $pred,
                    "packed arg does not immediately follow previous"),
                !:Shift = arg_shift(ShiftInt),
                !:NumBits = arg_num_bits(NumBitsInt)
            ;
                ArgPosWidth = apw_none_shifted(_, ArgCellOffset)
                % Leave !Shift and !NumBits unchanged.
            ),
            expect(unify(CellOffset, ArgCellOffset), $pred,
                "apw_{partial,none}_shifted offset != CellOffset"),
            GiveVarsTypes = UCParams ^ ucp_give_vars_types,
            make_fresh_var_pair(GiveVarsTypes, "_ArgX", "_ArgY", !.ArgNum,
                Type, HeadVarX, HeadVarY, !Info),
            !:ArgNum = !.ArgNum + 1,
            get_bulk_comparable_packed_args(UCParams, CellOffset,
                !ArgNum, !Shift, !NumBits, CtorArgRepns, LeftOverCtorArgRepns,
                !MaybeAllArgs, TailVarsX, TailVarsY, !Info),
            VarsX = [HeadVarX | TailVarsX],
            VarsY = [HeadVarY | TailVarsY]
        ;
            BulkComparability = not_bulk_comparable(_SignedIntSize),
            LeftOverCtorArgRepns = [CtorArgRepn | CtorArgRepns],
            !:MaybeAllArgs = not_all_args_in_word_so_far,
            VarsX = [],
            VarsY = []
        )
    ;
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _, _)
        ),
        LeftOverCtorArgRepns = [CtorArgRepn | CtorArgRepns],
        % This arg is in the next word, so if we started with
        % !.MaybeAllArgs = all_args_in_word_so_far, then return the same.
        VarsX = [],
        VarsY = []
    ).

:- type bulk_comparability
    --->    not_bulk_comparable(string) % Should be "8", "16" or "32".
    ;       bulk_comparable_unsigned.

:- func fill_bulk_comparability(fill_kind) = bulk_comparability.

fill_bulk_comparability(Fill) = BulkComparability :-
    (
        ( Fill = fill_enum
        ; Fill = fill_uint8
        ; Fill = fill_uint16
        ; Fill = fill_uint32
        ; Fill = fill_char21
        ),
        BulkComparability = bulk_comparable_unsigned
    ;
        ( Fill = fill_int8,  SizeStr = "8"
        ; Fill = fill_int16, SizeStr = "16"
        ; Fill = fill_int32, SizeStr = "32"
        ),
        BulkComparability = not_bulk_comparable(SizeStr)
    ).

%---------------------%

:- type unify_how
    --->    unify_unpacked
    ;       unify_packed(args_locn, cell_offset).

:- pred may_we_start_packing_at_this_arg_unify(uc_params::in,
    constructor_arg_repn::in, unify_how::out) is det.

may_we_start_packing_at_this_arg_unify(UCParams, CtorArgRepn, UnifyHow) :-
    AllowPackedUnifyCompare = UCParams ^ ucp_packed_unify_compare,
    (
        AllowPackedUnifyCompare = do_not_allow_packed_unify_compare,
        UnifyHow = unify_unpacked
    ;
        AllowPackedUnifyCompare = allow_packed_unify_compare,
        MaybePackableArgsLocn = UCParams ^ ucp_maybe_packable_args,
        (
            MaybePackableArgsLocn = unpackable_args,
            UnifyHow = unify_unpacked
        ;
            MaybePackableArgsLocn = packable_args(ArgsLocn),
            ArgPosWidth = CtorArgRepn ^ car_pos_width,
            (
                ( ArgPosWidth = apw_full(_, _)
                ; ArgPosWidth = apw_double(_, _, _)
                ; ArgPosWidth = apw_none_nowhere
                ),
                UnifyHow = unify_unpacked
            ;
                ( ArgPosWidth = apw_partial_first(_, CellOffset, _, _, _, _)
                ; ArgPosWidth = apw_partial_shifted(_, CellOffset, _, _, _, _)
                ),
                % The first arg in a packed word can be apw_partial_shifted
                % in words whose first packed entity is a remote sectag.
                UnifyHow = unify_packed(ArgsLocn, CellOffset)
            ;
                ArgPosWidth = apw_none_shifted(_, CellOffset),
                % For unifications, either we unify all the arguments
                % that are packed into the same word together, or we unify
                % them all separately, depending on AllowPackedUnifyCompare.
                % The only situation in which we can start packing here
                % is when a function symbol's representation includes
                % a sub-word-sized sectag, and the first argument is
                % of a dummy type.
                UnifyHow = unify_packed(ArgsLocn, CellOffset)
            )
        )
    ).

%---------------------%

:- type compare_how
    --->    compare_unpacked
    ;       compare_noop
    ;       compare_subword_signed(args_locn, cell_offset, arg_shift, string)
            % The string should give the size: it should be "8", "16" or "32".
    ;       compare_packed(args_locn, cell_offset, arg_shift, arg_num_bits).

:- pred may_we_start_packing_at_this_arg_compare(uc_params::in,
    constructor_arg_repn::in, compare_how::out,
    maybe_all_args_in_word_so_far::in, maybe_all_args_in_word_so_far::out)
    is det.

may_we_start_packing_at_this_arg_compare(UCParams, CtorArgRepn,
        CompareHow, !MaybeAllArgs) :-
    AllowPackedUnifyCompare = UCParams ^ ucp_packed_unify_compare,
    (
        AllowPackedUnifyCompare = do_not_allow_packed_unify_compare,
        CompareHow = compare_unpacked
        % The value of !:MaybeAllArgs won't be consulted.
    ;
        AllowPackedUnifyCompare = allow_packed_unify_compare,
        MaybePackableArgsLocn = UCParams ^ ucp_maybe_packable_args,
        (
            MaybePackableArgsLocn = unpackable_args,
            CompareHow = compare_unpacked
        ;
            MaybePackableArgsLocn = packable_args(ArgsLocn),
            ArgPosWidth = CtorArgRepn ^ car_pos_width,
            (
                ( ArgPosWidth = apw_full(_, _)
                ; ArgPosWidth = apw_double(_, _, _)
                ; ArgPosWidth = apw_none_nowhere
                ),
                CompareHow = compare_unpacked,
                !:MaybeAllArgs = all_args_in_word_so_far
            ;
                (
                    ArgPosWidth = apw_partial_first(_, CellOffset,
                        Shift, NumBits, _, Fill),
                    !:MaybeAllArgs = all_args_in_word_so_far
                ;
                    ArgPosWidth = apw_partial_shifted(_, CellOffset,
                        Shift, NumBits, _, Fill)
                    % Leave !MaybeAllArgs as it was.
                ),
                % The first arg in a packed word can be apw_partial_shifted
                % in words whose first packed entity is a remote sectag.
                BulkComparability = fill_bulk_comparability(Fill),
                (
                    BulkComparability = bulk_comparable_unsigned,
                    CompareHow = compare_packed(ArgsLocn, CellOffset,
                        Shift, NumBits)
                ;
                    BulkComparability = not_bulk_comparable(SignedIntSize),
                    CompareHow = compare_subword_signed(ArgsLocn, CellOffset,
                        Shift, SignedIntSize)
                )
            ;
                ArgPosWidth = apw_none_shifted(_, _),
                % After e.g. a remote sectag or an int8 argument, the next
                % packed argument to compare may be apw_none_shifted.
                CompareHow = compare_noop
                % Leave !MaybeAllArgs as it was.
            )
        )
    ).

%---------------------%

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
        TypeBody = hlds_du_type(TypeBodyDu),
        TypeBodyDu = type_body_du(_, _, _, MaybeRepn, _),
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
            CtorRepns = Repn ^ dur_ctor_repns,
            generate_index_proc_body_du(SpecDefnInfo, CtorRepns, X, Index,
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
    list(constructor_repn)::in, prog_var::in, prog_var::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_index_proc_body_du(SpecDefnInfo, CtorRepns, X, Index,
        Clause, !Info) :-
    list.map_foldl2(generate_index_du_case(SpecDefnInfo, Index),
        CtorRepns, Cases, 0, _, !Info),
    Context = SpecDefnInfo ^ spdi_context,
    goal_info_init(Context, GoalInfo),
    Goal = hlds_goal(switch(X, cannot_fail, Cases), GoalInfo),
    quantify_clause_body(all_modes, [X, Index], Goal, Context, Clause, !Info).

:- pred generate_index_du_case(spec_pred_defn_info::in, prog_var::in,
    constructor_repn::in, case::out, int::in, int::out,
    unify_proc_info::in, unify_proc_info::out) is det.

generate_index_du_case(SpecDefnInfo, Index, CtorRepn, Case, !N, !Info) :-
    CtorRepn = ctor_repn(_Ordinal, _MaybeExistConstraints, FunctorName,
        _ConsTag, _ArgRepns, FunctorArity, _Ctxt),
    TypeCtor = SpecDefnInfo ^ spdi_type_ctor,
    FunctorConsId = du_data_ctor(du_ctor(FunctorName, FunctorArity, TypeCtor)),
    Context = SpecDefnInfo ^ spdi_context,
    make_int_const_construction(Context, Index, !.N, UnifyIndexGoal),
    !:N = !.N + 1,
    Case = case(FunctorConsId, [], UnifyIndexGoal).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Utility predicates.
%

:- pred get_du_base_type(module_info::in, tvarset::in, mer_type::in,
    mer_type::out) is det.

get_du_base_type(ModuleInfo, TVarSet, Type, BaseType) :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    get_du_base_type_loop(TypeTable, TVarSet, Type, BaseType).

:- pred get_du_base_type_loop(type_table::in, tvarset::in, mer_type::in,
    mer_type::out) is det.

get_du_base_type_loop(TypeTable, TVarSet, Type, BaseType) :-
    % Circular subtype definitions are assumed to have been detected by now.
    type_to_ctor_and_args_det(Type, TypeCtor, TypeArgs),
    hlds_data.lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    (
        TypeBody = hlds_du_type(TypeBodyDu),
        TypeBodyDu = type_body_du(_, MaybeSuperType, _, _MaybeRepn, _),
        (
            MaybeSuperType = not_a_subtype,
            BaseType = Type
        ;
            MaybeSuperType = subtype_of(SuperType0),
            hlds_data.get_type_defn_tvarset(TypeDefn, TypeDefnTVarSet),
            hlds_data.get_type_defn_tparams(TypeDefn, TypeDefnTypeParams),
            merge_tvarsets_and_subst_type_args(TVarSet, TypeArgs,
                TypeDefnTVarSet, TypeDefnTypeParams, SuperType0, SuperType),
            get_du_base_type_loop(TypeTable, TVarSet, SuperType, BaseType)
        )
    ;
        TypeBody = hlds_abstract_type(_),
        unexpected($pred, "abstract type")
    ;
        TypeBody = hlds_eqv_type(_),
        unexpected($pred, "eqv type")
    ;
        TypeBody = hlds_foreign_type(_),
        unexpected($pred, "foreign type")
    ;
        TypeBody = hlds_solver_type(_),
        unexpected($pred, "solver type")
    ).

:- pred merge_tvarsets_and_subst_type_args(tvarset::in, list(mer_type)::in,
    tvarset::in, list(type_param)::in, mer_type::in, mer_type::out) is det.

merge_tvarsets_and_subst_type_args(TVarSet, TypeArgs,
        TVarSet0, TypeParams0, Type0, Type) :-
    tvarset_merge_renaming(TVarSet, TVarSet0, _MergedTVarSet, Renaming),
    apply_variable_renaming_to_tvar_list(Renaming, TypeParams0, TypeParams),
    map.from_corresponding_lists(TypeParams, TypeArgs, TSubst),
    apply_variable_renaming_to_type(Renaming, Type0, Type1),
    apply_rec_subst_to_type(TSubst, Type1, Type).

%---------------------------------------------------------------------------%

:- pred build_simple_call(module_info::in, module_name::in, string::in,
    list(prog_var)::in, prog_context::in, hlds_goal::out) is det.

build_simple_call(ModuleInfo, ModuleName, PredName, ArgVars, Context, Goal) :-
    generate_plain_call(ModuleInfo, pf_predicate, ModuleName, PredName,
        [], ArgVars, instmap_delta_bind_no_var, mode_no(0),
        detism_erroneous, purity_pure, [], Context, Goal).

:- pred build_spec_pred_call(type_ctor::in, special_pred_id::in,
    list(prog_var)::in, instmap_delta::in, determinism::in,
    prog_context::in, hlds_goal::out,
    unify_proc_info::in, unify_proc_info::out) is det.

build_spec_pred_call(TypeCtor, SpecialPredId, ArgVars, InstmapDelta, Detism,
        Context, Goal, !Info) :-
    unify_proc_info_get_module_info(!.Info, ModuleInfo),
    get_special_proc_det(ModuleInfo, TypeCtor, SpecialPredId,
        PredName, PredId, ProcId),
    GoalExpr = plain_call(PredId, ProcId, ArgVars, not_builtin, no, PredName),
    set_of_var.list_to_set(ArgVars, NonLocals),
    goal_info_init(NonLocals, InstmapDelta, Detism, purity_pure, GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%---------------------------------------------------------------------------%

    % We can start unify and compare predicates that may call other predicates
    % with an equality test, since it often succeeds, and when it does, it is
    % faster than executing the rest of the predicate body.
    %
:- pred maybe_wrap_with_pretest_equality(prog_context::in,
    prog_var::in, prog_var::in, maybe(prog_var)::in,
    hlds_goal::in, hlds_goal::out,
    unify_proc_info::in, unify_proc_info::out) is det.

maybe_wrap_with_pretest_equality(Context, X, Y, MaybeCompareRes,
        Goal0, Goal, !Info) :-
    ModuleInfo = !.Info ^ upi_module_info,
    module_info_get_globals(ModuleInfo, Globals),
    lookup_bool_option(Globals, should_pretest_equality, ShouldPretestEq),
    (
        ShouldPretestEq = no,
        Goal = Goal0
    ;
        ShouldPretestEq = yes,
        CastType = get_pretest_equality_cast_type(!.Info),
        unify_proc_info_new_var("CastX", CastType, CastX, !Info),
        unify_proc_info_new_var("CastY", CastType, CastY, !Info),
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

:- pred quantify_clause_body(clause_applicable_modes::in,
    list(prog_var)::in, hlds_goal::in, prog_context::in, clause::out,
    unify_proc_info::in, unify_proc_info::out) is det.

quantify_clause_body(ApplModes, HeadVars, Goal0, Context, Clause, !Info) :-
    unify_proc_info_get_var_table(!.Info, VarTable0),
    unify_proc_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    implicitly_quantify_clause_body_general(ord_nl_maybe_lambda,
        HeadVars, _Warnings, Goal0, Goal,
        VarTable0, VarTable, RttiVarMaps0, RttiVarMaps),
    unify_proc_info_set_var_table(VarTable, !Info),
    unify_proc_info_set_rtti_varmaps(RttiVarMaps, !Info),
    Clause = clause(ApplModes, Goal, impl_lang_mercury, Context, []).

%---------------------------------------------------------------------------%

:- func compare_type_ctor = type_ctor.

compare_type_ctor = TypeCtor :-
    Builtin = mercury_public_builtin_module,
    TypeCtor = type_ctor(qualified(Builtin, "comparison_result"), 0).

:- func compare_cons_id(string) = cons_id.

compare_cons_id(Name) = ConsId :-
    SymName = qualified(mercury_public_builtin_module, Name),
    DuCtor = du_ctor(SymName, 0, compare_type_ctor),
    ConsId = du_data_ctor(DuCtor).

:- func compare_functor(string) = unify_rhs.

compare_functor(Name) =
    rhs_functor(compare_cons_id(Name), is_not_exist_constr, []).

%---------------------------------------------------------------------------%

:- type maybe_give_vars_types
    --->    do_not_give_vars_types
    ;       give_vars_types.

:- pred compute_exist_constraint_implications(maybe_cons_exist_constraints::in,
    existq_tvars::out, maybe_give_vars_types::out) is det.

compute_exist_constraint_implications(MaybeExistConstraints, ExistQTVars,
        GiveVarsTypes) :-
    (
        MaybeExistConstraints = no_exist_constraints,
        ExistQTVars = [],
        GiveVarsTypes = give_vars_types
    ;
        MaybeExistConstraints = exist_constraints(ExistConstraints),
        ExistConstraints = cons_exist_constraints(ExistQTVars, _Constraints,
            _UnconstrainedQTVars, _ConstrainedQTVars),
        % If there are existential types involved, then it is too hard to get
        % the types right here (it would require allocating new type variables)
        % -- instead, typecheck.m will typecheck the clause to figure out
        % the correct types. So we just allocate the variables and leave it
        % up to typecheck.m to infer their types.
        GiveVarsTypes = do_not_give_vars_types
    ).

%---------------------%

:- pred make_ptag_and_cell_offset_args(int::in, ptag::in, cell_offset::in,
    prog_context::in, list(foreign_arg)::out, list(hlds_goal)::out,
    unify_proc_info::in, unify_proc_info::out) is det.

make_ptag_and_cell_offset_args(ArgNum, Ptag, CellOffset, Context, Args, Goals,
        !Info) :-
    Ptag = ptag(PtagUint8),
    PtagInt = uint8.cast_to_int(PtagUint8),
    make_fresh_int_var_and_arg(Context, "Ptag", ArgNum, PtagInt,
        PtagForeignArg, MakePtagGoal, !Info),
    CellOffset = cell_offset(CellOffsetInt),
    make_fresh_int_var_and_arg(Context, "CellOffsetVar", ArgNum, CellOffsetInt,
        CellOffsetForeignArg, MakeCellOffsetGoal, !Info),
    Args = [PtagForeignArg, CellOffsetForeignArg],
    Goals = [MakePtagGoal, MakeCellOffsetGoal].

%---------------------%

:- pred make_fresh_var(maybe_give_vars_types::in, string::in, int::in,
    mer_type::in, prog_var::out,
    unify_proc_info::in, unify_proc_info::out) is det.

make_fresh_var(GiveVarsTypes, Prefix, Num, Type, Var, !Info) :-
    NumStr = string.int_to_string(Num),
    Name = Prefix ++ NumStr,
    unify_proc_info_new_var_maybe_type(GiveVarsTypes, Name, Type, Var, !Info).

%---------------------%

:- pred make_fresh_var_pair(maybe_give_vars_types::in, string::in, string::in,
    int::in, mer_type::in, prog_var::out, prog_var::out,
    unify_proc_info::in, unify_proc_info::out) is det.

make_fresh_var_pair(GiveVarsTypes, PrefixX, PrefixY, Num,
        Type, VarX, VarY, !Info) :-
    NumStr = string.int_to_string(Num),
    NameX = PrefixX ++ NumStr,
    NameY = PrefixY ++ NumStr,
    unify_proc_info_new_var_maybe_type(GiveVarsTypes, NameX, Type,
        VarX, !Info),
    unify_proc_info_new_var_maybe_type(GiveVarsTypes, NameY, Type,
        VarY, !Info).

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
    unify_proc_info_new_var(Name, Type, Var, !Info).

%---------------------------------------------------------------------------%

:- pred make_fresh_int_var_and_arg(prog_context::in, string::in, int::in,
    int::in, foreign_arg::out, hlds_goal::out,
    unify_proc_info::in, unify_proc_info::out) is det.

make_fresh_int_var_and_arg(Context, Name, SuffixInt, Value, Arg, Goal,
        !Info) :-
    Type = int_type,
    make_fresh_var(give_vars_types, Name, SuffixInt, Type, Var, !Info),
    Arg = foreign_arg(Var, yes(foreign_arg_name_mode(Name, in_mode)),
        Type, bp_native_if_possible),
    make_int_const_construction(Context, Var, Value, Goal).

%---------------------------------------------------------------------------%

:- func compute_maybe_packable_args_locn(cons_tag) = maybe_packable_args.

compute_maybe_packable_args_locn(ConsTag) = ArgsLocn :-
    (
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        (
            RemoteArgsTagInfo = remote_args_only_functor,
            Ptag = ptag(0u8)
        ;
            RemoteArgsTagInfo = remote_args_unshared(Ptag)
        ;
            RemoteArgsTagInfo = remote_args_shared(Ptag, _)
        ;
            RemoteArgsTagInfo = remote_args_ctor(_),
            % This is a dummy ptag. We enable the use of remote_args_ctors
            % data type representations only in grades where we won't be
            % doing any argument packing, which means we also won't be doing
            % any bulk comparisons of packed arguments.
            Ptag = ptag(0u8)
        ),
        ArgsLocn = packable_args(args_remote(Ptag))
    ;
        ConsTag = local_args_tag(_),
        ArgsLocn = packable_args(args_local)
    ;
        ( ConsTag = no_tag
        ; ConsTag = direct_arg_tag(_)
        ),
        ArgsLocn = unpackable_args
    ;
        ( ConsTag = int_tag(_)
        ; ConsTag = float_tag(_)
        ; ConsTag = string_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = dummy_tag
        ; ConsTag = shared_local_tag_no_args(_, _, _)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ; ConsTag = closure_tag(_, _)
        ),
        % These ConsTags are for terms that have no arguments.
        % *If* we can compare constants as ints, we should never get here,
        % but in some situations (such as the now-deleted Erlang grade),
        % we cannot compare them as ints, which *does* allow us to get here.
        ArgsLocn = unpackable_args
    ).

%---------------------------------------------------------------------------%

    % We want to know whether a bulk comparison operation applies
    % only to *some* of the arguments packed together into a word,
    % or it *all* of the arguments packed together in a word, because
    % in the latter case, we can avoid the shift and mask operations
    % required to extract each bitfield from its word. (We always store
    % zeroes in the bits in a packed word that do not store arguments.)
    %
    % As we process the arguments to include in a bulk compare operation,
    % we use this type to keep track. The default is all_args_in_word_so_far,
    % but we fall back to not_all_args_in_word_so_far when we find an argument
    % that cannot be part of the bulk compare operation. We can go back to
    % all_args_in_word_so_far only when we get to the next packed word.
    %
:- type maybe_all_args_in_word_so_far
    --->    not_all_args_in_word_so_far
    ;       all_args_in_word_so_far.

%---------------------------------------------------------------------------%

:- type args_locn
    --->    args_local
    ;       args_remote(ptag).

:- type maybe_packable_args
    --->    unpackable_args
    ;       packable_args(args_locn).

:- type uc_params
    --->    uc_params(
                ucp_module_info             :: module_info,
                ucp_context                 :: prog_context,
                ucp_existq_tvars            :: existq_tvars,
                ucp_maybe_packable_args     :: maybe_packable_args,
                ucp_give_vars_types         :: maybe_give_vars_types,
                ucp_constants_as_ints       :: maybe_compare_constants_as_ints,
                ucp_packed_unify_compare    :: maybe_allow_packed_unify_compare
            ).

%---------------------------------------------------------------------------%

:- type unify_proc_info
    --->    unify_proc_info(
                upi_module_info     ::  module_info,
                upi_var_table       ::  var_table,
                upi_rtti_varmaps    ::  rtti_varmaps,
                upi_packed_ops      ::  maybe_packed_word_ops
            ).

:- type maybe_packed_word_ops
    --->    used_no_packed_word_ops
    ;       used_some_packed_word_ops.

:- pred unify_proc_info_init(module_info::in, unify_proc_info::out) is det.

unify_proc_info_init(ModuleInfo, Info) :-
    init_var_table(VarTable),
    rtti_varmaps_init(RttiVarMaps),
    Info = unify_proc_info(ModuleInfo, VarTable, RttiVarMaps,
        used_no_packed_word_ops).

:- pred unify_proc_info_get_module_info(unify_proc_info::in,
    module_info::out) is det.
:- pred unify_proc_info_get_var_table(unify_proc_info::in,
    var_table::out) is det.
:- pred unify_proc_info_get_rtti_varmaps(unify_proc_info::in,
    rtti_varmaps::out) is det.

unify_proc_info_get_module_info(Info, X) :-
    X = Info ^ upi_module_info.
unify_proc_info_get_var_table(Info, X) :-
    X = Info ^ upi_var_table.
unify_proc_info_get_rtti_varmaps(Info, X) :-
    X = Info ^ upi_rtti_varmaps.

:- pred unify_proc_info_set_module_info(module_info::in,
    unify_proc_info::in, unify_proc_info::out) is det.
:- pred unify_proc_info_set_var_table(var_table::in,
    unify_proc_info::in, unify_proc_info::out) is det.
:- pred unify_proc_info_set_rtti_varmaps(rtti_varmaps::in,
    unify_proc_info::in, unify_proc_info::out) is det.
:- pred unify_proc_info_set_packed_ops(maybe_packed_word_ops::in,
    unify_proc_info::in, unify_proc_info::out) is det.

unify_proc_info_set_module_info(X, !Info) :-
    !Info ^ upi_module_info := X.
unify_proc_info_set_var_table(X, !Info) :-
    !Info ^ upi_var_table := X.
unify_proc_info_set_rtti_varmaps(X, !Info) :-
    !Info ^ upi_rtti_varmaps := X.
unify_proc_info_set_packed_ops(X, !Info) :-
    !Info ^ upi_packed_ops := X.

%---------------------%

:- pred unify_proc_info_new_var(string::in, mer_type::in, prog_var::out,
    unify_proc_info::in, unify_proc_info::out) is det.

unify_proc_info_new_var(Name, Type, Var, !Info) :-
    ModuleInfo = !.Info ^ upi_module_info,
    IsDummy = is_type_a_dummy(ModuleInfo, Type),
    Entry = vte(Name, Type, IsDummy),
    VarTable0 = !.Info ^ upi_var_table,
    add_var_entry(Entry, Var, VarTable0, VarTable),
    !Info ^ upi_var_table := VarTable.

:- pred unify_proc_info_new_var_no_type(string::in, prog_var::out,
    unify_proc_info::in, unify_proc_info::out) is det.

unify_proc_info_new_var_no_type(Name, Var, !Info) :-
    Type = void_type,
    IsDummy = is_dummy_type,
    Entry = vte(Name, Type, IsDummy),
    VarTable0 = !.Info ^ upi_var_table,
    add_var_entry(Entry, Var, VarTable0, VarTable),
    !Info ^ upi_var_table := VarTable.

:- pred unify_proc_info_new_var_maybe_type(maybe_give_vars_types::in,
    string::in, mer_type::in, prog_var::out,
    unify_proc_info::in, unify_proc_info::out) is det.

unify_proc_info_new_var_maybe_type(GiveVarsTypes, Name, Type, Var, !Info) :-
    (
        GiveVarsTypes = give_vars_types,
        unify_proc_info_new_var(Name, Type, Var, !Info)
    ;
        GiveVarsTypes = do_not_give_vars_types,
        unify_proc_info_new_var_no_type(Name, Var, !Info)
    ).

:- pred unify_proc_info_extract(unify_proc_info::in,
    module_info::out, var_table::out) is det.

unify_proc_info_extract(Info, ModuleInfo, VarTable) :-
    ModuleInfo = Info ^ upi_module_info,
    VarTable = Info ^ upi_var_table.

%---------------------------------------------------------------------------%
:- end_module check_hlds.unify_proc.
%---------------------------------------------------------------------------%

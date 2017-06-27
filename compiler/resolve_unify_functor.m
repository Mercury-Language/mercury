%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module does two tasks that are logically part of type analysis
% but must be done after type inference is complete:
%
%   - it resolves function overloading; and
%   - it expands field access functions.
%
% Most other similar tasks are done in post_typecheck.m or purity.m.
%
%---------------------------------------------------------------------------%

:- module check_hlds.resolve_unify_functor.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.vartypes.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- type is_plain_unify
    --->    is_not_plain_unify
    ;       is_plain_unify
    ;       is_unknown_ref(error_spec).

    % Work out whether a var-functor unification is actually a function call.
    % If so, replace the unification goal with a call.
    %
:- pred resolve_unify_functor(module_info::in, prog_var::in, cons_id::in,
    list(prog_var)::in, unify_mode::in, unification::in, unify_context::in,
    hlds_goal_info::in, pred_info::in, pred_info::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    hlds_goal::out, is_plain_unify::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module term_io.
:- import_module varset.

%---------------------------------------------------------------------------%

resolve_unify_functor(ModuleInfo, X0, ConsId0, ArgVars0, Mode0,
        Unification0, UnifyContext, GoalInfo0, !PredInfo, !VarSet, !VarTypes,
        Goal, IsPlainUnify) :-
    lookup_var_type(!.VarTypes, X0, TypeOfX),
    list.length(ArgVars0, Arity),
    ( if
        % Is the function symbol apply/N or ''/N, representing a higher-order
        % function call? Or the impure/semipure equivalents impure_apply/N
        % and semipure_apply/N?
        % (XXX FIXME We should use nicer syntax for impure apply/N.)
        ConsId0 = cons(unqualified(ApplyName), _, _),
        ( ApplyName = "apply", Purity = purity_pure
        ; ApplyName = "", Purity = purity_pure
        ; ApplyName = "impure_apply", Purity = purity_impure
        ; ApplyName = "semipure_apply", Purity = purity_semipure
        ),
        Arity >= 1,
        ArgVars0 = [FuncVar | FuncArgVars]
    then
        % Convert the higher-order function call (apply/N) into a higher-order
        % predicate call (i.e., replace `X = apply(F, A, B, C)'
        % with `call(F, A, B, C, X)')
        ArgVars = FuncArgVars ++ [X0],
        Modes = [],
        Det = detism_erroneous,
        adjust_func_arity(pf_function, Arity, FullArity),
        HOCall = generic_call(
            higher_order(FuncVar, Purity, pf_function, FullArity),
            ArgVars, Modes, arg_reg_types_unset, Det),
        Goal = hlds_goal(HOCall, GoalInfo0),
        IsPlainUnify = is_not_plain_unify
    else if
        % Is the function symbol a user-defined function, rather than
        % a functor which represents a data constructor?

        % Find the set of candidate predicates which have the
        % specified name and arity (and module, if module-qualified)
        ConsId0 = cons(PredName, _, _),

        pred_info_get_markers(!.PredInfo, Markers),
        module_info_get_predicate_table(ModuleInfo, PredTable),
        % This search will usually fail, so do it first.
        predicate_table_lookup_func_sym_arity(PredTable,
            calls_are_fully_qualified(Markers), PredName, Arity, PredIds),
        PredIds = [_ | _],

        % We don't do this for compiler-generated predicates; they are assumed
        % to have been generated with all functions already expanded. If we did
        % this check for compiler-generated predicates, it would cause the
        % wrong behaviour in the case where there is a user-defined function
        % whose type is exactly the same as the type of a constructor.
        % (Normally that would cause a type ambiguity error, but
        % compiler-generated predicates are not type-checked.)
        not is_unify_or_compare_pred(!.PredInfo),

        % We don't do this for the clause introduced by the compiler for a
        % field access function -- that needs to be expanded into
        % unifications below.
        not pred_info_is_field_access_function(ModuleInfo, !.PredInfo),

        % Check if any of the candidate functions have argument/return types
        % which subsume the actual argument/return types of this function call,
        % and which have universal constraints consistent with what we expect.
        pred_info_get_typevarset(!.PredInfo, TVarSet),
        pred_info_get_exist_quant_tvars(!.PredInfo, ExistQTVars),
        pred_info_get_external_type_params(!.PredInfo, ExternalTypeParams),
        lookup_var_types(!.VarTypes, ArgVars0, ArgTypes0),
        ArgTypes = ArgTypes0 ++ [TypeOfX],
        pred_info_get_constraint_map(!.PredInfo, ConstraintMap),
        GoalId = goal_info_get_goal_id(GoalInfo0),
        ConstraintSearch =
            search_hlds_constraint_list(ConstraintMap, unproven, GoalId),
        Context = goal_info_get_context(GoalInfo0),
        find_matching_pred_id(ModuleInfo, PredIds, TVarSet, ExistQTVars,
            ArgTypes, ExternalTypeParams, yes(ConstraintSearch), Context,
            PredId, QualifiedFuncName)
    then
        % Convert function calls in unifications into plain calls:
        % replace `X = f(A, B, C)' with `f(A, B, C, X)'.

        ProcId = invalid_proc_id,
        ArgVars = ArgVars0 ++ [X0],
        FuncCallUnifyContext = call_unify_context(X0,
            rhs_functor(ConsId0, is_not_exist_constr, ArgVars0), UnifyContext),
        FuncCall = plain_call(PredId, ProcId, ArgVars, not_builtin,
            yes(FuncCallUnifyContext), QualifiedFuncName),
        Goal = hlds_goal(FuncCall, GoalInfo0),
        IsPlainUnify = is_not_plain_unify
    else if
        % Is the function symbol a higher-order predicate or function constant?
        ConsId0 = cons(Name, _, _),
        type_is_higher_order_details(TypeOfX, _Purity, PredOrFunc,
            EvalMethod, HOArgTypes),

        % We don't do this for the clause introduced by the compiler
        % for a field access function -- that needs to be expanded
        % into unifications below.
        not pred_info_is_field_access_function(ModuleInfo, !.PredInfo),

        % Find the pred_id of the constant.
        lookup_var_types(!.VarTypes, ArgVars0, ArgTypes0),
        AllArgTypes = ArgTypes0 ++ HOArgTypes,
        pred_info_get_typevarset(!.PredInfo, TVarSet),
        pred_info_get_exist_quant_tvars(!.PredInfo, ExistQVars),
        pred_info_get_external_type_params(!.PredInfo, ExternalTypeParams),
        pred_info_get_markers(!.PredInfo, Markers),
        Context = goal_info_get_context(GoalInfo0),
        get_pred_id_by_types(calls_are_fully_qualified(Markers), Name,
            PredOrFunc, TVarSet, ExistQVars, AllArgTypes, ExternalTypeParams,
            ModuleInfo, Context, PredId)
    then
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        ProcIds = pred_info_procids(PredInfo),
        (
            ProcIds = [ProcId0],
            MaybeProcId = yes(ProcId0)
        ;
            ProcIds = [_, _ | _],
            % We don't know which mode to pick. Defer it until mode checking.
            MaybeProcId = yes(invalid_proc_id)
        ;
            ProcIds = [],
            MaybeProcId = no
        ),
        (
            MaybeProcId = yes(ProcId),
            ShroudedPredProcId = shroud_pred_proc_id(proc(PredId, ProcId)),
            ConsId = closure_cons(ShroudedPredProcId, EvalMethod),
            GoalExpr = unify(X0,
                rhs_functor(ConsId, is_not_exist_constr, ArgVars0),
                Mode0, Unification0, UnifyContext),
            Goal = hlds_goal(GoalExpr, GoalInfo0),
            IsPlainUnify = is_not_plain_unify
        ;
            MaybeProcId = no,
            Goal = true_goal,
            Pieces = [words("Error: reference to"),
                words("undeclared function or predicate"),
                qual_sym_name_and_arity(sym_name_arity(Name, Arity)),
                suffix("."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_type_check, [Msg]),
            IsPlainUnify = is_unknown_ref(Spec)
        )
    else if
        % Is it a call to an automatically generated field access function.
        % This test must come after the tests for function calls and
        % higher-order terms above. We do it that way because it is easier
        % to check that the types match for functions calls and higher-order
        % terms.
        ConsId0 = cons(Name, Arity, _),
        is_field_access_function_name(ModuleInfo, Name, Arity,
            AccessType, FieldName),

        % We don't do this for compiler-generated predicates --
        % they will never contain calls to field access functions.
        not is_unify_or_compare_pred(!.PredInfo),

        % If there is a constructor for which the argument types match,
        % this unification couldn't be a call to a field access function,
        % otherwise there would have been an error reported for unresolved
        % overloading.
        pred_info_get_typevarset(!.PredInfo, TVarSet),
        lookup_var_types(!.VarTypes, ArgVars0, ArgTypes0),
        not find_matching_constructor(ModuleInfo, TVarSet, ConsId0,
            TypeOfX, ArgTypes0)
    then
        finish_field_access_function(ModuleInfo, !PredInfo, !VarTypes, !VarSet,
            AccessType, FieldName, UnifyContext, X0, ArgVars0, GoalInfo0,
            Goal),
        IsPlainUnify = is_not_plain_unify
    else
        % Module qualify ordinary construction/deconstruction unifications.
        type_to_ctor_det(TypeOfX, TypeCtorOfX),
        ( if ConsId0 = cons(SymName0, Arity, _OldTypeCtor) then
            ( if TypeOfX = tuple_type(_, _) then
                ConsId = tuple_cons(Arity)
            else if TypeOfX = builtin_type(builtin_type_char) then
                (
                    SymName0 = unqualified(Name0),
                    ( if encode_escaped_char(Char, Name0) then
                        ConsId = char_const(Char)
                    else
                        unexpected($module, $pred, "encode_escaped_char")
                    )
                ;
                    SymName0 = qualified(_, _),
                    unexpected($module, $pred, "qualified char const")
                )
            else
                Name = unqualify_name(SymName0),
                TypeCtorOfX = type_ctor(TypeCtorSymName, _),
                (
                    TypeCtorSymName = qualified(TypeCtorModule, _),
                    SymName = qualified(TypeCtorModule, Name),
                    ConsId = cons(SymName, Arity, TypeCtorOfX)
                ;
                    TypeCtorSymName = unqualified(_),
                    unexpected($module, $pred, "unqualified type_ctor")
                )
            )
        else
            ConsId = ConsId0
        ),
        RHS = rhs_functor(ConsId, is_not_exist_constr, ArgVars0),
        GoalExpr = unify(X0, RHS, Mode0, Unification0, UnifyContext),
        Goal = hlds_goal(GoalExpr, GoalInfo0),
        IsPlainUnify = is_plain_unify
    ).

%---------------------------------------------------------------------------%

    % Succeed if there is a constructor which matches the given cons_id,
    % type and argument types.
    %
:- pred find_matching_constructor(module_info::in, tvarset::in,
    cons_id::in, mer_type::in, list(mer_type)::in) is semidet.

find_matching_constructor(ModuleInfo, TVarSet, ConsId, Type, ArgTypes) :-
    type_to_ctor(Type, TypeCtor),
    module_info_get_cons_table(ModuleInfo, ConsTable),
    search_cons_table_of_type_ctor(ConsTable, TypeCtor, ConsId, ConsDefn),

    % Overloading resolution ignores the class constraints.
    ConsDefn = hlds_cons_defn(_, _, _, _, ConsExistQVars, _, ConsArgs, _),

    module_info_get_type_table(ModuleInfo, TypeTable),
    search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_tvarset(TypeDefn, TypeTVarSet),
    hlds_data.get_type_defn_kind_map(TypeDefn, TypeKindMap),

    ConsArgTypes = list.map(func(C) = C ^ arg_type, ConsArgs),
    % XXX is this correct?
    ExistQVars = [],
    ExternalTypeParams = [],
    arg_type_list_subsumes(TVarSet, ExistQVars, ArgTypes, ExternalTypeParams,
        TypeTVarSet, TypeKindMap, ConsExistQVars, ConsArgTypes).

%---------------------------------------------------------------------------%

    % Convert a field access function call into the equivalent unifications
    % so that later passes do not have to handle them as a special case.
    % The error messages from mode analysis and determinism analysis
    % shouldn't be too much worse than if the goals were special cases.
    %
:- pred finish_field_access_function(module_info::in,
    pred_info::in, pred_info::out, vartypes::in, vartypes::out,
    prog_varset::in, prog_varset::out, field_access_type::in, sym_name::in,
    unify_context::in, prog_var::in, list(prog_var)::in,
    hlds_goal_info::in, hlds_goal::out) is det.

finish_field_access_function(ModuleInfo, !PredInfo, !VarTypes, !VarSet,
        AccessType, FieldName, UnifyContext, Var, Args, GoalInfo,
        hlds_goal(GoalExpr, GoalInfo)) :-
    (
        AccessType = get,
        field_extraction_function_args(Args, TermVar),
        translate_get_function(ModuleInfo, !PredInfo, !VarTypes, !VarSet,
            FieldName, UnifyContext, Var, TermVar, GoalInfo, GoalExpr)
    ;
        AccessType = set,
        field_update_function_args(Args, TermInputVar, FieldVar),
        translate_set_function(ModuleInfo, !PredInfo, !VarTypes, !VarSet,
            FieldName, UnifyContext, FieldVar, TermInputVar, Var,
            GoalInfo, GoalExpr)
    ).

:- pred translate_get_function(module_info::in, pred_info::in, pred_info::out,
    vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
    sym_name::in, unify_context::in, prog_var::in, prog_var::in,
    hlds_goal_info::in, hlds_goal_expr::out) is det.

translate_get_function(ModuleInfo, !PredInfo, !VarTypes, !VarSet, FieldName,
        UnifyContext, FieldVar, TermInputVar, OldGoalInfo, GoalExpr) :-
    lookup_var_type(!.VarTypes, TermInputVar, TermType),
    get_constructor_containing_field(ModuleInfo, TermType, FieldName,
        ConsId, FieldNumber),

    GoalId = goal_info_get_goal_id(OldGoalInfo),
    get_cons_id_arg_types_adding_existq_tvars(ModuleInfo, GoalId, ConsId,
        TermType, ArgTypes0, ExistQVars, !PredInfo),

    % If the type of the field we are extracting contains existentially
    % quantified type variables then we need to rename any other occurrences
    % of those type variables in the arguments of the constructor so that
    % they match those in the type of the field. (We don't need to do this
    % for field updates because if any existentially quantified type variables
    % occur in field to set and other fields then the field update
    % should have been disallowed by typecheck.m because the result
    % can't be well-typed).
    (
        ExistQVars = [_ | _],
        lookup_var_type(!.VarTypes, FieldVar, FieldType),
        list.det_index1(ArgTypes0, FieldNumber, FieldArgType),
        type_subsumes_det(FieldArgType, FieldType, FieldSubst),
        apply_rec_subst_to_type_list(FieldSubst, ArgTypes0, ArgTypes)
    ;
        ExistQVars = [],
        ArgTypes = ArgTypes0
    ),

    split_list_at_index(FieldNumber, ArgTypes, TypesBeforeField,
        _, TypesAfterField),

    make_new_vars(TypesBeforeField, VarsBeforeField, !VarTypes, !VarSet),
    make_new_vars(TypesAfterField, VarsAfterField, !VarTypes, !VarSet),

    ArgVars = VarsBeforeField ++ [FieldVar | VarsAfterField],

    RestrictNonLocals = goal_info_get_nonlocals(OldGoalInfo),
    create_pure_atomic_unification_with_nonlocals(TermInputVar,
        rhs_functor(ConsId, is_not_exist_constr, ArgVars),
        OldGoalInfo, RestrictNonLocals, [FieldVar, TermInputVar],
        UnifyContext, FunctorGoal),
    FunctorGoal = hlds_goal(GoalExpr, _).

:- pred translate_set_function(module_info::in, pred_info::in, pred_info::out,
    vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
    sym_name::in, unify_context::in, prog_var::in, prog_var::in, prog_var::in,
    hlds_goal_info::in, hlds_goal_expr::out) is det.

translate_set_function(ModuleInfo, !PredInfo, !VarTypes, !VarSet, FieldName,
        UnifyContext, FieldVar, TermInputVar, TermOutputVar, OldGoalInfo,
        Goal) :-
    lookup_var_type(!.VarTypes, TermInputVar, TermType),
    get_constructor_containing_field(ModuleInfo, TermType, FieldName,
        ConsId0, FieldNumber),

    GoalId = goal_info_get_goal_id(OldGoalInfo),
    get_cons_id_arg_types_adding_existq_tvars(ModuleInfo, GoalId, ConsId0,
        TermType, ArgTypes, ExistQVars, !PredInfo),

    split_list_at_index(FieldNumber, ArgTypes,
        TypesBeforeField, TermFieldType, TypesAfterField),

    make_new_vars(TypesBeforeField, VarsBeforeField, !VarTypes, !VarSet),
    make_new_var(TermFieldType, SingletonFieldVar, !VarTypes, !VarSet),
    make_new_vars(TypesAfterField, VarsAfterField, !VarTypes, !VarSet),

    % Build a goal to deconstruct the input.
    DeconstructArgs = VarsBeforeField ++ [SingletonFieldVar | VarsAfterField],
    OldNonLocals = goal_info_get_nonlocals(OldGoalInfo),
    NonLocalArgs = VarsBeforeField ++ VarsAfterField,
    set_of_var.insert_list(NonLocalArgs, OldNonLocals,
        DeconstructRestrictNonLocals),

    create_pure_atomic_unification_with_nonlocals(TermInputVar,
        rhs_functor(ConsId0, is_not_exist_constr, DeconstructArgs),
        OldGoalInfo, DeconstructRestrictNonLocals,
        [TermInputVar | DeconstructArgs], UnifyContext, DeconstructGoal),

    % Build a goal to construct the output.
    ConstructArgs = VarsBeforeField ++ [FieldVar | VarsAfterField],
    set_of_var.insert_list(NonLocalArgs, OldNonLocals,
        ConstructRestrictNonLocals),

    % If the cons_id is existentially quantified, add a `new' prefix
    % so that polymorphism.m adds the appropriate type_infos.
    (
        ExistQVars = [],
        ConsId = ConsId0
    ;
        ExistQVars = [_ | _],
        ( if ConsId0 = cons(ConsName0, ConsArity, TypeCtor) then
            add_new_prefix(ConsName0, ConsName),
            ConsId = cons(ConsName, ConsArity, TypeCtor)
        else
            unexpected($module, $pred, "invalid cons_id")
        )
    ),

    create_pure_atomic_unification_with_nonlocals(TermOutputVar,
        rhs_functor(ConsId, is_not_exist_constr, ConstructArgs), OldGoalInfo,
        ConstructRestrictNonLocals, [TermOutputVar | ConstructArgs],
        UnifyContext, ConstructGoal),

    ConjExpr = conj(plain_conj, [DeconstructGoal, ConstructGoal]),
    Conj = hlds_goal(ConjExpr, OldGoalInfo),

    % Make mode analysis treat the translated access function
    % as an atomic goal.
    Goal = scope(barrier(removable), Conj).

:- pred get_cons_id_arg_types_adding_existq_tvars(module_info::in,
    goal_id::in, cons_id::in, mer_type::in, list(mer_type)::out,
    list(tvar)::out, pred_info::in, pred_info::out) is det.

get_cons_id_arg_types_adding_existq_tvars(ModuleInfo, GoalId, ConsId,
        TermType, ActualArgTypes, ActualExistQVars, !PredInfo) :-
    % Split the list of argument types at the named field.
    type_to_ctor_det(TermType, TypeCtor),
    get_cons_defn_det(ModuleInfo, TypeCtor, ConsId, ConsDefn),
    ConsDefn = hlds_cons_defn(_, _, TypeParams, _, ConsExistQVars,
        ConsConstraints, ConsArgs, _),
    ConsArgTypes = list.map(func(C) = C ^ arg_type, ConsArgs),

    (
        ConsExistQVars = [],
        ActualArgTypes0 = ConsArgTypes,
        ActualExistQVars = []
    ;
        ConsExistQVars = [_ | _],
        % Rename apart the existentially quantified type variables.
        list.length(ConsExistQVars, NumExistQVars),
        pred_info_get_typevarset(!.PredInfo, TVarSet0),
        varset.new_vars(NumExistQVars, ParentExistQVars, TVarSet0, TVarSet),
        pred_info_set_typevarset(TVarSet, !PredInfo),
        map.from_corresponding_lists(ConsExistQVars, ParentExistQVars,
            ConsToParentRenaming),
        apply_variable_renaming_to_type_list(ConsToParentRenaming,
            ConsArgTypes, ParentArgTypes),
        apply_variable_renaming_to_prog_constraint_list(ConsToParentRenaming,
            ConsConstraints, ParentConstraints),

        % Constrained existentially quantified tvars will have already been
        % created during typechecking, so we need to ensure that the new ones
        % we allocate here are bound to those created earlier, so that
        % the varmaps remain meaningful.

        pred_info_get_constraint_map(!.PredInfo, ConstraintMap),
        list.length(ConsConstraints, NumConstraints),
        lookup_hlds_constraint_list(ConstraintMap, assumed, GoalId,
            NumConstraints, ActualConstraints),
        constraint_list_subsumes_det(ParentConstraints, ActualConstraints,
            ExistTSubst),
        apply_rec_subst_to_type_list(ExistTSubst, ParentArgTypes,
            ActualArgTypes0),

        % The kinds will be ignored when the types are converted back to tvars.
        map.init(KindMap),
        apply_rec_subst_to_tvar_list(KindMap, ExistTSubst, ParentExistQVars,
            ActualExistQVarTypes),
        ( if
            type_list_to_var_list(ActualExistQVarTypes, ActualExistQVars0)
        then
            ActualExistQVars = ActualExistQVars0
        else
            unexpected($module, $pred, "existq_tvar bound to non-var")
        )
    ),
    type_to_ctor_and_args_det(TermType, _, TypeArgs),
    map.from_corresponding_lists(TypeParams, TypeArgs, UnivTSubst),
    apply_subst_to_type_list(UnivTSubst, ActualArgTypes0, ActualArgTypes).

:- pred constraint_list_subsumes_det(list(prog_constraint)::in,
    list(prog_constraint)::in, tsubst::out) is det.

constraint_list_subsumes_det(ConstraintsA, ConstraintsB, Subst) :-
    constraint_list_get_tvars(ConstraintsB, TVarsB),
    map.init(Subst0),
    ( if
        unify_constraint_list(ConstraintsA, ConstraintsB, TVarsB,
            Subst0, Subst1)
    then
        Subst = Subst1
    else
        unexpected($module, $pred, "failed")
    ).

:- pred unify_constraint_list(list(prog_constraint)::in,
    list(prog_constraint)::in, list(tvar)::in, tsubst::in, tsubst::out)
    is semidet.

unify_constraint_list([], [], _, !Subst).
unify_constraint_list([A | As], [B | Bs], TVars, !Subst) :-
    A = constraint(_ClassNameA, ArgTypesA),
    B = constraint(_ClassNameB, ArgTypesB),
    type_unify_list(ArgTypesA, ArgTypesB, TVars, !Subst),
    unify_constraint_list(As, Bs, TVars, !Subst).

:- pred split_list_at_index(int::in, list(T)::in, list(T)::out, T::out,
    list(T)::out) is det.

split_list_at_index(Index, List, Before, At, After) :-
    ( if
        list.split_list(Index - 1, List, BeforePrime, AtAndAfter),
        AtAndAfter = [AtPrime | AfterPrime]
    then
        Before = BeforePrime,
        At = AtPrime,
        After = AfterPrime
    else
        unexpected($module, $pred, "split_list_at_index")
    ).

%---------------------------------------------------------------------------%

    % Work out which constructor of the type has an argument with the
    % given field name.
    %
:- pred get_constructor_containing_field(module_info::in, mer_type::in,
    sym_name::in, cons_id::out, int::out) is det.

get_constructor_containing_field(ModuleInfo, TermType, FieldSymName,
        ConsId, FieldNumber) :-
    type_to_ctor_det(TermType, TermTypeCtor),
    module_info_get_type_table(ModuleInfo, TypeTable),
    lookup_type_ctor_defn(TypeTable, TermTypeCtor, TermTypeDefn),
    hlds_data.get_type_defn_body(TermTypeDefn, TermTypeBody),
    (
        TermTypeBody = hlds_du_type(Ctors, _, _, _, _, _, _, _, _),
        FieldName = unqualify_name(FieldSymName),
        get_constructor_containing_field_loop(TermTypeCtor, Ctors, FieldName,
            ConsId, FieldNumber)
    ;
        ( TermTypeBody = hlds_eqv_type(_)
        ; TermTypeBody = hlds_foreign_type(_)
        ; TermTypeBody = hlds_solver_type(_)
        ; TermTypeBody = hlds_abstract_type(_)
        ),
        unexpected($module, $pred, "not du type")
    ).

:- pred get_constructor_containing_field_loop(type_ctor::in,
    list(constructor)::in, string::in, cons_id::out, int::out) is det.

get_constructor_containing_field_loop(_, [], _, _, _) :-
    unexpected($module, $pred, "can't find field").
get_constructor_containing_field_loop(TypeCtor, [Ctor | Ctors],
        UnqualFieldName, ConsId, FieldNumber) :-
    Ctor = ctor(_, _, SymName, CtorArgs, Arity, _Ctxt),
    ( if
        search_for_named_field(CtorArgs, UnqualFieldName, 1, FieldNumberPrime)
    then
        ConsId = cons(SymName, Arity, TypeCtor),
        FieldNumber = FieldNumberPrime
    else
        get_constructor_containing_field_loop(TypeCtor, Ctors,
            UnqualFieldName, ConsId, FieldNumber)
    ).

:- pred search_for_named_field(list(constructor_arg)::in,
    string::in, int::in, int::out) is semidet.

search_for_named_field([CtorArg | CtorArgs], UnqualFieldName,
        CurFieldNumber, NamedFieldNumber) :-
    ( if
        CtorArg ^ arg_field_name = yes(ctor_field_name(ArgFieldName, _)),
        UnqualFieldName = unqualify_name(ArgFieldName)
    then
        NamedFieldNumber = CurFieldNumber
    else
        search_for_named_field(CtorArgs, UnqualFieldName,
            CurFieldNumber + 1, NamedFieldNumber)
    ).

%---------------------------------------------------------------------------%

:- pred create_pure_atomic_unification_with_nonlocals(prog_var::in,
    unify_rhs::in, hlds_goal_info::in, set_of_progvar::in, list(prog_var)::in,
    unify_context::in, hlds_goal::out) is det.

create_pure_atomic_unification_with_nonlocals(Var, RHS, OldGoalInfo,
        RestrictNonLocals, VarsList, UnifyContext, Goal) :-
    Context = goal_info_get_context(OldGoalInfo),
    GoalId = goal_info_get_goal_id(OldGoalInfo),
    UnifyContext = unify_context(UnifyMainContext, UnifySubContext),
    create_pure_atomic_complicated_unification(Var, RHS,
        Context, UnifyMainContext, UnifySubContext, Goal0),
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),

    % Compute the nonlocals of the goal.
    set_of_var.list_to_set(VarsList, NonLocals1),
    set_of_var.intersect(RestrictNonLocals, NonLocals1, NonLocals),
    goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),

    % Use the goal id from the original goal, so that the constraint_ids
    % will be as expected. (See the XXX comment near the definition of
    % constraint_id in hlds_data.m for more info.)
    goal_info_set_goal_id(GoalId, GoalInfo1, GoalInfo),
    Goal = hlds_goal(GoalExpr0, GoalInfo).

:- pred make_new_vars(list(mer_type)::in, list(prog_var)::out,
    vartypes::in, vartypes::out, prog_varset::in, prog_varset::out) is det.

make_new_vars(Types, Vars, !VarTypes, !VarSet) :-
    list.length(Types, NumVars),
    varset.new_vars(NumVars, Vars, !VarSet),
    vartypes_add_corresponding_lists(Vars, Types, !VarTypes).

:- pred make_new_var(mer_type::in, prog_var::out, vartypes::in, vartypes::out,
    prog_varset::in, prog_varset::out) is det.

make_new_var(Type, Var, !VarTypes, !VarSet) :-
    varset.new_var(Var, !VarSet),
    add_var_type(Var, Type, !VarTypes).

%---------------------------------------------------------------------------%
:- end_module check_hlds.resolve_unify_functor.
%---------------------------------------------------------------------------%

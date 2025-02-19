%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2014-2021, 2023-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck_unify_var_functor.m.
% Main author: fjh.
%
% This file contains the code to typecheck unifications of the form
% X = f(Y1, ..., Yn).
%
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck_unify_var_functor.
:- interface.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.typecheck_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred typecheck_unify_var_functor_builtin(unify_context::in,
    prog_context::in, prog_var::in, cons_id::in, builtin_type::in, string::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

:- pred typecheck_unify_var_functor_std(unify_context::in, prog_context::in,
    prog_var::in, cons_id::in, list(prog_var)::in, goal_id::in,
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module check_hlds.typecheck_error_undef.
:- import_module check_hlds.typecheck_error_unify.
:- import_module check_hlds.typecheck_error_util.
:- import_module check_hlds.typecheck_util.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_construct.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.vartypes.

:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

typecheck_unify_var_functor_builtin(UnifyContext, Context, LHSVar, ConsId,
        BuiltinType, BuiltinTypeName, TypeAssignSet0, TypeAssignSet, !Info) :-
    ( if BuiltinType = builtin_type_int(int_type_int) then
        typecheck_info_add_nosuffix_integer_var(LHSVar, !Info)
    else
        true
    ),
    ConsType = builtin_type(BuiltinType),
    list.foldl(
        type_assign_check_functor_type_builtin(ConsType, LHSVar),
        TypeAssignSet0, [], TypeAssignSet1),
    (
        TypeAssignSet1 = [_ | _],
        TypeAssignSet = TypeAssignSet1
    ;
        TypeAssignSet1 = [],
        % If we encountered an error, continue checking with the
        % original type assign set.
        TypeAssignSet = TypeAssignSet0,
        (
            TypeAssignSet0 = []
            % The error did not originate here, so generating an error
            % message here would be misleading.
        ;
            TypeAssignSet0 = [_ | _],
            varset.init(ConsTypeVarSet),
            ConsTypeInfo = cons_type_info(ConsTypeVarSet, [], ConsType, [],
                empty_hlds_constraints, source_builtin_type(BuiltinTypeName)),
            ConsIdSpec = report_error_unify_var_functor_result(!.Info,
                UnifyContext, Context, LHSVar, [ConsTypeInfo],
                ConsId, 0, TypeAssignSet0),
            typecheck_info_add_error(ConsIdSpec, !Info)
        )
    ).

:- pred type_assign_check_functor_type_builtin(mer_type::in,
    prog_var::in, type_assign::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assign_check_functor_type_builtin(ConsType, Y, TypeAssign0,
        !TypeAssignSet) :-
    % Unify the type of Var with the type of the constructor.
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    search_insert_var_type(Y, ConsType, MaybeTypeY, VarTypes0, VarTypes),
    (
        MaybeTypeY = yes(TypeY),
        ( if
            type_assign_unify_type(ConsType, TypeY, TypeAssign0, TypeAssign)
        then
            % The constraints are empty here because none are added by
            % unification with a functor.
            !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
        else
            true
        )
    ;
        MaybeTypeY = no,
        % The constraints are empty here because none are added by
        % unification with a functor.
        type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
        !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

typecheck_unify_var_functor_std(UnifyContext, Context, LHSVar, ConsId, ArgVars,
        GoalId, TypeAssignSet0, TypeAssignSet, !Info) :-
    % Get the list of possible constructors that match this functor/arity.
    % If there aren't any, report an undefined constructor error.
    list.length(ArgVars, Arity),
    typecheck_info_get_ctor_list(!.Info, ConsId, Arity, GoalId,
        ConsInfoResult),
    (
        ConsInfoResult = cons_info_non_du_ctor(ConsTypeInfo),
        typecheck_unify_var_functor_cons_infos(UnifyContext, Context, LHSVar,
            ConsId, Arity, ArgVars, [ConsTypeInfo],
            TypeAssignSet0, TypeAssignSet, !Info)
    ;
        ConsInfoResult = cons_info_du_ctor(DuCtor, ConsTypeInfos, ConsErrors),
        (
            ConsTypeInfos = [],
            TypeAssignSet = TypeAssignSet0,
            typecheck_info_get_error_clause_context(!.Info, ClauseContext),
            GoalContext = type_error_in_unify(UnifyContext),
            % Note that ConsErrors may be [], but the fact that there are
            % no ConsTypeInfos is itself an error.
            Spec = report_error_undef_du_ctor(ClauseContext, GoalContext,
                Context, DuCtor, ConsErrors),
            typecheck_info_add_error(Spec, !Info)
        ;
            ConsTypeInfos = [_ | _],
            typecheck_unify_var_functor_cons_infos(UnifyContext, Context,
                LHSVar, ConsId, Arity, ArgVars, ConsTypeInfos,
                TypeAssignSet0, TypeAssignSet, !Info)
        )
    ;
        ( ConsInfoResult = cons_info_field_access_func
        ; ConsInfoResult = cons_info_comp_gen_cons_id
        ),
        TypeAssignSet = TypeAssignSet0,
        typecheck_info_get_error_clause_context(!.Info, ClauseContext),
        GoalContext = type_error_in_unify(UnifyContext),
        Spec = report_error_undef_non_du_ctor(ClauseContext, GoalContext,
            Context, ConsId),
        typecheck_info_add_error(Spec, !Info)
    ).

:- pred typecheck_unify_var_functor_cons_infos(unify_context::in,
    prog_context::in, prog_var::in, cons_id::in, arity::in, list(prog_var)::in,
    list(cons_type_info)::in(non_empty_list),
    type_assign_set::in, type_assign_set::out,
    typecheck_info::in, typecheck_info::out) is det.

typecheck_unify_var_functor_cons_infos(UnifyContext, Context, LHSVar,
        ConsId, Arity, ArgVars, ConsTypeInfos,
        TypeAssignSet0, TypeAssignSet, !Info) :-
    (
        ConsTypeInfos = [_]
    ;
        ConsTypeInfos = [_, _ | _],
        Sources = list.map(project_cons_type_info_source, ConsTypeInfos),
        Symbol = overloaded_func(ConsId, Sources),
        typecheck_info_add_overloaded_symbol(Symbol, Context, !Info)
    ),

    % Produce the ConsTypeAssignSet, which is essentially the
    % cross-product of the ConsTypeInfos and the TypeAssignSet0.
    get_cons_type_assigns_for_cons_defns(ConsTypeInfos, TypeAssignSet0,
        [], ConsTypeAssignSet),
    ( if
        ConsTypeAssignSet = [],
        TypeAssignSet0 = [_ | _]
    then
        % This should never happen, since undefined ctors
        % should be caught by the check just above.
        unexpected($pred, "undefined cons?")
    else
        true
    ),

    % Check that the type of the functor matches the type of the
    % variable.
    typecheck_var_functor_types(LHSVar, ConsTypeAssignSet,
        [], ArgsTypeAssignSet),
    ( if
        ArgsTypeAssignSet = [],
        ConsTypeAssignSet = [_ | _]
    then
        ConsIdSpec = report_error_unify_var_functor_result(!.Info,
            UnifyContext, Context, LHSVar, ConsTypeInfos, ConsId, Arity,
            TypeAssignSet0),
        typecheck_info_add_error(ConsIdSpec, !Info)
    else
        true
    ),

    % Check that the type of the arguments of the functor matches
    % their expected type for this functor.
    typecheck_functor_arg_types(!.Info, ArgVars, ArgsTypeAssignSet,
        [], TypeAssignSet1),
    (
        TypeAssignSet1 = [_ | _],
        TypeAssignSet = TypeAssignSet1
    ;
        TypeAssignSet1 = [],
        % If we encountered an error, continue checking with the
        % original type assign set.
        TypeAssignSet = TypeAssignSet0,
        (
            ArgsTypeAssignSet = []
            % The error did not originate here, so generating an error
            % message here would be misleading.
        ;
            ArgsTypeAssignSet = [_ | _],
            ArgSpec = report_error_unify_var_functor_args(!.Info,
                UnifyContext, Context, LHSVar, ConsTypeInfos,
                ConsId, ArgVars, ArgsTypeAssignSet),
            typecheck_info_add_error(ArgSpec, !Info)
        )
    ).

%---------------------------------------------------------------------------%

:- type cons_info_result
    --->    cons_info_non_du_ctor(cons_type_info)
    ;       cons_info_du_ctor(du_ctor, list(cons_type_info), list(cons_error))
    ;       cons_info_field_access_func
    ;       cons_info_comp_gen_cons_id.

    % Note: changes here may require changes to
    %
    % - post_typecheck.resolve_unify_functor,
    % - intermod.module_qualify_unify_rhs,
    % - recompilation.usage.find_matching_constructors and
    % - recompilation.check.check_functor_ambiguities.
    %
:- pred typecheck_info_get_ctor_list(typecheck_info::in, cons_id::in, int::in,
    goal_id::in, cons_info_result::out) is det.

typecheck_info_get_ctor_list(Info, ConsId, Arity, GoalId, ConsInfoResult) :-
    typecheck_info_get_is_field_access_function(Info, IsFieldAccessFunc),
    ( if
        % If we are typechecking the clause added for a field access function
        % for which the user has supplied type or mode declarations, the goal
        % should only contain an application of the field access function,
        % not constructor applications or function calls. The clauses in
        % `.opt' files will already have been expanded into unifications.
        IsFieldAccessFunc = yes(PredStatus),
        PredStatus \= pred_status(status_opt_imported)
    then
        ( if
            ConsId = du_data_ctor(DuCtor),
            builtin_field_access_function_type(Info, GoalId,
                DuCtor, Arity, FieldAccessConsInfos)
        then
            split_cons_errors(FieldAccessConsInfos, ConsInfos, ConsErrors),
            ConsInfoResult = cons_info_du_ctor(DuCtor, ConsInfos, ConsErrors)
        else
            ConsInfoResult = cons_info_field_access_func
        )
    else
        typecheck_info_get_ctor_list_std(Info, ConsId, Arity, GoalId,
            ConsInfoResult)
    ).

:- pred typecheck_info_get_ctor_list_std(typecheck_info::in, cons_id::in,
    arity::in, goal_id::in, cons_info_result::out) is det.

typecheck_info_get_ctor_list_std(Info, ConsId, Arity, GoalId,
        ConsInfoResult) :-
    (
        (
            ConsId = some_int_const(IntConst),
            TypeName = type_name_of_int_const(IntConst)
        ;
            ConsId = float_const(_),
            TypeName = "float"
        ;
            ConsId = char_const(_),
            TypeName = "character"
        ;
            ConsId = string_const(_),
            TypeName = "string"
        ;
            ConsId = impl_defined_const(IDCKind),
            (
                ( IDCKind = idc_file
                ; IDCKind = idc_module
                ; IDCKind = idc_pred
                ; IDCKind = idc_grade
                ),
                TypeName = "string"
            ;
                IDCKind = idc_line,
                TypeName = "int"
            )
        ),
        typecheck_info_construct_builtin_cons_info(TypeName, ConsInfo),
        ConsInfoResult = cons_info_non_du_ctor(ConsInfo)
    ;
        ConsId = tuple_cons(_TupleArity),
        typecheck_info_construct_tuple_cons_info(Arity, ConsInfo),
        ConsInfoResult = cons_info_non_du_ctor(ConsInfo)
    ;
        ConsId = du_data_ctor(DuCtor),
        typecheck_info_get_ctor_list_du(Info, DuCtor, Arity, GoalId,
            ConsInfoResult)
    ;
        ( ConsId = base_typeclass_info_const(_, _, _, _)
        ; ConsId = closure_cons(_)
        ; ConsId = deep_profiling_proc_layout(_)
        ; ConsId = ground_term_const(_, _)
        ; ConsId = table_io_entry_desc(_)
        ; ConsId = tabling_info_const(_)
        ; ConsId = type_ctor_info_const(_, _, _)
        ; ConsId = type_info_cell_constructor(_)
        ; ConsId = type_info_const(_)
        ; ConsId = typeclass_info_cell_constructor
        ; ConsId = typeclass_info_const(_)
        ),
        % The compiler passes that can create these kinds of cons_ids
        % have not been run yet.
        ConsInfoResult = cons_info_comp_gen_cons_id
    ).

:- pred typecheck_info_get_ctor_list_du(typecheck_info::in, du_ctor::in,
    arity::in, goal_id::in, cons_info_result::out) is det.

typecheck_info_get_ctor_list_du(Info, DuCtor, Arity, GoalId, ConsInfoResult) :-
    typecheck_info_get_du_cons_ctor_list(Info, DuCtor, GoalId,
        DuConsInfos, DuConsErrors),

    % Check whether DuCtor is a field access function for which the user
    % has not supplied a declaration.
    ( if
        builtin_field_access_function_type(Info, GoalId, DuCtor, Arity,
            FieldAccessMaybeConsInfosPrime)
    then
        split_cons_errors(FieldAccessMaybeConsInfosPrime,
            FieldAccessConsInfos, FieldAccessConsErrors)
    else
        FieldAccessConsInfos = [],
        FieldAccessConsErrors = []
    ),

    % Check whether DuCtor is of a builtin type. It can be of only one
    % builtin type: "character".
    DuCtor = du_ctor(SymName, _Arity, _TypeCtor),
    ( if
        Arity = 0,
        SymName = unqualified(String),
        string.char_to_string(_, String)
    then
        typecheck_info_construct_builtin_cons_info("character", CharConsInfo),
        CharConsInfos = [CharConsInfo]
    else
        CharConsInfos = []
    ),

    % Check whether DuCtor is a tuple constructor.
    ( if SymName = unqualified("{}") then
        typecheck_info_construct_tuple_cons_info(Arity, TupleConsInfo),
        TupleConsInfos = [TupleConsInfo]
    else
        TupleConsInfos = []
    ),

    % Check whether DuCtor is the name of a predicate which takes at least
    % Arity arguments. If so, insert the resulting cons_type_info
    % at the start of the list.
    % XXX We insert it, but NOT at the start.
    builtin_pred_type(Info, DuCtor, Arity, GoalId, PredConsInfos),

    % Check for higher-order function calls.
    ( if builtin_apply_type(Info, DuCtor, Arity, ApplyConsInfosPrime) then
        ApplyConsInfos = ApplyConsInfosPrime
    else
        ApplyConsInfos = []
    ),

    ConsInfos = DuConsInfos ++ FieldAccessConsInfos ++
        CharConsInfos ++ TupleConsInfos ++ PredConsInfos ++ ApplyConsInfos,
    ConsErrors = DuConsErrors ++ FieldAccessConsErrors,
    ConsInfoResult = cons_info_du_ctor(DuCtor, ConsInfos, ConsErrors).

:- pred typecheck_info_construct_builtin_cons_info(string::in,
    cons_type_info::out) is det.

typecheck_info_construct_builtin_cons_info(BuiltinTypeName, ConsInfo) :-
    TypeCtor = type_ctor(unqualified(BuiltinTypeName), 0),
    construct_type(TypeCtor, [], ConsType),
    varset.init(ConsTypeVarSet),
    ConsInfo = cons_type_info(ConsTypeVarSet, [], ConsType, [],
        empty_hlds_constraints, source_builtin_type(BuiltinTypeName)).

:- pred typecheck_info_construct_tuple_cons_info(arity::in,
    cons_type_info::out) is det.

typecheck_info_construct_tuple_cons_info(TupleArity, ConsInfo) :-
    % Make some fresh type variables for the argument types. These have
    % kind `star' since there are values (namely the arguments of the
    % tuple constructor) which have these types.
    varset.init(TupleConsTypeVarSet0),
    varset.new_vars(TupleArity, TupleArgTVars,
        TupleConsTypeVarSet0, TupleConsTypeVarSet),
    var_list_to_type_list(map.init, TupleArgTVars, TupleArgTypes),

    TupleTypeCtor = type_ctor(unqualified("{}"), TupleArity),
    construct_type(TupleTypeCtor, TupleArgTypes, TupleConsType),

    % Tuples can't have existentially typed arguments.
    TupleExistQVars = [],
    ConsInfo = cons_type_info(TupleConsTypeVarSet, TupleExistQVars,
        TupleConsType, TupleArgTypes, empty_hlds_constraints,
        source_builtin_type("tuple")).

:- pred typecheck_info_get_du_cons_ctor_list(typecheck_info::in, du_ctor::in,
    goal_id::in, list(cons_type_info)::out, list(cons_error)::out) is det.

typecheck_info_get_du_cons_ctor_list(Info, DuCtor, GoalId,
        ConsInfos, ConsErrors) :-
    DuCtor = du_ctor(Name, Arity, ConsIdTypeCtor),
    typecheck_info_get_cons_table(Info, ConsTable),

    % Check if ConsId has been defined as a constructor in some
    % discriminated union type or types.
    ( if search_cons_table(ConsTable, DuCtor, ConsDefns) then
        convert_cons_defn_list(Info, GoalId, do_not_flip_constraints,
            DuCtor, ConsDefns, PlainConsInfos, PlainConsErrors)
    else
        PlainConsInfos = [],
        PlainConsErrors = []
    ),

    % For "existentially typed" functors, whether the functor is actually
    % existentially typed depends on whether it is used as a constructor
    % or as a deconstructor. As a constructor, it is universally typed,
    % but as a deconstructor, it is existentially typed. But type checking
    % and polymorphism need to know whether it is universally or
    % existentially quantified _before_ mode analysis has inferred
    % the mode of the unification. Therefore, we use a special syntax
    % for construction unifications with existentially quantified functors:
    % instead of just using the functor name (e.g. "Y = foo(X)",
    % the programmer must use the special functor name "new foo"
    % (e.g. "Y = 'new foo'(X)").
    %
    % Here we check for occurrences of functor names starting with "new ".
    % For these, we look up the original functor in the constructor symbol
    % table, and for any occurrences of that functor we flip the quantifiers
    % on the type definition (i.e. convert the existential quantifiers
    % and constraints into universal ones).

    ( if
        remove_new_prefix(Name, OrigName),
        OrigDuCtor = du_ctor(OrigName, Arity, ConsIdTypeCtor),
        search_cons_table(ConsTable, OrigDuCtor, ExistQConsDefns)
    then
        convert_cons_defn_list(Info, GoalId, flip_constraints_for_new,
            OrigDuCtor, ExistQConsDefns,
            UnivQuantConsInfos, UnivQuantConsErrors),
        ConsInfos = PlainConsInfos ++ UnivQuantConsInfos,
        ConsErrors = PlainConsErrors ++ UnivQuantConsErrors
    else
        ConsInfos = PlainConsInfos,
        ConsErrors = PlainConsErrors
    ).

    % Filter out the errors (they aren't actually reported as errors
    % unless there was no other matching constructor).
    %
:- pred split_cons_errors(list(maybe_cons_type_info)::in,
    list(cons_type_info)::out, list(cons_error)::out) is det.

split_cons_errors([], [], []).
split_cons_errors([MaybeConsInfo | MaybeConsInfos], Infos, Errors) :-
    split_cons_errors(MaybeConsInfos, InfosTail, ErrorsTail),
    (
        MaybeConsInfo = ok(ConsInfo),
        Infos = [ConsInfo | InfosTail],
        Errors = ErrorsTail
    ;
        MaybeConsInfo = error(ConsError),
        Infos = InfosTail,
        Errors = [ConsError | ErrorsTail]
    ).

%---------------------------------------------------------------------------%

:- type cons_constraints_action
    --->    do_not_flip_constraints
    ;       flip_constraints_for_new
    ;       flip_constraints_for_field_set.

:- pred convert_cons_defn_list(typecheck_info, goal_id,
    cons_constraints_action, du_ctor, list(hlds_cons_defn),
    list(cons_type_info), list(cons_error)).
:- mode convert_cons_defn_list(in, in, in(bound(do_not_flip_constraints)),
    in, in, out, out) is det.
:- mode convert_cons_defn_list(in, in, in(bound(flip_constraints_for_new)),
    in, in, out, out) is det.

convert_cons_defn_list(_Info, _GoalId, _Action, _DuCtor, [], [], []).
convert_cons_defn_list(Info, GoalId, Action, DuCtor,
        [ConsDefn | ConsDefns], ConsTypeInfos, ConsErrors) :-
    convert_cons_defn(Info, GoalId, Action, DuCtor, ConsDefn,
        HeadMaybeConsTypeInfo),
    convert_cons_defn_list(Info, GoalId, Action, DuCtor, ConsDefns,
        TailConsTypeInfos, TailConsErrors),
    (
        HeadMaybeConsTypeInfo = ok(HeadConsTypeInfo),
        ConsTypeInfos = [HeadConsTypeInfo | TailConsTypeInfos],
        ConsErrors = TailConsErrors
    ;
        HeadMaybeConsTypeInfo = error(HeadConsError),
        ConsTypeInfos = TailConsTypeInfos,
        ConsErrors = [HeadConsError | TailConsErrors]
    ).

:- pred convert_cons_defn(typecheck_info, goal_id,
    cons_constraints_action, du_ctor, hlds_cons_defn,
    maybe_cons_type_info).
:- mode convert_cons_defn(in, in, in(bound(do_not_flip_constraints)),
    in, in, out) is det.
:- mode convert_cons_defn(in, in, in(bound(flip_constraints_for_field_set)),
    in, in, out) is det.
:- mode convert_cons_defn(in, in, in,
    in, in, out) is det.
% The last mode should be
%
% :- mode convert_cons_defn(in, in, in(bound(flip_constraints_for_new)),
%     in, in, out) is det.
%
% However, as of 2024 03 04, this generates a spurious mode error:
%
%    In clause for `convert_cons_defn(in, in,
%      in(bound(flip_constraints_for_new)), in, in, out)':
%      mode mismatch in disjunction.
%      The variable `ExistQVars0' is ground in some
%      branches but not others.
%        In this branch, `ExistQVars0' is free.
%        In this branch, `ExistQVars0' is ground.

convert_cons_defn(Info, GoalId, Action, DuCtor, ConsDefn, ConsTypeInfo) :-
    % XXX We should investigate whether the job done by this predicate
    % on demand and therefore possibly lots of times for the same type,
    % would be better done just once, either by invoking it (at least with
    % Action = do_not_flip_constraints) before type checking even starts and
    % recording the result, or by putting the result into the ConsDefn
    % or some related data structure.

    ConsDefn = hlds_cons_defn(TypeCtor, ConsTypeVarSet, ConsTypeParams,
        ConsTypeKinds, MaybeExistConstraints, Args, _),
    ArgTypes = list.map(func(C) = C ^ arg_type, Args),
    typecheck_info_get_type_table(Info, TypeTable),
    lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, Body),

    % If this type has `:- pragma foreign_type' declarations, we can only use
    % its constructors in predicates which have foreign clauses and in the
    % unification and comparison predicates for the type (otherwise the code
    % wouldn't compile when using a back-end which caused another version
    % of the type to be selected). The constructors may also appear in the
    % automatically generated unification and comparison predicates.
    %
    % XXX This check isn't quite right -- we really need to check for
    % each procedure that there is a foreign_proc declaration for all
    % languages for which this type has a foreign_type declaration, but
    % this will do for now. Such a check may be difficult because by
    % this point we have thrown away the clauses which we are not using
    % in the current compilation.
    %
    % The `.opt' files don't contain the foreign clauses from the source
    % file that are not used when compiling in the current grade, so we
    % allow foreign type constructors in `opt_imported' predicates even
    % if there are no foreign clauses. Errors will be caught when creating
    % the `.opt' file.

    typecheck_info_get_pred_id(Info, PredId),
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_status(PredInfo, PredStatus),
    ( if
        Body = hlds_du_type(BodyDu),
        BodyDu ^ du_type_is_foreign_type = yes(_),
        pred_info_get_goal_type(PredInfo, GoalType),
        GoalType \= goal_not_for_promise(np_goal_type_clause_and_foreign),
        not is_unify_index_or_compare_pred(PredInfo),
        PredStatus \= pred_status(status_opt_imported)
    then
        ConsTypeInfo =
            error(other_lang_foreign_type_constructor(TypeCtor, TypeDefn))
    else if
        % Do not allow constructors for abstract_imported types unless
        % the current predicate is opt_imported.
        hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
        TypeStatus = type_status(status_abstract_imported),
        not is_unify_index_or_compare_pred(PredInfo),
        PredStatus \= pred_status(status_opt_imported)
    then
        ConsTypeInfo = error(abstract_imported_type)
    else if
        Action = flip_constraints_for_new,
        MaybeExistConstraints = no_exist_constraints
    then
        % Do not allow 'new' constructors except on existential types.
        ConsTypeInfo = error(new_on_non_existential_type(TypeCtor))
    else
        prog_type.var_list_to_type_list(ConsTypeKinds, ConsTypeParams,
            ConsTypeArgs),
        construct_type(TypeCtor, ConsTypeArgs, ConsType),
        UnivProgConstraints = [],
        (
            MaybeExistConstraints = no_exist_constraints,
            ExistQVars0 = [],
            ExistProgConstraints = []
        ;
            MaybeExistConstraints = exist_constraints(ExistConstraints),
            ExistConstraints = cons_exist_constraints(ExistQVars0,
                ExistProgConstraints, _, _)
        ),
        (
            Action = do_not_flip_constraints,
            ProgConstraints = univ_exist_constraints(UnivProgConstraints,
                ExistProgConstraints),
            ExistQVars = ExistQVars0
        ;
            Action = flip_constraints_for_new,
            % Make the existential constraints into universal ones, and discard
            % the existentially quantified variables (since they are now
            % universally quantified).
            ProgConstraints = univ_exist_constraints(ExistProgConstraints,
                UnivProgConstraints),
            ExistQVars = []
        ;
            Action = flip_constraints_for_field_set,
            % The constraints are existential for the deconstruction, and
            % universal for the construction. Even though all of the unproven
            % constraints here can be trivially reduced by the assumed ones,
            % we still need to process them so that the appropriate tables
            % get updated.
            ProgConstraints = univ_exist_constraints(ExistProgConstraints,
                ExistProgConstraints),
            ExistQVars = ExistQVars0
        ),
        module_info_get_class_table(ModuleInfo, ClassTable),
        make_body_hlds_constraints(ClassTable, ConsTypeVarSet,
            GoalId, ProgConstraints, Constraints),
        ConsTypeInfo = ok(cons_type_info(ConsTypeVarSet, ExistQVars, ConsType,
            ArgTypes, Constraints, source_type(TypeCtor, DuCtor)))
    ).

%---------------------------------------------------------------------------%

:- type cons_type_assign
    --->    cons_type_assign(
                type_assign,
                mer_type,
                list(mer_type),
                cons_type_info_source
            ).

:- type cons_type_assign_set == list(cons_type_assign).

    % typecheck_unify_var_functor_get_ctors_for_type_assigns(ConsTypeInfos,
    %   TypeAssignSet, !ConsTypeAssignSet):
    %
    % Iterate over all the different possible pairings of all the
    % constructor definitions and all the type assignments.
    % For each constructor definition in `ConsTypeInfos' and type assignment
    % in `TypeAssignSet', produce a pair
    %
    %   TypeAssign - cons_type(Type, ArgTypes)
    %
    % where `cons_type(Type, ArgTypes)' records one of the possible types for
    % the constructor in `ConsTypeInfos', and where `TypeAssign' is the type
    % assignment renamed apart from the types of the constructors.
    %
    % This predicate iterates over the cons_type_infos;
    % get_cons_type_assigns_for_cons_defn iterates over the type_assigns.
    %
:- pred get_cons_type_assigns_for_cons_defns(list(cons_type_info)::in,
    type_assign_set::in,
    cons_type_assign_set::in, cons_type_assign_set::out) is det.

get_cons_type_assigns_for_cons_defns([], _, !ConsTypeAssignSet).
get_cons_type_assigns_for_cons_defns([ConsTypeInfo | ConsTypeInfos],
        TypeAssigns, !ConsTypeAssignSet) :-
    get_cons_type_assigns_for_cons_defn(ConsTypeInfo, TypeAssigns,
        !ConsTypeAssignSet),
    get_cons_type_assigns_for_cons_defns(ConsTypeInfos, TypeAssigns,
        !ConsTypeAssignSet).

:- pred get_cons_type_assigns_for_cons_defn(cons_type_info::in,
    type_assign_set::in,
    cons_type_assign_set::in, cons_type_assign_set::out) is det.

get_cons_type_assigns_for_cons_defn(_, [], !ConsTypeAssignSet).
get_cons_type_assigns_for_cons_defn(ConsTypeInfo, [TypeAssign | TypeAssigns],
        !ConsTypeAssignSet) :-
    get_cons_type_assign(ConsTypeInfo, TypeAssign, ConsTypeAssign),
    !:ConsTypeAssignSet = [ConsTypeAssign | !.ConsTypeAssignSet],
    get_cons_type_assigns_for_cons_defn(ConsTypeInfo, TypeAssigns,
        !ConsTypeAssignSet).

    % Given an cons_type_info, construct a type for the constructor
    % and a list of types of the arguments, suitably renamed apart
    % from the current type_assign's typevarset. Return them in a
    % cons_type_assign with the updated-for-the-renaming type_assign.
    %
:- pred get_cons_type_assign(cons_type_info::in, type_assign::in,
    cons_type_assign::out) is det.

get_cons_type_assign(ConsTypeInfo, TypeAssign0, ConsTypeAssign) :-
    ConsTypeInfo = cons_type_info(ConsTypeVarSet, ConsExistQVars0,
        ConsType0, ArgTypes0, ClassConstraints0, Source),

    % Rename apart the type vars in the type of the constructor
    % and the types of its arguments.
    % (Optimize the common case of a non-polymorphic type.)
    ( if
        varset.is_empty(ConsTypeVarSet)
    then
        ConsType = ConsType0,
        ArgTypes = ArgTypes0,
        TypeAssign2 = TypeAssign0,
        ConstraintsToAdd = ClassConstraints0
    else if
        type_assign_rename_apart(TypeAssign0, ConsTypeVarSet,
            [ConsType0 | ArgTypes0], TypeAssign1, [ConsType1 | ArgTypes1],
            Renaming)
    then
        apply_variable_renaming_to_tvar_list(Renaming,
            ConsExistQVars0, ConsExistQVars),
        apply_variable_renaming_to_constraints(Renaming,
            ClassConstraints0, ConstraintsToAdd),
        type_assign_get_existq_tvars(TypeAssign1, ExistQTVars0),
        ExistQTVars = ConsExistQVars ++ ExistQTVars0,
        type_assign_set_existq_tvars(ExistQTVars, TypeAssign1, TypeAssign2),

        ConsType = ConsType1,
        ArgTypes = ArgTypes1
    else
        unexpected($pred, "type_assign_rename_apart failed")
    ),

    % Add the constraints for this functor to the current constraint set.
    % Note that there can still be (ground) constraints even if the varset
    % is empty.
    %
    % For functors which are data constructors, the fact that we don't take
    % the dual corresponds to assuming that they will be used as deconstructors
    % rather than as constructors.
    type_assign_get_typeclass_constraints(TypeAssign2, OldConstraints),
    merge_hlds_constraints(ConstraintsToAdd, OldConstraints, ClassConstraints),
    type_assign_set_typeclass_constraints(ClassConstraints,
        TypeAssign2, TypeAssign),
    ConsTypeAssign = cons_type_assign(TypeAssign, ConsType, ArgTypes, Source).

%---------------------------------------------------------------------------%

    % typecheck_var_functor_type(Var, ConsTypeAssignSet, !ArgsTypeAssignSet):
    %
    % For each possible cons type assignment in `ConsTypeAssignSet',
    % for each possible constructor type,
    % check that the type of `Var' matches this type.
    % If it does, add the type binding to !ArgsTypeAssignSet.
    %
:- pred typecheck_var_functor_types(prog_var::in, cons_type_assign_set::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

typecheck_var_functor_types(_, [], !ArgsTypeAssignSet).
typecheck_var_functor_types(Var, [ConsTypeAssign | ConsTypeAssigns],
        !ArgsTypeAssignSet) :-
    typecheck_var_functor_type(Var, ConsTypeAssign, !ArgsTypeAssignSet),
    typecheck_var_functor_types(Var, ConsTypeAssigns, !ArgsTypeAssignSet).

:- pred typecheck_var_functor_type(prog_var::in, cons_type_assign::in,
    args_type_assign_set::in, args_type_assign_set::out) is det.

typecheck_var_functor_type(Var, ConsTypeAssign0, !ArgsTypeAssignSet) :-
    ConsTypeAssign0 = cons_type_assign(TypeAssign0, ConsType, ConsArgTypes,
        Source0),

    % Unify the type of Var with the type of the constructor.
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    search_insert_var_type(Var, ConsType, MaybeOldVarType,
        VarTypes0, VarTypes),
    (
        MaybeOldVarType = yes(OldVarType),
        % VarTypes wasn't updated, so don't need to update its containing
        % type assign either.
        ( if
            type_assign_unify_type(ConsType, OldVarType,
                TypeAssign0, TypeAssign)
        then
            % The constraints are empty here because none are added by
            % unification with a functor.
            ArgsTypeAssign = args_type_assign(TypeAssign,
                ConsArgTypes, empty_hlds_constraints, atas_cons(Source0)),
            !:ArgsTypeAssignSet = [ArgsTypeAssign | !.ArgsTypeAssignSet]
        else
            true
        )
    ;
        MaybeOldVarType = no,
        type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
        % The constraints are empty here because none are added by
        % unification with a functor.
        ArgsTypeAssign = args_type_assign(TypeAssign,
            ConsArgTypes, empty_hlds_constraints, atas_cons(Source0)),
        !:ArgsTypeAssignSet = [ArgsTypeAssign | !.ArgsTypeAssignSet]
    ).

%---------------------------------------------------------------------------%

    % typecheck_functor_arg_types(Info, ArgVars, ArgsTypeAssigns, ...):
    %
    % For each possible cons type assignment in `ConsTypeAssignSet',
    % for each possible constructor argument types,
    % check that the types of `ArgVars' match these types.
    %
:- pred typecheck_functor_arg_types(typecheck_info::in, list(prog_var)::in,
    args_type_assign_set::in,
    type_assign_set::in, type_assign_set::out) is det.

typecheck_functor_arg_types(_, _, [], !TypeAssignSet).
typecheck_functor_arg_types(Info, ArgVars, [ArgsTypeAssign | ArgsTypeAssigns],
        !TypeAssignSet) :-
    ArgsTypeAssign = args_type_assign(TypeAssign, ArgTypes, _, _),
    type_assign_vars_have_types(Info, TypeAssign, ArgVars, ArgTypes,
        !TypeAssignSet),
    typecheck_functor_arg_types(Info, ArgVars, ArgsTypeAssigns,
        !TypeAssignSet).

    % type_assign_vars_have_types(Info, TypeAssign, ArgVars, Types,
    %   TypeAssignSet0, TypeAssignSet):
    % Let TAs = { TA | TA is an extension of TypeAssign for which
    %   the types of the ArgVars unify with their respective Types },
    % list.append(TAs, TypeAssignSet0, TypeAssignSet).
    %
:- pred type_assign_vars_have_types(typecheck_info::in, type_assign::in,
    list(prog_var)::in, list(mer_type)::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assign_vars_have_types(_, TypeAssign, [], [],
        TypeAssignSet, [TypeAssign | TypeAssignSet]).
type_assign_vars_have_types(_, _, [], [_ | _], _, _) :-
    unexpected($pred, "length mismatch").
type_assign_vars_have_types(_, _, [_ | _], [], _, _) :-
    unexpected($pred, "length mismatch").
type_assign_vars_have_types(Info, TypeAssign0,
        [ArgVar | ArgVars], [Type | Types], TypeAssignSet0, TypeAssignSet) :-
    type_assign_var_has_type(TypeAssign0, ArgVar, Type, [], TypeAssignSet1),
    type_assigns_vars_have_types(Info, TypeAssignSet1,
        ArgVars, Types, TypeAssignSet0, TypeAssignSet).

    % type_assigns_vars_have_types(Info, TypeAssigns, ArgVars, Types,
    %       TypeAssignSet0, TypeAssignSet):
    % Let TAs = { TA | TA is an extension of a member of TypeAssigns for which
    %   the types of the ArgVars unify with their respective Types },
    % list.append(TAs, TypeAssignSet0, TypeAssignSet).
    %
:- pred type_assigns_vars_have_types(typecheck_info::in,
    type_assign_set::in, list(prog_var)::in, list(mer_type)::in,
    type_assign_set::in, type_assign_set::out) is det.

type_assigns_vars_have_types(_, [], _, _, !TypeAssignSet).
type_assigns_vars_have_types(Info, [TypeAssign | TypeAssigns],
        ArgVars, Types, !TypeAssignSet) :-
    type_assign_vars_have_types(Info, TypeAssign, ArgVars, Types,
        !TypeAssignSet),
    type_assigns_vars_have_types(Info, TypeAssigns, ArgVars, Types,
        !TypeAssignSet).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % builtin_apply_type(Info, DuCtor, Arity, ConsTypeInfos):
    %
    % Succeed if DuCtor is the builtin apply/N or ''/N (N>=2),
    % which is used to invoke higher-order functions.
    % If so, bind ConsTypeInfos to a singleton list containing
    % the appropriate type for apply/N of the specified Arity.
    %
:- pred builtin_apply_type(typecheck_info::in, du_ctor::in, int::in,
    list(cons_type_info)::out) is semidet.

builtin_apply_type(_Info, DuCtor, Arity, ConsTypeInfos) :-
    DuCtor = du_ctor(unqualified(ApplyName), _, _),
    % XXX FIXME handle impure apply/N more elegantly (e.g. nicer syntax)
    (
        ApplyName = "apply",
        ApplyNameToUse = ApplyName,
        Purity = purity_pure
    ;
        ApplyName = "",
        ApplyNameToUse = "apply",
        Purity = purity_pure
    ;
        ApplyName = "impure_apply",
        ApplyNameToUse = ApplyName,
        Purity = purity_impure
    ;
        ApplyName = "semipure_apply",
        ApplyNameToUse = ApplyName,
        Purity = purity_semipure
    ),
    Arity >= 1,
    Arity1 = Arity - 1,
    higher_order_func_type(Purity, Arity1, TypeVarSet, FuncType,
        ArgTypes, RetType),
    ExistQVars = [],
    ConsTypeInfos = [cons_type_info(TypeVarSet, ExistQVars, RetType,
        [FuncType | ArgTypes], empty_hlds_constraints,
        source_apply(ApplyNameToUse))].

%---------------------%

    % builtin_field_access_function_type(Info, GoalId, DuCtor,
    %   Arity, ConsTypeInfos):
    %
    % Succeed if DuCtor is the name of one the automatically generated
    % field access functions (fieldname, '<fieldname> :=').
    %
:- pred builtin_field_access_function_type(typecheck_info::in, goal_id::in,
    du_ctor::in, arity::in, list(maybe_cons_type_info)::out) is semidet.

builtin_field_access_function_type(Info, GoalId, DuCtor, Arity,
        MaybeConsTypeInfos) :-
    % Taking the address of automatically generated field access functions
    % is not allowed, so currying does have to be considered here.
    % XXX zs: shouldn't that be "does NOT have to be considered"?
    DuCtor = du_ctor(Name, Arity, _),
    typecheck_info_get_module_info(Info, ModuleInfo),
    is_field_access_function_name(ModuleInfo, Name, Arity, AccessType,
        FieldName),

    module_info_get_ctor_field_table(ModuleInfo, CtorFieldTable),
    map.search(CtorFieldTable, FieldName, FieldDefns),

    UserArity = user_arity(Arity),
    list.filter_map(
        make_field_access_function_cons_type_info(Info, GoalId, Name,
            UserArity, AccessType, FieldName),
        FieldDefns, MaybeConsTypeInfos).

:- pred is_field_access_function_for_type_ctor(module_info::in,
    field_access_type::in, type_ctor::in, pred_id::in) is semidet.

is_field_access_function_for_type_ctor(ModuleInfo, AccessType, TypeCtor,
        PredId) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    require_complete_switch [AccessType]
    (
        AccessType = get,
        ArgTypes = [ArgType, _ResultType],
        type_to_ctor(ArgType, TypeCtor)
    ;
        AccessType = set,
        ArgTypes = [ArgType, _FieldType, ResultType],
        type_to_ctor(ArgType, TypeCtor),
        type_to_ctor(ResultType, TypeCtor)
    ).

:- pred make_field_access_function_cons_type_info(typecheck_info::in,
    goal_id::in, sym_name::in, user_arity::in, field_access_type::in,
    sym_name::in, hlds_ctor_field_defn::in,
    maybe_cons_type_info::out) is semidet.

make_field_access_function_cons_type_info(Info, GoalId, FuncName, UserArity,
        AccessType, FieldName, FieldDefn, ConsTypeInfo) :-
    get_field_access_constructor(Info, GoalId, FuncName, UserArity,
        AccessType, FieldDefn, OrigExistTVars, MaybeFunctorConsTypeInfo),
    (
        MaybeFunctorConsTypeInfo = ok(FunctorConsTypeInfo),
        typecheck_info_get_module_info(Info, ModuleInfo),
        module_info_get_class_table(ModuleInfo, ClassTable),
        convert_field_access_cons_type_info(ClassTable, AccessType,
            FieldName, FieldDefn, FunctorConsTypeInfo,
            OrigExistTVars, ConsTypeInfo)
    ;
        MaybeFunctorConsTypeInfo = error(_),
        ConsTypeInfo = MaybeFunctorConsTypeInfo
    ).

:- pred get_field_access_constructor(typecheck_info::in, goal_id::in,
    sym_name::in, user_arity::in, field_access_type::in,
    hlds_ctor_field_defn::in,
    existq_tvars::out, maybe_cons_type_info::out) is semidet.

get_field_access_constructor(Info, GoalId, FuncName, UserArity, AccessType,
        FieldDefn, OrigExistTVars, FunctorConsTypeInfo) :-
    FieldDefn = hlds_ctor_field_defn(_, _, TypeCtor, DuCtor, _),
    TypeCtor = type_ctor(qualified(TypeModule, _), _),

    % If the user has supplied a declaration for a field access function
    % of the same name and arity, operating on the same type constructor,
    % we use that instead of the automatically generated version,
    % unless we are typechecking the clause introduced for the
    % user-supplied declaration itself.
    % The user-declared version will be picked up by builtin_pred_type.
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_predicate_table(ModuleInfo, PredTable),
    UnqualFuncName = unqualify_name(FuncName),
    typecheck_info_get_is_field_access_function(Info, IsFieldAccessFunc),
    (
        IsFieldAccessFunc = no,
        predicate_table_lookup_func_m_n_a(PredTable, is_fully_qualified,
            TypeModule, UnqualFuncName, UserArity, PredIds),
        list.all_false(
            is_field_access_function_for_type_ctor(ModuleInfo, AccessType,
                TypeCtor),
            PredIds)
    ;
        IsFieldAccessFunc = yes(_)
    ),
    module_info_get_cons_table(ModuleInfo, ConsTable),
    lookup_cons_table_of_type_ctor(ConsTable, TypeCtor, DuCtor, ConsDefn),
    MaybeExistConstraints = ConsDefn ^ cons_maybe_exist,
    (
        MaybeExistConstraints = no_exist_constraints,
        OrigExistTVars = []
    ;
        MaybeExistConstraints = exist_constraints(ExistConstraints),
        ExistConstraints = cons_exist_constraints(OrigExistTVars, _, _, _)
    ),
    (
        AccessType = get,
        ConsAction = do_not_flip_constraints,
        convert_cons_defn(Info, GoalId, ConsAction, DuCtor, ConsDefn,
            FunctorConsTypeInfo)
    ;
        AccessType = set,
        ConsAction = flip_constraints_for_field_set,
        convert_cons_defn(Info, GoalId, ConsAction, DuCtor, ConsDefn,
            FunctorConsTypeInfo)
    ).

:- type maybe_cons_type_info
    --->    ok(cons_type_info)
    ;       error(cons_error).

:- pred convert_field_access_cons_type_info(class_table::in,
    field_access_type::in, sym_name::in, hlds_ctor_field_defn::in,
    cons_type_info::in, existq_tvars::in, maybe_cons_type_info::out) is det.

convert_field_access_cons_type_info(ClassTable, AccessType, FieldSymName,
        FieldDefn, FunctorConsTypeInfo, OrigExistTVars, ConsTypeInfo) :-
    FunctorConsTypeInfo = cons_type_info(TVarSet0, ExistQVars,
        FunctorType, ConsArgTypes, Constraints0, Source0),
    (
        Source0 = source_type(SourceType, ConsId)
    ;
        ( Source0 = source_builtin_type(_)
        ; Source0 = source_field_access(_, _, _, _)
        ; Source0 = source_apply(_)
        ; Source0 = source_pred(_)
        ),
        unexpected($pred, "not type")
    ),
    FieldDefn = hlds_ctor_field_defn(_, _, _, _, FieldNumber),
    list.det_index1(ConsArgTypes, FieldNumber, FieldType),
    FieldName = unqualify_name(FieldSymName),
    (
        AccessType = get,
        Source = source_field_access(get, SourceType, ConsId, FieldName),
        RetType = FieldType,
        ArgTypes = [FunctorType],
        ConsTypeInfo = ok(cons_type_info(TVarSet0, ExistQVars,
            RetType, ArgTypes, Constraints0, Source))
    ;
        AccessType = set,
        Source = source_field_access(set, SourceType, ConsId, FieldName),

        % When setting a polymorphic field, the type of the field in the result
        % is not necessarily the same as in the input. If a type variable
        % occurs only in the field being set, create a new type variable for it
        % in the result type.
        %
        % This allows code such as
        % :- type pair(T, U)
        %   ---> '-'(fst::T, snd::U).
        %
        %   Pair0 = 1 - 'a',
        %   Pair = Pair0 ^ snd := 2.

        type_vars_in_type(FieldType, TVarsInField),
        % Most of the time, TVarsInField is [], so provide a fast path
        % for this case.
        (
            TVarsInField = [],
            RetType = FunctorType,
            ArgTypes = [FunctorType, FieldType],
            % None of the constraints are affected by the updated field,
            % so the constraints are unchanged.
            ConsTypeInfo = ok(cons_type_info(TVarSet0, ExistQVars,
                RetType, ArgTypes, Constraints0, Source))
        ;
            TVarsInField = [_ | _],

            % XXX This demonstrates a problem - if a type variable occurs
            % in the types of multiple fields, any predicates changing values
            % of one of these fields cannot change their types. This is
            % especially a problem for existentially typed fields, because
            % setting the field always changes the type.
            %
            % Haskell gets around this problem by allowing multiple fields
            % to be set by the same expression. Haskell doesn't handle all
            % cases -- it is not possible to get multiple existentially typed
            % fields using record syntax and pass them to a function whose type
            % requires that the fields are of the same type. It probably won't
            % come up too often.
            %
            list.det_replace_nth(ConsArgTypes, FieldNumber, int_type,
                ArgTypesWithoutField),
            type_vars_in_types(ArgTypesWithoutField, TVarsInOtherArgs),
            set.intersect(
                set.list_to_set(TVarsInField),
                set.intersect(
                    set.list_to_set(TVarsInOtherArgs),
                    set.list_to_set(OrigExistTVars)
                ),
                ExistQVarsInFieldAndOthers),
            ( if set.is_empty(ExistQVarsInFieldAndOthers) then
                % Rename apart type variables occurring only in the field
                % to be replaced - the values of those type variables will be
                % supplied by the replacement field value.
                list.delete_elems(TVarsInField,
                    TVarsInOtherArgs, TVarsOnlyInField0),
                list.sort_and_remove_dups(TVarsOnlyInField0, TVarsOnlyInField),
                list.length(TVarsOnlyInField, NumNewTVars),
                varset.new_vars(NumNewTVars, NewTVars, TVarSet0, TVarSet),
                map.from_corresponding_lists(TVarsOnlyInField,
                    NewTVars, TVarRenaming),
                apply_variable_renaming_to_type(TVarRenaming, FieldType,
                    RenamedFieldType),
                apply_variable_renaming_to_type(TVarRenaming, FunctorType,
                    OutputFunctorType),
                % Rename the class constraints, projecting the constraints
                % onto the set of type variables occurring in the types of the
                % arguments of the call to `'field :='/2'. Note that we have
                % already flipped the constraints.
                type_vars_in_types([FunctorType, FieldType], CallTVars0),
                set.list_to_set(CallTVars0, CallTVars),
                project_and_rename_constraints(ClassTable, TVarSet, CallTVars,
                    TVarRenaming, Constraints0, Constraints),
                RetType = OutputFunctorType,
                ArgTypes = [FunctorType, RenamedFieldType],
                ConsTypeInfo = ok(cons_type_info(TVarSet, ExistQVars,
                    RetType, ArgTypes, Constraints, Source))
            else
                % This field cannot be set. Pass out some information so that
                % we can give a better error message. Errors involving changing
                % the types of universally quantified type variables will be
                % caught by typecheck_functor_arg_types.
                set.to_sorted_list(ExistQVarsInFieldAndOthers,
                    ExistQVarsInFieldAndOthers1),
                ConsTypeInfo = error(invalid_field_update(FieldSymName,
                    FieldDefn, TVarSet0, ExistQVarsInFieldAndOthers1))
            )
        )
    ).

%---------------------%

    % builtin_pred_type(Info, DuCtor, Arity, GoalId, PredConsInfoList):
    %
    % If DuCtor/Arity is a constant of a pred type, instantiates
    % the output parameters, otherwise fails.
    %
    % Instantiates PredConsInfoList to the set of cons_type_info structures
    % for each predicate with name `DuCtor' and arity greater than or equal to
    % Arity. GoalId is used to identify any constraints introduced.
    %
    % For example, functor `map.search/1' has type `pred(K, V)'
    % (hence PredTypeParams = [K, V]) and argument types [map(K, V)].
    %
:- pred builtin_pred_type(typecheck_info::in, du_ctor::in, int::in,
    goal_id::in, list(cons_type_info)::out) is det.

builtin_pred_type(Info, DuCtor, Arity, GoalId, ConsTypeInfos) :-
    DuCtor = du_ctor(SymName, _, _),
    typecheck_info_get_predicate_table(Info, PredicateTable),
    typecheck_info_get_calls_are_fully_qualified(Info, IsFullyQualified),
    predicate_table_lookup_sym(PredicateTable, IsFullyQualified, SymName,
        PredIds),
    (
        PredIds = [_ | _],
        predicate_table_get_pred_id_table(PredicateTable, PredIdTable),
        accumulate_cons_type_infos_for_pred_ids(Info, PredIdTable, GoalId,
            PredIds, Arity, [], ConsTypeInfos)
    ;
        PredIds = [],
        ConsTypeInfos = []
    ).

:- pred accumulate_cons_type_infos_for_pred_ids(typecheck_info::in,
    pred_id_table::in, goal_id::in, list(pred_id)::in, int::in,
    list(cons_type_info)::in, list(cons_type_info)::out) is det.

accumulate_cons_type_infos_for_pred_ids(_, _, _, [], _, !ConsTypeInfos).
accumulate_cons_type_infos_for_pred_ids(Info, PredTable, GoalId,
        [PredId | PredIds], Arity, !ConsTypeInfos) :-
    accumulate_cons_type_infos_for_pred_id(Info, PredTable, GoalId,
        PredId, Arity, !ConsTypeInfos),
    accumulate_cons_type_infos_for_pred_ids(Info, PredTable, GoalId,
        PredIds, Arity, !ConsTypeInfos).

:- pred accumulate_cons_type_infos_for_pred_id(typecheck_info::in,
    pred_id_table::in, goal_id::in, pred_id::in, int::in,
    list(cons_type_info)::in, list(cons_type_info)::out) is det.

accumulate_cons_type_infos_for_pred_id(Info, PredTable, GoalId,
        PredId, FuncArity, !ConsTypeInfos) :-
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_class_table(ModuleInfo, ClassTable),
    map.lookup(PredTable, PredId, PredInfo),
    pred_info_get_orig_arity(PredInfo, pred_form_arity(PredFormArityInt)),
    pred_info_get_is_pred_or_func(PredInfo, IsPredOrFunc),
    pred_info_get_class_context(PredInfo, PredClassContext),
    pred_info_get_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
        CompleteArgTypes),
    pred_info_get_purity(PredInfo, Purity),
    ( if
        IsPredOrFunc = pf_predicate,
        PredFormArityInt >= FuncArity,
        % We don't support first-class polymorphism, so you can't take the
        % address of an existentially quantified predicate.
        PredExistQVars = []
    then
        list.det_split_list(FuncArity, CompleteArgTypes,
            ArgTypes, PredTypeParams),
        construct_higher_order_pred_type(Purity, PredTypeParams, PredType),
        make_body_hlds_constraints(ClassTable, PredTypeVarSet,
            GoalId, PredClassContext, PredConstraints),
        ConsTypeInfo = cons_type_info(PredTypeVarSet, PredExistQVars,
            PredType, ArgTypes, PredConstraints, source_pred(PredId)),
        !:ConsTypeInfos = [ConsTypeInfo | !.ConsTypeInfos]
    else if
        IsPredOrFunc = pf_function,
        PredAsFuncArity = PredFormArityInt - 1,
        PredAsFuncArity >= FuncArity,
        % We don't support first-class polymorphism, so you can't take
        % the address of an existentially quantified function. You can however
        % call such a function, so long as you pass *all* the parameters.
        ( PredExistQVars = []
        ; PredAsFuncArity = FuncArity
        )
    then
        list.det_split_list(FuncArity, CompleteArgTypes,
            FuncArgTypes, FuncTypeParams),
        pred_args_to_func_args(FuncTypeParams,
            FuncArgTypeParams, FuncReturnTypeParam),
        (
            FuncArgTypeParams = [],
            FuncType = FuncReturnTypeParam
        ;
            FuncArgTypeParams = [_ | _],
            construct_higher_order_func_type(Purity,
                FuncArgTypeParams, FuncReturnTypeParam, FuncType)
        ),
        make_body_hlds_constraints(ClassTable, PredTypeVarSet,
            GoalId, PredClassContext, PredConstraints),
        ConsTypeInfo = cons_type_info(PredTypeVarSet,
            PredExistQVars, FuncType, FuncArgTypes, PredConstraints,
            source_pred(PredId)),
        !:ConsTypeInfos = [ConsTypeInfo | !.ConsTypeInfos]
    else
        true
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Add new universal constraints for constraints containing variables that
    % have been renamed. These new constraints are the ones that will need
    % to be supplied by the caller. The other constraints will be supplied
    % from non-updated fields.
    %
:- pred project_and_rename_constraints(class_table::in, tvarset::in,
    set(tvar)::in, tvar_renaming::in,
    hlds_constraints::in, hlds_constraints::out) is det.

project_and_rename_constraints(ClassTable, TVarSet, CallTVars, TVarRenaming,
        !Constraints) :-
    !.Constraints = hlds_constraints(Unproven0, Assumed,
        Redundant0, Ancestors),

    % Project the constraints down onto the list of tvars in the call.
    list.filter(project_constraint(CallTVars), Unproven0, NewUnproven0),
    list.filter_map(rename_constraint(TVarRenaming), NewUnproven0,
        NewUnproven),
    update_redundant_constraints(ClassTable, TVarSet, NewUnproven,
        Redundant0, Redundant),
    list.append(NewUnproven, Unproven0, Unproven),
    !:Constraints = hlds_constraints(Unproven, Assumed, Redundant, Ancestors).

:- pred project_constraint(set(tvar)::in, hlds_constraint::in) is semidet.

project_constraint(CallTVars, Constraint) :-
    Constraint = hlds_constraint(_Ids, _ClassName, TypesToCheck),
    type_vars_in_types(TypesToCheck, TVarsToCheck0),
    set.list_to_set(TVarsToCheck0, TVarsToCheck),
    set.intersect(TVarsToCheck, CallTVars, RelevantTVars),
    set.is_non_empty(RelevantTVars).

:- pred rename_constraint(tvar_renaming::in, hlds_constraint::in,
    hlds_constraint::out) is semidet.

rename_constraint(TVarRenaming, Constraint0, Constraint) :-
    Constraint0 = hlds_constraint(Ids, ClassName, ArgTypes0),
    some [Var] (
        type_list_contains_var(ArgTypes0, Var),
        map.contains(TVarRenaming, Var)
    ),
    apply_variable_renaming_to_type_list(TVarRenaming, ArgTypes0, ArgTypes),
    Constraint = hlds_constraint(Ids, ClassName, ArgTypes).

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_unify_var_functor.
%---------------------------------------------------------------------------%

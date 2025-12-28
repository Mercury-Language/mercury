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

:- module check_hlds.typecheck_cons_infos.
:- interface.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.typecheck_error_undef.
:- import_module check_hlds.typecheck_info.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

:- type cons_info_result
    --->    cons_info_builtin_const(builtin_type, string)
            % The id and the user-visible name of the builtin type,
            % whose values are all constants.
    ;       cons_info_tuple(cons_type_info)
            % The cons_id is a tuple_cons. Such types have only one definition
            % for any given arity (and cannot be curried).
            % Note that at this stage of compilation, a tuple may
            % still be represented as a du_ctor named "{}".
    ;       cons_info_du_ctor(du_ctor, list(cons_type_info), list(cons_error))
            % The cons_id is the du_ctor given by the first argument.
            % The second argument has an entry for all the possible
            % entities that this du_ctor can refer to, including what
            % arg_type_vector->result_type function they represent,
            % any existential quantification that apply to them and
            % any typeclass constraints on them.
            %
            % The entities can be
            %
            % - data constructors (the most common entity),
            % - character constants (if the name has a single code point),
            % - tuples (if the name is "{}"),
            % - get or set field access functions (either user-declared
            %   or declared automatically by the compiler),
            % - closures containing function applications, or
            % - closures containing predicate invocations.
            %
            % XXX Document the exact meaning of the third argument.
    ;       cons_info_field_access_func
            % This cons_id occurs inside a field access function, but
            % is NOT the du_ctor that the field access function is FOR.
    ;       cons_info_comp_gen_cons_id.
            % This cons_id is one that should not occur here, because
            % it is introduced only by compiler passes that execute
            % *after* typechecking.

    % Note: changes here may require changes to
    %
    % - post_typecheck.resolve_unify_functor,
    % - intermod.module_qualify_unify_rhs,
    % - recompilation.usage.find_matching_constructors and
    % - recompilation.check.check_functor_ambiguities.
    %
:- pred typecheck_info_construct_all_cons_infos(typecheck_info::in,
    cons_id::in, int::in, goal_id::in, cons_info_result::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.typecheck_util.
:- import_module hlds.
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
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_construct.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module set.
:- import_module string.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

typecheck_info_construct_all_cons_infos(Info, ConsId, Arity, GoalId,
        ConsInfoResult) :-
    (
        (
            ConsId = some_int_const(IntConst),
            BuiltinType = builtin_type_int(type_of_int_const(IntConst)),
            TypeName = type_name_of_int_const(IntConst)
        ;
            ConsId = float_const(_),
            BuiltinType = builtin_type_float,
            TypeName = "float"
        ;
            ConsId = char_const(_),
            % char_const cons_ids won't appear in our input, because
            % they are introduced in the post-typecheck pass, by
            % resolve_unify_functor. Including this code here is
            % trivial future-proofing against potential future changes
            % that could cause typechecking to be repeated.
            BuiltinType = builtin_type_char,
            TypeName = "character"
        ;
            ConsId = string_const(_),
            BuiltinType = builtin_type_string,
            TypeName = "string"
        ;
            ConsId = impl_defined_const(IDCKind),
            (
                ( IDCKind = idc_file
                ; IDCKind = idc_module
                ; IDCKind = idc_pred
                ; IDCKind = idc_grade
                ),
                BuiltinType = builtin_type_string,
                TypeName = "string"
            ;
                IDCKind = idc_line,
                BuiltinType = builtin_type_int(int_type_int),
                TypeName = "int"
            )
        ),
        ConsInfoResult = cons_info_builtin_const(BuiltinType, TypeName)
    ;
        ConsId = tuple_cons(_TupleArity),
        typecheck_info_construct_tuple_cons_info(Arity, ConsInfo),
        ConsInfoResult = cons_info_tuple(ConsInfo)
    ;
        ConsId = du_data_ctor(DuCtor),
        typecheck_info_get_in_field_access_function(Info, InFieldAccessFunc),
        ( if
            % If we are typechecking the clause added for a field access
            % function for which the user has supplied type or mode
            % declarations, the goal should only contain an application
            % of the field access function, not constructor applications
            % or function calls. The clauses we get from elsewhere,
            % meaning from `.opt' files, will already have been expanded
            % into unifications.
            InFieldAccessFunc = in_field_access_func(PredStatus,
                _InAccessType, _InFieldSymName, _InOoMFieldDefns),
            pred_status_defined_in_this_module(PredStatus) = yes
            % We ignore the other fields of in_field_access_func, because
            % they refer to the field access function we are in, and NOT
            % (necessarily) the field access function (if any) that
            % DuCtor is for.
        then
            UserArity = user_arity(Arity),
            ( if
                is_du_ctor_synonym_for_field_access_function(Info, DuCtor,
                    UserArity, DuCtorSymName, AccessType, FieldSymName,
                    FieldDefns)
            then
                get_auto_generated_field_access_func_cons_infos(Info, GoalId,
                    DuCtorSymName, UserArity, AccessType, FieldSymName,
                    FieldDefns, FieldAccessConsInfos, FieldAccessConsErrors),
                ConsInfoResult = cons_info_du_ctor(DuCtor,
                    FieldAccessConsInfos, FieldAccessConsErrors)
            else
                ConsInfoResult = cons_info_field_access_func
            )
        else
            typecheck_info_construct_du_cons_infos(Info, DuCtor, Arity, GoalId,
                ConsInfoResult)
        )
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

%---------------------%

:- pred typecheck_info_construct_builtin_cons_info(string::in,
    cons_type_info::out) is det.

typecheck_info_construct_builtin_cons_info(BuiltinTypeName, ConsInfo) :-
    TypeCtor = type_ctor(unqualified(BuiltinTypeName), 0),
    construct_type(TypeCtor, [], ConsType),
    varset.init(ConsTypeVarSet),
    ConsInfo = cons_type_info(ConsTypeVarSet, [], ConsType, [],
        empty_hlds_constraint_db, source_builtin_type(BuiltinTypeName)).

%---------------------%

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
        TupleConsType, TupleArgTypes, empty_hlds_constraint_db,
        source_builtin_type("tuple")).

%---------------------%

:- pred typecheck_info_construct_du_cons_infos(typecheck_info::in, du_ctor::in,
    arity::in, goal_id::in, cons_info_result::out) is det.

typecheck_info_construct_du_cons_infos(Info, DuCtor, Arity, GoalId,
        ConsInfoResult) :-
    typecheck_info_get_du_cons_ctor_list(Info, DuCtor, GoalId,
        DuConsInfos, DuConsErrors),

    % Check whether DuCtor is a field access function for which the user
    % has not supplied a declaration.
    UserArity = user_arity(Arity),
    ( if
        is_du_ctor_synonym_for_field_access_function(Info, DuCtor, UserArity,
            DuCtorSymName, AccessType, FieldSymName, FieldDefns)
    then
        get_auto_generated_field_access_func_cons_infos(Info, GoalId,
            DuCtorSymName, UserArity, AccessType, FieldSymName, FieldDefns,
            FieldAccessConsInfos, FieldAccessConsErrors)
    else
        FieldAccessConsInfos = [],
        FieldAccessConsErrors = []
    ),

    % Check whether DuCtor is of a builtin type. It can be of only one
    % builtin type: "character". (The values of all the other builtin types
    % have their own kinds of cons_ids, and none of them is a du_ctor.)
    % Type "character" also has its own kind of cons_id, char_const/1,
    % but we also accept single-code-point strings as character constants.
    % We shouldn't do that, but there no point in breaking backward
    % compatibility (which goes back to Prolog) on this point.
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
    % Arity arguments. If so, add the resulting cons_type_info to the list.
    builtin_pred_type(Info, DuCtor, Arity, GoalId, PredConsInfos),

    % Check for higher-order function calls.
    ( if builtin_apply_type(Info, DuCtor, Arity, ApplyConsInfo) then
        ApplyConsInfos = [ApplyConsInfo]
    else
        ApplyConsInfos = []
    ),

    ConsInfos = DuConsInfos ++ FieldAccessConsInfos ++
        CharConsInfos ++ TupleConsInfos ++ PredConsInfos ++ ApplyConsInfos,
    ConsErrors = DuConsErrors ++ FieldAccessConsErrors,
    ConsInfoResult = cons_info_du_ctor(DuCtor, ConsInfos, ConsErrors).

:- pred typecheck_info_get_du_cons_ctor_list(typecheck_info::in, du_ctor::in,
    goal_id::in, list(cons_type_info)::out, list(cons_error)::out) is det.

typecheck_info_get_du_cons_ctor_list(Info, DuCtor, GoalId,
        ConsInfos, ConsErrors) :-
    DuCtor = du_ctor(Name, Arity, ConsIdTypeCtor),
    typecheck_info_get_cons_table(Info, ConsTable),

    % Check if ConsId has been defined as a constructor in some
    % discriminated union type or types.
    ( if search_cons_table(ConsTable, DuCtor, ConsDefns) then
        hlds_cons_defns_to_cons_type_infos_and_errors(Info, GoalId,
            do_not_flip_constraints, DuCtor, ConsDefns,
            PlainConsInfos, PlainConsErrors)
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
        hlds_cons_defns_to_cons_type_infos_and_errors(Info, GoalId,
            flip_constraints_for_new, OrigDuCtor, ExistQConsDefns,
            UnivQuantConsInfos, UnivQuantConsErrors),
        ConsInfos = PlainConsInfos ++ UnivQuantConsInfos,
        ConsErrors = PlainConsErrors ++ UnivQuantConsErrors
    else
        ConsInfos = PlainConsInfos,
        ConsErrors = PlainConsErrors
    ).

%---------------------------------------------------------------------------%

:- type cons_constraints_action
    --->    do_not_flip_constraints
    ;       flip_constraints_for_new
    ;       flip_constraints_for_field_set.

:- pred hlds_cons_defns_to_cons_type_infos_and_errors(typecheck_info, goal_id,
    cons_constraints_action, du_ctor, list(hlds_cons_defn),
    list(cons_type_info), list(cons_error)).
:- mode hlds_cons_defns_to_cons_type_infos_and_errors(in, in,
    in(bound(do_not_flip_constraints)), in, in, out, out) is det.
:- mode hlds_cons_defns_to_cons_type_infos_and_errors(in, in,
    in(bound(flip_constraints_for_new)), in, in, out, out) is det.

hlds_cons_defns_to_cons_type_infos_and_errors(_Info, _GoalId, _Action,
        _DuCtor, [], [], []).
hlds_cons_defns_to_cons_type_infos_and_errors(Info, GoalId, Action,
        DuCtor, [ConsDefn | ConsDefns], ConsTypeInfos, ConsErrors) :-
    hlds_cons_defn_to_maybe_cons_type_info(Info, GoalId, Action,
        DuCtor, ConsDefn, HeadMaybeConsTypeInfo),
    hlds_cons_defns_to_cons_type_infos_and_errors(Info, GoalId, Action,
        DuCtor, ConsDefns, TailConsTypeInfos, TailConsErrors),
    (
        HeadMaybeConsTypeInfo = ok(HeadConsTypeInfo),
        % One of the callers of hlds_cons_defn_to_maybe_cons_type_info
        % cares about HeadConsTypeInfo being for a source_type or not,
        % but the callers of this predicate do not.
        ConsTypeInfos = [coerce(HeadConsTypeInfo) | TailConsTypeInfos],
        ConsErrors = TailConsErrors
    ;
        HeadMaybeConsTypeInfo = error(HeadConsError),
        ConsTypeInfos = TailConsTypeInfos,
        ConsErrors = [HeadConsError | TailConsErrors]
    ).

:- type du_cons_type_info_source =< cons_type_info_source
    --->    source_type(type_ctor, du_ctor).
:- type du_cons_type_info =< cons_type_info
    --->    cons_type_info(tvarset, existq_tvars, mer_type,
                list(mer_type), hlds_constraint_db, du_cons_type_info_source).
:- type maybe_du_cons_type_info =< maybe_cons_type_info
    --->    ok(du_cons_type_info)
    ;       error(cons_error).

:- pred hlds_cons_defn_to_maybe_cons_type_info(typecheck_info, goal_id,
    cons_constraints_action, du_ctor, hlds_cons_defn, maybe_du_cons_type_info).
:- mode hlds_cons_defn_to_maybe_cons_type_info(in, in,
    in(bound(do_not_flip_constraints)), in, in, out) is det.
:- mode hlds_cons_defn_to_maybe_cons_type_info(in, in,
    in(bound(flip_constraints_for_field_set)), in, in, out) is det.
:- mode hlds_cons_defn_to_maybe_cons_type_info(in, in, in, in, in, out) is det.
% The last mode should be
%
% :- mode hlds_cons_defn_to_maybe_cons_type_info(in, in,
%     in(bound(flip_constraints_for_new)), in, in, out) is det.
%
% However, as of 2024 03 04, this generates a spurious mode error:
%
%    In clause for `hlds_cons_defn_to_maybe_cons_type_info(in, in,
%      in(bound(flip_constraints_for_new)), in, in, out)':
%      mode mismatch in disjunction.
%      The variable `ExistQVars0' is ground in some
%      branches but not others.
%        In this branch, `ExistQVars0' is free.
%        In this branch, `ExistQVars0' is ground.

hlds_cons_defn_to_maybe_cons_type_info(Info, GoalId, Action, DuCtor,
        ConsDefn, MaybeConsTypeInfo) :-
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
        MaybeConsTypeInfo =
            error(other_lang_foreign_type_constructor(TypeCtor, TypeDefn))
    else if
        % Do not allow constructors for abstract_imported types unless
        % the current predicate is opt_imported.
        hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
        TypeStatus = type_status(status_abstract_imported),
        not is_unify_index_or_compare_pred(PredInfo),
        PredStatus \= pred_status(status_opt_imported)
    then
        MaybeConsTypeInfo = error(abstract_imported_type)
    else if
        Action = flip_constraints_for_new,
        MaybeExistConstraints = no_exist_constraints
    then
        % Do not allow 'new' constructors except on existential types.
        MaybeConsTypeInfo = error(new_on_non_existential_type(TypeCtor))
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
        make_body_hlds_constraint_db(ClassTable, ConsTypeVarSet,
            GoalId, ProgConstraints, ConstraintDb),
        ConsTypeInfo = cons_type_info(ConsTypeVarSet, ExistQVars, ConsType,
            ArgTypes, ConstraintDb, source_type(TypeCtor, DuCtor)),
        MaybeConsTypeInfo = ok(ConsTypeInfo)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % is_du_ctor_synonym_for_field_access_function(Info, DuCtor, Arity,
    %   AccessType, FieldName, FieldDefns):
    %
    % Does DuCtor have the same name and arity as one or more of the
    % automatically generated field access functions?
    %
    % If yes, succeed, and return whether it is a get or set function,
    % the full sym_name of the field, and the definition(s) of those fields.
    % There may be more than one, ever since we allowed the same field name
    % to be defined more than once in a module (they do have to be
    % in different types).
    %
:- pred is_du_ctor_synonym_for_field_access_function(typecheck_info::in,
    du_ctor::in, user_arity::in,
    sym_name::out, field_access_type::out, sym_name::out,
    list(hlds_ctor_field_defn)::out) is semidet.

is_du_ctor_synonym_for_field_access_function(Info, DuCtor, UserArity,
        DuCtorSymName, AccessType, FieldSymName, FieldDefns) :-
    DuCtor = du_ctor(DuCtorSymName, DuCtorArity, _),
    % Since we do not allow taking the address of an automatically generated
    % field access function, we don't have to consider the case of a reference
    % to such a function with an arity that differs between the definition
    % of the function (DuCtorArity, if this du_ctor refers to that function)
    % and the reference to that function (UserArity).
    %
    % A curried reference could specify a lower arity, if this were allowed.
    UserArity = user_arity(DuCtorArity),
    (
        DuCtorArity = 1,
        FieldSymName = DuCtorSymName,
        AccessType = get
    ;
        DuCtorArity = 2,
        remove_sym_name_suffix(DuCtorSymName, " :=", FieldSymName),
        AccessType = set
    ),
    typecheck_info_get_module_info(Info, ModuleInfo),
    module_info_get_ctor_field_table(ModuleInfo, CtorFieldTable),
    map.search(CtorFieldTable, FieldSymName, OoMFieldDefns),
    FieldDefns = one_or_more_to_list(OoMFieldDefns).

%---------------------------------------------------------------------------%

    % get_auto_generated_field_access_func_cons_infos(Info, GoalId, DuCtor,
    %   Arity, ConsTypeInfos, ConsErrors):
    %
    % Succeed if DuCtor is the name of one the automatically generated
    % field access functions (fieldname, '<fieldname> :=').
    %
:- pred get_auto_generated_field_access_func_cons_infos(typecheck_info::in,
    goal_id::in, sym_name::in, user_arity::in, field_access_type::in,
    sym_name::in, list(hlds_ctor_field_defn)::in,
    list(cons_type_info)::out, list(cons_error)::out) is det.

get_auto_generated_field_access_func_cons_infos(_, _, _, _, _, _, [], [], []).
get_auto_generated_field_access_func_cons_infos(Info, GoalId,
        DuCtorSymName, UserArity, AccessType, FieldSymName,
        [FieldDefn | FieldDefns], ConsTypeInfos, ConsErrors) :-
    get_auto_generated_field_access_func_cons_infos(Info, GoalId,
        DuCtorSymName, UserArity, AccessType, FieldSymName,
        FieldDefns, TailConsTypeInfos, TailConsErrors),
    typecheck_info_get_module_info(Info, ModuleInfo),
    typecheck_info_get_in_field_access_function(Info, InFieldAccessFunc),
    ( if
        are_we_in_an_effective_field_access_function(ModuleInfo,
            InFieldAccessFunc, DuCtorSymName, UserArity, AccessType,
            FieldDefn, TypeCtor, DuCtor)
    then
        get_auto_generated_field_access_func_cons_info(Info, GoalId,
            AccessType, FieldSymName, TypeCtor, DuCtor,
            FieldDefn, MaybeConsTypeInfo),
        (
            MaybeConsTypeInfo = ok(ConsTypeInfo),
            ConsTypeInfos = [ConsTypeInfo | TailConsTypeInfos],
            ConsErrors = TailConsErrors
        ;
            MaybeConsTypeInfo = error(ConsError),
            ConsTypeInfos = TailConsTypeInfos,
            ConsErrors = [ConsError | TailConsErrors]
        )
    else
        ConsTypeInfos = TailConsTypeInfos,
        ConsErrors = TailConsErrors
    ).

:- pred get_auto_generated_field_access_func_cons_info(typecheck_info::in,
    goal_id::in, field_access_type::in,
    sym_name::in, type_ctor::in, du_ctor::in, hlds_ctor_field_defn::in,
    maybe_cons_type_info::out) is det.

get_auto_generated_field_access_func_cons_info(Info, GoalId,
        AccessType, FieldSymName, TypeCtor, DuCtor,
        FieldDefn, MaybeConsTypeInfo) :-
    typecheck_info_get_module_info(Info, ModuleInfo),
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
        hlds_cons_defn_to_maybe_cons_type_info(Info, GoalId, ConsAction,
            DuCtor, ConsDefn, MaybeFunctorConsTypeInfo)
    ;
        AccessType = set,
        ConsAction = flip_constraints_for_field_set,
        hlds_cons_defn_to_maybe_cons_type_info(Info, GoalId, ConsAction,
            DuCtor, ConsDefn, MaybeFunctorConsTypeInfo)
    ),
    (
        MaybeFunctorConsTypeInfo = ok(FunctorConsTypeInfo),
        module_info_get_class_table(ModuleInfo, ClassTable),
        functor_to_field_access_function_cons_type_info(ClassTable, AccessType,
            FieldSymName, FieldDefn, FunctorConsTypeInfo,
            OrigExistTVars, MaybeConsTypeInfo)
    ;
        MaybeFunctorConsTypeInfo = error(ConsError),
        MaybeConsTypeInfo = error(ConsError)
    ).

%---------------------%

    % If the user has supplied a declaration for a field access function
    % of the same name and arity, operating on the same type constructor,
    % we use that instead of the automatically generated version,
    % unless we are typechecking the clause introduced for the
    % user-supplied declaration itself.
    %
    % The user-declared version will be picked up by builtin_pred_type.
    %
:- pred are_we_in_an_effective_field_access_function(module_info::in,
    maybe_in_field_access_func::in, sym_name::in, user_arity::in,
    field_access_type::in, hlds_ctor_field_defn::in,
    type_ctor::out, du_ctor::out) is semidet.

are_we_in_an_effective_field_access_function(ModuleInfo, InFieldAccessFunc,
        DuCtorSymName, UserArity, AccessType, FieldDefn, TypeCtor, DuCtor) :-
    FieldDefn = hlds_ctor_field_defn(_, _, TypeCtor, DuCtor, _),
    TypeCtor = type_ctor(qualified(TypeModule, _), _),
    (
        InFieldAccessFunc = not_in_field_access_func,
        module_info_get_predicate_table(ModuleInfo, PredTable),
        DuCtorName = unqualify_name(DuCtorSymName),
        predicate_table_lookup_func_m_n_a(PredTable, is_fully_qualified,
            TypeModule, DuCtorName, UserArity, PredIds),
        list.all_false(
            is_field_access_function_for_type_ctor(ModuleInfo, AccessType,
                TypeCtor),
            PredIds)
    ;
        InFieldAccessFunc = in_field_access_func(_, _, _, _)
    ).

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

%---------------------%

:- type maybe_cons_type_info
    --->    ok(cons_type_info)
    ;       error(cons_error).

:- pred functor_to_field_access_function_cons_type_info(class_table::in,
    field_access_type::in, sym_name::in, hlds_ctor_field_defn::in,
    du_cons_type_info::in, existq_tvars::in, maybe_cons_type_info::out) is det.

functor_to_field_access_function_cons_type_info(ClassTable, AccessType,
        FieldSymName, FieldDefn, FunctorConsTypeInfo, OrigExistTVars,
        MaybeConsTypeInfo) :-
    FunctorConsTypeInfo = cons_type_info(TVarSet0, ExistQVars,
        FunctorType, ConsArgTypes, Constraints0, Source0),
    Source0 = source_type(SourceType, ConsId),
    FieldDefn = hlds_ctor_field_defn(_, _, _, _, FieldNumber),
    list.det_index1(ConsArgTypes, FieldNumber, FieldType),
    FieldName = unqualify_name(FieldSymName),
    (
        AccessType = get,
        Source = source_field_access(get, SourceType, ConsId, FieldName),
        ReturnType = FieldType,
        ArgTypes = [FunctorType],
        MaybeConsTypeInfo = ok(cons_type_info(TVarSet0, ExistQVars,
            ReturnType, ArgTypes, Constraints0, Source))
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
            MaybeConsTypeInfo = ok(cons_type_info(TVarSet0, ExistQVars,
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
                apply_renaming_to_type(TVarRenaming, FieldType,
                    RenamedFieldType),
                apply_renaming_to_type(TVarRenaming, FunctorType,
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
                MaybeConsTypeInfo = ok(cons_type_info(TVarSet, ExistQVars,
                    RetType, ArgTypes, Constraints, Source))
            else
                % This field cannot be set. Pass out some information so that
                % we can give a better error message. Errors involving changing
                % the types of universally quantified type variables will be
                % caught by typecheck_functor_arg_types.
                set.to_sorted_list(ExistQVarsInFieldAndOthers,
                    ExistQVarsInFieldAndOthers1),
                MaybeConsTypeInfo = error(invalid_field_update(FieldSymName,
                    FieldDefn, TVarSet0, ExistQVarsInFieldAndOthers1))
            )
        )
    ).

%---------------------------------------------------------------------------%

    % builtin_apply_type(Info, DuCtor, Arity, ConsTypeInfo):
    %
    % Succeed if DuCtor is the builtin apply/N or ''/N (N>=2),
    % which is used to invoke higher-order functions.
    % If so, bind ConsTypeInfo to a singleton list containing
    % the appropriate type for apply/N of the specified Arity.
    %
:- pred builtin_apply_type(typecheck_info::in, du_ctor::in, int::in,
    cons_type_info::out) is semidet.

builtin_apply_type(_Info, DuCtor, Arity, ConsTypeInfo) :-
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
    general_higher_order_func_type(Purity, Arity - 1,
        TypeVarSet, FuncType, ArgTypes, ReturnType),
    ExistQVars = [],
    % The function is the first argument of the apply operation.
    ApplyArgTypes = [FuncType | ArgTypes],
    Source = source_apply(ApplyNameToUse),
    ConsTypeInfo = cons_type_info(TypeVarSet, ExistQVars, ReturnType,
        ApplyArgTypes, empty_hlds_constraint_db, Source).

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
        make_body_hlds_constraint_db(ClassTable, PredTypeVarSet,
            GoalId, PredClassContext, PredConstraintDb),
        ConsTypeInfo = cons_type_info(PredTypeVarSet, PredExistQVars,
            PredType, ArgTypes, PredConstraintDb, source_pred(PredId)),
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
        make_body_hlds_constraint_db(ClassTable, PredTypeVarSet,
            GoalId, PredClassContext, PredConstraintDb),
        ConsTypeInfo = cons_type_info(PredTypeVarSet,
            PredExistQVars, FuncType, FuncArgTypes, PredConstraintDb,
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
    hlds_constraint_db::in, hlds_constraint_db::out) is det.

project_and_rename_constraints(ClassTable, TVarSet, CallTVars, TVarRenaming,
        !ConstraintDb) :-
    !.ConstraintDb = hlds_constraint_db(Unproven0, Assumed,
        Redundant0, Ancestors),

    % Project the constraints down onto the list of tvars in the call.
    list.filter(project_constraint(CallTVars), Unproven0, NewUnproven0),
    list.filter_map(rename_constraint(TVarRenaming), NewUnproven0,
        NewUnproven),
    update_redundant_constraints(ClassTable, TVarSet, NewUnproven,
        Redundant0, Redundant),
    list.append(NewUnproven, Unproven0, Unproven),
    !:ConstraintDb = hlds_constraint_db(Unproven, Assumed,
        Redundant, Ancestors).

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
    apply_renaming_to_types(TVarRenaming, ArgTypes0, ArgTypes),
    Constraint = hlds_constraint(Ids, ClassName, ArgTypes).

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_cons_infos.
%---------------------------------------------------------------------------%

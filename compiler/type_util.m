%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: type_util.m.
% Main author: fjh.

% This file provides some utility predicates which operate on types.
% It is used by various stages of the compilation after type-checking,
% include the mode checker and the code generator.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module check_hlds__type_util.

:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module list.
:- import_module std_util.

%-----------------------------------------------------------------------------%

    % Succeed iff type is an "atomic" type - one which can be unified
    % using a simple_test rather than a complicated_unify.
    %
:- pred type_is_atomic(mer_type::in, module_info::in) is semidet.

:- pred type_ctor_is_atomic(type_ctor::in, module_info::in) is semidet.

    % Obtain the type definition and type definition body respectively,
    % if known, for the principal type constructor of the given type.
    %
    % Fail if the given type is a type variable.
    %
:- pred type_to_type_defn(module_info::in, mer_type::in, hlds_type_defn::out)
    is semidet.

:- pred type_to_type_defn_body(module_info::in, mer_type::in,
    hlds_type_body::out) is semidet.

    % Succeed iff there was either a `where equality is <predname>' or a
    % `where comparison is <predname>' declaration for the principal type
    % constructor of the specified type, and return the ids of the declared
    % unify and/or comparison predicates. Note that even if the type
    % constructor has only a `where comparison is' clause, it effectively
    % has user-defined equality, two values being equal only if the
    % compare pred returns equal.
    %
    % If the type is a type variable and thus has no principal type
    % constructor, fail.
    %
:- pred type_has_user_defined_equality_pred(module_info::in, mer_type::in,
    unify_compare::out) is semidet.

:- pred type_body_has_user_defined_equality_pred(module_info::in,
    hlds_type_body::in, unify_compare::out) is semidet.

    % Succeed iff the principal type constructor for the given type
    % is a solver type.
    %
    % If the type is a type variable and thus has no principal type
    % constructor, fail.
    %
:- pred type_is_solver_type(module_info::in, mer_type::in) is semidet.

:- pred type_has_solver_type_details(module_info::in, mer_type::in,
    solver_type_details::out) is semidet.

:- pred type_body_has_solver_type_details(module_info::in,
    hlds_type_body::in, solver_type_details::out) is semidet.

:- pred is_solver_type(module_info::in, mer_type::in) is semidet.

:- pred type_body_is_solver_type(module_info::in, hlds_type_body::in)
    is semidet.

    % Succeeds iff one or more of the type constructors for a given
    % type is existentially quantified.
    %
:- pred is_existq_type(module_info::in, mer_type::in) is semidet.

    % Certain types, e.g. io.state and store.store(S), are just dummy types
    % used to ensure logical semantics; there is no need to actually pass them,
    % and so when importing or exporting procedures to/from C, we don't include
    % arguments with these types.
    %
    % A type is a dummy type in one of two cases: either it is a builtin
    % dummy type, or it has only a single function symbol of arity zero.
    %
:- pred is_dummy_argument_type(module_info::in, mer_type::in) is semidet.

    % A test for types that are defined in Mercury, but whose definitions
    % are `lies', i.e. they are not sufficiently accurate for RTTI
    % structures describing the types. Since the RTTI will be hand defined,
    % the compiler shouldn't generate RTTI for these types.
    %
:- pred type_ctor_has_hand_defined_rtti(type_ctor::in, hlds_type_body::in)
    is semidet.

    % Given a type, determine what category its principal constructor
    % falls into.
    %
:- func classify_type(module_info, mer_type) = type_category.

    % Given a type_ctor, determine what sort it is.
    %
:- func classify_type_ctor(module_info, type_ctor) = type_category.

    % Given a type_ctor, look up its module/name/arity
    %
:- pred type_ctor_module(module_info::in, type_ctor::in,
    module_name::out) is det.

:- pred type_ctor_name(module_info::in, type_ctor::in, string::out) is det.

:- pred type_ctor_arity(module_info::in, type_ctor::in, arity::out) is det.

    % If the type is a du type or a tuple type, return the list of its
    % constructors.
    %
:- pred type_constructors(mer_type::in, module_info::in,
    list(constructor)::out) is semidet.

    % Given a type on which it is possible to have a complete switch,
    % return the number of alternatives. (It is possible to have a complete
    % switch on any du type and on the builtin type character. It is not
    % feasible to have a complete switch on the builtin types integer,
    % float, and switch. One cannot have a switch on an abstract type,
    % and equivalence types will have been expanded out by the time
    % we consider switches.)
    %
:- pred switch_type_num_functors(module_info::in, mer_type::in,
    int::out) is semidet.

    % Work out the types of the arguments of a functor, given the cons_id
    % and type of the functor. Aborts if the functor is existentially typed.
    % Note that this will substitute appropriate values for any type variables
    % in the functor's argument types, to match their bindings in the
    % functor's type.
    %
:- pred get_cons_id_arg_types(module_info::in, mer_type::in,
    cons_id::in, list(mer_type)::out) is det.

    % The same as gget_cons_id_arg_types except that it fails rather than
    % aborting if the functor is existentially typed.
    %
:- pred get_cons_id_non_existential_arg_types(module_info::in,
    mer_type::in, cons_id::in, list(mer_type)::out) is semidet.

    % The same as gget_cons_id_arg_types except that the cons_id is output
    % non-deterministically. The cons_id is not module-qualified.
    %
:- pred cons_id_arg_types(module_info::in, mer_type::in,
    cons_id::out, list(mer_type)::out) is nondet.

    % Given a type and a cons_id, look up the definitions of that type
    % and constructor. Aborts if the cons_id is not user-defined.
    % Note that this will NOT bind type variables in the functor's argument
    % types; they will be left unbound, so the caller can find out the
    % original types from the constructor definition. The caller must do
    % that substitution itself if required.
    %
:- pred get_type_and_cons_defn(module_info::in, mer_type::in,
    cons_id::in, hlds_type_defn::out, hlds_cons_defn::out) is det.

    % Like gget_type_and_cons_defn (above), except that it only returns
    % the definition of the constructor, not the type.
    %
:- pred get_cons_defn(module_info::in, type_ctor::in, cons_id::in,
    hlds_cons_defn::out) is semidet.

    % Given a type and a cons_id, look up the definition of that constructor;
    % if it is existentially typed, return its definition, otherwise fail.
    % Note that this will NOT bind type variables in the functor's argument
    % types; they will be left unbound, so the caller can find out the
    % original types from the constructor definition. The caller must do
    % that substitution itself if required.
    %
:- pred get_existq_cons_defn(module_info::in, mer_type::in, cons_id::in,
    ctor_defn::out) is semidet.

:- pred is_existq_cons(module_info::in, mer_type::in, cons_id::in) is semidet.

    % Check whether a type is a no_tag type (i.e. one with only one
    % constructor, and whose one constructor has only one argument),
    % and if so, return its constructor symbol and argument type.
    %
:- pred type_is_no_tag_type(module_info::in, mer_type::in, sym_name::out,
    mer_type::out) is semidet.

    % cons_id_adjusted_arity(ModuleInfo, Type, ConsId):
    %
    % Returns the number of arguments of specified constructor id, adjusted
    % to include the extra typeclassinfo and typeinfo arguments inserted
    % by polymorphism.m for existentially typed constructors.
    %
:- func cons_id_adjusted_arity(module_info, mer_type, cons_id) = int.

%-----------------------------------------------------------------------------%

    % If possible, get the argument types for the cons_id. We need to pass in
    % the arity rather than using the arity from the cons_id because the arity
    % in the cons_id will not include any extra type_info arguments for
    % existentially quantified types.
    %
:- pred maybe_get_cons_id_arg_types(module_info::in, maybe(mer_type)::in,
    cons_id::in, arity::in, list(maybe(mer_type))::out) is det.

:- pred maybe_get_higher_order_arg_types(maybe(mer_type)::in, arity::in,
    list(maybe(mer_type))::out) is det.

%-----------------------------------------------------------------------------%
%
% Predicates for doing renamings and substitutions on HLDS data structures.
%

:- pred apply_variable_renaming_to_constraint(tvar_renaming::in,
    hlds_constraint::in, hlds_constraint::out) is det.

:- pred apply_subst_to_constraint(tsubst::in, hlds_constraint::in,
    hlds_constraint::out) is det.

:- pred apply_rec_subst_to_constraint(tsubst::in, hlds_constraint::in,
    hlds_constraint::out) is det.

%-------------%

:- pred apply_variable_renaming_to_constraint_list(tvar_renaming::in,
    list(hlds_constraint)::in, list(hlds_constraint)::out) is det.

:- pred apply_subst_to_constraint_list(tsubst::in, list(hlds_constraint)::in,
    list(hlds_constraint)::out) is det.

:- pred apply_rec_subst_to_constraint_list(tsubst::in,
    list(hlds_constraint)::in, list(hlds_constraint)::out) is det.

%-------------%

:- pred apply_variable_renaming_to_constraints(tvar_renaming::in,
    hlds_constraints::in, hlds_constraints::out) is det.

:- pred apply_subst_to_constraints(tsubst::in, hlds_constraints::in,
    hlds_constraints::out) is det.

:- pred apply_rec_subst_to_constraints(tsubst::in, hlds_constraints::in,
    hlds_constraints::out) is det.

%-------------%

:- pred apply_variable_renaming_to_constraint_proofs(tvar_renaming::in,
    constraint_proof_map::in, constraint_proof_map::out) is det.

:- pred apply_subst_to_constraint_proofs(tsubst::in,
    constraint_proof_map::in, constraint_proof_map::out) is det.

:- pred apply_rec_subst_to_constraint_proofs(tsubst::in,
    constraint_proof_map::in, constraint_proof_map::out) is det.

%-------------%

:- pred apply_variable_renaming_to_constraint_map(tvar_renaming::in,
    constraint_map::in, constraint_map::out) is det.

:- pred apply_subst_to_constraint_map(tsubst::in,
    constraint_map::in, constraint_map::out) is det.

:- pred apply_rec_subst_to_constraint_map(tsubst::in,
    constraint_map::in, constraint_map::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module check_hlds.purity.
:- import_module hlds.hlds_out.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_io_goal.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module svmap.
:- import_module varset.

type_ctor_module(_ModuleInfo, TypeName - _Arity, ModuleName) :-
    sym_name_get_module_name(TypeName, unqualified(""), ModuleName).

type_ctor_name(_ModuleInfo, Name0 - _Arity, Name) :-
    unqualify_name(Name0, Name).

type_ctor_arity(_ModuleInfo, _Name - Arity, Arity).

type_is_atomic(Type, ModuleInfo) :-
    type_to_ctor_and_args(Type, TypeCtor, _),
    type_ctor_is_atomic(TypeCtor, ModuleInfo).

type_ctor_is_atomic(TypeCtor, ModuleInfo) :-
    TypeCategory = classify_type_ctor(ModuleInfo, TypeCtor),
    type_category_is_atomic(TypeCategory) = yes.

:- func type_category_is_atomic(type_category) = bool.

type_category_is_atomic(type_cat_int) = yes.
type_category_is_atomic(type_cat_char) = yes.
type_category_is_atomic(type_cat_string) = yes.
type_category_is_atomic(type_cat_float) = yes.
type_category_is_atomic(type_cat_higher_order) = no.
type_category_is_atomic(type_cat_tuple) = no.
type_category_is_atomic(type_cat_enum) = yes.
type_category_is_atomic(type_cat_dummy) = yes.
type_category_is_atomic(type_cat_variable) = no.
type_category_is_atomic(type_cat_type_info) = no.
type_category_is_atomic(type_cat_type_ctor_info) = no.
type_category_is_atomic(type_cat_typeclass_info) = no.
type_category_is_atomic(type_cat_base_typeclass_info) = no.
type_category_is_atomic(type_cat_void) = yes.
type_category_is_atomic(type_cat_user_ctor) = no.

type_ctor_has_hand_defined_rtti(Type, Body) :-
    Type = qualified(mercury_private_builtin_module, Name) - 0,
    ( Name = "type_info"
    ; Name = "type_ctor_info"
    ; Name = "typeclass_info"
    ; Name = "base_typeclass_info"
    ),
    \+ ( Body = du_type(_, _, _, _, _, yes(_))
       ; Body = foreign_type(_)
       ; Body = solver_type(_, _)
       ).

%-----------------------------------------------------------------------------%

classify_type(ModuleInfo, VarType) = TypeCategory :-
    ( type_to_ctor_and_args(VarType, TypeCtor, _) ->
        TypeCategory = classify_type_ctor(ModuleInfo, TypeCtor)
    ;
        TypeCategory = type_cat_variable
    ).

classify_type_ctor(ModuleInfo, TypeCtor) = TypeCategory :-
    PrivateBuiltin = mercury_private_builtin_module,
    ( TypeCtor = unqualified("character") - 0 ->
        TypeCategory = type_cat_char
    ; TypeCtor = unqualified("int") - 0 ->
        TypeCategory = type_cat_int
    ; TypeCtor = unqualified("float") - 0 ->
        TypeCategory = type_cat_float
    ; TypeCtor = unqualified("string") - 0 ->
        TypeCategory = type_cat_string
    ; TypeCtor = unqualified("void") - 0 ->
        TypeCategory = type_cat_void
    ; TypeCtor = qualified(PrivateBuiltin, "type_info") - 0 ->
        TypeCategory = type_cat_type_info
    ; TypeCtor = qualified(PrivateBuiltin, "type_ctor_info") - 0 ->
        TypeCategory = type_cat_type_ctor_info
    ; TypeCtor = qualified(PrivateBuiltin, "typeclass_info") - 0 ->
        TypeCategory = type_cat_typeclass_info
    ; TypeCtor = qualified(PrivateBuiltin, "base_typeclass_info") - 0 ->
        TypeCategory = type_cat_base_typeclass_info
    ; type_ctor_is_higher_order(TypeCtor, _, _, _) ->
        TypeCategory = type_cat_higher_order
    ; type_ctor_is_tuple(TypeCtor) ->
        TypeCategory = type_cat_tuple
    ; type_ctor_is_enumeration(TypeCtor, ModuleInfo) ->
        TypeCategory = type_cat_enum
    ;
        TypeCategory = type_cat_user_ctor
    ).

type_has_user_defined_equality_pred(ModuleInfo, Type, UserEqComp) :-
    type_to_type_defn_body(ModuleInfo, Type, TypeBody),
    type_body_has_user_defined_equality_pred(ModuleInfo, TypeBody, UserEqComp).

type_body_has_user_defined_equality_pred(ModuleInfo, TypeBody, UserEqComp) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals__get_target(Globals, Target),
    (
        TypeBody = du_type(_, _, _, _, _, _),
        (
            TypeBody ^ du_type_is_foreign_type = yes(ForeignTypeBody),
            have_foreign_type_for_backend(Target, ForeignTypeBody, yes)
        ->
            UserEqComp = foreign_type_body_has_user_defined_eq_comp_pred(
                ModuleInfo, ForeignTypeBody)
        ;
            TypeBody ^ du_type_usereq = yes(UserEqComp)
        )
    ;
        TypeBody = foreign_type(ForeignTypeBody),
        UserEqComp = foreign_type_body_has_user_defined_eq_comp_pred(
            ModuleInfo, ForeignTypeBody)
    ;
        TypeBody = solver_type(_SolverTypeDetails, yes(UserEqComp))
    ).

type_is_solver_type(ModuleInfo, Type) :-
    type_to_type_defn_body(ModuleInfo, Type, TypeBody),
    (
        TypeBody = solver_type(_, _)
    ;
        TypeBody = abstract_type(solver_type)
    ;
        TypeBody = eqv_type(EqvType),
        type_is_solver_type(ModuleInfo, EqvType)
    ).

type_has_solver_type_details(ModuleInfo, Type, SolverTypeDetails) :-
    type_to_type_defn_body(ModuleInfo, Type, TypeBody),
    type_body_has_solver_type_details(ModuleInfo, TypeBody,
        SolverTypeDetails).

type_body_has_solver_type_details(_ModuleInfo,
        solver_type(SolverTypeDetails, _MaybeUserEqComp), SolverTypeDetails).
type_body_has_solver_type_details( ModuleInfo,
        eqv_type(Type), SolverTypeDetails) :-
    type_has_solver_type_details(ModuleInfo, Type, SolverTypeDetails).

type_to_type_defn(ModuleInfo, Type, TypeDefn) :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    type_to_ctor_and_args(Type, TypeCtor, _TypeArgs),
    map__search(TypeTable, TypeCtor, TypeDefn).

type_to_type_defn_body(ModuleInfo, Type, TypeBody) :-
    type_to_type_defn(ModuleInfo, Type, TypeDefn),
    hlds_data__get_type_defn_body(TypeDefn, TypeBody).

    % XXX We can't assume that type variables refer to solver types
    % because otherwise the compiler will try to construct initialisation
    % forwarding predicates for exported abstract types defined to be
    % equivalent to a type variable parameter.  This, of course, will
    % lead to the compiler throwing an exception.  The correct solution
    % is to introduce a solver typeclass, but that's something for another day.
    %
is_solver_type(ModuleInfo, Type) :-
    % Type_to_type_defn_body will fail for builtin types such as `int/0'.
    % Such types are not solver types so gis_solver_type fails too.
    % Type_to_type_defn_body also fails for type variables.
    type_to_type_defn_body(ModuleInfo, Type, TypeBody),
    type_body_is_solver_type(ModuleInfo, TypeBody).

    % Succeed if the type body is for a solver type.
type_body_is_solver_type(ModuleInfo, TypeBody) :-
    (
        TypeBody = solver_type(_, _)
    ;
        TypeBody = abstract_type(solver_type)
    ;
        TypeBody = eqv_type(Type),
        is_solver_type(ModuleInfo, Type)
    ).

is_existq_type(Module, Type) :-
    type_constructors(Type, Module, Constructors),
    some [Constructor] (
        list.member(Constructor, Constructors),
        Constructor ^ cons_exist \= []
    ).

is_dummy_argument_type(ModuleInfo, Type) :-
    ( type_to_ctor_and_args(Type, TypeCtor, _) ->
        % Keep this in sync with is_dummy_argument_type_with_constructors
        % above.
        (
            TypeCtor = CtorSymName - TypeArity,
            CtorSymName = qualified(unqualified(ModuleName), TypeName),
            is_builtin_dummy_argument_type(ModuleName, TypeName, TypeArity)
        ;
            module_info_get_type_table(ModuleInfo, TypeTable),
            % This can fail for some builtin type constructors such as func,
            % pred, and tuple, none of which are dummy types.
            map__search(TypeTable, TypeCtor, TypeDefn),
            get_type_defn_body(TypeDefn, TypeBody),
            Ctors = TypeBody ^ du_type_ctors,
            UserEqCmp = TypeBody ^ du_type_usereq,
            constructor_list_represents_dummy_argument_type(Ctors, UserEqCmp)
        )
    ;
        fail
    ).

:- pred type_ctor_is_enumeration(type_ctor::in, module_info::in) is semidet.

type_ctor_is_enumeration(TypeCtor, ModuleInfo) :-
    module_info_get_type_table(ModuleInfo, TypeDefnTable),
    map__search(TypeDefnTable, TypeCtor, TypeDefn),
    hlds_data__get_type_defn_body(TypeDefn, TypeBody),
    TypeBody ^ du_type_is_enum = is_enum.

%-----------------------------------------------------------------------------%

    % If the type is a du type, return the list of its constructors.

type_constructors(Type, ModuleInfo, Constructors) :-
    type_to_ctor_and_args(Type, TypeCtor, TypeArgs),
    ( type_ctor_is_tuple(TypeCtor) ->
        % Tuples are never existentially typed.
        ExistQVars = [],
        ClassConstraints = [],
        CtorArgs = list__map((func(ArgType) = no - ArgType), TypeArgs),
        Constructors = [ctor(ExistQVars, ClassConstraints, unqualified("{}"),
            CtorArgs)]
    ;
        module_info_get_type_table(ModuleInfo, TypeTable),
        map__search(TypeTable, TypeCtor, TypeDefn),
        hlds_data__get_type_defn_tparams(TypeDefn, TypeParams),
        hlds_data__get_type_defn_body(TypeDefn, TypeBody),
        substitute_type_args(TypeParams, TypeArgs, TypeBody ^ du_type_ctors,
            Constructors)
    ).

%-----------------------------------------------------------------------------%

switch_type_num_functors(ModuleInfo, Type, NumFunctors) :-
    type_to_ctor_and_args(Type, TypeCtor, _),
    ( TypeCtor = unqualified("character") - 0 ->
        % XXX The following code uses the source machine's character size,
        % not the target's, so it won't work if cross-compiling to a machine
        % with a different size character.
        char__max_char_value(MaxChar),
        char__min_char_value(MinChar),
        NumFunctors = MaxChar - MinChar + 1
    ; type_ctor_is_tuple(TypeCtor) ->
        NumFunctors = 1
    ;
        module_info_get_type_table(ModuleInfo, TypeTable),
        map__search(TypeTable, TypeCtor, TypeDefn),
        hlds_data__get_type_defn_body(TypeDefn, TypeBody),
        map__count(TypeBody ^ du_type_cons_tag_values, NumFunctors)
    ).

%-----------------------------------------------------------------------------%

get_cons_id_arg_types(ModuleInfo, Type, ConsId, ArgTypes) :-
    get_cons_id_arg_types_2(abort_on_exist_qvar, ModuleInfo, Type, ConsId,
        ArgTypes).

get_cons_id_non_existential_arg_types(ModuleInfo, Type, ConsId, ArgTypes) :-
    get_cons_id_arg_types_2(fail_on_exist_qvar, ModuleInfo, Type, ConsId,
        ArgTypes).

:- type exist_qvar_action
    --->    fail_on_exist_qvar
    ;       abort_on_exist_qvar.

:- pred get_cons_id_arg_types_2(exist_qvar_action, module_info, mer_type,
    cons_id, list(mer_type)).
:- mode get_cons_id_arg_types_2(in(bound(fail_on_exist_qvar)), in, in,
    in, out) is semidet.
:- mode get_cons_id_arg_types_2(in(bound(abort_on_exist_qvar)), in, in,
    in, out) is det.

get_cons_id_arg_types_2(EQVarAction, ModuleInfo, VarType, ConsId, ArgTypes) :-
    ( type_to_ctor_and_args(VarType, TypeCtor, TypeArgs) ->
        (
            % The argument types of a tuple cons_id are the arguments
            % of the tuple type.
            type_ctor_is_tuple(TypeCtor)
        ->
            ArgTypes = TypeArgs
        ;
            do_get_type_and_cons_defn(ModuleInfo, TypeCtor, ConsId, TypeDefn,
                ConsDefn),
            ConsDefn = hlds_cons_defn(ExistQVars0, _Constraints0, Args, _, _),
            Args = [_ | _]
        ->
            hlds_data__get_type_defn_tparams(TypeDefn, TypeParams),

            % XXX handle ExistQVars
            (
                ExistQVars0 = []
            ;
                ExistQVars0 = [_ | _],
                (
                    EQVarAction = abort_on_exist_qvar,
                    error("get_cons_id_arg_types: existentially typed cons_id")
                ;
                    EQVarAction = fail_on_exist_qvar,
                    fail
                )
            ),

            map__from_corresponding_lists(TypeParams, TypeArgs, TSubst),
            assoc_list__values(Args, ArgTypes0),
            apply_subst_to_type_list(TSubst, ArgTypes0, ArgTypes)
        ;
            ArgTypes = []
        )
    ;
        ArgTypes = []
    ).

cons_id_arg_types(ModuleInfo, VarType, ConsId, ArgTypes) :-
    type_to_ctor_and_args(VarType, TypeCtor, TypeArgs),
    module_info_get_type_table(ModuleInfo, Types),
    map__search(Types, TypeCtor, TypeDefn),
    hlds_data__get_type_defn_body(TypeDefn, TypeDefnBody),
    map__member(TypeDefnBody ^ du_type_cons_tag_values, ConsId, _),

    module_info_get_cons_table(ModuleInfo, Ctors),
    map__lookup(Ctors, ConsId, ConsDefns),
    list__member(ConsDefn, ConsDefns),

    ConsDefn = hlds_cons_defn(ExistQVars0, _, Args, TypeCtor, _),

    % XXX handle ExistQVars
    ExistQVars0 = [],

    hlds_data__get_type_defn_tparams(TypeDefn, TypeParams),

    map__from_corresponding_lists(TypeParams, TypeArgs, TSubst),
    assoc_list__values(Args, ArgTypes0),
    apply_subst_to_type_list(TSubst, ArgTypes0, ArgTypes).

is_existq_cons(ModuleInfo, VarType, ConsId) :-
    is_existq_cons(ModuleInfo, VarType, ConsId, _).

:- pred is_existq_cons(module_info::in, mer_type::in, cons_id::in,
    hlds_cons_defn::out) is semidet.

is_existq_cons(ModuleInfo, VarType, ConsId, ConsDefn) :-
    type_to_ctor_and_args(VarType, TypeCtor, _),
    get_cons_defn(ModuleInfo, TypeCtor, ConsId, ConsDefn),
    ConsDefn = hlds_cons_defn(ExistQVars, _, _, _, _),
    ExistQVars = [_ | _].

    % Given a type and a cons_id, look up the definition of that constructor;
    % if it is existentially typed, return its definition, otherwise fail.
get_existq_cons_defn(ModuleInfo, VarType, ConsId, CtorDefn) :-
    is_existq_cons(ModuleInfo, VarType, ConsId, ConsDefn),
    ConsDefn = hlds_cons_defn(ExistQVars, Constraints, Args, _, _),
    assoc_list__values(Args, ArgTypes),
    module_info_get_type_table(ModuleInfo, Types),
    type_to_ctor_and_args(VarType, TypeCtor, _),
    map__lookup(Types, TypeCtor, TypeDefn),
    hlds_data__get_type_defn_tvarset(TypeDefn, TypeVarSet),
    hlds_data__get_type_defn_tparams(TypeDefn, TypeParams),
    hlds_data__get_type_defn_kind_map(TypeDefn, KindMap),
    prog_type.var_list_to_type_list(KindMap, TypeParams, TypeCtorArgs),
    type_to_ctor_and_args(VarType, TypeCtor, _),
    construct_type(TypeCtor, TypeCtorArgs, RetType),
    CtorDefn = ctor_defn(TypeVarSet, ExistQVars, KindMap, Constraints,
        ArgTypes, RetType).

get_type_and_cons_defn(ModuleInfo, Type, ConsId, TypeDefn, ConsDefn) :-
    (
        type_to_ctor_and_args(Type, TypeCtor, _),
        do_get_type_and_cons_defn(ModuleInfo,
            TypeCtor, ConsId, TypeDefnPrime, ConsDefnPrime)
    ->
        TypeDefn = TypeDefnPrime,
        ConsDefn = ConsDefnPrime
    ;
        error("gget_type_and_cons_defn")
    ).

:- pred do_get_type_and_cons_defn(module_info::in, type_ctor::in, cons_id::in,
    hlds_type_defn::out, hlds_cons_defn::out) is semidet.

do_get_type_and_cons_defn(ModuleInfo, TypeCtor, ConsId, TypeDefn, ConsDefn) :-
    get_cons_defn(ModuleInfo, TypeCtor, ConsId, ConsDefn),
    module_info_get_type_table(ModuleInfo, Types),
    map__lookup(Types, TypeCtor, TypeDefn).

get_cons_defn(ModuleInfo, TypeCtor, ConsId, ConsDefn) :-
    module_info_get_cons_table(ModuleInfo, Ctors),
    % will fail for builtin cons_ids.
    map__search(Ctors, ConsId, ConsDefns),
    MatchingCons = (pred(ThisConsDefn::in) is semidet :-
            ThisConsDefn = hlds_cons_defn(_, _, _, TypeCtor, _)
        ),
    list__filter(MatchingCons, ConsDefns, [ConsDefn]).

%-----------------------------------------------------------------------------%

type_is_no_tag_type(ModuleInfo, Type, Ctor, ArgType) :-
    type_to_ctor_and_args(Type, TypeCtor, TypeArgs),
    module_info_get_no_tag_types(ModuleInfo, NoTagTypes),
    map__search(NoTagTypes, TypeCtor, NoTagType),
    NoTagType = no_tag_type(TypeParams, Ctor, ArgType0),
    (
        TypeParams = [],
        ArgType = ArgType0
    ;
        TypeParams = [_ | _],
        map__from_corresponding_lists(TypeParams, TypeArgs, Subn),
        apply_subst_to_type(Subn, ArgType0, ArgType)
    ).

%-----------------------------------------------------------------------------%

    % Substitute the actual values of the type parameters in list of
    % constructors, for a particular instance of a polymorphic type.
    %
:- pred substitute_type_args(list(type_param)::in, list(mer_type)::in,
    list(constructor)::in, list(constructor)::out) is det.

substitute_type_args(TypeParams, TypeArgs, Constructors0, Constructors) :-
    (
        TypeParams = [],
        Constructors = Constructors0
    ;
        TypeParams = [_ | _],
        map__from_corresponding_lists(TypeParams, TypeArgs, Subst),
        substitute_type_args_2(Subst, Constructors0, Constructors)
    ).

:- pred substitute_type_args_2(tsubst::in, list(constructor)::in,
    list(constructor)::out) is det.

substitute_type_args_2(_, [], []).
substitute_type_args_2(Subst, [Ctor0 | Ctors0], [Ctor | Ctors]) :-
    % Note: prog_io.m ensures that the existentially quantified variables,
    % if any, are distinct from the parameters, and that the (existential)
    % constraints can only contain existentially quantified variables,
    % so there's no need to worry about applying the substitution to ExistQVars
    % or Constraints.
    Ctor0 = ctor(ExistQVars, Constraints, Name, Args0),
    substitute_type_args_3(Subst, Args0, Args),
    substitute_type_args_2(Subst, Ctors0, Ctors),
    Ctor = ctor(ExistQVars, Constraints, Name, Args).

:- pred substitute_type_args_3(tsubst::in, list(constructor_arg)::in,
    list(constructor_arg)::out) is det.

substitute_type_args_3(_, [], []).
substitute_type_args_3(Subst, [Name - Arg0 | Args0], [Name - Arg | Args]) :-
    apply_subst_to_type(Subst, Arg0, Arg),
    substitute_type_args_3(Subst, Args0, Args).

%-----------------------------------------------------------------------------%

cons_id_adjusted_arity(ModuleInfo, Type, ConsId) = AdjustedArity :-
    % Figure out the arity of this constructor, _including_ any type-infos
    % or typeclass-infos inserted for existential data types.
    ConsArity = cons_id_arity(ConsId),
    ( get_existq_cons_defn(ModuleInfo, Type, ConsId, ConsDefn) ->
        ConsDefn = ctor_defn(_TVarSet, ExistQTVars, _KindMap,
            Constraints, _ArgTypes, _ResultType),
        list__length(Constraints, NumTypeClassInfos),
        constraint_list_get_tvars(Constraints, ConstrainedTVars),
        list__delete_elems(ExistQTVars, ConstrainedTVars,
            UnconstrainedExistQTVars),
        list__length(UnconstrainedExistQTVars, NumTypeInfos),
        AdjustedArity = ConsArity + NumTypeClassInfos + NumTypeInfos
    ;
        AdjustedArity = ConsArity
    ).

%-----------------------------------------------------------------------------%

maybe_get_cons_id_arg_types(ModuleInfo, MaybeType, ConsId0, Arity,
        MaybeTypes) :-
    ( ConsId0 = cons(_SymName, _) ->
        ConsId = ConsId0,
        (
            MaybeType = yes(Type),

            % XXX get_cons_id_non_existential_arg_types will fail
            % for ConsIds with existentially typed arguments.
            get_cons_id_non_existential_arg_types(ModuleInfo, Type,
                ConsId, Types),
            list__length(Types, Arity)
        ->
            MaybeTypes = list__map(func(T) = yes(T), Types)
        ;
            list__duplicate(Arity, no, MaybeTypes)
        )
    ;
        MaybeTypes = []
    ).

maybe_get_higher_order_arg_types(MaybeType, Arity, MaybeTypes) :-
    (
        MaybeType = yes(Type),
        type_is_higher_order(Type, _, _, _, Types)
    ->
        MaybeTypes = list__map(func(T) = yes(T), Types)
    ;
        list__duplicate(Arity, no, MaybeTypes)
    ).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_constraint(Renaming, !Constraint) :-
    !.Constraint = constraint(Ids, ClassName, ClassArgTypes0),
    apply_variable_renaming_to_type_list(Renaming, ClassArgTypes0,
        ClassArgTypes),
    !:Constraint = constraint(Ids, ClassName, ClassArgTypes).

apply_subst_to_constraint(Subst, !Constraint) :-
    !.Constraint = constraint(Ids, ClassName, Types0),
    apply_subst_to_type_list(Subst, Types0, Types),
    !:Constraint = constraint(Ids, ClassName, Types).

apply_rec_subst_to_constraint(Subst, !Constraint) :-
    !.Constraint = constraint(Ids, Name, Types0),
    apply_rec_subst_to_type_list(Subst, Types0, Types),
    !:Constraint = constraint(Ids, Name, Types).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_constraint_list(Renaming, !Constraints) :-
    list__map(apply_variable_renaming_to_constraint(Renaming), !Constraints).

apply_subst_to_constraint_list(Subst, !Constraints) :-
    list__map(apply_subst_to_constraint(Subst), !Constraints).

apply_rec_subst_to_constraint_list(Subst, !Constraints) :-
    list__map(apply_rec_subst_to_constraint(Subst), !Constraints).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_constraints(Renaming, !Constraints) :-
    !.Constraints = constraints(Unproven0, Assumed0, Redundant0),
    apply_variable_renaming_to_constraint_list(Renaming, Unproven0, Unproven),
    apply_variable_renaming_to_constraint_list(Renaming, Assumed0, Assumed),
    Pred = (pred(_::in, C0::in, C::out) is det :-
        apply_variable_renaming_to_constraint_list(Renaming, C0, C)
    ),
    map.map_values(Pred, Redundant0, Redundant),
    !:Constraints = constraints(Unproven, Assumed, Redundant).

apply_subst_to_constraints(Subst, !Constraints) :-
    !.Constraints = constraints(Unproven0, Assumed0, Redundant0),
    apply_subst_to_constraint_list(Subst, Unproven0, Unproven),
    apply_subst_to_constraint_list(Subst, Assumed0, Assumed),
    Pred = (pred(_::in, C0::in, C::out) is det :-
        apply_subst_to_constraint_list(Subst, C0, C)
    ),
    map.map_values(Pred, Redundant0, Redundant),
    !:Constraints = constraints(Unproven, Assumed, Redundant).

apply_rec_subst_to_constraints(Subst, !Constraints) :-
    !.Constraints = constraints(Unproven0, Assumed0, Redundant0),
    apply_rec_subst_to_constraint_list(Subst, Unproven0, Unproven),
    apply_rec_subst_to_constraint_list(Subst, Assumed0, Assumed),
    Pred = (pred(_::in, C0::in, C::out) is det :-
        apply_rec_subst_to_constraint_list(Subst, C0, C)
    ),
    map.map_values(Pred, Redundant0, Redundant),
    !:Constraints = constraints(Unproven, Assumed, Redundant).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_constraint_proofs(Renaming, Proofs0, Proofs) :-
    ( map__is_empty(Proofs0) ->
        % Optimize the simple case.
        Proofs = Proofs0
    ;
        map__keys(Proofs0, Keys0),
        map__values(Proofs0, Values0),
        apply_variable_renaming_to_prog_constraint_list(Renaming, Keys0, Keys),
        list__map(rename_constraint_proof(Renaming), Values0, Values),
        map__from_corresponding_lists(Keys, Values, Proofs)
    ).

    % Apply a type variable renaming to a class constraint proof.
    %
:- pred rename_constraint_proof(tvar_renaming::in, constraint_proof::in,
    constraint_proof::out) is det.

rename_constraint_proof(_TSubst, apply_instance(Num), apply_instance(Num)).
rename_constraint_proof(TSubst, superclass(ClassConstraint0),
        superclass(ClassConstraint)) :-
    apply_variable_renaming_to_prog_constraint(TSubst, ClassConstraint0,
        ClassConstraint).

apply_subst_to_constraint_proofs(Subst, Proofs0, Proofs) :-
    map__foldl(apply_subst_to_constraint_proofs_2(Subst), Proofs0,
        map__init, Proofs).

:- pred apply_subst_to_constraint_proofs_2(tsubst::in,
    prog_constraint::in, constraint_proof::in,
    constraint_proof_map::in, constraint_proof_map::out) is det.

apply_subst_to_constraint_proofs_2(Subst, Constraint0, Proof0, Map0, Map) :-
    apply_subst_to_prog_constraint(Subst, Constraint0, Constraint),
    (
        Proof0 = apply_instance(_),
        Proof = Proof0
    ;
        Proof0 = superclass(Super0),
        apply_subst_to_prog_constraint(Subst, Super0, Super),
        Proof = superclass(Super)
    ),
    map__set(Map0, Constraint, Proof, Map).

apply_rec_subst_to_constraint_proofs(Subst, Proofs0, Proofs) :-
    map__foldl(apply_rec_subst_to_constraint_proofs_2(Subst), Proofs0,
        map__init, Proofs).

:- pred apply_rec_subst_to_constraint_proofs_2(tsubst::in,
    prog_constraint::in, constraint_proof::in,
    constraint_proof_map::in, constraint_proof_map::out) is det.

apply_rec_subst_to_constraint_proofs_2(Subst, Constraint0, Proof0, !Map) :-
    apply_rec_subst_to_prog_constraint(Subst, Constraint0, Constraint),
    (
        Proof0 = apply_instance(_),
        Proof = Proof0
    ;
        Proof0 = superclass(Super0),
        apply_rec_subst_to_prog_constraint(Subst, Super0, Super),
        Proof = superclass(Super)
    ),
    map__set(!.Map, Constraint, Proof, !:Map).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_constraint_map(Renaming, !ConstraintMap) :-
    map__map_values(apply_variable_renaming_to_constraint_map_2(Renaming),
        !ConstraintMap).

:- pred apply_variable_renaming_to_constraint_map_2(tvar_renaming::in,
    constraint_id::in, prog_constraint::in, prog_constraint::out) is det.

apply_variable_renaming_to_constraint_map_2(Renaming, _Key, !Value) :-
    apply_variable_renaming_to_prog_constraint(Renaming, !Value).

apply_subst_to_constraint_map(Subst, !ConstraintMap) :-
    map__map_values(apply_subst_to_constraint_map_2(Subst), !ConstraintMap).

:- pred apply_subst_to_constraint_map_2(tsubst::in, constraint_id::in,
    prog_constraint::in, prog_constraint::out) is det.

apply_subst_to_constraint_map_2(Subst, _Key, !Value) :-
    apply_subst_to_prog_constraint(Subst, !Value).

apply_rec_subst_to_constraint_map(Subst, !ConstraintMap) :-
    map__map_values(apply_rec_subst_to_constraint_map_2(Subst),
        !ConstraintMap).

:- pred apply_rec_subst_to_constraint_map_2(tsubst::in, constraint_id::in,
    prog_constraint::in, prog_constraint::out) is det.

apply_rec_subst_to_constraint_map_2(Subst, _Key, !Value) :-
    apply_rec_subst_to_prog_constraint(Subst, !Value).

%-----------------------------------------------------------------------------%

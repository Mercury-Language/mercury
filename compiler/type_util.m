%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: type_util.m.
% Main author: fjh.
%
% This file provides some utility predicates which operate on types.
% It is used by various stages of the compilation after type-checking,
% include the mode checker and the code generator.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.type_util.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % Given a type_ctor, look up its module/name/arity.
    %
:- func type_ctor_module(module_info, type_ctor) = module_name.
:- func type_ctor_name(module_info, type_ctor) = string.
:- func type_ctor_arity(module_info, type_ctor) = arity.

    % Succeed iff type is an "atomic" type - one which can be unified
    % using a simple_test rather than a complicated_unify.
    %
:- pred type_is_atomic(module_info::in, mer_type::in) is semidet.

:- pred type_ctor_is_atomic(module_info::in, type_ctor::in) is semidet.

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

    % Succeed if the type body is for a solver type.
    %
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

    % Report whether it is OK to include a value of the given time
    % in a heap cell allocated with GC_malloc_atomic.
    %
:- func type_may_use_atomic_alloc(module_info, mer_type) =
    may_use_atomic_alloc.

    % update_type_may_use_atomic_alloc(ModuleInfo, Type, !MaybeUseAtomic):
    %
    % Find out whether it is OK to include a value of the given time
    % in a heap cell allocated with GC_malloc_atomic. If yes, leave
    % !MaybeUseAtomic alone. If no, set !:MaybeUseAtomic to
    % may_not_use_atomic_alloc.
    %
:- pred update_type_may_use_atomic_alloc(module_info::in, mer_type::in,
    may_use_atomic_alloc::in, may_use_atomic_alloc::out) is det.

    % If the type is a du type or a tuple type, return the list of its
    % constructors.
    %
:- pred type_constructors(mer_type::in, module_info::in,
    list(constructor)::out) is semidet.

    % Given a type on which it is possible to have a complete switch,
    % return the number of alternatives. (It is possible to have a complete
    % switch on any du type and on the builtin type character. It is not
    % feasible to have a complete switch on the builtin types integer,
    % float, and string. One cannot have a switch on an abstract type,
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

    % Like get_type_and_cons_defn (above), except that it only returns
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
:- import_module libs.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module map.
:- import_module pair.

%-----------------------------------------------------------------------------%

type_ctor_module(_ModuleInfo, type_ctor(TypeSymName, _Arity)) = ModuleName :-
    sym_name_get_module_name(TypeSymName, unqualified(""), ModuleName).

type_ctor_name(_ModuleInfo, type_ctor(TypeSymName, _Arity)) =
    unqualify_name(TypeSymName).

type_ctor_arity(_ModuleInfo, type_ctor(_Name, Arity)) = Arity.

type_is_atomic(ModuleInfo, Type) :-
    type_to_ctor_and_args(Type, TypeCtor, _),
    type_ctor_is_atomic(ModuleInfo, TypeCtor).

type_ctor_is_atomic(ModuleInfo, TypeCtor) :-
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

type_to_type_defn(ModuleInfo, Type, TypeDefn) :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    type_to_ctor_and_args(Type, TypeCtor, _TypeArgs),
    map.search(TypeTable, TypeCtor, TypeDefn).

type_to_type_defn_body(ModuleInfo, Type, TypeBody) :-
    type_to_type_defn(ModuleInfo, Type, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody).

type_has_user_defined_equality_pred(ModuleInfo, Type, UserEqComp) :-
    type_to_type_defn_body(ModuleInfo, Type, TypeBody),
    type_body_has_user_defined_equality_pred(ModuleInfo, TypeBody, UserEqComp).

type_body_has_user_defined_equality_pred(ModuleInfo, TypeBody, UserEqComp) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    (
        TypeBody = hlds_du_type(_, _, _, _, _, _),
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
        TypeBody = hlds_foreign_type(ForeignTypeBody),
        UserEqComp = foreign_type_body_has_user_defined_eq_comp_pred(
            ModuleInfo, ForeignTypeBody)
    ;
        TypeBody = hlds_solver_type(_SolverTypeDetails, yes(UserEqComp))
    ).

type_is_solver_type(ModuleInfo, Type) :-
    type_to_type_defn_body(ModuleInfo, Type, TypeBody),
    (
        TypeBody = hlds_solver_type(_, _)
    ;
        TypeBody = hlds_abstract_type(solver_type)
    ;
        TypeBody = hlds_eqv_type(EqvType),
        type_is_solver_type(ModuleInfo, EqvType)
    ).

type_has_solver_type_details(ModuleInfo, Type, SolverTypeDetails) :-
    type_to_type_defn_body(ModuleInfo, Type, TypeBody),
    type_body_has_solver_type_details(ModuleInfo, TypeBody,
        SolverTypeDetails).

type_body_has_solver_type_details(ModuleInfo, Type, SolverTypeDetails) :-
    (
        Type = hlds_solver_type(SolverTypeDetails, _MaybeUserEqComp)
    ;
        Type = hlds_eqv_type(EqvType),
        type_has_solver_type_details(ModuleInfo, EqvType, SolverTypeDetails)
    ).

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

type_body_is_solver_type(ModuleInfo, TypeBody) :-
    (
        TypeBody = hlds_solver_type(_, _)
    ;
        TypeBody = hlds_abstract_type(solver_type)
    ;
        TypeBody = hlds_eqv_type(Type),
        is_solver_type(ModuleInfo, Type)
    ).

is_existq_type(Module, Type) :-
    type_constructors(Type, Module, Constructors),
    some [Constructor] (
        list.member(Constructor, Constructors),
        Constructor ^ cons_exist = [_ | _]
    ).

is_dummy_argument_type(ModuleInfo, Type) :-
    ( type_to_ctor_and_args(Type, TypeCtor, _) ->
        % Keep this in sync with is_dummy_argument_type_with_constructors
        % above.
        (
            TypeCtor = type_ctor(CtorSymName, TypeArity),
            CtorSymName = qualified(unqualified(ModuleName), TypeName),
            is_builtin_dummy_argument_type(ModuleName, TypeName, TypeArity)
        ;
            module_info_get_type_table(ModuleInfo, TypeTable),
            % This can fail for some builtin type constructors such as func,
            % pred, and tuple, none of which are dummy types.
            map.search(TypeTable, TypeCtor, TypeDefn),
            get_type_defn_body(TypeDefn, TypeBody),
            Ctors = TypeBody ^ du_type_ctors,
            UserEqCmp = TypeBody ^ du_type_usereq,
            constructor_list_represents_dummy_argument_type(Ctors, UserEqCmp)
        )
    ;
        fail
    ).

type_ctor_has_hand_defined_rtti(Type, Body) :-
    Type = type_ctor(qualified(mercury_private_builtin_module, Name), 0),
    ( Name = "type_info"
    ; Name = "type_ctor_info"
    ; Name = "typeclass_info"
    ; Name = "base_typeclass_info"
    ),
    \+ ( Body = hlds_du_type(_, _, _, _, _, yes(_))
       ; Body = hlds_foreign_type(_)
       ; Body = hlds_solver_type(_, _)
       ).

%-----------------------------------------------------------------------------%

classify_type(ModuleInfo, VarType) = TypeCategory :-
    ( type_to_ctor_and_args(VarType, TypeCtor, _) ->
        TypeCategory = classify_type_ctor(ModuleInfo, TypeCtor)
    ;
        TypeCategory = type_cat_variable
    ).

classify_type_ctor(ModuleInfo, TypeCtor) = TypeCategory :-
    TypeCtor = type_ctor(TypeSymName, Arity),
    (
        TypeSymName = unqualified(TypeName),
        Arity = 0,
        (
            TypeName = "character",
            TypeCategoryPrime = type_cat_char
        ;
            TypeName = "int",
            TypeCategoryPrime = type_cat_int
        ;
            TypeName = "float",
            TypeCategoryPrime = type_cat_float
        ;
            TypeName = "string",
            TypeCategoryPrime = type_cat_string
        ;
            TypeName = "void",
            TypeCategoryPrime = type_cat_void
        )
    ->
        TypeCategory = TypeCategoryPrime
    ;
        TypeSymName = qualified(ModuleSymName, TypeName),
        ModuleSymName = mercury_private_builtin_module,
        Arity = 0,
        (
            TypeName = "type_info",
            TypeCategoryPrime = type_cat_type_info
        ;
            TypeName = "type_ctor_info",
            TypeCategoryPrime = type_cat_type_ctor_info
        ;
            TypeName = "typeclass_info",
            TypeCategoryPrime = type_cat_typeclass_info
        ;
            TypeName = "base_typeclass_info",
            TypeCategoryPrime = type_cat_base_typeclass_info
        )
    ->
        TypeCategory = TypeCategoryPrime
    ;
        TypeSymName = qualified(unqualified(ModuleName), TypeName),
        is_builtin_dummy_argument_type(ModuleName, TypeName, Arity)
    ->
        TypeCategory = type_cat_dummy
    ;
        ( type_ctor_is_higher_order(TypeCtor, _, _, _) ->
            TypeCategory = type_cat_higher_order
        ; type_ctor_is_tuple(TypeCtor) ->
            TypeCategory = type_cat_tuple
        ; type_ctor_is_enumeration(TypeCtor, ModuleInfo) ->
            TypeCategory = type_cat_enum
        ;
            TypeCategory = type_cat_user_ctor
        )
    ).

%-----------------------------------------------------------------------------%

:- pred type_ctor_is_enumeration(type_ctor::in, module_info::in) is semidet.

type_ctor_is_enumeration(TypeCtor, ModuleInfo) :-
    module_info_get_type_table(ModuleInfo, TypeDefnTable),
    map.search(TypeDefnTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    TypeBody ^ du_type_is_enum = is_enum.

%-----------------------------------------------------------------------------%

update_type_may_use_atomic_alloc(ModuleInfo, Type, !MayUseAtomic) :-
    (
        !.MayUseAtomic = may_not_use_atomic_alloc
        % There is no point in testing Type.
    ;
        !.MayUseAtomic = may_use_atomic_alloc,
        !:MayUseAtomic = type_may_use_atomic_alloc(ModuleInfo, Type)
    ).

type_may_use_atomic_alloc(ModuleInfo, Type) = TypeMayUseAtomic :-
    TypeCategory = classify_type(ModuleInfo, Type),
    (
        ( TypeCategory = type_cat_int
        ; TypeCategory = type_cat_char
        ; TypeCategory = type_cat_enum
        ; TypeCategory = type_cat_dummy
        ; TypeCategory = type_cat_type_ctor_info
        ),
        TypeMayUseAtomic = may_use_atomic_alloc
    ;
        TypeCategory = type_cat_float,
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, unboxed_float, UBF),
        (
            UBF = yes,
            TypeMayUseAtomic = may_use_atomic_alloc
        ;
            UBF = no,
            TypeMayUseAtomic = may_not_use_atomic_alloc
        )
    ;
        ( TypeCategory = type_cat_string
        ; TypeCategory = type_cat_higher_order
        ; TypeCategory = type_cat_tuple
        ; TypeCategory = type_cat_variable
        ; TypeCategory = type_cat_type_info
        ; TypeCategory = type_cat_typeclass_info
        ; TypeCategory = type_cat_base_typeclass_info
        ; TypeCategory = type_cat_void
        ; TypeCategory = type_cat_user_ctor
        ),
        TypeMayUseAtomic = may_not_use_atomic_alloc
    ).

%-----------------------------------------------------------------------------%

type_constructors(Type, ModuleInfo, Constructors) :-
    type_to_ctor_and_args(Type, TypeCtor, TypeArgs),
    ( type_ctor_is_tuple(TypeCtor) ->
        % Tuples are never existentially typed.
        ExistQVars = [],
        ClassConstraints = [],
        CtorArgs = list.map((func(ArgType) = no - ArgType), TypeArgs),
        Constructors = [ctor(ExistQVars, ClassConstraints, unqualified("{}"),
            CtorArgs)]
    ;
        module_info_get_type_table(ModuleInfo, TypeTable),
        map.search(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        substitute_type_args(TypeParams, TypeArgs, TypeBody ^ du_type_ctors,
            Constructors)
    ).

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
        map.from_corresponding_lists(TypeParams, TypeArgs, Subst),
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

switch_type_num_functors(ModuleInfo, Type, NumFunctors) :-
    type_to_ctor_and_args(Type, TypeCtor, _),
    ( TypeCtor = type_ctor(unqualified("character"), 0) ->
        % XXX The following code uses the source machine's character size,
        % not the target's, so it won't work if cross-compiling to a machine
        % with a different size character.
        char.max_char_value(MaxChar),
        char.min_char_value(MinChar),
        NumFunctors = MaxChar - MinChar + 1
    ; type_ctor_is_tuple(TypeCtor) ->
        NumFunctors = 1
    ;
        module_info_get_type_table(ModuleInfo, TypeTable),
        map.search(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        map.count(TypeBody ^ du_type_cons_tag_values, NumFunctors)
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
            hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),

            % XXX handle ExistQVars
            (
                ExistQVars0 = []
            ;
                ExistQVars0 = [_ | _],
                (
                    EQVarAction = abort_on_exist_qvar,
                    unexpected(this_file,
                        "get_cons_id_arg_types: existentially typed cons_id")
                ;
                    EQVarAction = fail_on_exist_qvar,
                    fail
                )
            ),

            map.from_corresponding_lists(TypeParams, TypeArgs, TSubst),
            assoc_list.values(Args, ArgTypes0),
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
    map.search(Types, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeDefnBody),
    map.member(TypeDefnBody ^ du_type_cons_tag_values, ConsId, _),

    module_info_get_cons_table(ModuleInfo, Ctors),
    map.lookup(Ctors, ConsId, ConsDefns),
    list.member(ConsDefn, ConsDefns),

    ConsDefn = hlds_cons_defn(ExistQVars0, _, Args, TypeCtor, _),

    % XXX handle ExistQVars
    ExistQVars0 = [],

    hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),

    map.from_corresponding_lists(TypeParams, TypeArgs, TSubst),
    assoc_list.values(Args, ArgTypes0),
    apply_subst_to_type_list(TSubst, ArgTypes0, ArgTypes).

is_existq_cons(ModuleInfo, VarType, ConsId) :-
    is_existq_cons(ModuleInfo, VarType, ConsId, _).

:- pred is_existq_cons(module_info::in, mer_type::in, cons_id::in,
    hlds_cons_defn::out) is semidet.

get_type_and_cons_defn(ModuleInfo, Type, ConsId, TypeDefn, ConsDefn) :-
    (
        type_to_ctor_and_args(Type, TypeCtor, _),
        do_get_type_and_cons_defn(ModuleInfo,
            TypeCtor, ConsId, TypeDefnPrime, ConsDefnPrime)
    ->
        TypeDefn = TypeDefnPrime,
        ConsDefn = ConsDefnPrime
    ;
        unexpected(this_file, "gget_type_and_cons_defn")
    ).

:- pred do_get_type_and_cons_defn(module_info::in, type_ctor::in, cons_id::in,
    hlds_type_defn::out, hlds_cons_defn::out) is semidet.

do_get_type_and_cons_defn(ModuleInfo, TypeCtor, ConsId, TypeDefn, ConsDefn) :-
    get_cons_defn(ModuleInfo, TypeCtor, ConsId, ConsDefn),
    module_info_get_type_table(ModuleInfo, Types),
    map.lookup(Types, TypeCtor, TypeDefn).

get_cons_defn(ModuleInfo, TypeCtor, ConsId, ConsDefn) :-
    module_info_get_cons_table(ModuleInfo, Ctors),
    % will fail for builtin cons_ids.
    map.search(Ctors, ConsId, ConsDefns),
    MatchingCons =
        (pred(ThisConsDefn::in) is semidet :-
            ThisConsDefn = hlds_cons_defn(_, _, _, TypeCtor, _)
        ),
    list.filter(MatchingCons, ConsDefns, [ConsDefn]).

    % Given a type and a cons_id, look up the definition of that constructor;
    % if it is existentially typed, return its definition, otherwise fail.
get_existq_cons_defn(ModuleInfo, VarType, ConsId, CtorDefn) :-
    is_existq_cons(ModuleInfo, VarType, ConsId, ConsDefn),
    ConsDefn = hlds_cons_defn(ExistQVars, Constraints, Args, _, _),
    assoc_list.values(Args, ArgTypes),
    module_info_get_type_table(ModuleInfo, Types),
    type_to_ctor_and_args(VarType, TypeCtor, _),
    map.lookup(Types, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_tvarset(TypeDefn, TypeVarSet),
    hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
    hlds_data.get_type_defn_kind_map(TypeDefn, KindMap),
    prog_type.var_list_to_type_list(KindMap, TypeParams, TypeCtorArgs),
    type_to_ctor_and_args(VarType, TypeCtor, _),
    construct_type(TypeCtor, TypeCtorArgs, RetType),
    CtorDefn = ctor_defn(TypeVarSet, ExistQVars, KindMap, Constraints,
        ArgTypes, RetType).

is_existq_cons(ModuleInfo, VarType, ConsId, ConsDefn) :-
    type_to_ctor_and_args(VarType, TypeCtor, _),
    get_cons_defn(ModuleInfo, TypeCtor, ConsId, ConsDefn),
    ConsDefn = hlds_cons_defn(ExistQVars, _, _, _, _),
    ExistQVars = [_ | _].

%-----------------------------------------------------------------------------%

type_is_no_tag_type(ModuleInfo, Type, Ctor, ArgType) :-
    type_to_ctor_and_args(Type, TypeCtor, TypeArgs),
    module_info_get_no_tag_types(ModuleInfo, NoTagTypes),
    map.search(NoTagTypes, TypeCtor, NoTagType),
    NoTagType = no_tag_type(TypeParams, Ctor, ArgType0),
    (
        TypeParams = [],
        ArgType = ArgType0
    ;
        TypeParams = [_ | _],
        map.from_corresponding_lists(TypeParams, TypeArgs, Subn),
        apply_subst_to_type(Subn, ArgType0, ArgType)
    ).

%-----------------------------------------------------------------------------%

cons_id_adjusted_arity(ModuleInfo, Type, ConsId) = AdjustedArity :-
    % Figure out the arity of this constructor, _including_ any type-infos
    % or typeclass-infos inserted for existential data types.
    ConsArity = cons_id_arity(ConsId),
    ( get_existq_cons_defn(ModuleInfo, Type, ConsId, ConsDefn) ->
        ConsDefn = ctor_defn(_TVarSet, ExistQTVars, _KindMap,
            Constraints, _ArgTypes, _ResultType),
        list.length(Constraints, NumTypeClassInfos),
        constraint_list_get_tvars(Constraints, ConstrainedTVars),
        list.delete_elems(ExistQTVars, ConstrainedTVars,
            UnconstrainedExistQTVars),
        list.length(UnconstrainedExistQTVars, NumTypeInfos),
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
            list.length(Types, Arity)
        ->
            MaybeTypes = list.map(func(T) = yes(T), Types)
        ;
            list.duplicate(Arity, no, MaybeTypes)
        )
    ;
        MaybeTypes = []
    ).

maybe_get_higher_order_arg_types(MaybeType, Arity, MaybeTypes) :-
    (
        MaybeType = yes(Type),
        type_is_higher_order_details(Type, _, _, _, ArgTypes)
    ->
        MaybeTypes = list.map(func(T) = yes(T), ArgTypes)
    ;
        list.duplicate(Arity, no, MaybeTypes)
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
    list.map(apply_variable_renaming_to_constraint(Renaming), !Constraints).

apply_subst_to_constraint_list(Subst, !Constraints) :-
    list.map(apply_subst_to_constraint(Subst), !Constraints).

apply_rec_subst_to_constraint_list(Subst, !Constraints) :-
    list.map(apply_rec_subst_to_constraint(Subst), !Constraints).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_constraints(Renaming, !Constraints) :-
    !.Constraints = constraints(Unproven0, Assumed0, Redundant0, Ancestors0),
    apply_variable_renaming_to_constraint_list(Renaming, Unproven0, Unproven),
    apply_variable_renaming_to_constraint_list(Renaming, Assumed0, Assumed),
    Pred = (pred(_::in, C0::in, C::out) is det :-
        apply_variable_renaming_to_constraint_list(Renaming, C0, C)
    ),
    map.map_values(Pred, Redundant0, Redundant),
    map.keys(Ancestors0, AncestorsKeys0),
    map.values(Ancestors0, AncestorsValues0),
    apply_variable_renaming_to_prog_constraint_list(Renaming, AncestorsKeys0,
        AncestorsKeys),
    list.map(apply_variable_renaming_to_prog_constraint_list(Renaming),
        AncestorsValues0, AncestorsValues),
    map.from_corresponding_lists(AncestorsKeys, AncestorsValues, Ancestors),
    !:Constraints = constraints(Unproven, Assumed, Redundant, Ancestors).

apply_subst_to_constraints(Subst, !Constraints) :-
    !.Constraints = constraints(Unproven0, Assumed0, Redundant0, Ancestors0),
    apply_subst_to_constraint_list(Subst, Unproven0, Unproven),
    apply_subst_to_constraint_list(Subst, Assumed0, Assumed),
    Pred = (pred(_::in, C0::in, C::out) is det :-
        apply_subst_to_constraint_list(Subst, C0, C)
    ),
    map.map_values(Pred, Redundant0, Redundant),
    map.keys(Ancestors0, AncestorsKeys0),
    map.values(Ancestors0, AncestorsValues0),
    apply_subst_to_prog_constraint_list(Subst, AncestorsKeys0,
        AncestorsKeys),
    list.map(apply_subst_to_prog_constraint_list(Subst),
        AncestorsValues0, AncestorsValues),
    map.from_corresponding_lists(AncestorsKeys, AncestorsValues, Ancestors),
    !:Constraints = constraints(Unproven, Assumed, Redundant, Ancestors).

apply_rec_subst_to_constraints(Subst, !Constraints) :-
    !.Constraints = constraints(Unproven0, Assumed0, Redundant0, Ancestors0),
    apply_rec_subst_to_constraint_list(Subst, Unproven0, Unproven),
    apply_rec_subst_to_constraint_list(Subst, Assumed0, Assumed),
    Pred = (pred(_::in, C0::in, C::out) is det :-
        apply_rec_subst_to_constraint_list(Subst, C0, C)
    ),
    map.map_values(Pred, Redundant0, Redundant),
    map.keys(Ancestors0, AncestorsKeys0),
    map.values(Ancestors0, AncestorsValues0),
    apply_rec_subst_to_prog_constraint_list(Subst, AncestorsKeys0,
        AncestorsKeys),
    list.map(apply_rec_subst_to_prog_constraint_list(Subst),
        AncestorsValues0, AncestorsValues),
    map.from_corresponding_lists(AncestorsKeys, AncestorsValues, Ancestors),
    !:Constraints = constraints(Unproven, Assumed, Redundant, Ancestors).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_constraint_proofs(Renaming, Proofs0, Proofs) :-
    ( map.is_empty(Proofs0) ->
        % Optimize the simple case.
        Proofs = Proofs0
    ;
        map.keys(Proofs0, Keys0),
        map.values(Proofs0, Values0),
        apply_variable_renaming_to_prog_constraint_list(Renaming, Keys0, Keys),
        list.map(rename_constraint_proof(Renaming), Values0, Values),
        map.from_corresponding_lists(Keys, Values, Proofs)
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
    map.foldl(apply_subst_to_constraint_proofs_2(Subst), Proofs0,
        map.init, Proofs).

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
    map.set(Map0, Constraint, Proof, Map).

apply_rec_subst_to_constraint_proofs(Subst, Proofs0, Proofs) :-
    map.foldl(apply_rec_subst_to_constraint_proofs_2(Subst), Proofs0,
        map.init, Proofs).

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
    map.set(!.Map, Constraint, Proof, !:Map).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_constraint_map(Renaming, !ConstraintMap) :-
    map.map_values(apply_variable_renaming_to_constraint_map_2(Renaming),
        !ConstraintMap).

:- pred apply_variable_renaming_to_constraint_map_2(tvar_renaming::in,
    constraint_id::in, prog_constraint::in, prog_constraint::out) is det.

apply_variable_renaming_to_constraint_map_2(Renaming, _Key, !Value) :-
    apply_variable_renaming_to_prog_constraint(Renaming, !Value).

apply_subst_to_constraint_map(Subst, !ConstraintMap) :-
    map.map_values(apply_subst_to_constraint_map_2(Subst), !ConstraintMap).

:- pred apply_subst_to_constraint_map_2(tsubst::in, constraint_id::in,
    prog_constraint::in, prog_constraint::out) is det.

apply_subst_to_constraint_map_2(Subst, _Key, !Value) :-
    apply_subst_to_prog_constraint(Subst, !Value).

apply_rec_subst_to_constraint_map(Subst, !ConstraintMap) :-
    map.map_values(apply_rec_subst_to_constraint_map_2(Subst),
        !ConstraintMap).

:- pred apply_rec_subst_to_constraint_map_2(tsubst::in, constraint_id::in,
    prog_constraint::in, prog_constraint::out) is det.

apply_rec_subst_to_constraint_map_2(Subst, _Key, !Value) :-
    apply_rec_subst_to_prog_constraint(Subst, !Value).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "type_util.m".

%-----------------------------------------------------------------------------%
:- end_module type_util.
%-----------------------------------------------------------------------------%

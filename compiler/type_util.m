%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2014-2021 The Mercury team.
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
% XXX TYPE_REPN
% Put the contents of this type into meaningful groups.
% XXX TYPE_REPN
% Consider which of these predicates are used only during semantic checking,
% and which are used afterwards as well. Consider moving the latter
% to a new module in the hlds (as opposed to the check_hlds) package.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.type_util.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

    % Given a type_ctor, look up its module/name/arity.
    %
:- func type_ctor_module(type_ctor) = module_name.
:- func type_ctor_name(type_ctor) = string.
:- func type_ctor_arity(type_ctor) = arity.
:- pred type_ctor_module_name_arity(type_ctor::in,
    module_name::out, string::out, arity::out) is det.

    % Succeed iff type is an "atomic" type - one which can be unified
    % using a simple_test rather than a complicated_unify.
    %
:- pred type_is_atomic(module_info::in, mer_type::in) is semidet.

:- pred type_ctor_is_atomic(module_info::in, type_ctor::in) is semidet.

    % Obtain the type definition and type definition body respectively,
    % if known, for the principal type constructor of the given type.
    %
    % Fail if the given type is a type variable or if the type is a builtin
    % type.
    %
:- pred type_to_type_defn(module_info::in, mer_type::in, hlds_type_defn::out)
    is semidet.
:- pred type_to_type_defn_from_type_table(type_table::in, mer_type::in,
    hlds_type_defn::out) is semidet.

:- pred type_to_type_defn_body(module_info::in, mer_type::in,
    hlds_type_body::out) is semidet.
:- pred type_to_type_defn_body_from_type_table(type_table::in, mer_type::in,
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
    noncanonical::out) is semidet.

:- pred type_body_has_user_defined_equality_pred(module_info::in,
    hlds_type_body::in, noncanonical::out) is semidet.

    % Succeed iff the type (not just the principal type constructor) is known
    % to not have user-defined equality or comparison predicates.
    %
    % If the type is a type variable, or is abstract, etc., make the
    % conservative approximation and fail.
    %
:- pred type_definitely_has_no_user_defined_equality_pred(module_info::in,
    mer_type::in) is semidet.

:- pred var_is_or_may_contain_solver_type(module_info::in, vartypes::in,
    prog_var::in) is semidet.

    % Succeed iff the principal type constructor for the given type is
    % declared a solver type, or if the type is a pred or func type.
    % Pred and func types are considered solver types because higher-order
    % terms that contain non-local solver variables are not ground unless
    % all of the non-locals are ground.
    %
    % If the type is a type variable and thus has no principal type
    % constructor, fail.
    %
:- pred type_is_or_may_contain_solver_type(module_info::in, mer_type::in)
    is semidet.

:- pred type_has_solver_type_details(module_info::in, mer_type::in,
    solver_type_details::out) is semidet.

:- pred type_body_has_solver_type_details(module_info::in,
    hlds_type_body::in, solver_type_details::out) is semidet.

:- pred type_is_solver_type(module_info::in, mer_type::in) is semidet.
:- pred type_is_solver_type_from_type_table(type_table::in, mer_type::in)
    is semidet.

    % Succeed if the type body is for a solver type.
    %
:- pred type_body_is_solver_type(module_info::in, hlds_type_body::in)
    is semidet.
:- pred type_body_is_solver_type_from_type_table(type_table::in,
    hlds_type_body::in) is semidet.

    % Succeeds iff one or more of the type constructors for a given
    % type is existentially quantified.
    %
:- pred type_is_existq_type(module_info::in, mer_type::in) is semidet.

:- type is_dummy_type
    --->    is_dummy_type
    ;       is_not_dummy_type.

    % Certain types are just dummy types used to ensure logical semantics
    % or to act as a placeholder; they contain no information, and thus
    % there is no need to actually pass them around, so we don't. Also,
    % when importing or exporting procedures to/from C, we don't include
    % arguments with these types.
    %
    % A type is a dummy type in one of three cases:
    %
    % - its principal type constructor is a builtin dummy type constructor
    %   such as io.state or store.store(S);
    % - it has only a single function symbol with zero arguments;
    % - it has only a single function symbol with one argument, which is itself
    %   a dummy type.
    %
    % A type cannot be a dummy type if it is the subject of a foreign_enum
    % pragma, or if it has a reserved tag or user defined equality.
    %
    % A subtype is only a dummy type if its base type is a dummy type.
    %
    % NOTE: changes here may require changes to
    % `non_sub_du_constructor_list_represents_dummy_type'.
    %
:- func is_type_a_dummy(module_info, mer_type) = is_dummy_type.

:- type is_either_dummy_type
    --->    at_least_one_is_dummy_type
    ;       neither_is_dummy_type.

    % Return at_least_one_is_dummy_type if *either* of the two types
    % is a dummy type.
    %
    % Usually used to check the "dummyness" of both the type of an argument
    % in both the caller and the callee of a call.
    %
:- func is_either_type_a_dummy(module_info, mer_type, mer_type) =
    is_either_dummy_type.

    % A test for types that are defined in Mercury, but whose definitions
    % are `lies', i.e. they are not sufficiently accurate for RTTI
    % structures describing the types. Since the RTTI will be hand defined,
    % the compiler shouldn't generate RTTI for these types.
    %
:- pred type_ctor_has_hand_defined_rtti(type_ctor::in, hlds_type_body::in)
    is semidet.

    % Return the base type constructor for a given type constructor.
    % This predicate must only be called with a type constructor that is known
    % to be a subtype or supertype type constructor, not with arbitrary type
    % constructors.
    %
:- pred get_base_type_ctor(type_table::in, type_ctor::in, type_ctor::out)
    is semidet.

    % Return the supertype of a type, if any.
    % This will substitute the type's arguments into its declared supertype.
    %
:- pred get_supertype(type_table::in, tvarset::in, type_ctor::in,
    list(mer_type)::in, mer_type::out) is semidet.

    % Given a type, determine what category its principal constructor
    % falls into.
    %
:- func classify_type(module_info, mer_type) = type_ctor_category.

    % Given a type_ctor, determine what sort it is.
    %
:- func classify_type_ctor(module_info, type_ctor) = type_ctor_category.

    % Given a type_ctor, determine what sort it is, *if* it is a special
    % kind of type_ctor. Unlike classify_type_ctor itself, it does not need
    % type representations to have been computed yet.
    %
:- pred classify_type_ctor_if_special(type_ctor::in, type_ctor_category::out)
    is semidet.

    % Given a type_ctor's type_ctor_defn's body, determine what sort it is.
    %
:- func classify_type_defn_body(hlds_type_body) = type_ctor_category.

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
:- pred type_constructors(module_info::in, mer_type::in,
    list(constructor)::out) is semidet.

    % Given a type on which it is possible to have a complete switch, return
    % the number of alternatives. (It is possible to have a complete switch on
    % any du type, on the builtin type character and on the builtin fixed size
    % integer types. It is not feasible to have a complete switch on the
    % builtin types int, uint, float, and string. One cannot have a switch on
    % an abstract type, and equivalence types will have been expanded out by
    % the time we consider switches.)
    %
:- pred switch_type_num_functors(module_info::in, mer_type::in, int::out)
    is semidet.

    % Work out the types of the arguments of a functor, given the cons_id
    % and type of the functor. Aborts if the functor is existentially typed.
    % Note that this will substitute appropriate values for any type variables
    % in the functor's argument types, to match their bindings in the
    % functor's type.
    %
:- pred get_cons_id_arg_types(module_info::in, mer_type::in,
    cons_id::in, list(mer_type)::out) is det.

    % The same as get_cons_id_arg_types except that it fails rather than
    % aborting if the functor is existentially typed.
    %
:- pred get_cons_id_non_existential_arg_types(module_info::in,
    mer_type::in, cons_id::in, list(mer_type)::out) is semidet.

    % The same as get_cons_id_arg_types except that the cons_id is output
    % non-deterministically. The cons_id is not module-qualified.
    %
:- pred cons_id_arg_types(module_info::in, mer_type::in,
    cons_id::out, list(mer_type)::out) is nondet.

    % Is this type a du type?
    %
:- pred type_is_du_type(module_info::in, mer_type::in) is semidet.

    % Given a type constructor and one of its cons_ids, look up the definition
    % of that cons_id. Fails if the cons_id is not user-defined.
    %
    % Note that this will NOT bind type variables in the functor's argument
    % types; they will be left unbound, so the caller can find out the
    % original types from the constructor definition. The caller must do
    % that substitution itself if required.
    %
    % The versions with "repn" in the name return a definition of the
    % constructor that includes type representation information.
    % These versions may be called only after the pass in which
    % type representations are decided.
    %
:- pred get_cons_defn(module_info::in, type_ctor::in, cons_id::in,
    hlds_cons_defn::out) is semidet.
:- pred get_cons_defn_det(module_info::in, type_ctor::in, cons_id::in,
    hlds_cons_defn::out) is det.
:- pred get_cons_repn_defn(module_info::in, cons_id::in,
    constructor_repn::out) is semidet.
:- pred get_cons_repn_defn_det(module_info::in, cons_id::in,
    constructor_repn::out) is det.

    % This type is used to return information about a constructor definition,
    % extracted from the hlds_type_defn and hlds_cons_defn data types.
    %
:- type ctor_defn
    --->    ctor_defn(
                ctor_tvars          :: tvarset,

                % The kinds of the type variables.
                ctor_tvar_kinds     :: tvar_kind_map,

                % Existential constraints, if any.
                ctor_maybe_exist    :: maybe_cons_exist_constraints,

                % The type of the functor's arguments.
                ctor_arg_types      :: list(mer_type),

                % The functor's result type.
                ctor_result_type    :: mer_type
            ).

    % Given a type and a cons_id, look up the definition of that constructor;
    % if it is existentially typed, return its definition, otherwise fail.
    % Note that this will NOT bind type variables in the functor's argument
    % types; they will be left unbound, so the caller can find out the
    % original types from the constructor definition. The caller must do
    % that substitution itself if required.
    %
:- pred get_existq_cons_defn(module_info::in, mer_type::in, cons_id::in,
    ctor_defn::out) is semidet.

:- pred cons_id_is_existq_cons(module_info::in, mer_type::in, cons_id::in)
    is semidet.

    % Check whether a type is a no_tag type (i.e. one with only one
    % constructor, and whose one constructor has only one argument).
    %
:- pred type_is_no_tag_type(module_info::in, mer_type::in) is semidet.

    % As above, but return the constructor symbol and argument type on
    % success.
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

    % Check if (values/program terms of) the type is NOT allocated in a
    % region in region-based memory management.
    %
:- pred type_not_stored_in_region(mer_type::in, module_info::in) is semidet.

    % Succeed iff the given variable is of region_type.
    %
:- pred is_region_var(vartypes::in, prog_var::in) is semidet.

%-----------------------------------------------------------------------------%

    % Given a list of variables, return the permutation
    % of that list which has all the type_info-related variables
    % preceding the non-type_info-related variables (with the relative
    % order of variables within each group being the same as in the
    % original list).
    %
:- func put_typeinfo_vars_first(list(prog_var), vartypes) = list(prog_var).

    % Given a list of variables, remove all the type_info-related
    % variables.
    %
:- func remove_typeinfo_vars(vartypes, list(prog_var)) = list(prog_var).
:- func remove_typeinfo_vars_from_set(vartypes, set(prog_var))
    = set(prog_var).
:- func remove_typeinfo_vars_from_set_of_var(vartypes, set_of_progvar)
    = set_of_progvar.

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

:- pred apply_variable_renaming_to_constraint_proof_map(tvar_renaming::in,
    constraint_proof_map::in, constraint_proof_map::out) is det.

:- pred apply_subst_to_constraint_proof_map(tsubst::in,
    constraint_proof_map::in, constraint_proof_map::out) is det.

:- pred apply_rec_subst_to_constraint_proof_map(tsubst::in,
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
:- import_module backend_libs.string_encoding.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module require.
:- import_module term.

%-----------------------------------------------------------------------------%

type_ctor_module(type_ctor(TypeSymName, _Arity)) = ModuleName :-
    % If a type_ctor has an unqualified sym_name, return "builtin" as its
    % module name, since
    % 
    % - after typechecking, all type_ctors will be module qualified, with
    %   the only exceptions being the type_ctors of builtin types; and
    %
    % - before typechecking, no type_ctors will be module qualified beyond
    %   whatever qualification may have been written down explicitly
    %   by the programmer, which makes calling this function futile.
    sym_name_get_module_name_default(TypeSymName,
        unqualified("builtin"), ModuleName).

type_ctor_name(type_ctor(TypeSymName, _Arity)) =
    unqualify_name(TypeSymName).

type_ctor_arity(type_ctor(_TypeSymName, Arity)) = Arity.

type_ctor_module_name_arity(type_ctor(TypeSymName, Arity), ModuleName, Name,
        Arity) :-
    % See the comment in type_ctor_module.
    sym_name_get_module_name_default_name(TypeSymName,
        unqualified("builtin"), ModuleName, Name).

%-----------------------------------------------------------------------------%

type_is_atomic(ModuleInfo, Type) :-
    type_to_ctor(Type, TypeCtor),
    type_ctor_is_atomic(ModuleInfo, TypeCtor).

type_ctor_is_atomic(ModuleInfo, TypeCtor) :-
    TypeCategory = classify_type_ctor(ModuleInfo, TypeCtor),
    type_ctor_category_is_atomic(TypeCategory) = yes.

:- func type_ctor_category_is_atomic(type_ctor_category) = bool.

type_ctor_category_is_atomic(CtorCat) = IsAtomic :-
    (
        ( CtorCat = ctor_cat_builtin(_)
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_user(cat_user_abstract_dummy)
        ; CtorCat = ctor_cat_user(cat_user_direct_dummy)
        ),
        IsAtomic = yes
    ;
        ( CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_system(_)
        ; CtorCat = ctor_cat_user(cat_user_notag)
        ; CtorCat = ctor_cat_user(cat_user_abstract_notag)
        ; CtorCat = ctor_cat_user(cat_user_general)
        ),
        IsAtomic = no
    ).

type_to_type_defn(ModuleInfo, Type, TypeDefn) :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    type_to_ctor(Type, TypeCtor),
    search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn).

type_to_type_defn_from_type_table(TypeTable, Type, TypeDefn) :-
    type_to_ctor(Type, TypeCtor),
    search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn).

type_to_type_defn_body(ModuleInfo, Type, TypeBody) :-
    type_to_type_defn(ModuleInfo, Type, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody).

type_to_type_defn_body_from_type_table(TypeTable, Type, TypeBody) :-
    type_to_type_defn_from_type_table(TypeTable, Type, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody).

type_has_user_defined_equality_pred(ModuleInfo, Type, UserEqComp) :-
    type_to_type_defn_body(ModuleInfo, Type, TypeBody),
    type_body_has_user_defined_equality_pred(ModuleInfo, TypeBody, UserEqComp).

type_body_has_user_defined_equality_pred(ModuleInfo, TypeBody, NonCanonical) :-
    require_complete_switch [TypeBody]
    (
        TypeBody = hlds_du_type(TypeBodyDu),
        TypeBodyDu = type_body_du(_, _, _, _, MaybeForeignType),
        ( if
            MaybeForeignType = yes(ForeignTypeBody),
            module_info_get_globals(ModuleInfo, Globals),
            globals.get_target(Globals, Target),
            have_foreign_type_for_backend(Target, ForeignTypeBody, yes)
        then
            foreign_type_body_has_user_defined_eq_comp_pred(ModuleInfo,
                ForeignTypeBody, NonCanonical)
        else
            TypeBodyDu ^ du_type_canonical = noncanon(NonCanonical)
        )
    ;
        TypeBody = hlds_foreign_type(ForeignTypeBody),
        foreign_type_body_has_user_defined_eq_comp_pred(ModuleInfo,
            ForeignTypeBody, NonCanonical)
    ;
        TypeBody = hlds_solver_type(DetailsSolver),
        DetailsSolver =
            type_details_solver(_SolverTypeDetails, noncanon(NonCanonical))
    ;
        ( TypeBody = hlds_abstract_type(_)
        ; TypeBody = hlds_eqv_type(_)
        ),
        fail
    ).

type_definitely_has_no_user_defined_equality_pred(ModuleInfo, Type) :-
    type_definitely_has_no_user_defined_eq_pred_2(ModuleInfo, Type,
        set.init, _).

:- pred type_definitely_has_no_user_defined_eq_pred_2(module_info::in,
    mer_type::in, set(mer_type)::in, set(mer_type)::out) is semidet.

type_definitely_has_no_user_defined_eq_pred_2(ModuleInfo, Type, !SeenTypes) :-
    ( if set.contains(!.SeenTypes, Type) then
        % Don't loop on recursive types.
        true
    else
        set.insert(Type, !SeenTypes),
        require_complete_switch [Type]
        (
            Type = builtin_type(_)
        ;
            Type = tuple_type(Args, _Kind),
            types_definitely_have_no_user_defined_eq_pred(ModuleInfo,
                Args, !SeenTypes)
        ;
            ( Type = defined_type(_, _, _)
            ; Type = higher_order_type(_, _, _, _, _)
            ; Type = apply_n_type(_, _, _)
            ; Type = kinded_type(_, _)
            ),
            type_to_type_defn_body(ModuleInfo, Type, TypeBody),
            type_body_definitely_has_no_user_defined_equality_pred(ModuleInfo,
                Type, TypeBody, !SeenTypes),
            type_to_ctor_and_args_det(Type, _, Args),
            types_definitely_have_no_user_defined_eq_pred(ModuleInfo,
                Args, !SeenTypes)
        ;
            Type = type_variable(_, _),
            fail
        )
    ).

:- pred types_definitely_have_no_user_defined_eq_pred(module_info::in,
    list(mer_type)::in, set(mer_type)::in, set(mer_type)::out) is semidet.

types_definitely_have_no_user_defined_eq_pred(ModuleInfo, Types, !SeenTypes) :-
    list.foldl(type_definitely_has_no_user_defined_eq_pred_2(ModuleInfo),
        Types, !SeenTypes).

:- pred type_body_definitely_has_no_user_defined_equality_pred(module_info::in,
    mer_type::in, hlds_type_body::in, set(mer_type)::in, set(mer_type)::out)
    is semidet.

type_body_definitely_has_no_user_defined_equality_pred(ModuleInfo, Type,
        TypeBody, !SeenTypes) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    (
        TypeBody = hlds_du_type(TypeBodyDu),
        ( if
            TypeBodyDu ^ du_type_is_foreign_type = yes(ForeignTypeBody),
            have_foreign_type_for_backend(Target, ForeignTypeBody, yes)
        then
            not foreign_type_body_has_user_defined_eq_comp_pred(ModuleInfo,
                ForeignTypeBody, _)
        else
            TypeBodyDu ^ du_type_canonical = canon,
            % type_constructors does substitution of types variables.
            type_constructors(ModuleInfo, Type, Ctors),
            list.foldl(ctor_definitely_has_no_user_defined_eq_pred(ModuleInfo),
                Ctors, !SeenTypes)
        )
    ;
        TypeBody = hlds_eqv_type(EqvType),
        type_definitely_has_no_user_defined_equality_pred(ModuleInfo, EqvType)
    ;
        TypeBody = hlds_foreign_type(ForeignTypeBody),
        not foreign_type_body_has_user_defined_eq_comp_pred(ModuleInfo,
            ForeignTypeBody, _)
    ;
        TypeBody = hlds_solver_type(DetailsSolver),
        DetailsSolver = type_details_solver(_, canon)
    ;
        TypeBody = hlds_abstract_type(_),
        fail
    ).

:- pred ctor_definitely_has_no_user_defined_eq_pred(module_info::in,
    constructor::in, set(mer_type)::in, set(mer_type)::out) is semidet.

ctor_definitely_has_no_user_defined_eq_pred(ModuleInfo, Ctor, !SeenTypes) :-
    % There must not be any existentially quantified type variables.
    Ctor = ctor(_, no_exist_constraints, _, Args, _, _),
    % The data constructor argument types must not have user-defined equality
    % or comparison predicates.
    ArgTypes = list.map((func(A) = A ^ arg_type), Args),
    list.foldl(type_definitely_has_no_user_defined_eq_pred_2(ModuleInfo),
        ArgTypes, !SeenTypes).

var_is_or_may_contain_solver_type(ModuleInfo, VarTypes, Var) :-
    lookup_var_type(VarTypes, Var, VarType),
    type_is_or_may_contain_solver_type(ModuleInfo, VarType).

type_is_or_may_contain_solver_type(ModuleInfo, Type) :-
    (
        type_is_higher_order(Type)
    ;
        type_to_type_defn_body(ModuleInfo, Type, TypeBody),
        (
            TypeBody = hlds_solver_type(_)
        ;
            TypeBody = hlds_abstract_type(abstract_solver_type)
        ;
            TypeBody = hlds_eqv_type(EqvType),
            type_is_or_may_contain_solver_type(ModuleInfo, EqvType)
        )
    ).

type_has_solver_type_details(ModuleInfo, Type, SolverTypeDetails) :-
    type_to_type_defn_body(ModuleInfo, Type, TypeBody),
    type_body_has_solver_type_details(ModuleInfo, TypeBody,
        SolverTypeDetails).

type_body_has_solver_type_details(ModuleInfo, Type, SolverTypeDetails) :-
    require_complete_switch [Type]
    (
        Type = hlds_solver_type(DetailsSolver),
        DetailsSolver =
            type_details_solver(SolverTypeDetails, _MaybeUserEqComp)
    ;
        Type = hlds_eqv_type(EqvType),
        type_has_solver_type_details(ModuleInfo, EqvType, SolverTypeDetails)
    ;
        ( Type = hlds_du_type(_)
        ; Type = hlds_foreign_type(_)
        ; Type = hlds_abstract_type(_)
        ),
        fail
    ).

type_is_solver_type(ModuleInfo, Type) :-
    % XXX We can't assume that type variables refer to solver types
    % because otherwise the compiler will try to construct initialisation
    % forwarding predicates for exported abstract types defined to be
    % equivalent to a type variable parameter. This, of course, will
    % lead to the compiler throwing an exception. The correct solution
    % is to introduce a solver typeclass, but that's something for another day.
    %
    % Type_to_type_defn_body will fail for builtin types such as `int/0'.
    % Such types are not solver types so is_solver_type fails too.
    % Type_to_type_defn_body also fails for type variables.
    type_to_type_defn_body(ModuleInfo, Type, TypeBody),
    type_body_is_solver_type(ModuleInfo, TypeBody).

type_is_solver_type_from_type_table(TypeTable, Type) :-
    % XXX The comment in type_is_solver_type applies here as well.
    type_to_type_defn_body_from_type_table(TypeTable, Type, TypeBody),
    type_body_is_solver_type_from_type_table(TypeTable, TypeBody).

type_body_is_solver_type(ModuleInfo, TypeBody) :-
    % Please keep in sync with get_body_is_solver_type in add_type.m.
    require_complete_switch [TypeBody]
    (
        TypeBody = hlds_solver_type(_),
        IsSolverType = solver_type
    ;
        TypeBody = hlds_abstract_type(AbstractType),
        require_complete_switch [AbstractType]
        (
            AbstractType = abstract_solver_type,
            IsSolverType = solver_type
        ;
            ( AbstractType = abstract_type_general
            ; AbstractType = abstract_dummy_type
            ; AbstractType = abstract_notag_type
            ; AbstractType = abstract_type_fits_in_n_bits(_)
            ; AbstractType = abstract_subtype(_)
            ),
            IsSolverType = non_solver_type
        )
    ;
        TypeBody = hlds_eqv_type(Type),
        % type_body_is_solver_type and get_body_is_solver_type differ
        % in their treatment of equivalence types.
        ( if type_is_solver_type(ModuleInfo, Type) then
            IsSolverType = solver_type
        else
            IsSolverType = non_solver_type
        )
    ;
        ( TypeBody = hlds_du_type(_)
        ; TypeBody = hlds_foreign_type(_)
        ),
        IsSolverType = non_solver_type
    ),
    IsSolverType = solver_type.

type_body_is_solver_type_from_type_table(TypeTable, TypeBody) :-
    require_complete_switch [TypeBody]
    (
        TypeBody = hlds_solver_type(_),
        IsSolverType = yes
    ;
        TypeBody = hlds_abstract_type(AbstractType),
        require_complete_switch [AbstractType]
        (
            AbstractType = abstract_solver_type,
            IsSolverType = yes
        ;
            ( AbstractType = abstract_type_general
            ; AbstractType = abstract_dummy_type
            ; AbstractType = abstract_notag_type
            ; AbstractType = abstract_type_fits_in_n_bits(_)
            ; AbstractType = abstract_subtype(_)
            ),
            IsSolverType = no
        )
    ;
        TypeBody = hlds_eqv_type(Type),
        ( if type_is_solver_type_from_type_table(TypeTable, Type) then
            IsSolverType = yes
        else
            IsSolverType = no
        )
    ;
        ( TypeBody = hlds_du_type(_)
        ; TypeBody = hlds_foreign_type(_)
        ),
        IsSolverType = no
    ),
    IsSolverType = yes.

type_is_existq_type(ModuleInfo, Type) :-
    type_constructors(ModuleInfo, Type, Constructors),
    some [Constructor] (
        list.member(Constructor, Constructors),
        Constructor ^ cons_maybe_exist = exist_constraints(_)
    ).

is_type_a_dummy(ModuleInfo, Type) = IsDummy :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    IsDummy = is_type_a_dummy_loop(TypeTable, Type, []).

is_either_type_a_dummy(ModuleInfo, TypeA, TypeB) = IsDummy :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    IsDummyA = is_type_a_dummy_loop(TypeTable, TypeA, []),
    (
        IsDummyA = is_dummy_type,
        IsDummy = at_least_one_is_dummy_type
    ;
        IsDummyA = is_not_dummy_type,
        IsDummyB = is_type_a_dummy_loop(TypeTable, TypeB, []),
        (
            IsDummyB = is_dummy_type,
            IsDummy = at_least_one_is_dummy_type
        ;
            IsDummyB = is_not_dummy_type,
            IsDummy = neither_is_dummy_type
        )
    ).

:- func is_type_a_dummy_loop(type_table, mer_type, list(mer_type))
    = is_dummy_type.

is_type_a_dummy_loop(TypeTable, Type, CoveredTypes) = IsDummy :-
    % Since the sizes of types in any given program is bounded, this test
    % will ensure termination.
    ( if list.member(Type, CoveredTypes) then
        % The type is circular.
        IsDummy = is_not_dummy_type
    else if type_to_ctor_and_args(Type, TypeCtor, ArgTypes) then
        % Keep this in sync with is_dummy_argument_type_with_constructors
        % above.
        % XXX gone since 097b45acec46527f1485419e66ce28d5ba224846
        IsBuiltinDummy = is_type_ctor_a_builtin_dummy(TypeCtor),
        (
            IsBuiltinDummy = is_builtin_dummy_type_ctor,
            IsDummy = is_dummy_type
        ;
            IsBuiltinDummy = is_not_builtin_dummy_type_ctor,
            % This can fail for some builtin type constructors such as func,
            % pred, and tuple, none of which are dummy types.
            ( if search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn) then
                get_type_defn_body(TypeDefn, TypeBody),
                (
                    TypeBody = hlds_du_type(TypeBodyDu),
                    TypeBodyDu = type_body_du(_, _, _, MaybeTypeRepn, _),
                    (
                        MaybeTypeRepn = no,
                        unexpected($pred, "MaybeTypeRepn = no")
                    ;
                        MaybeTypeRepn = yes(TypeRepn)
                    ),
                    DuTypeKind = TypeRepn ^ dur_kind,
                    (
                        DuTypeKind = du_type_kind_direct_dummy,
                        IsDummy = is_dummy_type
                    ;
                        ( DuTypeKind = du_type_kind_mercury_enum
                        ; DuTypeKind = du_type_kind_foreign_enum(_)
                        ; DuTypeKind = du_type_kind_general
                        ),
                        IsDummy = is_not_dummy_type
                    ;
                        DuTypeKind = du_type_kind_notag(_, SingleArgTypeInDefn,
                            _),
                        get_type_defn_tparams(TypeDefn, TypeParams),
                        map.from_corresponding_lists(TypeParams, ArgTypes,
                            Subst),
                        apply_subst_to_type(Subst, SingleArgTypeInDefn,
                            SingleArgType),
                        IsDummy = is_type_a_dummy_loop(TypeTable,
                            SingleArgType, [Type | CoveredTypes])
                    )
                ;
                    TypeBody = hlds_abstract_type(AbstractDetails),
                    (
                        ( AbstractDetails = abstract_type_general
                        ; AbstractDetails = abstract_type_fits_in_n_bits(_)
                        ; AbstractDetails = abstract_notag_type
                        ; AbstractDetails = abstract_solver_type
                        ),
                        IsDummy = is_not_dummy_type
                    ;
                        AbstractDetails = abstract_dummy_type,
                        IsDummy = is_dummy_type
                    ;
                        AbstractDetails = abstract_subtype(SuperTypeCtor),
                        % It does not matter what the supertype type parameters
                        % are bound to, only that they are not dummy types.
                        SuperTypeCtor = type_ctor(_, Arity),
                        list.duplicate(Arity, int_type, FakeArgTypes),
                        construct_type(SuperTypeCtor, FakeArgTypes, SuperType),
                        IsDummy = is_type_a_dummy_loop(TypeTable, SuperType,
                            [Type | CoveredTypes])
                    )
                ;
                    ( TypeBody = hlds_eqv_type(_)
                    ; TypeBody = hlds_foreign_type(_)
                    ; TypeBody = hlds_solver_type(_)
                    ),
                    IsDummy = is_not_dummy_type
                )
            else
                IsDummy = is_not_dummy_type
            )
        )
    else
        IsDummy = is_not_dummy_type
    ).

type_ctor_has_hand_defined_rtti(Type, Body) :-
    Type = type_ctor(qualified(mercury_private_builtin_module, Name), 0),
    ( Name = "type_info"
    ; Name = "type_ctor_info"
    ; Name = "typeclass_info"
    ; Name = "base_typeclass_info"
    ),
    require_complete_switch [Body]
    (
        Body = hlds_du_type(TypeBodyDu),
        TypeBodyDu = type_body_du(_, _, _, _, IsForeignType),
        (
            IsForeignType = yes(_),
            HasHandDefinedRtti = no
        ;
            IsForeignType = no,
            HasHandDefinedRtti = yes
        )
    ;
        ( Body = hlds_foreign_type(_)
        ; Body = hlds_solver_type(_)
        ),
        HasHandDefinedRtti = no
    ;
        ( Body = hlds_abstract_type(_)
        ; Body = hlds_eqv_type(_)
        ),
        HasHandDefinedRtti = yes
    ),
    HasHandDefinedRtti = yes.

%-----------------------------------------------------------------------------%

get_base_type_ctor(TypeTable, TypeCtor, BaseTypeCtor) :-
    % Circular subtype definitions are assumed to have been detected by now.
    hlds_data.search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    require_complete_switch [TypeBody]
    (
        TypeBody = hlds_du_type(TypeBodyDu),
        TypeBodyDu = type_body_du(_, MaybeSuperType, _, _, _),
        (
            MaybeSuperType = not_a_subtype,
            BaseTypeCtor = TypeCtor
        ;
            MaybeSuperType = subtype_of(SuperType),
            type_to_ctor(SuperType, SuperTypeCtor),
            get_base_type_ctor(TypeTable, SuperTypeCtor, BaseTypeCtor)
        )
    ;
        TypeBody = hlds_abstract_type(AbstractDetails),
        require_complete_switch [AbstractDetails]
        (
            ( AbstractDetails = abstract_type_general
            ; AbstractDetails = abstract_type_fits_in_n_bits(_)
            ; AbstractDetails = abstract_dummy_type
            ; AbstractDetails = abstract_notag_type
            ),
            BaseTypeCtor = TypeCtor
        ;
            AbstractDetails = abstract_subtype(SuperTypeCtor),
            get_base_type_ctor(TypeTable, SuperTypeCtor, BaseTypeCtor)
        ;
            AbstractDetails = abstract_solver_type,
            unexpected($pred, "abstract solver type")
        )
    ;
        TypeBody = hlds_eqv_type(EqvType),
        type_to_ctor(EqvType, EqvTypeCtor),
        get_base_type_ctor(TypeTable, EqvTypeCtor, BaseTypeCtor)
    ;
        TypeBody = hlds_foreign_type(_),
        unexpected($pred, "foreign type")
    ;
        TypeBody = hlds_solver_type(_),
        unexpected($pred, "solver type")
    ).

%-----------------------------------------------------------------------------%

get_supertype(TypeTable, TVarSet, TypeCtor, Args, SuperType) :-
    hlds_data.search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    TypeBody = hlds_du_type(TypeBodyDu),
    TypeBodyDu = type_body_du(_, subtype_of(SuperType0), _, _, _),
    require_det (
        % Create substitution from type parameters to Args.
        hlds_data.get_type_defn_tvarset(TypeDefn, TVarSet0),
        hlds_data.get_type_defn_tparams(TypeDefn, TypeParams0),
        tvarset_merge_renaming(TVarSet, TVarSet0, _NewTVarSet, Renaming),
        apply_variable_renaming_to_tvar_list(Renaming,
            TypeParams0, TypeParams),
        map.from_corresponding_lists(TypeParams, Args, TSubst),

        % Apply substitution to the declared supertype.
        apply_variable_renaming_to_type(Renaming, SuperType0, SuperType1),
        apply_rec_subst_to_type(TSubst, SuperType1, SuperType)
    ).

%-----------------------------------------------------------------------------%

classify_type(ModuleInfo, VarType) = TypeCategory :-
    ( if type_to_ctor(VarType, TypeCtor) then
        TypeCategory = classify_type_ctor(ModuleInfo, TypeCtor)
    else
        TypeCategory = ctor_cat_variable
    ).

classify_type_ctor(ModuleInfo, TypeCtor) = TypeCategory :-
    ( if classify_type_ctor_if_special(TypeCtor, TypeCategoryPrime) then
        TypeCategory = TypeCategoryPrime
    else
        module_info_get_type_table(ModuleInfo, TypeTable),
        lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        TypeCategory = classify_type_defn_body(TypeBody)
    ).

classify_type_ctor_if_special(TypeCtor, TypeCategory) :-
    % Please keep the code of this predicate in sync with the code of
    % classify_type_defn_body.
    %
    % Please also keep the relevant parts of this code in sync with
    %
    % - builtin_type_to_string
    % - int_type_to_string
    % - type_ctor_is_higher_order
    % - type_ctor_is_tuple
    % - check_builtin_dummy_type_ctor
    %
    TypeCtor = type_ctor(TypeSymName, Arity),
    ( TypeSymName = unqualified(TypeName)
    ; TypeSymName = qualified(_ModuleSymName, TypeName)
    ),
    (
        (
            TypeName = "int",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_int))
        ;
            TypeName = "uint",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_uint))
        ;
            TypeName = "int8",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_int8))
        ;
            TypeName = "uint8",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_uint8))
        ;
            TypeName = "int16",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_int16))
        ;
            TypeName = "uint16",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_uint16))
        ;
            TypeName = "int32",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_int32))
        ;
            TypeName = "uint32",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_uint32))
        ;
            TypeName = "int64",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_int64))
        ;
            TypeName = "uint64",
            TypeCategory = ctor_cat_builtin(cat_builtin_int(int_type_uint64))
        ;
            TypeName = "character",
            TypeCategory = ctor_cat_builtin(cat_builtin_char)
        ;
            TypeName = "float",
            TypeCategory = ctor_cat_builtin(cat_builtin_float)
        ;
            TypeName = "string",
            TypeCategory = ctor_cat_builtin(cat_builtin_string)
        ;
            TypeName = "void",
            TypeCategory = ctor_cat_void
        ),
        (
            TypeSymName = unqualified(_TypeName)
        ;
            TypeSymName = qualified(ModuleSymName, _TypeName),
            ModuleSymName = mercury_public_builtin_module
        ),
        Arity = 0
    ;
        (
            TypeName = "type_info",
            TypeCategory = ctor_cat_system(cat_system_type_info)
        ;
            TypeName = "type_ctor_info",
            TypeCategory = ctor_cat_system(cat_system_type_ctor_info)
        ;
            TypeName = "typeclass_info",
            TypeCategory = ctor_cat_system(cat_system_typeclass_info)
        ;
            TypeName = "base_typeclass_info",
            TypeCategory = ctor_cat_system(cat_system_base_typeclass_info)
        ),
        TypeSymName = qualified(ModuleSymName, _TypeName),
        ModuleSymName = mercury_private_builtin_module,
        Arity = 0
    ;
        (
            TypeName = "state",
            TypeSymName = qualified(ModuleSymName, _TypeName),
            ModuleSymName = mercury_io_module,
            Arity = 0
        ;
            TypeName = "store",
            TypeSymName = qualified(ModuleSymName, _TypeName),
            ModuleSymName = mercury_std_lib_module_name(unqualified("store")),
            Arity = 1
        ),
        TypeCategory = ctor_cat_builtin_dummy
    ;
        (
            TypeName = "pred"
        ;
            TypeName = "func"
        ),
        % The previous version of classify_type_ctor was implemented
        % as a series of nested if-then-elses, with two conditions
        % that could recognize higher order type constructors.
        (
            % This was the first condition.
            TypeSymName = qualified(ModuleSymName, _TypeName),
            ModuleSymName = mercury_public_builtin_module,
            Arity = 0
        ;
            % This was the second condition.
            (
                TypeSymName = unqualified(_TypeName)
            ;
                TypeSymName = qualified(ModuleSymName, _TypeName),
                ModuleSymName = unqualified(Qualifier),
                ( Qualifier = "impure"
                ; Qualifier = "semipure"
                )
            )
            % The arity may be anything.
        ),
        % XXX zs: Having two conditions that look so different seems wrong.
        TypeCategory = ctor_cat_higher_order
    ;
        % XXX The compiler does not recognize any type named tuple/0 in
        % user code, but it nevertheless needs to know about this type,
        % because the compiler itself generates references to it. The
        % type_infos for tuples types (whose type name is "{}", not "tuple")
        % reference the hand-written type_ctor_info for the type named "tuple".
        % Since the name of the type is part of the name of the target language
        % variable holding the type_ctor_info, it helps if it does not contain
        % nonalphanumeric characters.
        TypeName = "tuple",
        TypeSymName = qualified(ModuleSymName, _TypeName),
        ModuleSymName = mercury_public_builtin_module,
        Arity = 0,
        TypeCategory = ctor_cat_tuple
    ;
        TypeName = "{}",
        TypeSymName = unqualified(_TypeName),
        % The arity may be anything.
        TypeCategory = ctor_cat_tuple
    ).

classify_type_defn_body(TypeBody) = TypeCategory :-
    % Unlike classify_type_ctor, we don't have to (a) test for types that do
    % not have definitions, or (b) look up the definition, since our caller has
    % already done that.

    % XXX Why don't we have a category for solver types?
    % XXX Why do we classify abstract_enum_types as general?
    (
        TypeBody = hlds_du_type(TypeBodyDu),
        TypeBodyDu = type_body_du(_, _, _, MaybeTypeRepn, _),
        (
            MaybeTypeRepn = no,
            unexpected($pred, "MaybeTypeRepn = no")
        ;
            MaybeTypeRepn = yes(Repn)
        ),
        DuTypeKind = Repn ^ dur_kind,
        (
            DuTypeKind = du_type_kind_mercury_enum,
            TypeCategory = ctor_cat_enum(cat_enum_mercury)
        ;
            DuTypeKind = du_type_kind_foreign_enum(_),
            TypeCategory = ctor_cat_enum(cat_enum_foreign)
        ;
            DuTypeKind = du_type_kind_direct_dummy,
            TypeCategory = ctor_cat_user(cat_user_direct_dummy)
        ;
            DuTypeKind = du_type_kind_notag(_, _, _),
            TypeCategory = ctor_cat_user(cat_user_notag)
        ;
            DuTypeKind = du_type_kind_general,
            TypeCategory = ctor_cat_user(cat_user_general)
        )
    ;
        TypeBody = hlds_abstract_type(AbstractDetails),
        (
            ( AbstractDetails = abstract_type_general
            ; AbstractDetails = abstract_type_fits_in_n_bits(_)
            ; AbstractDetails = abstract_subtype(_)
            ; AbstractDetails = abstract_solver_type
            ),
            TypeCategory = ctor_cat_user(cat_user_general)
        ;
            AbstractDetails = abstract_dummy_type,
            TypeCategory = ctor_cat_user(cat_user_abstract_dummy)
        ;
            AbstractDetails = abstract_notag_type,
            TypeCategory = ctor_cat_user(cat_user_abstract_notag)
        )
    ;
        % XXX We should be able to return more precise descriptions
        % than this.
        ( TypeBody = hlds_eqv_type(_)
        ; TypeBody = hlds_foreign_type(_)
        ; TypeBody = hlds_solver_type(_)
        ),
        TypeCategory = ctor_cat_user(cat_user_general)
    ).

%-----------------------------------------------------------------------------%

type_may_use_atomic_alloc(ModuleInfo, Type) = TypeMayUseAtomic :-
    TypeCategory = classify_type(ModuleInfo, Type),
    (
        TypeCategory = ctor_cat_builtin(cat_builtin_int(IntType)),
        (
            ( IntType = int_type_int
            ; IntType = int_type_uint
            ; IntType = int_type_int8
            ; IntType = int_type_uint8
            ; IntType = int_type_int16
            ; IntType = int_type_uint16
            ; IntType = int_type_int32
            ; IntType = int_type_uint32
            ),
            TypeMayUseAtomic = may_use_atomic_alloc
        ;
            ( IntType = int_type_int64
            ; IntType = int_type_uint64
            ),
            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, unboxed_int64s, UBI64),
            (
                UBI64 = yes,
                TypeMayUseAtomic = may_use_atomic_alloc
            ;
                UBI64 = no,
                TypeMayUseAtomic = may_not_use_atomic_alloc
            )
        )
    ;
        ( TypeCategory = ctor_cat_builtin(cat_builtin_char)
        ; TypeCategory = ctor_cat_enum(_)
        ; TypeCategory = ctor_cat_builtin_dummy
        ; TypeCategory = ctor_cat_system(cat_system_type_ctor_info)
        ),
        TypeMayUseAtomic = may_use_atomic_alloc
    ;
        TypeCategory = ctor_cat_builtin(cat_builtin_float),
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
        ( TypeCategory = ctor_cat_builtin(cat_builtin_string)
        ; TypeCategory = ctor_cat_higher_order
        ; TypeCategory = ctor_cat_tuple
        ; TypeCategory = ctor_cat_variable
        ; TypeCategory = ctor_cat_system(cat_system_type_info)
        ; TypeCategory = ctor_cat_system(cat_system_typeclass_info)
        ; TypeCategory = ctor_cat_system(cat_system_base_typeclass_info)
        ; TypeCategory = ctor_cat_void
        ; TypeCategory = ctor_cat_user(_) % for direct_dummy, alloc is moot
        ),
        TypeMayUseAtomic = may_not_use_atomic_alloc
    ).

update_type_may_use_atomic_alloc(ModuleInfo, Type, !MayUseAtomic) :-
    (
        !.MayUseAtomic = may_not_use_atomic_alloc
        % There is no point in testing Type.
    ;
        !.MayUseAtomic = may_use_atomic_alloc,
        !:MayUseAtomic = type_may_use_atomic_alloc(ModuleInfo, Type)
    ).

%-----------------------------------------------------------------------------%

type_constructors(ModuleInfo, Type, Constructors) :-
    type_to_ctor_and_args(Type, TypeCtor, TypeArgs),
    ( if type_ctor_is_tuple(TypeCtor) then
        % Tuples are never existentially typed.
        MaybeExistConstraints = no_exist_constraints,
        Context = term.context_init,
        CtorArgs = list.map(
            (func(ArgType) = ctor_arg(no, ArgType, Context)),
            TypeArgs),
        Constructors = [ctor(0u32, MaybeExistConstraints, unqualified("{}"),
            CtorArgs, list.length(CtorArgs), Context)]
    else
        module_info_get_type_table(ModuleInfo, TypeTable),
        search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        TypeBody = hlds_du_type(TypeBodyDu),
        substitute_type_args(TypeParams, TypeArgs,
            one_or_more_to_list(TypeBodyDu ^ du_type_ctors),
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
        substitute_type_args_ctors(Subst, Constructors0, Constructors)
    ).

:- pred substitute_type_args_ctors(tsubst::in, list(constructor)::in,
    list(constructor)::out) is det.

substitute_type_args_ctors(_, [], []).
substitute_type_args_ctors(Subst, [Ctor0 | Ctors0], [Ctor | Ctors]) :-
    % Note: the parser ensures that the existentially quantified variables,
    % if any, are distinct from the parameters, and that the (existential)
    % constraints can only contain existentially quantified variables,
    % so there's no need to worry about applying the substitution to ExistQVars
    % or Constraints.
    Ctor0 = ctor(Ordinal, MaybeExistConstraints, Name, Args0, Arity, Ctxt),
    substitute_type_args_ctor_args(Subst, Args0, Args),
    Ctor = ctor(Ordinal, MaybeExistConstraints, Name, Args, Arity, Ctxt),
    substitute_type_args_ctors(Subst, Ctors0, Ctors).

:- pred substitute_type_args_ctor_args(tsubst::in, list(constructor_arg)::in,
    list(constructor_arg)::out) is det.

substitute_type_args_ctor_args(_, [], []).
substitute_type_args_ctor_args(Subst, [Arg0 | Args0], [Arg | Args]) :-
    apply_subst_to_type(Subst, Arg0 ^ arg_type, ArgType),
    Arg = Arg0 ^ arg_type := ArgType,
    substitute_type_args_ctor_args(Subst, Args0, Args).

%-----------------------------------------------------------------------------%

switch_type_num_functors(ModuleInfo, Type, NumFunctors) :-
    type_to_ctor(Type, TypeCtor),
    ( if
        TypeCtor = type_ctor(unqualified("character"), 0)
    then
        module_info_get_globals(ModuleInfo, Globals),
        globals.get_target(Globals, Target),
        target_char_range(Target, MinChar, MaxChar),
        NumFunctors = MaxChar - MinChar + 1
    else if
        % It's not worth bothering with the 32- and 64-bit integer types here
        % -- a complete switch on any of those types would be so large that it
        % would overwhelm the compiler anyway.
        TypeCtor = type_ctor(unqualified(IntType), 0),
        ( IntType = "int8", NumFunctors0 = 256
        ; IntType = "uint8", NumFunctors0 = 256
        ; IntType = "int16", NumFunctors0 = 65536
        ; IntType = "uint16", NumFunctors0 = 65536
        )
    then
        NumFunctors = NumFunctors0
    else if
        type_ctor_is_tuple(TypeCtor)
    then
        NumFunctors = 1
    else
        module_info_get_type_table(ModuleInfo, TypeTable),
        search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        TypeBody = hlds_du_type(type_body_du(OoMConstructors, _, _, _, _)),
        OoMConstructors = one_or_more(_HeadCtor, TailCtors),
        NumFunctors = 1 + list.length(TailCtors)
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
    ( if type_to_ctor_and_args(VarType, TypeCtor, TypeArgs) then
        ( if
            % The argument types of a tuple cons_id are the arguments
            % of the tuple type.
            type_ctor_is_tuple(TypeCtor)
        then
            ArgTypes = TypeArgs
        else if
            get_cons_defn(ModuleInfo, TypeCtor, ConsId, ConsDefn),
            ConsDefn = hlds_cons_defn(_, _, TypeParams, _,
                MaybeExistConstraints0, Args, _),
            Args = [_ | _]
        then
            (
                MaybeExistConstraints0 = no_exist_constraints
            ;
                MaybeExistConstraints0 = exist_constraints(_ExistConstraints),
                % XXX handle _ExistConstraints
                (
                    EQVarAction = abort_on_exist_qvar,
                    unexpected($pred, "existentially typed cons_id")
                ;
                    EQVarAction = fail_on_exist_qvar,
                    fail
                )
            ),

            map.from_corresponding_lists(TypeParams, TypeArgs, TSubst),
            ArgTypes0 = list.map(func(C) = C ^ arg_type, Args),
            apply_subst_to_type_list(TSubst, ArgTypes0, ArgTypes)
        else
            ArgTypes = []
        )
    else
        ArgTypes = []
    ).

cons_id_arg_types(ModuleInfo, VarType, ConsId, ArgTypes) :-
    type_to_ctor_and_args(VarType, TypeCtor, TypeArgs),
    module_info_get_type_table(ModuleInfo, TypeTable),
    search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeDefnBody),
    TypeDefnBody = hlds_du_type(type_body_du(OoMCtors, _, _, _, _)),
    OoMCtors = one_or_more(HeadCtor, TailCtors),
    ( Ctor = HeadCtor
    ; list.member(Ctor, TailCtors)
    ),
    Ctor = ctor(_Ordinal, _MaybeExistConstraints, Name, _Args, Arity, _Ctxt),
    ConsId = cons(Name, Arity, TypeCtor),

    % We should look it up in a type_ctor-specific table, not a global one.
    module_info_get_cons_table(ModuleInfo, CtorTable),
    search_cons_table_of_type_ctor(CtorTable, TypeCtor, ConsId, ConsDefn),
    ConsDefn =
        hlds_cons_defn(_, _, TypeParams, _, MaybeExistConstraints, Args, _),

    % XXX handle ExistConstraints
    MaybeExistConstraints = no_exist_constraints,

    map.from_corresponding_lists(TypeParams, TypeArgs, TSubst),
    ArgTypes0 = list.map(func(C) = C ^ arg_type, Args),
    apply_subst_to_type_list(TSubst, ArgTypes0, ArgTypes).

type_is_du_type(ModuleInfo, Type) :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    type_to_ctor(Type, TypeCtor),
    search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeDefnBody),
    TypeDefnBody = hlds_du_type(_).

get_cons_defn(ModuleInfo, TypeCtor, ConsId, ConsDefn) :-
    module_info_get_cons_table(ModuleInfo, Ctors),
    % This search will fail for builtin cons_ids.
    search_cons_table_of_type_ctor(Ctors, TypeCtor, ConsId, ConsDefn).

get_cons_defn_det(ModuleInfo, TypeCtor, ConsId, ConsDefn) :-
    ( if get_cons_defn(ModuleInfo, TypeCtor, ConsId, ConsDefnPrime) then
        ConsDefn = ConsDefnPrime
    else
        unexpected($pred, "get_cons_defn failed")
    ).

get_cons_repn_defn(ModuleInfo, ConsId, ConsIdConsRepn) :-
    ConsId = cons(ConsSymName, ConsArity, TypeCtor),
    module_info_get_type_table(ModuleInfo, TypeTable),
    search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    get_type_defn_body(TypeDefn, TypeBody),
    TypeBody = hlds_du_type(type_body_du(_, _, _, MaybeRepn, _)),
    MaybeRepn = yes(Repn),
    Repn = du_type_repn(_, ConsRepnMap, _, _, _),
    ConsName = unqualify_name(ConsSymName),
    map.search(ConsRepnMap, ConsName, MatchingConsRepns),
    MatchingConsRepns = one_or_more(HeadConsRepn, TailConsRepns),
    find_cons_repn_with_given_arity(ConsArity,
        HeadConsRepn, TailConsRepns, ConsIdConsRepn).

:- pred find_cons_repn_with_given_arity(arity::in,
    constructor_repn::in, list(constructor_repn)::in,
    constructor_repn::out) is semidet.

find_cons_repn_with_given_arity(ConsArity,
        HeadConsRepn, TailConsRepns, ConsIdConsRepn) :-
    ( if ConsArity = HeadConsRepn ^ cr_num_args then
        ConsIdConsRepn = HeadConsRepn
    else
        TailConsRepns = [HeadTailConsRepn | TailTailConsRepns],
        find_cons_repn_with_given_arity(ConsArity,
            HeadTailConsRepn, TailTailConsRepns, ConsIdConsRepn)
    ).

get_cons_repn_defn_det(ModuleInfo, ConsId, ConsRepnDefn) :-
    ( if get_cons_repn_defn(ModuleInfo, ConsId, ConsRepnDefnPrime) then
        ConsRepnDefn = ConsRepnDefnPrime
    else
        unexpected($pred, "get_cons_repn_defn failed")
    ).

get_existq_cons_defn(ModuleInfo, VarType, ConsId, CtorDefn) :-
    cons_id_is_existq_cons_return_defn(ModuleInfo, VarType, ConsId, ConsDefn),
    ConsDefn = hlds_cons_defn(_TypeCtor, TypeVarSet, TypeParams, KindMap,
        MaybeExistConstraints, Args, _Context),
    ArgTypes = list.map(func(C) = C ^ arg_type, Args),
    prog_type.var_list_to_type_list(KindMap, TypeParams, TypeCtorArgs),
    type_to_ctor(VarType, TypeCtor),
    construct_type(TypeCtor, TypeCtorArgs, RetType),
    CtorDefn = ctor_defn(TypeVarSet, KindMap, MaybeExistConstraints,
        ArgTypes, RetType).

cons_id_is_existq_cons(ModuleInfo, VarType, ConsId) :-
    cons_id_is_existq_cons_return_defn(ModuleInfo, VarType, ConsId, _).

:- pred cons_id_is_existq_cons_return_defn(module_info::in, mer_type::in,
    cons_id::in, hlds_cons_defn::out) is semidet.

cons_id_is_existq_cons_return_defn(ModuleInfo, VarType, ConsId, ConsDefn) :-
    type_to_ctor(VarType, TypeCtor),
    get_cons_defn(ModuleInfo, TypeCtor, ConsId, ConsDefn),
    ConsDefn ^ cons_maybe_exist = exist_constraints(_).

%-----------------------------------------------------------------------------%

type_is_no_tag_type(ModuleInfo, Type) :-
    type_is_no_tag_type(ModuleInfo, Type, _Ctor, _ArgType).

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
    ( if get_existq_cons_defn(ModuleInfo, Type, ConsId, ConsDefn) then
        ConsDefn = ctor_defn(_TVarSet, _KindMap, MaybeExistConstraints,
            _ArgTypes, _ResultType),
        (
            MaybeExistConstraints = exist_constraints(ExistConstraints),
            ExistConstraints = cons_exist_constraints(ExistQTVars, Constraints,
                UnconstrainedExistQTVarsEC, _ConstrainedExistQTVars),
            list.length(Constraints, NumTypeClassInfos),
            list.length(UnconstrainedExistQTVarsEC,
                NumUnconstrainedExistQTVarsEC),
            constraint_list_get_tvars(Constraints, ConstrainedTVars),
            list.delete_elems(ExistQTVars, ConstrainedTVars,
                UnconstrainedExistQTVars),
            list.length(UnconstrainedExistQTVars, NumTypeInfos),
            AdjustedArity = NumTypeInfos + NumTypeClassInfos + ConsArity,
            % XXX ARG_PACK Sanity check.
            expect(unify(NumTypeInfos, NumUnconstrainedExistQTVarsEC), $pred,
                "NumTypeInfos != NumUnconstrainedExistQTVars")
        ;
            MaybeExistConstraints = no_exist_constraints,
            AdjustedArity = ConsArity
        )
    else
        AdjustedArity = ConsArity
    ).

%-----------------------------------------------------------------------------%

type_not_stored_in_region(Type, ModuleInfo) :-
    ( type_is_atomic(ModuleInfo, Type)
    ; is_type_a_dummy(ModuleInfo, Type) = is_dummy_type
    ; Type = type_info_type
    ; Type = type_ctor_info_type
    ; type_is_var(Type)
    ).

is_region_var(VarTypes, Var)  :-
    lookup_var_type(VarTypes, Var, Type),
    Type = region_type.

%-----------------------------------------------------------------------------%

put_typeinfo_vars_first(VarsList, VarTypes) =
        TypeInfoVarsList ++ NonTypeInfoVarsList :-
    split_vars_typeinfo_no_typeinfo(VarsList, VarTypes,
        TypeInfoVarsList, NonTypeInfoVarsList).

remove_typeinfo_vars(VarTypes, VarsList) = NonTypeInfoVarsList :-
    list.negated_filter(var_is_introduced_type_info_type(VarTypes),
        VarsList, NonTypeInfoVarsList).

remove_typeinfo_vars_from_set(VarTypes, VarsSet0) = VarsSet :-
    VarsList0 = set.to_sorted_list(VarsSet0),
    VarsList = remove_typeinfo_vars(VarTypes, VarsList0),
    VarsSet = set.sorted_list_to_set(VarsList).

remove_typeinfo_vars_from_set_of_var(VarTypes, VarsSet0) = VarsSet :-
    % XXX could be done more efficiently, operating directly on the set_of_var
    VarsList0 = set_of_var.to_sorted_list(VarsSet0),
    VarsList = remove_typeinfo_vars(VarTypes, VarsList0),
    VarsSet = set_of_var.sorted_list_to_set(VarsList).

:- pred split_vars_typeinfo_no_typeinfo(list(prog_var)::in,
    vartypes::in, list(prog_var)::out, list(prog_var)::out) is det.

split_vars_typeinfo_no_typeinfo(VarsList, VarTypes, TypeInfoVarsList,
        NonTypeInfoVarsList) :-
    list.filter(var_is_introduced_type_info_type(VarTypes),
        VarsList, TypeInfoVarsList, NonTypeInfoVarsList).

:- pred var_is_introduced_type_info_type(vartypes::in, prog_var::in)
    is semidet.

var_is_introduced_type_info_type(VarTypes, Var) :-
    lookup_var_type(VarTypes, Var, Type),
    is_introduced_type_info_type(Type).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_constraint(Renaming, !Constraint) :-
    !.Constraint = hlds_constraint(Ids, ClassName, ArgTypes0),
    apply_variable_renaming_to_type_list(Renaming, ArgTypes0, ArgTypes),
    !:Constraint = hlds_constraint(Ids, ClassName, ArgTypes).

apply_subst_to_constraint(Subst, !Constraint) :-
    !.Constraint = hlds_constraint(Ids, ClassName, ArgTypes0),
    apply_subst_to_type_list(Subst, ArgTypes0, ArgTypes),
    !:Constraint = hlds_constraint(Ids, ClassName, ArgTypes).

apply_rec_subst_to_constraint(Subst, !Constraint) :-
    !.Constraint = hlds_constraint(Ids, ClassName, ArgTypes0),
    apply_rec_subst_to_type_list(Subst, ArgTypes0, ArgTypes),
    !:Constraint = hlds_constraint(Ids, ClassName, ArgTypes).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_constraint_list(Renaming, !Constraints) :-
    list.map(apply_variable_renaming_to_constraint(Renaming), !Constraints).

apply_subst_to_constraint_list(Subst, !Constraints) :-
    list.map(apply_subst_to_constraint(Subst), !Constraints).

apply_rec_subst_to_constraint_list(Subst, !Constraints) :-
    list.map(apply_rec_subst_to_constraint(Subst), !Constraints).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_constraints(Renaming, !Constraints) :-
    !.Constraints = hlds_constraints(Unproven0, Assumed0,
        Redundant0, Ancestors0),
    % Most of the time, !.Constraints contains nothing. Even when some
    % of its fields are not empty, some others may be.
    ( if
        Unproven0 = [],
        Assumed0 = [],
        map.is_empty(Redundant0),
        map.is_empty(Ancestors0)
    then
        true
    else
        apply_variable_renaming_to_constraint_list(Renaming,
            Unproven0, Unproven),
        apply_variable_renaming_to_constraint_list(Renaming,
            Assumed0, Assumed),
        ( if map.is_empty(Redundant0) then
            Redundant = Redundant0
        else
            Pred =
                ( pred(C0::in, C::out) is det :-
                    set.to_sorted_list(C0, L0),
                    apply_variable_renaming_to_constraint_list(Renaming,
                        L0, L),
                    set.list_to_set(L, C)
                ),
            map.map_values_only(Pred, Redundant0, Redundant)
        ),
        ( if map.is_empty(Ancestors0) then
            Ancestors = Ancestors0
        else
            map.keys(Ancestors0, AncestorsKeys0),
            map.values(Ancestors0, AncestorsValues0),
            apply_variable_renaming_to_prog_constraint_list(Renaming,
                AncestorsKeys0, AncestorsKeys),
            list.map(apply_variable_renaming_to_prog_constraint_list(Renaming),
                AncestorsValues0, AncestorsValues),
            map.from_corresponding_lists(AncestorsKeys, AncestorsValues,
                Ancestors)
        ),
        !:Constraints =
            hlds_constraints(Unproven, Assumed, Redundant, Ancestors)
    ).

apply_subst_to_constraints(Subst, !Constraints) :-
    !.Constraints = hlds_constraints(Unproven0, Assumed0,
        Redundant0, Ancestors0),
    apply_subst_to_constraint_list(Subst, Unproven0, Unproven),
    apply_subst_to_constraint_list(Subst, Assumed0, Assumed),
    Pred =
        ( pred(C0::in, C::out) is det :-
            set.to_sorted_list(C0, L0),
            apply_subst_to_constraint_list(Subst, L0, L),
            set.list_to_set(L, C)
        ),
    map.map_values_only(Pred, Redundant0, Redundant),
    map.keys(Ancestors0, AncestorsKeys0),
    map.values(Ancestors0, AncestorsValues0),
    apply_subst_to_prog_constraint_list(Subst, AncestorsKeys0, AncestorsKeys),
    list.map(apply_subst_to_prog_constraint_list(Subst),
        AncestorsValues0, AncestorsValues),
    map.from_corresponding_lists(AncestorsKeys, AncestorsValues, Ancestors),
    !:Constraints = hlds_constraints(Unproven, Assumed, Redundant, Ancestors).

apply_rec_subst_to_constraints(Subst, !Constraints) :-
    !.Constraints = hlds_constraints(Unproven0, Assumed0,
        Redundant0, Ancestors0),
    apply_rec_subst_to_constraint_list(Subst, Unproven0, Unproven),
    apply_rec_subst_to_constraint_list(Subst, Assumed0, Assumed),
    Pred =
        ( pred(C0::in, C::out) is det :-
            set.to_sorted_list(C0, L0),
            apply_rec_subst_to_constraint_list(Subst, L0, L),
            set.list_to_set(L, C)
        ),
    map.map_values_only(Pred, Redundant0, Redundant),
    map.keys(Ancestors0, AncestorsKeys0),
    map.values(Ancestors0, AncestorsValues0),
    apply_rec_subst_to_prog_constraint_list(Subst,
        AncestorsKeys0, AncestorsKeys),
    list.map(apply_rec_subst_to_prog_constraint_list(Subst),
        AncestorsValues0, AncestorsValues),
    map.from_corresponding_lists(AncestorsKeys, AncestorsValues, Ancestors),
    !:Constraints = hlds_constraints(Unproven, Assumed, Redundant, Ancestors).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_constraint_proof_map(Renaming,
        ProofMap0, ProofMap) :-
    ( if map.is_empty(ProofMap0) then
        % Optimize the simple case.
        ProofMap = ProofMap0
    else
        map.keys(ProofMap0, Keys0),
        map.values(ProofMap0, Values0),
        apply_variable_renaming_to_prog_constraint_list(Renaming, Keys0, Keys),
        list.map(rename_constraint_proof(Renaming), Values0, Values),
        map.from_corresponding_lists(Keys, Values, ProofMap)
    ).

    % Apply a type variable renaming to a class constraint proof.
    %
:- pred rename_constraint_proof(tvar_renaming::in,
    constraint_proof::in, constraint_proof::out) is det.

rename_constraint_proof(TSubst, Proof0, Proof) :-
    (
        Proof0 = apply_instance(_Num),
        Proof = Proof0
    ;
        Proof0 = superclass(ClassConstraint0),
        apply_variable_renaming_to_prog_constraint(TSubst,
            ClassConstraint0, ClassConstraint),
        Proof = superclass(ClassConstraint)
    ).

apply_subst_to_constraint_proof_map(Subst, ProofMap0, ProofMap) :-
    map.foldl(apply_subst_to_constraint_proof_map_2(Subst), ProofMap0,
        map.init, ProofMap).

:- pred apply_subst_to_constraint_proof_map_2(tsubst::in,
    prog_constraint::in, constraint_proof::in,
    constraint_proof_map::in, constraint_proof_map::out) is det.

apply_subst_to_constraint_proof_map_2(Subst, Constraint0, Proof0, !ProofMap) :-
    apply_subst_to_prog_constraint(Subst, Constraint0, Constraint),
    (
        Proof0 = apply_instance(_),
        Proof = Proof0
    ;
        Proof0 = superclass(Super0),
        apply_subst_to_prog_constraint(Subst, Super0, Super),
        Proof = superclass(Super)
    ),
    map.set(Constraint, Proof, !ProofMap).

apply_rec_subst_to_constraint_proof_map(Subst, ProofMap0, ProofMap) :-
    map.foldl(apply_rec_subst_to_constraint_proof_map_2(Subst), ProofMap0,
        map.init, ProofMap).

:- pred apply_rec_subst_to_constraint_proof_map_2(tsubst::in,
    prog_constraint::in, constraint_proof::in,
    constraint_proof_map::in, constraint_proof_map::out) is det.

apply_rec_subst_to_constraint_proof_map_2(Subst, Constraint0, Proof0,
        !ProofMap) :-
    apply_rec_subst_to_prog_constraint(Subst, Constraint0, Constraint),
    (
        Proof0 = apply_instance(_),
        Proof = Proof0
    ;
        Proof0 = superclass(Super0),
        apply_rec_subst_to_prog_constraint(Subst, Super0, Super),
        Proof = superclass(Super)
    ),
    map.set(Constraint, Proof, !ProofMap).

%-----------------------------------------------------------------------------%

apply_variable_renaming_to_constraint_map(Renaming, !ConstraintMap) :-
    map.map_values_only(apply_variable_renaming_to_prog_constraint(Renaming),
        !ConstraintMap).

apply_subst_to_constraint_map(Subst, !ConstraintMap) :-
    map.map_values_only(apply_subst_to_prog_constraint(Subst), !ConstraintMap).

apply_rec_subst_to_constraint_map(Subst, !ConstraintMap) :-
    map.map_values_only(apply_rec_subst_to_prog_constraint(Subst),
        !ConstraintMap).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.type_util.
%-----------------------------------------------------------------------------%

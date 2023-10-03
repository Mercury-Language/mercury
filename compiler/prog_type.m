%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% Copyright (C) 2014-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_type.m.
% Main author: fjh.
%
% Utility predicates dealing with types in the parse tree. The predicates for
% doing type substitutions are in prog_type_subst.m, while utility predicates
% for dealing with types in the HLDS are in type_util.m.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_type.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module one_or_more.
:- import_module set.
:- import_module term.

%---------------------------------------------------------------------------%
%
% Simple tests for certain properties of types. These tests work modulo any
% kind annotations, so in the early stages of the compiler (i.e., before type
% checking) these should be used rather than direct tests. Once we reach
% type checking, all kind annotations should have been removed, so it would
% be preferable to switch on the top functor rather than use these predicates
% in an if-then-else expression, since switches will give better error
% detection.
%

    % Succeeds iff the given type is a variable.
    %
:- pred type_is_var(mer_type::in) is semidet.

    % Succeeds iff the given type is not a variable.
    %
:- pred type_is_nonvar(mer_type::in) is semidet.

    % Succeed if the given type is a tuple type, returning
    % the argument types.
    %
:- pred type_is_tuple(mer_type::in, list(mer_type)::out) is semidet.

    % Succeeds iff the given type is a higher-order predicate or function type.
    %
:- pred type_is_higher_order(mer_type::in) is semidet.

    % type_is_higher_order_details(Type, Purity, PredOrFunc, ArgTypes,
    %   EvalMethod):
    %
    % Succeeds iff Type is a higher-order predicate or function type with
    % the specified argument types (for functions, the return type is appended
    % to the end of the argument types), purity, and evaluation method.
    %
:- pred type_is_higher_order_details(mer_type::in, purity::out,
    pred_or_func::out, lambda_eval_method::out, list(mer_type)::out)
    is semidet.

:- pred type_is_higher_order_details_det(mer_type::in, purity::out,
    pred_or_func::out, lambda_eval_method::out, list(mer_type)::out)
    is det.

%---------------------------------------------------------------------------%

:- type non_kinded_type =< mer_type
    --->    type_variable(tvar, kind)
    ;       defined_type(sym_name, list(mer_type), kind)
    ;       builtin_type(builtin_type)
    ;       tuple_type(list(mer_type), kind)
    ;       higher_order_type(pred_or_func, list(mer_type), ho_inst_info,
                purity, lambda_eval_method)
    ;       apply_n_type(tvar, list(mer_type), kind).

    % Remove the kind annotation at the top-level if there is one,
    % otherwise return the type unchanged.
    %
:- func strip_kind_annotation(mer_type) = non_kinded_type.

%---------------------------------------------------------------------------%

    % Succeeds iff the given type is ground (that is, contains no type
    % variables).
    %
:- pred type_is_ground(mer_type::in) is semidet.

    % Succeeds iff the given type contains no type variables except
    % for those in the given list.
    %
:- pred type_is_ground_except_vars(mer_type::in, list(tvar)::in) is semidet.

    % Succeeds iff the given type is not ground.
    %
:- pred type_is_nonground(mer_type::in) is semidet.

    % Succeeds iff the given type with the substitution applied is ground.
    %
:- pred subst_type_is_ground(mer_type::in, tsubst::in) is semidet.

    % Succeeds iff the given type with the substitution applied is not
    % ground.
    %
:- pred subst_type_is_nonground(mer_type::in, tsubst::in) is semidet.

%---------------------------------------------------------------------------%

    % Given a non-variable type, return its type_ctor and argument types.
    % Fail if the type is a variable.
    %
:- pred type_to_ctor_and_args(mer_type::in, type_ctor::out,
    list(mer_type)::out) is semidet.

    % Given a non-variable type, return its type_ctor and argument types.
    % Abort if the type is a variable.
    %
:- pred type_to_ctor_and_args_det(mer_type::in, type_ctor::out,
    list(mer_type)::out) is det.

    % Given a non-variable type, return its type_ctor.
    % Fail if the type is a variable.
    %
:- pred type_to_ctor(mer_type::in, type_ctor::out) is semidet.

    % Given a non-variable type, return its type_ctor.
    % Abort if the type is a variable.
    %
:- pred type_to_ctor_det(mer_type::in, type_ctor::out) is det.

    % type_has_variable_arity_ctor(Type, TypeCtor, ArgTypes):
    %
    % Check if the principal type constructor of Type is of variable arity.
    % If yes, return the type constructor as TypeCtor and its args as
    % ArgTypes. If not, fail.
    %
:- pred type_has_variable_arity_ctor(mer_type::in, type_ctor::out,
    list(mer_type)::out) is semidet.

%---------------------------------------------------------------------------%

    % type_ctor_is_higher_order(TypeCtor, PredOrFunc) succeeds iff
    % TypeCtor is a higher-order predicate or function type.
    %
:- pred type_ctor_is_higher_order(type_ctor::in, purity::out,
    pred_or_func::out, lambda_eval_method::out) is semidet.

    % type_ctor_is_tuple(TypeCtor) succeeds iff TypeCtor is a tuple type.
    %
:- pred type_ctor_is_tuple(type_ctor::in) is semidet.

%---------------------------------------------------------------------------%

    % Convert a list of types to a list of vars. Fail if any of the type are
    % not variables.
    %
:- pred type_list_to_var_list(list(mer_type)::in, list(tvar)::out) is semidet.

    % Convert a var into a variable type.
    %
:- pred var_to_type(tvar_kind_map::in, tvar::in, mer_type::out) is det.

    % Convert a list of vars into a list of variable types.
    %
:- pred var_list_to_type_list(tvar_kind_map::in, list(tvar)::in,
    list(mer_type)::out) is det.

%---------------------------------------------------------------------------%

    % Return a list of the type variables of a type, or a list of types,
    % in order of their first occurrence in a depth-first, left-right
    % traversal.
    %
:- pred type_vars_in_type(mer_type::in, list(tvar)::out) is det.
:- pred type_vars_in_types(list(mer_type)::in, list(tvar)::out) is det.

    % Return the set of the type variables of a type, or a list of types.
    %
:- pred set_of_type_vars_in_type(mer_type::in, set(tvar)::out) is det.
:- pred set_of_type_vars_in_types(list(mer_type)::in, set(tvar)::out) is det.

    % Nondeterministically return the variables in a type.
    %
:- pred type_contains_var(mer_type::in, tvar::out) is nondet.

    % Nondeterministically return the variables in a list of types.
    %
:- pred type_list_contains_var(list(mer_type)::in, tvar::out) is nondet.

%---------------------------------------------------------------------------%

    % Given a constant and an arity, return a type_ctor.
    % Fails if the constant is not an atom.
    %
    % This really ought to take a name and an arity -
    % use of integers/floats/strings as type names should be rejected
    % by the parser, not by module_qual.m.
    %
:- pred make_type_ctor(const::in, int::in, type_ctor::out) is semidet.

    % Given a type_ctor and a list of argument types,
    % construct a type.
    %
:- pred construct_type(type_ctor::in, list(mer_type)::in, mer_type::out)
    is det.

:- pred construct_higher_order_type(purity::in, pred_or_func::in,
    lambda_eval_method::in, list(mer_type)::in, mer_type::out) is det.

:- pred construct_higher_order_pred_type(purity::in, lambda_eval_method::in,
    list(mer_type)::in, mer_type::out) is det.

:- pred construct_higher_order_pred_type(purity::in, lambda_eval_method::in,
    list(mer_type)::in, list(mer_mode)::in, determinism::in, mer_type::out)
    is det.

:- pred construct_higher_order_func_type(purity::in, lambda_eval_method::in,
    list(mer_type)::in, mer_type::in, mer_type::out) is det.

:- pred construct_higher_order_func_type(purity::in, lambda_eval_method::in,
    list(mer_type)::in, mer_type::in, list(mer_mode)::in, mer_mode::in,
    determinism::in, mer_type::out) is det.

%---------------------------------------------------------------------------%

    % Make error messages more readable by removing some or all
    % module qualifiers from type and mode names contained in the given type
    % or types, regardless of how deeply they are nested.
    %
:- pred strip_module_names_from_type(strip_what_module_names::in,
    mer_type::in, mer_type::out) is det.
:- pred strip_module_names_from_type_list(strip_what_module_names::in,
    list(mer_type)::in, list(mer_type)::out) is det.

%---------------------------------------------------------------------------%

    % Return the list of type variables contained in a list of constraints.
    %
:- pred prog_constraints_get_tvars(prog_constraints::in, list(tvar)::out)
    is det.

    % Return the list of type variables contained in a list of constraints.
    %
:- pred constraint_list_get_tvars(list(prog_constraint)::in, list(tvar)::out)
    is det.

    % Return the list of type variables contained in a constraint.
    %
:- pred constraint_get_tvars(prog_constraint::in, list(tvar)::out) is det.

:- pred get_unconstrained_tvars(list(tvar)::in, list(prog_constraint)::in,
    list(tvar)::out) is det.

%---------------------------------------------------------------------------%

    % The list of type_ctors which are builtins which do not have a
    % hlds_type_defn.
    %
:- func builtin_type_ctors_with_no_hlds_type_defn = list(type_ctor).

%---------------------------------------------------------------------------%

:- type is_dummy_type
    --->    is_dummy_type
    ;       is_not_dummy_type.

:- type is_builtin_dummy_type_ctor
    --->    is_builtin_dummy_type_ctor
    ;       is_builtin_non_dummy_type_ctor
    ;       is_not_builtin_dummy_type_ctor.

:- type type_ctor_category
    --->    ctor_cat_builtin(type_ctor_cat_builtin)
    ;       ctor_cat_builtin_dummy
    ;       ctor_cat_void
    ;       ctor_cat_variable
    ;       ctor_cat_higher_order
    ;       ctor_cat_tuple
    ;       ctor_cat_enum(type_ctor_cat_enum)
    ;       ctor_cat_system(type_ctor_cat_system)
    ;       ctor_cat_user(type_ctor_cat_user).

:- type nb_type_ctor_category =< type_ctor_category
    --->    ctor_cat_builtin_dummy
    ;       ctor_cat_void
    ;       ctor_cat_variable
    ;       ctor_cat_higher_order
    ;       ctor_cat_tuple
    ;       ctor_cat_enum(type_ctor_cat_enum)
    ;       ctor_cat_system(type_ctor_cat_system)
    ;       ctor_cat_user(type_ctor_cat_user).

:- type type_ctor_cat_builtin
    --->    cat_builtin_int(int_type)
    ;       cat_builtin_float
    ;       cat_builtin_char
    ;       cat_builtin_string.

:- type type_ctor_cat_system
    --->    cat_system_type_info
    ;       cat_system_type_ctor_info
    ;       cat_system_typeclass_info
    ;       cat_system_base_typeclass_info.

:- type type_ctor_cat_enum
    --->    cat_enum_mercury
            % XXX TYPE_REPN Should we add an arg specifying
            % the number of bits needed to store the enum?
    ;       cat_enum_foreign.

:- type type_ctor_cat_user
    --->    cat_user_direct_dummy
    ;       cat_user_abstract_dummy
    ;       cat_user_notag
    ;       cat_user_abstract_notag
    ;       cat_user_general.

%---------------------------------------------------------------------------%

    % is_builtin_dummy_type_ctor(type_ctor):
    %
    % Is the given type constructor a dummy type irrespective
    % of its definition?
    %
:- func is_type_ctor_a_builtin_dummy(type_ctor) = is_builtin_dummy_type_ctor.

:- pred type_is_io_state(mer_type::in) is semidet.

:- pred type_ctor_is_array(type_ctor::in) is semidet.

:- pred type_ctor_is_bitmap(type_ctor::in) is semidet.

    % A test for type_info-related types that are introduced by
    % polymorphism.m.  These need to be handled specially in certain
    % places.  For example, mode inference never infers unique modes
    % for these types, since it would not be useful, and since we
    % want to minimize the number of different modes that we infer.
    %
:- pred is_introduced_type_info_type(mer_type::in) is semidet.

:- pred is_introduced_type_info_type_ctor(type_ctor::in) is semidet.

:- func is_introduced_type_info_type_category(type_ctor_category) = bool.

%---------------------------------------------------------------------------%

    % Check for a "new " prefix at the start of the functor name,
    % and remove it if present; if there is no such prefix, fail.
    % (These prefixes are used for construction unifications
    % with existentially typed functors.)
    %
:- pred remove_new_prefix(sym_name::in, sym_name::out) is semidet.

    % Prepend a "new " prefix at the start of the given functor name.
    % (These prefixes are used for construction unifications
    % with existentially typed functors.)
    %
:- pred add_new_prefix(sym_name::in, sym_name::out) is det.

%---------------------------------------------------------------------------%

:- type polymorphism_cell
    --->    type_info_cell(type_ctor)
    ;       typeclass_info_cell.

:- func cell_cons_id(polymorphism_cell) = cons_id.

:- func cell_inst_cons_id(polymorphism_cell, int) = cons_id.

    % Module-qualify the cons_id using module information from the type.
    % The second output value is the cons_id required for use in insts which
    % can be different from that used in types for typeclass_info and
    % type_info. The list(prog_var) is the list of arguments to the cons_id
    % and is just used for obtaining the arity for typeclass_info and type_info
    % cons_ids.
    %
:- pred qualify_cons_id(list(prog_var)::in, cons_id::in,
    cons_id::out, cons_id::out) is det.

%---------------------------------------------------------------------------%

    % Given a list of constructors for a type, check whether that type
    % is a private_builtin.type_info/0 or similar type.
    %
:- pred type_constructors_are_type_info(list(constructor)::in) is semidet.

    % Is the discriminated union type (not a subtype) with the given list of
    % constructors a notag type?
    %
:- pred non_sub_du_type_is_notag(one_or_more(constructor)::in,
    maybe_canonical::in) is semidet.

    % Is the discriminated union type (not a subtype) with the given list of
    % constructors an enum? If yes, return the number of enum values.
    %
:- pred non_sub_du_type_is_enum(type_details_du::in, int::out) is semidet.

    % Return the number of bits required to represent
    % the given number of values, 0 to n-1.
    %
:- pred num_bits_needed_for_n_dense_values(int::in, int::out) is det.

    % Is the discriminated union type (not a subtype) with the given list of
    % constructors a dummy type?
    %
:- pred non_sub_du_type_is_dummy(type_details_du::in) is semidet.

%---------------------------------------------------------------------------%
%
% Type unification.
%

    % Unify (with occurs check) two types with respect to a type substitution
    % and update the type bindings. The third argument is a list of type
    % variables which cannot be bound (i.e. head type variables).
    %
    % No kind checking is done, since it is assumed that kind errors
    % will be picked up elsewhere.
    %
:- pred type_unify(mer_type::in, mer_type::in, list(tvar)::in, tsubst::in,
    tsubst::out) is semidet.

:- pred type_unify_list(list(mer_type)::in, list(mer_type)::in, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

%---------------------------------------------------------------------------%
%
% Type subsumption.
%

    % type_subsumes(TypeA, TypeB, Subst) succeeds iff TypeA subsumes
    % (is more general than) TypeB, producing a type substitution
    % which when applied to TypeA will give TypeB.
    %
:- pred type_subsumes(mer_type::in, mer_type::in, tsubst::out) is semidet.

    % Same as type_subsumes, but aborts instead of failing.
    %
:- pred type_subsumes_det(mer_type::in, mer_type::in, tsubst::out) is det.

    % type_list_subsumes(TypesA, TypesB, Subst) succeeds iff the list
    % TypesA subsumes (is more general than) TypesB, producing a
    % type substitution which when applied to TypesA will give TypesB.
    %
:- pred type_list_subsumes(list(mer_type)::in, list(mer_type)::in, tsubst::out)
    is semidet.

    % Same as type_list_subsumes, but aborts instead of failing.
    %
:- pred type_list_subsumes_det(list(mer_type)::in, list(mer_type)::in,
    tsubst::out) is det.

    % arg_type_list_subsumes(TVarSet, ExistQVars, ArgTypes, HeadTypeParams,
    %   CalleeTVarSet, CalleeExistQVars, CalleeArgTypes):
    % XXX This comment has suffered bit rot.
    %
    % Check that the argument types of the called predicate, function or
    % constructor subsume the types of the arguments of the call. This checks
    % that none of the existentially quantified type variables of the callee
    % are bound.
    %
:- pred arg_type_list_subsumes(tvarset::in, existq_tvars::in,
    list(mer_type)::in, list(tvar)::in,
    tvarset::in, tvar_kind_map::in, existq_tvars::in, list(mer_type)::in)
    is semidet.

%---------------------------------------------------------------------------%

    % compute_caller_callee_type_substitution(CalleeArgTypes, CallerArgTypes,
    %   ExternalTypeParams, CalleeExistQTVars, TypeSubn):
    %
    % Work out a type substitution to map the callee's argument types
    % into the caller's.
    %
:- pred compute_caller_callee_type_substitution(list(mer_type)::in,
    list(mer_type)::in, list(tvar)::in, list(tvar)::in, tsubst::out) is det.

%---------------------------------------------------------------------------%

    % Apply a renaming (partial map) to a list.
    % Useful for applying a variable renaming to a list of variables.
    %
:- pred apply_partial_map_to_list(map(T, T)::in, list(T)::in, list(T)::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type_subst.

:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

type_is_var(Type) :-
    strip_kind_annotation(Type) = type_variable(_, _).

type_is_nonvar(Type) :-
    not type_is_var(Type).

type_is_tuple(Type, ArgTypes) :-
    strip_kind_annotation(Type) = tuple_type(ArgTypes, _).

type_is_higher_order(Type) :-
    strip_kind_annotation(Type) = higher_order_type(_, _, _, _, _).

type_is_higher_order_details(Type, Purity, PredOrFunc, EvalMethod, ArgTypes) :-
    strip_kind_annotation(Type) =
        higher_order_type(PredOrFunc, ArgTypes, _HOInstInfo, Purity,
            EvalMethod).

type_is_higher_order_details_det(Type, !:Purity, !:PredOrFunc, !:EvalMethod,
        !:PredArgTypes) :-
    ( if
        type_is_higher_order_details(Type, !:Purity, !:PredOrFunc,
            !:EvalMethod, !:PredArgTypes)
    then
        true
    else
        unexpected($pred, "type is not higher-order")
    ).

%---------------------------------------------------------------------------%

strip_kind_annotation(Type0) = Type :-
    (
        Type0 = kinded_type(Type1, _),
        Type = strip_kind_annotation(Type1)
    ;
        ( Type0 = type_variable(_, _)
        ; Type0 = defined_type(_, _, _)
        ; Type0 = builtin_type(_)
        ; Type0 = tuple_type(_, _)
        ; Type0 = higher_order_type(_, _, _, _, _)
        ; Type0 = apply_n_type(_, _, _)
        ),
        Type = coerce(Type0)
    ).

%---------------------------------------------------------------------------%

type_is_ground(Type) :-
    not type_contains_var(Type, _).

type_is_ground_except_vars(Type, Except) :-
    all [TVar] (
        type_contains_var(Type, TVar)
    =>
        list.contains(Except, TVar)
    ).

type_is_nonground(Type) :-
    type_contains_var(Type, _).

subst_type_is_ground(Type, TSubst) :-
    not subst_type_is_nonground(Type, TSubst).

subst_type_is_nonground(Type, TSubst) :-
    type_contains_var(Type, TVar),
    ( if map.search(TSubst, TVar, Binding) then
        subst_type_is_nonground(Binding, TSubst)
    else
        true
    ).

%---------------------------------------------------------------------------%

type_to_ctor_and_args(Type, TypeCtor, ArgTypes) :-
    require_complete_switch [Type]
    (
        Type = type_variable(_, _),
        fail
    ;
        Type = defined_type(SymName, ArgTypes, _),
        Arity = list.length(ArgTypes),
        TypeCtor = type_ctor(SymName, Arity)
    ;
        Type = builtin_type(BuiltinType),
        builtin_type_name(BuiltinType, Name),
        SymName = unqualified(Name),
        Arity = 0,
        ArgTypes = [],
        TypeCtor = type_ctor(SymName, Arity)
    ;
        Type = higher_order_type(PorF, ArgTypes, _HO, Purity, _EvalMethod),
        list.length(ArgTypes, NumArgTypes),
        (
            PorF = pf_predicate,
            PorFStr = "pred",
            UserArity = NumArgTypes
        ;
            PorF = pf_function,
            PorFStr = "func",
            UserArity = NumArgTypes - 1
        ),
        SymName0 = unqualified(PorFStr),
        (
            Purity = purity_pure,
            SymName = SymName0
        ;
            Purity = purity_semipure,
            SymName = add_outermost_qualifier("semipure", SymName0)
        ;
            Purity = purity_impure,
            SymName = add_outermost_qualifier("impure", SymName0)
        ),
        TypeCtor = type_ctor(SymName, UserArity)
    ;
        Type = tuple_type(ArgTypes, _),
        SymName = unqualified("{}"),
        Arity = list.length(ArgTypes),
        TypeCtor = type_ctor(SymName, Arity)
    ;
        Type = apply_n_type(_, _, _),
        sorry($pred, "apply/N types")
    ;
        Type = kinded_type(SubType, _),
        type_to_ctor_and_args(SubType, TypeCtor, ArgTypes)
    ).

type_to_ctor_and_args_det(Type, TypeCtor, ArgTypes) :-
    ( if type_to_ctor_and_args(Type, TypeCtorPrime, ArgTypesPrime) then
        TypeCtor = TypeCtorPrime,
        ArgTypes = ArgTypesPrime
    else
        unexpected($pred, "type_to_ctor_and_args failed: " ++ string(Type))
    ).

type_to_ctor(Type, TypeCtor) :-
    % This should be subject to unused argument elimination.
    type_to_ctor_and_args(Type, TypeCtor, _ArgTypes).

type_to_ctor_det(Type, TypeCtor) :-
    % This should be subject to unused argument elimination.
    type_to_ctor_and_args_det(Type, TypeCtor, _ArgTypes).

type_has_variable_arity_ctor(Type, TypeCtor, ArgTypes) :-
    ( if
        type_is_higher_order_details(Type, _Purity, PredOrFunc, _, ArgTypes0)
    then
        ArgTypes = ArgTypes0,
        PredOrFuncStr = parse_tree_out_misc.pred_or_func_to_str(PredOrFunc),
        TypeCtor = type_ctor(unqualified(PredOrFuncStr), 0)
    else if
        type_is_tuple(Type, ArgTypes1)
    then
        ArgTypes = ArgTypes1,
        % XXX why tuple/0 and not {}/N ?
        TypeCtor = type_ctor(unqualified("tuple"), 0)
    else
        fail
    ).

%---------------------------------------------------------------------------%

type_ctor_is_higher_order(TypeCtor, Purity, PredOrFunc, EvalMethod) :-
    % Please keep this code in sync with classify_type_ctor_if_special.
    % XXX Unlike classify_type_ctor_if_special, this code here does NOT test
    % for mercury_public_builtin_module as ModuleSymName. This preserves
    % old behavior, but I (zs) think that it may nevertheless be a bug,
    % either here, or in classify_type_ctor_if_special.
    TypeCtor = type_ctor(SymName, _Arity),
    (
        SymName = qualified(ModuleSymName, PorFStr),
        ModuleSymName = unqualified(Qualifier),
        (
            Qualifier = "impure",
            Purity = purity_impure,
            EvalMethod = lambda_normal
        ;
            Qualifier = "semipure",
            Purity = purity_semipure,
            EvalMethod = lambda_normal
        )
    ;
        SymName = unqualified(PorFStr),
        EvalMethod = lambda_normal,
        Purity = purity_pure
    ),
    (
        PorFStr = "pred",
        PredOrFunc = pf_predicate
    ;
        PorFStr = "func",
        PredOrFunc = pf_function
    ).

% Please keep this code in sync with classify_type_ctor_if_special.
type_ctor_is_tuple(type_ctor(unqualified("{}"), _)).

%---------------------------------------------------------------------------%

type_list_to_var_list([], []).
type_list_to_var_list([Type | Types], [Var | Vars]) :-
    Type = type_variable(Var, _),
    type_list_to_var_list(Types, Vars).

var_to_type(KindMap, Var, Type) :-
    get_tvar_kind(KindMap, Var, Kind),
    Type = type_variable(Var, Kind).

var_list_to_type_list(_, [], []).
var_list_to_type_list(KindMap, [Var | Vars], [Type | Types]) :-
    var_to_type(KindMap, Var, Type),
    var_list_to_type_list(KindMap, Vars, Types).

%---------------------------------------------------------------------------%

type_vars_in_type(Type, TVars) :-
    type_vars_in_type_acc(Type, [], RevTVars),
    list.reverse(RevTVars, TVarsDups),
    list.remove_dups(TVarsDups, TVars).

type_vars_in_types(Types, TVars) :-
    type_vars_in_types_acc(Types, [], RevTVars),
    list.reverse(RevTVars, TVarsDups),
    list.remove_dups(TVarsDups, TVars).

:- pred type_vars_in_type_acc(mer_type::in,
    list(tvar)::in, list(tvar)::out) is det.

type_vars_in_type_acc(type_variable(Var, _), !RevTVars) :-
    !:RevTVars = [Var | !.RevTVars].
type_vars_in_type_acc(defined_type(_, ArgTypes, _), !RevTVars) :-
    type_vars_in_types_acc(ArgTypes, !RevTVars).
type_vars_in_type_acc(builtin_type(_), !RevTVars).
type_vars_in_type_acc(higher_order_type(_, ArgTypes, _, _, _), !RevTVars) :-
    type_vars_in_types_acc(ArgTypes, !RevTVars).
type_vars_in_type_acc(tuple_type(ArgTypes, _), !RevTVars) :-
    type_vars_in_types_acc(ArgTypes, !RevTVars).
type_vars_in_type_acc(apply_n_type(Var, ArgTypes, _), !RevTVars) :-
    !:RevTVars= [Var | !.RevTVars],
    type_vars_in_types_acc(ArgTypes, !RevTVars).
type_vars_in_type_acc(kinded_type(Type, _), !RevTVars) :-
    type_vars_in_type_acc(Type, !RevTVars).

:- pred type_vars_in_types_acc(list(mer_type)::in,
    list(tvar)::in, list(tvar)::out) is det.

type_vars_in_types_acc([], !RevTVars).
type_vars_in_types_acc([Type | Types], !RevTVars) :-
    type_vars_in_type_acc(Type, !RevTVars),
    type_vars_in_types_acc(Types, !RevTVars).

%---------------------%

set_of_type_vars_in_type(Type, SetOfTVars) :-
    type_vars_in_type(Type, TVars),
    set.list_to_set(TVars, SetOfTVars).

set_of_type_vars_in_types(Types, SetOfTVars) :-
    type_vars_in_types(Types, TVars),
    set.list_to_set(TVars, SetOfTVars).

%---------------------%

type_contains_var(type_variable(Var, _), Var).
type_contains_var(defined_type(_, ArgTypes, _), Var) :-
    type_list_contains_var(ArgTypes, Var).
type_contains_var(higher_order_type(_, ArgTypes, _, _, _), Var) :-
    type_list_contains_var(ArgTypes, Var).
type_contains_var(tuple_type(ArgTypes, _), Var) :-
    type_list_contains_var(ArgTypes, Var).
type_contains_var(apply_n_type(Var, _, _), Var).
type_contains_var(apply_n_type(_, ArgTypes, _), Var) :-
    type_list_contains_var(ArgTypes, Var).
type_contains_var(kinded_type(Type, _), Var) :-
    type_contains_var(Type, Var).

type_list_contains_var([Type | _], Var) :-
    type_contains_var(Type, Var).
type_list_contains_var([_ | Types], Var) :-
    type_list_contains_var(Types, Var).

%---------------------------------------------------------------------------%

make_type_ctor(term.atom(Name), Arity, type_ctor(unqualified(Name), Arity)).

construct_type(TypeCtor, ArgTypes, Type) :-
    ( if
        TypeCtor = type_ctor(unqualified(Name), 0),
        builtin_type_name(BuiltinType, Name)
    then
        Type = builtin_type(BuiltinType)
    else if
        type_ctor_is_higher_order(TypeCtor, Purity, PredOrFunc, EvalMethod)
    then
        construct_higher_order_type(Purity, PredOrFunc, EvalMethod, ArgTypes,
            Type)
    else if
        type_ctor_is_tuple(TypeCtor)
    then
        % XXX kind inference: we assume the kind is star.
        Type = tuple_type(ArgTypes, kind_star)
    else
        TypeCtor = type_ctor(SymName, _),
        % XXX kind inference: we assume the kind is star.
        Type = defined_type(SymName, ArgTypes, kind_star)
    ).

construct_higher_order_type(Purity, PredOrFunc, EvalMethod, ArgTypes, Type) :-
    (
        PredOrFunc = pf_predicate,
        construct_higher_order_pred_type(Purity, EvalMethod, ArgTypes, Type)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgTypes, FuncArgTypes, FuncRetType),
        construct_higher_order_func_type(Purity, EvalMethod, FuncArgTypes,
            FuncRetType, Type)
    ).

construct_higher_order_pred_type(Purity, EvalMethod, ArgTypes, Type) :-
    Type = higher_order_type(pf_predicate, ArgTypes, none_or_default_func,
        Purity, EvalMethod).

construct_higher_order_pred_type(Purity, EvalMethod, ArgTypes, ArgModes,
        Detism, Type) :-
    PredInstInfo = pred_inst_info(pf_predicate, ArgModes, arg_reg_types_unset,
        Detism),
    Type = higher_order_type(pf_predicate, ArgTypes,
        higher_order(PredInstInfo), Purity, EvalMethod).

construct_higher_order_func_type(Purity, EvalMethod, ArgTypes, RetType,
        Type) :-
    Type = higher_order_type(pf_function, ArgTypes ++ [RetType],
        none_or_default_func, Purity, EvalMethod).

construct_higher_order_func_type(Purity, EvalMethod, ArgTypes, RetType,
        ArgModes, RetMode, Detism, Type) :-
    PredInstInfo = pred_inst_info(pf_function, ArgModes ++ [RetMode],
        arg_reg_types_unset, Detism),
    Type = higher_order_type(pf_function, ArgTypes ++ [RetType],
        higher_order(PredInstInfo), Purity, EvalMethod).

%---------------------------------------------------------------------------%

strip_module_names_from_type(StripWhat, Type0, Type) :-
    (
        ( Type0 = type_variable(_, _)
        ; Type0 = builtin_type(_)
        ),
        Type = Type0
    ;
        Type0 = defined_type(SymName0, ArgTypes0, Kind),
        strip_module_names_from_sym_name(StripWhat, SymName0, SymName),
        strip_module_names_from_type_list(StripWhat, ArgTypes0, ArgTypes),
        Type = defined_type(SymName, ArgTypes, Kind)
    ;
        Type0 = higher_order_type(PorF, ArgTypes0, HOInstInfo0, Purity, EM),
        strip_module_names_from_type_list(StripWhat, ArgTypes0, ArgTypes),
        strip_module_names_from_ho_inst_info(StripWhat,
            HOInstInfo0, HOInstInfo),
        Type = higher_order_type(PorF, ArgTypes, HOInstInfo, Purity, EM)
    ;
        Type0 = tuple_type(ArgTypes0, Kind),
        strip_module_names_from_type_list(StripWhat, ArgTypes0, ArgTypes),
        Type = tuple_type(ArgTypes, Kind)
    ;
        Type0 = apply_n_type(Var, ArgTypes0, Kind),
        strip_module_names_from_type_list(StripWhat, ArgTypes0, ArgTypes),
        Type = apply_n_type(Var, ArgTypes, Kind)
    ;
        Type0 = kinded_type(SubType0, Kind),
        strip_module_names_from_type(StripWhat, SubType0, SubType),
        Type = kinded_type(SubType, Kind)
    ).

strip_module_names_from_type_list(StripWhat, Types0, Types) :-
    list.map(strip_module_names_from_type(StripWhat), Types0, Types).

%---------------------------------------------------------------------------%

prog_constraints_get_tvars(constraints(Univ, Exist), TVars) :-
    constraint_list_get_tvars(Univ, UnivTVars),
    constraint_list_get_tvars(Exist, ExistTVars),
    list.append(UnivTVars, ExistTVars, TVars).

constraint_list_get_tvars(Constraints, TVars) :-
    list.map(constraint_get_tvars, Constraints, TVarsList),
    list.condense(TVarsList, TVars).

constraint_get_tvars(constraint(_ClassName, ArgTypes), TVars) :-
    type_vars_in_types(ArgTypes, TVars).

get_unconstrained_tvars(Tvars, Constraints, Unconstrained) :-
    constraint_list_get_tvars(Constraints, ConstrainedTvars),
    list.delete_elems(Tvars, ConstrainedTvars, Unconstrained0),
    list.remove_dups(Unconstrained0, Unconstrained).

%---------------------------------------------------------------------------%

builtin_type_ctors_with_no_hlds_type_defn =
    % Every element of this list must be reflected in the code of
    % builtin_type_ctor in type_ctor_info.m.
    [ type_ctor(qualified(mercury_public_builtin_module, "int"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "int8"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "int16"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "int32"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "int64"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "uint"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "uint8"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "uint16"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "uint32"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "uint64"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "string"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "character"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "float"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "pred"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "func"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "void"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "tuple"), 0)
    ].

%---------------------------------------------------------------------------%

is_type_ctor_a_builtin_dummy(TypeCtor) = IsBuiltinDummy :-
    % Please keep the set of type_ctors for which we return
    % is_builtin_dummy_type_ctor in sync with classify_type_ctor_if_special.
    TypeCtor = type_ctor(CtorSymName, TypeArity),
    (
        CtorSymName = qualified(ModuleName, TypeName),
        ( if
            (
                TypeName = "state",
                TypeArity = 0,
                ModuleName = mercury_io_module
            ;
                TypeName = "store",
                TypeArity = 1,
                ModuleName = mercury_std_lib_module_name(unqualified("store"))
            )
        then
            IsBuiltinDummy = is_builtin_dummy_type_ctor
        else if
            (
                TypeName = "store_at_ref_type",
                TypeArity = 1,
                ModuleName = mercury_private_builtin_module
            ;
                TypeName = "comparison_result",
                TypeArity = 0,
                ModuleName = mercury_public_builtin_module
            )
        then
            IsBuiltinDummy = is_builtin_non_dummy_type_ctor
        else
            IsBuiltinDummy = is_not_builtin_dummy_type_ctor
        )
    ;
        CtorSymName = unqualified(_TypeName),
        IsBuiltinDummy = is_not_builtin_dummy_type_ctor
    ).

type_is_io_state(Type) :-
    type_to_ctor_and_args(Type, TypeCtor, []),
    ModuleName = mercury_io_module,
    TypeCtor = type_ctor(qualified(ModuleName, "state"), 0).

type_ctor_is_array(type_ctor(qualified(unqualified("array"), "array"), 1)).

type_ctor_is_bitmap(type_ctor(qualified(unqualified("bitmap"), "bitmap"), 0)).

is_introduced_type_info_type(Type) :-
    type_to_ctor(Type, TypeCtor),
    is_introduced_type_info_type_ctor(TypeCtor).

is_introduced_type_info_type_ctor(TypeCtor) :-
    TypeCtor = type_ctor(qualified(PrivateBuiltin, Name), 0),
    PrivateBuiltin = mercury_private_builtin_module,
    ( Name = "type_info"
    ; Name = "type_ctor_info"
    ; Name = "typeclass_info"
    ; Name = "base_typeclass_info"
    ).

is_introduced_type_info_type_category(TypeCtorCat) = IsIntroduced :-
    (
        ( TypeCtorCat = ctor_cat_builtin(_)
        ; TypeCtorCat = ctor_cat_higher_order
        ; TypeCtorCat = ctor_cat_tuple
        ; TypeCtorCat = ctor_cat_enum(_)
        ; TypeCtorCat = ctor_cat_builtin_dummy
        ; TypeCtorCat = ctor_cat_variable
        ; TypeCtorCat = ctor_cat_void
        ; TypeCtorCat = ctor_cat_user(_)
        ),
        IsIntroduced = no
    ;
        TypeCtorCat = ctor_cat_system(_),
        IsIntroduced = yes
    ).

%---------------------------------------------------------------------------%

remove_new_prefix(unqualified(Name0), unqualified(Name)) :-
    string.append("new ", Name, Name0).
remove_new_prefix(qualified(Module, Name0), qualified(Module, Name)) :-
    string.append("new ", Name, Name0).

add_new_prefix(unqualified(Name0), unqualified(Name)) :-
    string.append("new ", Name0, Name).
add_new_prefix(qualified(Module, Name0), qualified(Module, Name)) :-
    string.append("new ", Name0, Name).

%---------------------------------------------------------------------------%

cell_cons_id(type_info_cell(Ctor)) = type_info_cell_constructor(Ctor).
cell_cons_id(typeclass_info_cell) = typeclass_info_cell_constructor.

cell_inst_cons_id(Which, Arity) = InstConsId :-
    % Neither of these function symbols exist, even with fake arity,
    % but they do not need to.
    (
        Which = type_info_cell(_),
        Symbol = "type_info"
    ;
        Which = typeclass_info_cell,
        Symbol = "typeclass_info"
    ),
    PrivateBuiltin = mercury_private_builtin_module,
    TypeCtor = cons_id_dummy_type_ctor,
    InstConsId = cons(qualified(PrivateBuiltin, Symbol), Arity, TypeCtor).

%---------------------------------------------------------------------------%

qualify_cons_id(Args, ConsId0, ConsId, InstConsId) :-
    (
        ConsId0 = cons(Name0, OrigArity, TypeCtor),
        ( if TypeCtor = type_ctor(qualified(TypeModule, _), _) then
            UnqualName = unqualify_name(Name0),
            Name = qualified(TypeModule, UnqualName),
            ConsId = cons(Name, OrigArity, TypeCtor)
        else
            ConsId = ConsId0
        ),
        InstConsId = ConsId
    ;
        ConsId0 = type_info_cell_constructor(CellCtor),
        ConsId = ConsId0,
        InstConsId = cell_inst_cons_id(type_info_cell(CellCtor),
            list.length(Args))
    ;
        ConsId0 = typeclass_info_cell_constructor,
        ConsId = ConsId0,
        InstConsId = cell_inst_cons_id(typeclass_info_cell, list.length(Args))
    ;
        ( ConsId0 = tuple_cons(_)
        ; ConsId0 = closure_cons(_, _)
        ; ConsId0 = some_int_const(_)
        ; ConsId0 = float_const(_)
        ; ConsId0 = char_const(_)
        ; ConsId0 = string_const(_)
        ; ConsId0 = impl_defined_const(_)
        ; ConsId0 = type_ctor_info_const(_, _, _)
        ; ConsId0 = base_typeclass_info_const(_, _, _, _)
        ; ConsId0 = type_info_const(_)
        ; ConsId0 = typeclass_info_const(_)
        ; ConsId0 = ground_term_const(_, _)
        ; ConsId0 = table_io_entry_desc(_)
        ; ConsId0 = tabling_info_const(_)
        ; ConsId0 = deep_profiling_proc_layout(_)
        ),
        ConsId = ConsId0,
        InstConsId = ConsId
    ).

%---------------------------------------------------------------------------%

type_constructors_are_type_info(Ctors) :-
    Ctors = [Ctor],
    Ctor = ctor(_Ordinal, MaybeExistConstraints, FunctorName,
        [_CtorArg], 1, _Context),
    unqualify_private_builtin(FunctorName, Name),
    name_is_type_info(Name),
    MaybeExistConstraints = no_exist_constraints.

    % If the sym_name is in the private_builtin module, unqualify it,
    % otherwise fail. All, user-defined types should be module-qualified
    % by the time this predicate is called, so we assume that any unqualified
    % names are in private_builtin.
    %
:- pred unqualify_private_builtin(sym_name::in, string::out) is semidet.

unqualify_private_builtin(unqualified(Name), Name).
unqualify_private_builtin(qualified(ModuleName, Name), Name) :-
    ModuleName = mercury_private_builtin_module.

:- pred name_is_type_info(string::in) is semidet.

name_is_type_info("type_info").
name_is_type_info("type_ctor_info").
name_is_type_info("typeclass_info").
name_is_type_info("base_typeclass_info").

%---------------------%

non_sub_du_type_is_notag(OoMCtors, MaybeCanonical) :-
    OoMCtors = one_or_more(Ctor, []),
    Ctor = ctor(_Ordinal, MaybeExistConstraints, _FunctorName, [_CtorArg], 1,
        _Context),
    MaybeExistConstraints = no_exist_constraints,
    MaybeCanonical = canon.

non_sub_du_type_is_enum(DuDetails, NumFunctors) :-
    DuDetails = type_details_du(OoMCtors, _MaybeCanon, _MaybeDirectArgCtors),
    Ctors = one_or_more_to_list(OoMCtors),
    Ctors = [_, _ | _],
    all_functors_are_constants(Ctors, 0, NumFunctors).

num_bits_needed_for_n_dense_values(NumValues, NumBits) :-
    int.log2(NumValues, NumBits).

:- pred all_functors_are_constants(list(constructor)::in,
    int::in, int::out) is semidet.

all_functors_are_constants([], !NumFunctors).
all_functors_are_constants([Ctor | Ctors], !NumFunctors) :-
    Ctor = ctor(_Ordinal, MaybeExistConstraints, _Name, ArgTypes, _Arity,
        _Context),
    ArgTypes = [],
    MaybeExistConstraints = no_exist_constraints,
    !:NumFunctors = !.NumFunctors + 1,
    all_functors_are_constants(Ctors, !NumFunctors).

non_sub_du_type_is_dummy(DuDetails) :-
    DuDetails = type_details_du(Ctors, MaybeCanonical, MaybeDirectArgCtors),
    Ctors = one_or_more(Ctor, []),
    Ctor = ctor(_Ordinal, MaybeExistConstraints, _FunctorName, [], 0,
        _Context),
    MaybeExistConstraints = no_exist_constraints,
    MaybeCanonical = canon,
    MaybeDirectArgCtors = no.

%---------------------------------------------------------------------------%

type_unify(X, Y, HeadTypeParams, !Bindings) :-
    ( if X = type_variable(VarX, _) then
        type_unify_var(VarX, Y, HeadTypeParams, !Bindings)
    else if Y = type_variable(VarY, _) then
        type_unify_var(VarY, X, HeadTypeParams, !Bindings)
    else if type_unify_nonvar(X, Y, HeadTypeParams, !Bindings) then
        true
    else
        % Some special cases are not handled above. We handle them separately
        % here.
        type_unify_special(X, Y, HeadTypeParams, !Bindings)
    ).

:- pred type_unify_var(tvar::in, mer_type::in, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_var(VarX, TypeY, HeadTypeParams, !Bindings) :-
    ( if TypeY = type_variable(VarY, KindY) then
        type_unify_var_var(VarX, VarY, KindY, HeadTypeParams, !Bindings)
    else if map.search(!.Bindings, VarX, BindingOfX) then
        % VarX has a binding. Y is not a variable.
        type_unify(BindingOfX, TypeY, HeadTypeParams, !Bindings)
    else
        % VarX has no binding, so bind it to TypeY.
        not type_occurs(TypeY, VarX, !.Bindings),
        not list.member(VarX, HeadTypeParams),
        map.det_insert(VarX, TypeY, !Bindings)
    ).

:- pred type_unify_var_var(tvar::in, tvar::in, kind::in, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_var_var(X, Y, Kind, HeadTypeParams, !Bindings) :-
    ( if list.member(Y, HeadTypeParams) then
        type_unify_head_type_param(X, Y, Kind, HeadTypeParams, !Bindings)
    else if list.member(X, HeadTypeParams) then
        type_unify_head_type_param(Y, X, Kind, HeadTypeParams, !Bindings)
    else if map.search(!.Bindings, X, BindingOfX) then
        ( if map.search(!.Bindings, Y, BindingOfY) then
            % Both X and Y already have bindings - just unify the
            % types they are bound to.
            type_unify(BindingOfX, BindingOfY, HeadTypeParams, !Bindings)
        else
            % Y hasn't been bound yet.
            apply_rec_subst_to_type(!.Bindings, BindingOfX, SubstBindingOfX),
            ( if SubstBindingOfX = type_variable(Y, _) then
                true
            else
                not type_occurs(SubstBindingOfX, Y, !.Bindings),
                map.det_insert(Y, SubstBindingOfX, !Bindings)
            )
        )
    else
        % Neither X nor Y is a head type param. X had not been bound yet.
        ( if map.search(!.Bindings, Y, BindingOfY) then
            apply_rec_subst_to_type(!.Bindings, BindingOfY, SubstBindingOfY),
            ( if SubstBindingOfY = type_variable(X, _) then
                true
            else
                not type_occurs(SubstBindingOfY, X, !.Bindings),
                map.det_insert(X, SubstBindingOfY, !Bindings)
            )
        else
            % Both X and Y are unbound type variables - bind one to the other.
            ( if X = Y then
                true
            else
                map.det_insert(X, type_variable(Y, Kind), !Bindings)
            )
        )
    ).

:- pred type_unify_head_type_param(tvar::in, tvar::in, kind::in,
    list(tvar)::in, tsubst::in, tsubst::out) is semidet.

type_unify_head_type_param(Var, HeadVar, Kind, HeadTypeParams, !Bindings) :-
    ( if map.search(!.Bindings, Var, BindingOfVar) then
        BindingOfVar = type_variable(Var2, _),
        type_unify_head_type_param(Var2, HeadVar, Kind, HeadTypeParams,
            !Bindings)
    else
        ( if Var = HeadVar then
            true
        else
            not list.member(Var, HeadTypeParams),
            map.det_insert(Var, type_variable(HeadVar, Kind), !Bindings)
        )
    ).

    % Unify two types, neither of which are variables. Two special cases
    % which are not handled here are apply_n types and kinded types.
    % Those are handled below.
    %
:- pred type_unify_nonvar(mer_type::in, mer_type::in, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_nonvar(TypeX, TypeY, HeadTypeParams, !Bindings) :-
    (
        TypeX = defined_type(SymName, ArgTypesX, _),
        TypeY = defined_type(SymName, ArgTypesY, _),
        % Instead of insisting that the names are equal and the arg lists
        % unify, we should consider attempting to expand equivalence types
        % first. That would require the type table to be passed in to the
        % unification algorithm, though.
        type_unify_list(ArgTypesX, ArgTypesY, HeadTypeParams, !Bindings)
    ;
        TypeX = builtin_type(BuiltinType),
        TypeY = builtin_type(BuiltinType)
    ;
        TypeX = higher_order_type(PorF, ArgTypesX, _, Purity, EvalMethod),
        TypeY = higher_order_type(PorF, ArgTypesY, _, Purity, EvalMethod),
        type_unify_list(ArgTypesX, ArgTypesY, HeadTypeParams, !Bindings)
    ;
        TypeX = tuple_type(ArgTypesX, _),
        TypeY = tuple_type(ArgTypesY, _),
        type_unify_list(ArgTypesX, ArgTypesY, HeadTypeParams, !Bindings)
    ).

    % Handle apply_n types and kinded types.
    %
:- pred type_unify_special(mer_type::in, mer_type::in, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_special(TypeX, TypeY, HeadTypeParams, !Bindings) :-
    ( if TypeX = apply_n_type(VarX, ArgTypesX, _) then
        type_unify_apply(TypeY, VarX, ArgTypesX, HeadTypeParams, !Bindings)
    else if TypeY = apply_n_type(VarY, ArgTypesY, _) then
        type_unify_apply(TypeX, VarY, ArgTypesY, HeadTypeParams, !Bindings)
    else if TypeX = kinded_type(RawX, _) then
        ( if TypeY = kinded_type(RawY, _) then
            type_unify(RawX, RawY, HeadTypeParams, !Bindings)
        else
            type_unify(RawX, TypeY, HeadTypeParams, !Bindings)
        )
    else if TypeY = kinded_type(RawY, _) then
        type_unify(TypeX, RawY, HeadTypeParams, !Bindings)
    else
        fail
    ).

    % The idea here is that we try to strip off arguments from Y starting
    % from the end and unify each with the corresponding argument of X.
    % If we reach an atomic type before the arguments run out, we fail.
    % If we reach a variable before the arguments run out, we unify it
    % with what remains of the apply_n expression. If we manage to unify
    % all of the arguments, we unify the apply_n variable with what remains
    % of the other expression.
    %
    % Note that Y is not a variable, since that case would have been caught
    % by type_unify.
    %
:- pred type_unify_apply(mer_type::in, tvar::in, list(mer_type)::in,
    list(tvar)::in, tsubst::in, tsubst::out) is semidet.

type_unify_apply(TypeY, VarX, ArgTypesX0, HeadTypeParams, !Bindings) :-
    (
        TypeY = defined_type(NameY, ArgTypesY0, KindY0),
        type_unify_args(ArgTypesX0, ArgTypesY0, ArgTypesY, KindY0, KindY,
            HeadTypeParams, !Bindings),
        type_unify_var(VarX, defined_type(NameY, ArgTypesY, KindY),
            HeadTypeParams, !Bindings)
    ;
        TypeY = builtin_type(_),
        ArgTypesX0 = [],
        type_unify_var(VarX, TypeY, HeadTypeParams, !Bindings)
    ;
        TypeY = higher_order_type(_, _, _, _, _),
        ArgTypesX0 = [],
        type_unify_var(VarX, TypeY, HeadTypeParams, !Bindings)
    ;
        TypeY = tuple_type(ArgTypesY0, KindY0),
        type_unify_args(ArgTypesX0, ArgTypesY0, ArgTypesY, KindY0, KindY,
            HeadTypeParams, !Bindings),
        type_unify_var(VarX, tuple_type(ArgTypesY, KindY), HeadTypeParams,
            !Bindings)
    ;
        TypeY = apply_n_type(VarY, ArgTypesY0, Kind0),
        list.length(ArgTypesX0, NArgTypesX0),
        list.length(ArgTypesY0, NArgTypesY0),
        compare(Result, NArgTypesX0, NArgTypesY0),
        (
            Result = (<),
            type_unify_args(ArgTypesX0, ArgTypesY0, ArgTypesY, Kind0, Kind,
                HeadTypeParams, !Bindings),
            type_unify_var(VarX, apply_n_type(VarY, ArgTypesY, Kind),
                HeadTypeParams, !Bindings)
        ;
            Result = (=),
            % We know here that the list of remaining args will be empty.
            type_unify_args(ArgTypesX0, ArgTypesY0, _, Kind0, Kind,
                HeadTypeParams, !Bindings),
            type_unify_var_var(VarX, VarY, Kind, HeadTypeParams, !Bindings)
        ;
            Result = (>),
            type_unify_args(ArgTypesY0, ArgTypesX0, ArgTypesX, Kind0, Kind,
                HeadTypeParams, !Bindings),
            type_unify_var(VarY, apply_n_type(VarX, ArgTypesX, Kind),
                HeadTypeParams, !Bindings)
        )
    ;
        TypeY = kinded_type(RawY, _),
        type_unify_apply(RawY, VarX, ArgTypesX0, HeadTypeParams, !Bindings)
    ;
        TypeY = builtin_type(_),
        % XXX I (zs) am not sure *why* it is ok to fail here.
        fail
    ).

:- pred type_unify_args(list(mer_type)::in, list(mer_type)::in,
    list(mer_type)::out, kind::in, kind::out, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_args(ArgTypesX, ArgTypesY0, ArgTypesY,
        KindY0, KindY, HeadTypeParams, !Bindings) :-
    list.reverse(ArgTypesX, RevArgTypesX),
    list.reverse(ArgTypesY0, RevArgTypesY0),
    type_unify_rev_args(RevArgTypesX, RevArgTypesY0, RevArgTypesY,
        KindY0, KindY, HeadTypeParams, !Bindings),
    list.reverse(RevArgTypesY, ArgTypesY).

:- pred type_unify_rev_args(list(mer_type)::in, list(mer_type)::in,
    list(mer_type)::out, kind::in, kind::out, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_rev_args([], ArgTypesY, ArgTypesY, KindY, KindY, _, !Bindings).
type_unify_rev_args([ArgTypeX | ArgTypesX], [ArgTypeY0 | ArgTypesY0],
        ArgTypesY, KindY0, KindY, HeadTypeParams, !Bindings) :-
    type_unify(ArgTypeX, ArgTypeY0, HeadTypeParams, !Bindings),
    KindY1 = kind_arrow(get_type_kind(ArgTypeY0), KindY0),
    type_unify_rev_args(ArgTypesX, ArgTypesY0, ArgTypesY,
        KindY1, KindY, HeadTypeParams, !Bindings).

type_unify_list([], [], _HeadTypeParams, !Bindings).
type_unify_list([X | Xs], [Y | Ys], HeadTypeParams, !Bindings) :-
    type_unify(X, Y, HeadTypeParams, !Bindings),
    type_unify_list(Xs, Ys, HeadTypeParams, !Bindings).

    % type_occurs(Type, Var, Subst) succeeds iff Type contains Var,
    % perhaps indirectly via the substitution. (The variable must not
    % be mapped by the substitution.)
    %
:- pred type_occurs(mer_type::in, tvar::in, tsubst::in) is semidet.

type_occurs(TypeX, Y, Bindings) :-
    require_complete_switch [TypeX]
    (
        TypeX = type_variable(X, _),
        ( if X = Y then
            true
        else
            map.search(Bindings, X, BindingOfX),
            type_occurs(BindingOfX, Y, Bindings)
        )
    ;
        TypeX = defined_type(_, ArgTypes, _),
        type_occurs_list(ArgTypes, Y, Bindings)
    ;
        TypeX = higher_order_type(_, ArgTypes, _, _, _),
        type_occurs_list(ArgTypes, Y, Bindings)
    ;
        TypeX = tuple_type(ArgTypes, _),
        type_occurs_list(ArgTypes, Y, Bindings)
    ;
        TypeX = apply_n_type(X, ArgTypes, _),
        (
            X = Y
        ;
            type_occurs_list(ArgTypes, Y, Bindings)
        ;
            map.search(Bindings, X, BindingOfX),
            type_occurs(BindingOfX, Y, Bindings)
        )
    ;
        TypeX = kinded_type(TypeX1, _),
        type_occurs(TypeX1, Y, Bindings)
    ;
        TypeX = builtin_type(_),
        fail
    ).

:- pred type_occurs_list(list(mer_type)::in, tvar::in, tsubst::in) is semidet.

type_occurs_list([X | Xs], Y,  Bindings) :-
    (
        type_occurs(X, Y, Bindings)
    ;
        type_occurs_list(Xs, Y, Bindings)
    ).

%---------------------------------------------------------------------------%

type_subsumes(TypeA, TypeB, TypeSubst) :-
    % TypeA subsumes TypeB iff TypeA can be unified with TypeB
    % without binding any of the type variables in TypeB.
    type_vars_in_type(TypeB, TypeBVars),
    map.init(TypeSubst0),
    type_unify(TypeA, TypeB, TypeBVars, TypeSubst0, TypeSubst).

type_subsumes_det(TypeA, TypeB, TypeSubst) :-
    ( if type_subsumes(TypeA, TypeB, TypeSubstPrime) then
        TypeSubst = TypeSubstPrime
    else
        unexpected($pred, "type_subsumes failed")
    ).

type_list_subsumes(TypesA, TypesB, TypeSubst) :-
    % TypesA subsumes TypesB iff TypesA can be unified with TypesB
    % without binding any of the type variables in TypesB.
    type_vars_in_types(TypesB, TypesBVars),
    map.init(TypeSubst0),
    type_unify_list(TypesA, TypesB, TypesBVars, TypeSubst0, TypeSubst).

type_list_subsumes_det(TypesA, TypesB, TypeSubst) :-
    ( if type_list_subsumes(TypesA, TypesB, TypeSubstPrime) then
        TypeSubst = TypeSubstPrime
    else
        unexpected($pred, "type_list_subsumes failed")
    ).

arg_type_list_subsumes(TVarSet, ExistQVars, ActualArgTypes, HeadTypeParams,
        CalleeTVarSet, PredKindMap, PredExistQVars, PredArgTypes) :-
    % Rename the type variables in the callee's argument types.
    tvarset_merge_renaming(TVarSet, CalleeTVarSet, _TVarSet1, Renaming),
    apply_variable_renaming_to_tvar_kind_map(Renaming, PredKindMap,
        ParentKindMap),
    apply_variable_renaming_to_type_list(Renaming, PredArgTypes,
        ParentArgTypes),
    apply_variable_renaming_to_tvar_list(Renaming, PredExistQVars,
        ParentExistQVars),

    % Check that the types of the candidate predicate/function
    % subsume the actual argument types.
    % [This is the right thing to do even for calls to
    % existentially typed preds, because we're using the
    % type variables from the callee's pred decl (obtained
    % from the pred_info via pred_info_get_arg_types) not the types
    % inferred from the callee's clauses (and stored in the
    % clauses_info and proc_info) -- the latter
    % might not subsume the actual argument types.]

    (
        ExistQVars = [],
        type_list_subsumes(ParentArgTypes, ActualArgTypes, ParentToActualSubst)
    ;
        ExistQVars = [_ | _],
        % For calls to existentially type preds, we may need to bind
        % type variables in the caller, not just those in the callee.
        type_unify_list(ParentArgTypes, ActualArgTypes, HeadTypeParams,
            map.init, ParentToActualSubst)
    ),

    % Check that the type substitution did not bind any existentially
    % typed variables to non-ground types.
    (
        ParentExistQVars = []
        % Optimize common case.
    ;
        ParentExistQVars = [_ | _],
        apply_rec_subst_to_tvar_list(ParentKindMap, ParentToActualSubst,
            ParentExistQVars, ActualExistQTypes),
        all [T] (
            list.member(T, ActualExistQTypes)
        =>
            T = type_variable(_, _)
        )

        % It might make sense to also check that the type substitution
        % did not bind any existentially typed variables to universally
        % quantified type variables in the caller's argument types.
    ).

%---------------------------------------------------------------------------%

compute_caller_callee_type_substitution(CalleeArgTypes, CallerArgTypes,
        ExternalTypeParams, CalleeExistQVars, TypeSubn) :-
    (
        CalleeExistQVars = [],
        ( if type_list_subsumes(CalleeArgTypes, CallerArgTypes, TypeSubn0) then
            TypeSubn = TypeSubn0
        else
            % The callee's arg types should always be unifiable with the
            % caller's, otherwise there is a type error that should have
            % been detected by typechecking. But polymorphism.m introduces
            % type-incorrect code -- e.g. compare(Res, EnumA, EnumB) gets
            % converted into builtin_compare_int(Res, EnumA, EnumB), which
            % is a type error, since it assumes that an enumeration is an int.
            % In those cases, we don't need to worry about the type
            % substitution. (Perhaps it would be better if polymorphism
            % introduced calls to unsafe_type_cast/2 for such cases.)
            map.init(TypeSubn)
        )
    ;
        CalleeExistQVars = [_ | _],
        % For calls to existentially type preds, we may need to bind
        % type variables in the caller, as well as in the callee.
        ( if
            map.init(TypeSubn0),
            type_unify_list(CalleeArgTypes, CallerArgTypes, ExternalTypeParams,
                TypeSubn0, TypeSubn1)
        then
            TypeSubn = TypeSubn1
        else
            unexpected($pred, "type unification failed")
        )
    ).

%---------------------------------------------------------------------------%

apply_partial_map_to_list(_PartialMap, [], []).
apply_partial_map_to_list(PartialMap, [X | Xs], [Y | Ys]) :-
    ( if map.search(PartialMap, X, Y0) then
        Y = Y0
    else
        Y = X
    ),
    apply_partial_map_to_list(PartialMap, Xs, Ys).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_type.
%---------------------------------------------------------------------------%

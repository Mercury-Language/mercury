%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_type.m.
% Main author: fjh.
%
% Utility predicates dealing with types in the parse tree. The predicates for
% doing type substitutions are in prog_type_subst.m, while utility predicates
% for dealing with types in the HLDS are in type_util.m.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_type.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module term.

%-----------------------------------------------------------------------------%
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

    % Succeed if the given type is a tuple type, returning
    % the argument types.
    %
:- pred type_is_tuple(mer_type::in, list(mer_type)::out) is semidet.

    % Remove the kind annotation at the top-level if there is one,
    % otherwise return the type unchanged.
    %
:- func strip_kind_annotation(mer_type) = mer_type.

%-----------------------------------------------------------------------------%

    % Succeeds iff the given type is ground (that is, contains no type
    % variables).
    %
:- pred type_is_ground(mer_type::in) is semidet.

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

    % type_has_variable_arity_ctor(Type, TypeCtor, TypeArgs):
    %
    % Check if the principal type constructor of Type is of variable arity.
    % If yes, return the type constructor as TypeCtor and its args as
    % TypeArgs. If not, fail.
    %
:- pred type_has_variable_arity_ctor(mer_type::in, type_ctor::out,
    list(mer_type)::out) is semidet.

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

    % type_ctor_is_higher_order(TypeCtor, PredOrFunc) succeeds iff
    % TypeCtor is a higher-order predicate or function type.
    %
:- pred type_ctor_is_higher_order(type_ctor::in, purity::out,
    pred_or_func::out, lambda_eval_method::out) is semidet.

    % type_ctor_is_tuple(TypeCtor) succeeds iff TypeCtor is a tuple type.
    %
:- pred type_ctor_is_tuple(type_ctor::in) is semidet.

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

    % Return a list of the type variables of a type, in order of their
    % first occurrence in a depth-first, left-right traversal.
    %
:- pred type_vars(mer_type::in, list(tvar)::out) is det.

    % Return a list of the type variables of a list of types, in order
    % of their first occurrence in a depth-first, left-right traversal.
    %
:- pred type_vars_list(list(mer_type)::in, list(tvar)::out) is det.

    % Nondeterministically return the variables in a type.
    %
:- pred type_contains_var(mer_type::in, tvar::out) is nondet.

    % Nondeterministically return the variables in a list of types.
    %
:- pred type_list_contains_var(list(mer_type)::in, tvar::out) is nondet.

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

    % Make error messages more readable by removing "builtin."
    % qualifiers.
    %
:- pred strip_builtin_qualifiers_from_type(mer_type::in, mer_type::out) is det.

:- pred strip_builtin_qualifiers_from_type_list(list(mer_type)::in,
    list(mer_type)::out) is det.

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

%-----------------------------------------------------------------------------%

    % The list of type_ctors which are builtins which do not have a
    % hlds_type_defn.
    %
:- func builtin_type_ctors_with_no_hlds_type_defn = list(type_ctor).

:- type is_builtin_dummy_type_ctor
    --->    is_builtin_dummy_type_ctor
    ;       is_not_builtin_dummy_type_ctor.

    % is_builtin_dummy_type_ctor(type_ctor):
    %
    % Is the given type constructor a dummy type irrespective
    % of its definition?
    %
:- func check_builtin_dummy_type_ctor(type_ctor) = is_builtin_dummy_type_ctor.

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

:- type type_ctor_category
    --->    ctor_cat_builtin(type_ctor_cat_builtin)
    ;       ctor_cat_higher_order
    ;       ctor_cat_tuple
    ;       ctor_cat_enum(type_ctor_cat_enum)
    ;       ctor_cat_builtin_dummy
    ;       ctor_cat_variable
    ;       ctor_cat_system(type_ctor_cat_system)
    ;       ctor_cat_void
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
    ;       cat_enum_foreign.

:- type type_ctor_cat_user
    --->    cat_user_direct_dummy
    ;       cat_user_notag
    ;       cat_user_general.

    % Given a constant and an arity, return a type_ctor.
    % Fails if the constant is not an atom.
    %
    % This really ought to take a name and an arity -
    % use of integers/floats/strings as type names should be rejected
    % by the parser, not by module_qual.m.
    %
:- pred make_type_ctor(const::in, int::in, type_ctor::out) is semidet.

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

    % This type is used to return information about a constructor definition,
    % extracted from the hlds_type_defn and hlds_cons_defn data types.
    %
:- type ctor_defn
    --->    ctor_defn(
                ctor_tvars          :: tvarset,
                ctor_existq_tvars   :: existq_tvars,
                ctor_tvar_kinds     :: tvar_kind_map,
                                    % kinds of existq_tvars
                ctor_constraints    :: list(prog_constraint),
                                    % existential constraints
                ctor_arg_types      :: list(mer_type),
                                    % functor argument types
                ctor_result_type    :: mer_type
                                    % functor result type
            ).

    % Given a list of constructors for a type, check whether that type
    % is a private_builtin.type_info/0 or similar type.
    %
:- pred type_constructors_are_type_info(list(constructor)::in) is semidet.

    % Is the discriminated union type with the given list of constructors
    % an enum? Is yes, return the number of bits required to represent it.
    %
:- pred du_type_is_enum(list(constructor)::in, int::out) is semidet.

    % type_ctor_should_be_notag(Globals, TypeCtor, ReservedTag, Ctors,
    %   MaybeUserEqComp, SingleFunctorName, SingleArgType, MaybeSingleArgName):
    %
    % Succeed if the type constructor with the given name (TypeCtor) and
    % details (ReservedTag, Ctors, MaybeUserEqComp) is a no_tag type. If it is,
    % return the name of its single function symbol, the type of its one
    % argument, and its name (if any).
    %
:- pred type_ctor_should_be_notag(globals::in, type_ctor::in,
    uses_reserved_tag::in, list(constructor)::in, maybe(unify_compare)::in,
    sym_name::out, mer_type::out, maybe(string)::out) is semidet.

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

    % Apply a renaming (partial map) to a list.
    % Useful for applying a variable renaming to a list of variables.
    %
:- pred apply_partial_map_to_list(map(T, T)::in, list(T)::in, list(T)::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.prog_type_subst.

:- import_module int.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

type_is_var(Type) :-
    strip_kind_annotation(Type) = type_variable(_, _).

type_is_nonvar(Type) :-
    not type_is_var(Type).

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
        unexpected($module, $pred, "type is not higher-order")
    ).

type_is_tuple(Type, ArgTypes) :-
    strip_kind_annotation(Type) = tuple_type(ArgTypes, _).

strip_kind_annotation(Type0) = Type :-
    ( if Type0 = kinded_type(Type1, _) then
        Type = strip_kind_annotation(Type1)
    else
        Type = Type0
    ).

%-----------------------------------------------------------------------------%

type_is_ground(Type) :-
    not type_contains_var(Type, _).

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

type_has_variable_arity_ctor(Type, TypeCtor, TypeArgs) :-
    ( if
        type_is_higher_order_details(Type, _Purity, PredOrFunc, _, TypeArgs0)
    then
        TypeArgs = TypeArgs0,
        PredOrFuncStr = prog_out.pred_or_func_to_str(PredOrFunc),
        TypeCtor = type_ctor(unqualified(PredOrFuncStr), 0)
    else if
        type_is_tuple(Type, TypeArgs1)
    then
        TypeArgs = TypeArgs1,
        % XXX why tuple/0 and not {}/N ?
        TypeCtor = type_ctor(unqualified("tuple"), 0)
    else
        fail
    ).

type_to_ctor_and_args(Type, TypeCtor, Args) :-
    (
        Type = defined_type(SymName, Args, _),
        Arity = list.length(Args),
        TypeCtor = type_ctor(SymName, Arity)
    ;
        Type = builtin_type(BuiltinType),
        builtin_type_to_string(BuiltinType, Name),
        SymName = unqualified(Name),
        Arity = 0,
        Args = [],
        TypeCtor = type_ctor(SymName, Arity)
    ;
        Type = higher_order_type(PorF, Args, _HOInstInfo, Purity, _EvalMethod),
        (
            PorF = pf_predicate,
            PorFStr = "func",
            Arity = list.length(Args)
        ;
            PorF = pf_function,
            PorFStr = "pred",
            Arity = list.length(Args) - 1
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
        TypeCtor = type_ctor(SymName, Arity)
    ;
        Type = tuple_type(Args, _),
        SymName = unqualified("{}"),
        Arity = list.length(Args),
        TypeCtor = type_ctor(SymName, Arity)
    ;
        Type = apply_n_type(_, _, _),
        sorry($module, $pred, "apply/N types")
    ;
        Type = kinded_type(SubType, _),
        type_to_ctor_and_args(SubType, TypeCtor, Args)
    ).

type_to_ctor_and_args_det(Type, TypeCtor, Args) :-
    ( if type_to_ctor_and_args(Type, TypeCtorPrime, ArgsPrime) then
        TypeCtor = TypeCtorPrime,
        Args = ArgsPrime
    else
        unexpected($module, $pred,
            "type_to_ctor_and_args failed: " ++ string(Type))
    ).

type_to_ctor(Type, TypeCtor) :-
    % This should be subject to unused argument elimination.
    type_to_ctor_and_args(Type, TypeCtor, _Args).

type_to_ctor_det(Type, TypeCtor) :-
    % This should be subject to unused argument elimination.
    type_to_ctor_and_args_det(Type, TypeCtor, _Args).

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

type_vars(Type, TVars) :-
    type_vars_2(Type, [], RevTVars),
    list.reverse(RevTVars, TVarsDups),
    list.remove_dups(TVarsDups, TVars).

:- pred type_vars_2(mer_type::in, list(tvar)::in, list(tvar)::out) is det.

type_vars_2(type_variable(Var, _), Vs, [Var | Vs]).
type_vars_2(defined_type(_, Args, _), !V) :-
    type_vars_list_2(Args, !V).
type_vars_2(builtin_type(_), !V).
type_vars_2(higher_order_type(_, Args, _, _, _), !V) :-
    type_vars_list_2(Args, !V).
type_vars_2(tuple_type(Args, _), !V) :-
    type_vars_list_2(Args, !V).
type_vars_2(apply_n_type(Var, Args, _), !V) :-
    !:V = [Var | !.V],
    type_vars_list_2(Args, !V).
type_vars_2(kinded_type(Type, _), !V) :-
    type_vars_2(Type, !V).

type_vars_list(Types, TVars) :-
    type_vars_list_2(Types, [], RevTVars),
    list.reverse(RevTVars, TVarsDups),
    list.remove_dups(TVarsDups, TVars).

:- pred type_vars_list_2(list(mer_type)::in, list(tvar)::in, list(tvar)::out)
    is det.

type_vars_list_2([], !V).
type_vars_list_2([Type | Types], !V) :-
    type_vars_2(Type, !V),
    type_vars_list_2(Types, !V).

type_contains_var(type_variable(Var, _), Var).
type_contains_var(defined_type(_, Args, _), Var) :-
    type_list_contains_var(Args, Var).
type_contains_var(higher_order_type(_, Args, _, _, _), Var) :-
    type_list_contains_var(Args, Var).
type_contains_var(tuple_type(Args, _), Var) :-
    type_list_contains_var(Args, Var).
type_contains_var(apply_n_type(Var, _, _), Var).
type_contains_var(apply_n_type(_, Args, _), Var) :-
    type_list_contains_var(Args, Var).
type_contains_var(kinded_type(Type, _), Var) :-
    type_contains_var(Type, Var).

type_list_contains_var([Type | _], Var) :-
    type_contains_var(Type, Var).
type_list_contains_var([_ | Types], Var) :-
    type_list_contains_var(Types, Var).

construct_type(TypeCtor, Args, Type) :-
    ( if
        TypeCtor = type_ctor(unqualified(Name), 0),
        builtin_type_to_string(BuiltinType, Name)
    then
        Type = builtin_type(BuiltinType)
    else if
        type_ctor_is_higher_order(TypeCtor, Purity, PredOrFunc, EvalMethod)
    then
        construct_higher_order_type(Purity, PredOrFunc, EvalMethod, Args, Type)
    else if
        type_ctor_is_tuple(TypeCtor)
    then
        % XXX kind inference: we assume the kind is star.
        Type = tuple_type(Args, kind_star)
    else
        TypeCtor = type_ctor(SymName, _),
        % XXX kind inference: we assume the kind is star.
        Type = defined_type(SymName, Args, kind_star)
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
    Type = higher_order_type(pf_predicate, ArgTypes, higher_order(PredInstInfo),
        Purity, EvalMethod).

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

strip_builtin_qualifiers_from_type(type_variable(Var, Kind),
        type_variable(Var, Kind)).
strip_builtin_qualifiers_from_type(defined_type(Name0, Args0, Kind),
        defined_type(Name, Args, Kind)) :-
    ( if
        Name0 = qualified(Module, Name1),
        Module = mercury_public_builtin_module
    then
        Name = unqualified(Name1)
    else
        Name = Name0
    ),
    strip_builtin_qualifiers_from_type_list(Args0, Args).
strip_builtin_qualifiers_from_type(builtin_type(BuiltinType),
        builtin_type(BuiltinType)).
strip_builtin_qualifiers_from_type(
        higher_order_type(PorF, Args0, HOInstInfo, Purity, EvalMethod),
        higher_order_type(PorF, Args, HOInstInfo, Purity, EvalMethod)) :-
    strip_builtin_qualifiers_from_type_list(Args0, Args).
strip_builtin_qualifiers_from_type(tuple_type(Args0, Kind),
        tuple_type(Args, Kind)) :-
    strip_builtin_qualifiers_from_type_list(Args0, Args).
strip_builtin_qualifiers_from_type(apply_n_type(Var, Args0, Kind),
        apply_n_type(Var, Args, Kind)) :-
    strip_builtin_qualifiers_from_type_list(Args0, Args).
strip_builtin_qualifiers_from_type(kinded_type(Type0, Kind),
        kinded_type(Type, Kind)) :-
    strip_builtin_qualifiers_from_type(Type0, Type).

strip_builtin_qualifiers_from_type_list(Types0, Types) :-
    list.map(strip_builtin_qualifiers_from_type, Types0, Types).

%-----------------------------------------------------------------------------%

prog_constraints_get_tvars(constraints(Univ, Exist), TVars) :-
    constraint_list_get_tvars(Univ, UnivTVars),
    constraint_list_get_tvars(Exist, ExistTVars),
    list.append(UnivTVars, ExistTVars, TVars).

constraint_list_get_tvars(Constraints, TVars) :-
    list.map(constraint_get_tvars, Constraints, TVarsList),
    list.condense(TVarsList, TVars).

constraint_get_tvars(constraint(_ClassName, ArgTypes), TVars) :-
    type_vars_list(ArgTypes, TVars).

get_unconstrained_tvars(Tvars, Constraints, Unconstrained) :-
    constraint_list_get_tvars(Constraints, ConstrainedTvars),
    list.delete_elems(Tvars, ConstrainedTvars, Unconstrained0),
    list.remove_dups(Unconstrained0, Unconstrained).

%-----------------------------------------------------------------------------%

    % Every element of this list must be reflected in the code of
    % builtin_type_ctor in type_ctor_info.m.
builtin_type_ctors_with_no_hlds_type_defn =
    [ type_ctor(qualified(mercury_public_builtin_module, "int"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "uint"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "int8"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "uint8"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "int16"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "uint16"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "int32"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "uint32"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "string"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "character"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "float"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "pred"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "func"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "void"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "tuple"), 0)
    ].

check_builtin_dummy_type_ctor(TypeCtor) = IsBuiltinDummy :-
    % Please keep this code in sync with classify_type_ctor_if_special.
    TypeCtor = type_ctor(CtorSymName, TypeArity),
    ( if
        CtorSymName = qualified(ModuleName, TypeName),
        ModuleName = mercury_io_module,
        TypeName = "state",
        TypeArity = 0
    then
        IsBuiltinDummy = is_builtin_dummy_type_ctor
    else if
        CtorSymName = qualified(ModuleName, TypeName),
        ModuleName = mercury_std_lib_module_name(unqualified("store")),
        TypeName = "store",
        TypeArity = 1
    then
        IsBuiltinDummy = is_builtin_dummy_type_ctor
    else
        IsBuiltinDummy = is_not_builtin_dummy_type_ctor
    ).

type_is_io_state(Type) :-
    type_to_ctor_and_args(Type, TypeCtor, []),
    ModuleName = mercury_io_module,
    TypeCtor = type_ctor(qualified(ModuleName, "state"), 0).

type_ctor_is_array(type_ctor(qualified(unqualified("array"), "array"), 1)).

type_ctor_is_bitmap(
        type_ctor(qualified(unqualified("bitmap"), "bitmap"), 0)).

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

%-----------------------------------------------------------------------------%

remove_new_prefix(unqualified(Name0), unqualified(Name)) :-
    string.append("new ", Name, Name0).
remove_new_prefix(qualified(Module, Name0), qualified(Module, Name)) :-
    string.append("new ", Name, Name0).

add_new_prefix(unqualified(Name0), unqualified(Name)) :-
    string.append("new ", Name0, Name).
add_new_prefix(qualified(Module, Name0), qualified(Module, Name)) :-
    string.append("new ", Name0, Name).

%-----------------------------------------------------------------------------%

make_type_ctor(term.atom(Name), Arity, type_ctor(unqualified(Name), Arity)).

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

qualify_cons_id(Args, ConsId0, ConsId, InstConsId) :-
    (
        ConsId0 = cons(Name0, OrigArity, TypeCtor),
        ( if TypeCtor = type_ctor(qualified(TypeModule, _), _) then
            UnqualName = unqualify_name(Name0),
            Name = qualified(TypeModule, UnqualName),
            ConsId = cons(Name, OrigArity, TypeCtor),
            InstConsId = cons(Name, OrigArity, cons_id_dummy_type_ctor)
        else
            ConsId = ConsId0,
            InstConsId = cons(Name0, OrigArity, cons_id_dummy_type_ctor)
        )
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
        ; ConsId0 = int_const(_)
        ; ConsId0 = uint_const(_)
        ; ConsId0 = int8_const(_)
        ; ConsId0 = uint8_const(_)
        ; ConsId0 = int16_const(_)
        ; ConsId0 = uint16_const(_)
        ; ConsId0 = int32_const(_)
        ; ConsId0 = uint32_const(_)
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

%-----------------------------------------------------------------------------%

type_constructors_are_type_info(Ctors) :-
    type_is_single_ctor_single_arg(Ctors, Ctor, _, _),
    ctor_is_type_info(Ctor).

:- pred type_is_single_ctor_single_arg(list(constructor)::in, sym_name::out,
    mer_type::out, maybe(ctor_field_name)::out) is semidet.

type_is_single_ctor_single_arg(Ctors, Ctor, ArgType, MaybeArgName) :-
    Ctors = [SingleCtor],
    SingleCtor = ctor(ExistQVars, _Constraints, Ctor,
        [ctor_arg(MaybeArgName, ArgType, _, _)], 1, _Ctxt),
    ExistQVars = [].

:- pred ctor_is_type_info(sym_name::in) is semidet.

ctor_is_type_info(Ctor) :-
    unqualify_private_builtin(Ctor, Name),
    name_is_type_info(Name).

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

%-----------------------------------------------------------------------------%

du_type_is_enum(Ctors, NumBits) :-
    Ctors = [_, _ | _],
    all [Ctor] (
        list.member(Ctor, Ctors)
    => (
        Ctor = ctor(ExistQTVars, ExistConstraints, _Name, Args, _Arity,
            _Context),
        ExistQTVars = [],
        ExistConstraints = [],
        Args = []
    )),
    list.length(Ctors, NumFunctors),
    int.log2(NumFunctors, NumBits).

%-----------------------------------------------------------------------------%

type_ctor_should_be_notag(Globals, _TypeCtor, ReserveTagPragma, Ctors,
        MaybeUserEqCmp, SingleFunctorName, SingleArgType,
        MaybeSingleArgName) :-
    ReserveTagPragma = does_not_use_reserved_tag,
    globals.lookup_bool_option(Globals, unboxed_no_tag_types, yes),
    MaybeUserEqCmp = no,

    type_is_single_ctor_single_arg(Ctors, SingleFunctorName, SingleArgType,
        MaybeCtorFieldName),

    % We do not handle unary tuples as no_tag types -- they are rare enough
    % that it is not worth the implementation effort.
    %
    % XXX Since the tuple type constructor doesn't have a HLDS type defn body,
    % will this test ever fail? Even if it can fail, we should test TypeCtor,
    % not SingleFunctorName.
    SingleFunctorName \= unqualified("{}"),

    (
        MaybeCtorFieldName = no,
        MaybeSingleArgName = no
    ;
        MaybeCtorFieldName = yes(ctor_field_name(SymName, _)),
        MaybeSingleArgName = yes(unqualify_name(SymName))
    ).

%-----------------------------------------------------------------------------%
%
% Type unification.
%

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
        TypeX = defined_type(SymName, ArgsX, _),
        TypeY = defined_type(SymName, ArgsY, _),
        % Instead of insisting that the names are equal and the arg lists
        % unify, we should consider attempting to expand equivalence types
        % first. That would require the type table to be passed in to the
        % unification algorithm, though.
        type_unify_list(ArgsX, ArgsY, HeadTypeParams, !Bindings)
    ;
        TypeX = builtin_type(BuiltinType),
        TypeY = builtin_type(BuiltinType)
    ;
        TypeX = higher_order_type(PorF, ArgsX, _, Purity, EvalMethod),
        TypeY = higher_order_type(PorF, ArgsY, _, Purity, EvalMethod),
        type_unify_list(ArgsX, ArgsY, HeadTypeParams, !Bindings)
    ;
        TypeX = tuple_type(ArgsX, _),
        TypeY = tuple_type(ArgsY, _),
        type_unify_list(ArgsX, ArgsY, HeadTypeParams, !Bindings)
    ).

    % Handle apply_n types and kinded types.
    %
:- pred type_unify_special(mer_type::in, mer_type::in, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_special(TypeX, TypeY, HeadTypeParams, !Bindings) :-
    ( if TypeX = apply_n_type(VarX, ArgsX, _) then
        type_unify_apply(TypeY, VarX, ArgsX, HeadTypeParams, !Bindings)
    else if TypeY = apply_n_type(VarY, ArgsY, _) then
        type_unify_apply(TypeX, VarY, ArgsY, HeadTypeParams, !Bindings)
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

type_unify_apply(TypeY, VarX, ArgsX0, HeadTypeParams, !Bindings) :-
    (
        TypeY = defined_type(NameY, ArgsY0, KindY0),
        type_unify_args(ArgsX0, ArgsY0, ArgsY, KindY0, KindY, HeadTypeParams,
            !Bindings),
        type_unify_var(VarX, defined_type(NameY, ArgsY, KindY), HeadTypeParams,
            !Bindings)
    ;
        TypeY = builtin_type(_),
        ArgsX0 = [],
        type_unify_var(VarX, TypeY, HeadTypeParams, !Bindings)
    ;
        TypeY = higher_order_type(_, _, _, _, _),
        ArgsX0 = [],
        type_unify_var(VarX, TypeY, HeadTypeParams, !Bindings)
    ;
        TypeY = tuple_type(ArgsY0, KindY0),
        type_unify_args(ArgsX0, ArgsY0, ArgsY, KindY0, KindY, HeadTypeParams,
            !Bindings),
        type_unify_var(VarX, tuple_type(ArgsY, KindY), HeadTypeParams,
            !Bindings)
    ;
        TypeY = apply_n_type(VarY, ArgsY0, Kind0),
        list.length(ArgsX0, NArgsX0),
        list.length(ArgsY0, NArgsY0),
        compare(Result, NArgsX0, NArgsY0),
        (
            Result = (<),
            type_unify_args(ArgsX0, ArgsY0, ArgsY, Kind0, Kind,
                HeadTypeParams, !Bindings),
            type_unify_var(VarX, apply_n_type(VarY, ArgsY, Kind),
                HeadTypeParams, !Bindings)
        ;
            Result = (=),
            % We know here that the list of remaining args will be empty.
            type_unify_args(ArgsX0, ArgsY0, _, Kind0, Kind, HeadTypeParams,
                !Bindings),
            type_unify_var_var(VarX, VarY, Kind, HeadTypeParams, !Bindings)
        ;
            Result = (>),
            type_unify_args(ArgsY0, ArgsX0, ArgsX, Kind0, Kind,
                HeadTypeParams, !Bindings),
            type_unify_var(VarY, apply_n_type(VarX, ArgsX, Kind),
                HeadTypeParams, !Bindings)
        )
    ;
        TypeY = kinded_type(RawY, _),
        type_unify_apply(RawY, VarX, ArgsX0, HeadTypeParams, !Bindings)
    ;
        TypeY = builtin_type(_),
        % XXX I (zs) am not sure *why* it is ok to fail here.
        fail
    ).

:- pred type_unify_args(list(mer_type)::in, list(mer_type)::in,
    list(mer_type)::out, kind::in, kind::out, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_args(ArgsX, ArgsY0, ArgsY, KindY0, KindY, HeadTypeParams,
        !Bindings) :-
    list.reverse(ArgsX, RevArgsX),
    list.reverse(ArgsY0, RevArgsY0),
    type_unify_rev_args(RevArgsX, RevArgsY0, RevArgsY, KindY0, KindY,
        HeadTypeParams, !Bindings),
    list.reverse(RevArgsY, ArgsY).

:- pred type_unify_rev_args(list(mer_type)::in, list(mer_type)::in,
    list(mer_type)::out, kind::in, kind::out, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_rev_args([], ArgsY, ArgsY, KindY, KindY, _, !Bindings).
type_unify_rev_args([ArgX | ArgsX], [ArgY0 | ArgsY0], ArgsY, KindY0, KindY,
        HeadTypeParams, !Bindings) :-
    type_unify(ArgX, ArgY0, HeadTypeParams, !Bindings),
    KindY1 = kind_arrow(get_type_kind(ArgY0), KindY0),
    type_unify_rev_args(ArgsX, ArgsY0, ArgsY, KindY1, KindY,
        HeadTypeParams, !Bindings).

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
        TypeX = defined_type(_, Args, _),
        type_occurs_list(Args, Y, Bindings)
    ;
        TypeX = higher_order_type(_, Args, _, _, _),
        type_occurs_list(Args, Y, Bindings)
    ;
        TypeX = tuple_type(Args, _),
        type_occurs_list(Args, Y, Bindings)
    ;
        TypeX = apply_n_type(X, Args, _),
        (
            X = Y
        ;
            type_occurs_list(Args, Y, Bindings)
        ;
            map.search(Bindings, X, BindingOfX),
            type_occurs(BindingOfX, Y, Bindings)
        )
    ;
        TypeX = kinded_type(X, _),
        type_occurs(X, Y, Bindings)
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

%-----------------------------------------------------------------------------%

type_subsumes(TypeA, TypeB, TypeSubst) :-
    % TypeA subsumes TypeB iff TypeA can be unified with TypeB
    % without binding any of the type variables in TypeB.
    type_vars(TypeB, TypeBVars),
    map.init(TypeSubst0),
    type_unify(TypeA, TypeB, TypeBVars, TypeSubst0, TypeSubst).

type_subsumes_det(TypeA, TypeB, TypeSubst) :-
    ( if type_subsumes(TypeA, TypeB, TypeSubstPrime) then
        TypeSubst = TypeSubstPrime
    else
        unexpected($module, $pred, "type_subsumes failed")
    ).

type_list_subsumes(TypesA, TypesB, TypeSubst) :-
    % TypesA subsumes TypesB iff TypesA can be unified with TypesB
    % without binding any of the type variables in TypesB.
    type_vars_list(TypesB, TypesBVars),
    map.init(TypeSubst0),
    type_unify_list(TypesA, TypesB, TypesBVars, TypeSubst0, TypeSubst).

type_list_subsumes_det(TypesA, TypesB, TypeSubst) :-
    ( if type_list_subsumes(TypesA, TypesB, TypeSubstPrime) then
        TypeSubst = TypeSubstPrime
    else
        unexpected($module, $pred, "type_list_subsumes failed")
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

%-----------------------------------------------------------------------------%

apply_partial_map_to_list(_PartialMap, [], []).
apply_partial_map_to_list(PartialMap, [X | Xs], [Y | Ys]) :-
    ( if map.search(PartialMap, X, Y0) then
        Y = Y0
    else
        Y = X
    ),
    apply_partial_map_to_list(PartialMap, Xs, Ys).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_type.
%-----------------------------------------------------------------------------%

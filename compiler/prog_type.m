%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_type.m.
% Main author: fjh.
%
% Utility predicates dealing with type in the parse tree. The predicates for
% doing type substitutions are in prog_type_subst.m, while utility predicates
% for dealing with types in the HLDS are in type_util.m.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_type.
:- interface.

:- import_module libs.globals.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module term.

%-----------------------------------------------------------------------------%
%
% Simple tests for certain properties of types. These tests work modulo any
% kind annotations, so in the early stages of the compiler (i.e., before type
% checking) these should be used rather than direct tests. Once we reach
% type checking all kind annotations should have been removed, so it would
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

    % Succeeds iff the given type is a higher-order predicate or function
    % type.
    %
:- pred type_is_higher_order(mer_type::in) is semidet.

    % type_is_higher_order_details(Type, Purity, PredOrFunc, ArgTypes,
    %   EvalMeth):
    %
    % Succeeds iff Type is a higher-order predicate or function type with
    % the specified argument types (for functions, the return type is
    % appended to the end of the argument types), purity, and
    % evaluation method.
    %
:- pred type_is_higher_order_details(mer_type::in, purity::out,
    pred_or_func::out, lambda_eval_method::out, list(mer_type)::out)
    is semidet.

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

    % type_has_variable_arity_ctor(Type, TypeCtor, TypeArgs)
    % Check if the principal type constructor of Type is of variable arity.
    % If yes, return the type constructor as TypeCtor and its args as
    % TypeArgs. If not, fail.
    %
:- pred type_has_variable_arity_ctor(mer_type::in, type_ctor::out,
    list(mer_type)::out) is semidet.

    % Given a non-variable type, return its type-id and argument types.
    %
:- pred type_to_ctor_and_args(mer_type::in, type_ctor::out,
    list(mer_type)::out) is semidet.

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

:- pred construct_higher_order_func_type(purity::in, lambda_eval_method::in,
    list(mer_type)::in, mer_type::in, mer_type::out) is det.

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

    % is_builtin_dummy_argument_type(ModuleName, TypeName, TypeArity):
    %
    % Is the given type a dummy type irrespective of its definition?
    %
:- pred is_builtin_dummy_argument_type(string::in, string::in, arity::in)
    is semidet.

    % Certain types, e.g. io.state and store.store(S), are just dummy types
    % used to ensure logical semantics; there is no need to actually pass them,
    % and so when importing or exporting procedures to/from C, we don't include
    % arguments with these types.
    %
    % A type is a dummy type in one of two cases: either it is a builtin
    % dummy type, or it has only a single function symbol of arity zero.
    %
:- pred constructor_list_represents_dummy_argument_type(list(constructor)::in,
    maybe(unify_compare)::in) is semidet.

:- pred type_is_io_state(mer_type::in) is semidet.

:- pred type_ctor_is_array(type_ctor::in) is semidet.

    % A test for type_info-related types that are introduced by
    % polymorphism.m.  These need to be handled specially in certain
    % places.  For example, mode inference never infers unique modes
    % for these types, since it would not be useful, and since we
    % want to minimize the number of different modes that we infer.
    %
:- pred is_introduced_type_info_type(mer_type::in) is semidet.

:- pred is_introduced_type_info_type_ctor(type_ctor::in) is semidet.

:- func is_introduced_type_info_type_category(type_category) = bool.

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

    % In the forwards mode, this predicate checks for a "new " prefix
    % at the start of the functor name, and removes it if present;
    % it fails if there is no such prefix.
    % In the reverse mode, this predicate prepends such a prefix.
    % (These prefixes are used for construction unifications
    % with existentially typed functors.)
    %
:- pred remove_new_prefix(sym_name, sym_name).
:- mode remove_new_prefix(in, out) is semidet.
:- mode remove_new_prefix(out, in) is det.

:- type type_category
    --->    type_cat_int
    ;       type_cat_char
    ;       type_cat_string
    ;       type_cat_float
    ;       type_cat_higher_order
    ;       type_cat_tuple
    ;       type_cat_enum
    ;       type_cat_dummy
    ;       type_cat_variable
    ;       type_cat_type_info
    ;       type_cat_type_ctor_info
    ;       type_cat_typeclass_info
    ;       type_cat_base_typeclass_info
    ;       type_cat_void
    ;       type_cat_user_ctor.

    % Construct builtin types.
    %
:- func int_type = mer_type.
:- func string_type = mer_type.
:- func float_type = mer_type.
:- func char_type = mer_type.
:- func void_type = mer_type.
:- func c_pointer_type = mer_type.
:- func heap_pointer_type = mer_type.
:- func sample_type_info_type = mer_type.
:- func sample_typeclass_info_type = mer_type.
:- func comparison_result_type = mer_type.
:- func io_state_type = mer_type.

    % Construct the types of type_infos and type_ctor_infos.
    %
:- func type_info_type = mer_type.
:- func type_ctor_info_type = mer_type.

    % Given a constant and an arity, return a type_ctor.
    % Fails if the constant is not an atom.
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
:- pred qualify_cons_id(mer_type::in, list(prog_var)::in, cons_id::in,
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

    % Check whether the type with the given list of constructors would be
    % a no_tag type (which requires the list to include exactly one constructor
    % with exactly one argument), and if so, return its constructor symbol,
    % argument type, and the argument's name (if it has one).
    %
    % This doesn't do any checks for options that might be set (such as
    % turning off no_tag_types). If you want those checks you should use
    % type_is_no_tag_type/4, or if you really know what you are doing,
    % perform the checks yourself.
    %
:- pred type_constructors_are_no_tag_type(list(constructor)::in, sym_name::out,
    mer_type::out, maybe(string)::out) is semidet.

    % Given a list of constructors for a type, check whether that type
    % is a private_builtin.type_info/0 or similar type.
    %
:- pred type_constructors_are_type_info(list(constructor)::in) is semidet.

    % type_with_constructors_should_be_no_tag(Globals, TypeCtor, ReservedTag,
    %   Ctors, UserEqComp, FunctorName, FunctorArgType, MaybeFunctorArgName):
    %
    % Check whether some constructors are a no_tag type, and that this
    % is compatible with the ReservedTag setting for this type and
    % the grade options set in the globals.
    % Assign single functor of arity one a `no_tag' tag (unless we are
    % reserving a tag, or if it is one of the dummy types).
    %
:- pred type_with_constructors_should_be_no_tag(globals::in, type_ctor::in,
    bool::in, list(constructor)::in, maybe(unify_compare)::in, sym_name::out,
    mer_type::out, maybe(string)::out) is semidet.

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

    % type_list_subsumes(TypesA, TypesB, Subst) succeeds iff the list
    % TypesA subsumes (is more general than) TypesB, producing a
    % type substitution which when applied to TypesA will give TypesB.
    %
:- pred type_list_subsumes(list(mer_type)::in, list(mer_type)::in, tsubst::out)
    is semidet.

    % This does the same as type_list_subsumes, but aborts instead of failing.
    %
:- pred type_list_subsumes_det(list(mer_type)::in, list(mer_type)::in,
    tsubst::out) is det.

    % arg_type_list_subsumes(TVarSet, ArgTypes, CalleeTVarSet,
    %   CalleeExistQVars, CalleeArgTypes):
    %
    % Check that the argument types of the called predicate, function or
    % constructor subsume the types of the arguments of the call. This checks
    % that none of the existentially quantified type variables of the callee
    % are bound.
    %
:- pred arg_type_list_subsumes(tvarset::in, list(mer_type)::in, tvarset::in,
    tvar_kind_map::in, existq_tvars::in, list(mer_type)::in) is semidet.

    % Apply a renaming (partial map) to a list.
    % Useful for applying a variable renaming to a list of variables.
    %
:- pred apply_partial_map_to_list(map(T, T)::in, list(T)::in, list(T)::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module libs.compiler_util.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.prog_type_subst.

:- import_module pair.
:- import_module string.
:- import_module svmap.

%-----------------------------------------------------------------------------%

type_is_var(Type) :-
    strip_kind_annotation(Type) = type_variable(_, _).

type_is_nonvar(Type) :-
    \+ type_is_var(Type).

type_is_higher_order(Type) :-
    strip_kind_annotation(Type) = higher_order_type(_, _, _, _).

type_is_higher_order_details(Type, Purity, PredOrFunc, EvalMethod,
        PredArgTypes) :-
    strip_kind_annotation(Type) =
        higher_order_type(ArgTypes, MaybeRetType, Purity, EvalMethod),
    (
        MaybeRetType = yes(RetType),
        PredOrFunc = function,
        PredArgTypes = list.append(ArgTypes, [RetType])
    ;
        MaybeRetType = no,
        PredOrFunc = predicate,
        PredArgTypes = ArgTypes
    ).

type_is_tuple(Type, ArgTypes) :-
    strip_kind_annotation(Type) = tuple_type(ArgTypes, _).

strip_kind_annotation(Type0) = Type :-
    ( Type0 = kinded_type(Type1, _) ->
        Type = strip_kind_annotation(Type1)
    ;
        Type = Type0
    ).

%-----------------------------------------------------------------------------%

type_is_ground(Type) :-
    \+ type_contains_var(Type, _).

type_is_nonground(Type) :-
    type_contains_var(Type, _).

subst_type_is_ground(Type, TSubst) :-
    \+ subst_type_is_nonground(Type, TSubst).

subst_type_is_nonground(Type, TSubst) :-
    type_contains_var(Type, TVar),
    ( map.search(TSubst, TVar, Binding) ->
        subst_type_is_nonground(Binding, TSubst)
    ;
        true
    ).

type_has_variable_arity_ctor(Type, TypeCtor, TypeArgs) :-
    ( type_is_higher_order_details(Type, _Purity, PredOrFunc, _, TypeArgs0) ->
        TypeArgs = TypeArgs0,
        PredOrFuncStr = prog_out.pred_or_func_to_str(PredOrFunc),
        TypeCtor = type_ctor(unqualified(PredOrFuncStr), 0)
    ; type_is_tuple(Type, TypeArgs1) ->
        TypeArgs = TypeArgs1,
        % XXX why tuple/0 and not {}/N ?
        TypeCtor = type_ctor(unqualified("tuple"), 0)
    ;
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
        Type = higher_order_type(Args0, MaybeRet, Purity, _EvalMethod),
        Arity = list.length(Args0),
        (
            MaybeRet = yes(Ret),
            PorFStr = "func",
            Args = list.append(Args0, [Ret])
        ;
            MaybeRet = no,
            PorFStr = "pred",
            Args = Args0
        ),
        SymName0 = unqualified(PorFStr),
        (
            Purity = purity_pure,
            SymName = SymName0
        ;
            Purity = purity_semipure,
            SymName = insert_module_qualifier("semipure", SymName0)
        ;
            Purity = purity_impure,
            SymName = insert_module_qualifier("impure", SymName0)
        ),
        TypeCtor = type_ctor(SymName, Arity)
    ;
        Type = tuple_type(Args, _),
        SymName = unqualified("{}"),
        Arity = list.length(Args),
        TypeCtor = type_ctor(SymName, Arity)
    ;
        Type = apply_n_type(_, _, _),
        sorry(this_file, "apply/N types")
    ;
        Type = kinded_type(SubType, _),
        type_to_ctor_and_args(SubType, TypeCtor, Args)
    ).

type_ctor_is_higher_order(TypeCtor, Purity, PredOrFunc, EvalMethod) :-
    TypeCtor = type_ctor(SymName, _Arity),
    get_purity_and_eval_method(SymName, Purity, EvalMethod, PorFStr),
    (
        PorFStr = "pred",
        PredOrFunc = predicate
    ;
        PorFStr = "func",
        PredOrFunc = function
    ).

:- pred get_purity_and_eval_method(sym_name::in, purity::out,
    lambda_eval_method::out, string::out) is semidet.

get_purity_and_eval_method(SymName, Purity, EvalMethod, PorFStr) :-
    (
        SymName = qualified(unqualified(Qualifier), PorFStr),
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
    ).

type_ctor_is_tuple(type_ctor(unqualified("{}"), _)).

type_list_to_var_list([], []).
type_list_to_var_list([Type | Types], [Var | Vars]) :-
    Type = type_variable(Var, _),
    type_list_to_var_list(Types, Vars).

var_list_to_type_list(_, [], []).
var_list_to_type_list(KindMap, [Var | Vars], [Type | Types]) :-
    get_tvar_kind(KindMap, Var, Kind),
    Type = type_variable(Var, Kind),
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
type_vars_2(higher_order_type(Args, MaybeRet, _, _), !V) :-
    type_vars_list_2(Args, !V),
    (
        MaybeRet = yes(Ret),
        type_vars_2(Ret, !V)
    ;
        MaybeRet = no
    ).
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
type_contains_var(higher_order_type(Args, _, _, _), Var) :-
    type_list_contains_var(Args, Var).
type_contains_var(higher_order_type(_, yes(Ret), _, _), Var) :-
    type_contains_var(Ret, Var).
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
    (
        TypeCtor = type_ctor(unqualified(Name), 0),
        builtin_type_to_string(BuiltinType, Name)
    ->
        Type = builtin_type(BuiltinType)
    ;
        type_ctor_is_higher_order(TypeCtor, Purity, PredOrFunc, EvalMethod)
    ->
        construct_higher_order_type(Purity, PredOrFunc, EvalMethod, Args, Type)
    ;
        type_ctor_is_tuple(TypeCtor)
    ->
        % XXX kind inference: we assume the kind is star.
        Type = tuple_type(Args, kind_star)
    ;
        TypeCtor = type_ctor(SymName, _),
        % XXX kind inference: we assume the kind is star.
        Type = defined_type(SymName, Args, kind_star)
    ).

construct_higher_order_type(Purity, PredOrFunc, EvalMethod, ArgTypes, Type) :-
    (
        PredOrFunc = predicate,
        construct_higher_order_pred_type(Purity, EvalMethod, ArgTypes, Type)
    ;
        PredOrFunc = function,
        pred_args_to_func_args(ArgTypes, FuncArgTypes, FuncRetType),
        construct_higher_order_func_type(Purity, EvalMethod, FuncArgTypes,
            FuncRetType, Type)
    ).

construct_higher_order_pred_type(Purity, EvalMethod, ArgTypes, Type) :-
    Type = higher_order_type(ArgTypes, no, Purity, EvalMethod).

construct_higher_order_func_type(Purity, EvalMethod, ArgTypes, RetType,
        Type) :-
    Type = higher_order_type(ArgTypes, yes(RetType), Purity, EvalMethod).

strip_builtin_qualifiers_from_type(type_variable(Var, Kind),
        type_variable(Var, Kind)).
strip_builtin_qualifiers_from_type(defined_type(Name0, Args0, Kind),
        defined_type(Name, Args, Kind)) :-
    (
        Name0 = qualified(Module, Name1),
        Module = mercury_public_builtin_module
    ->
        Name = unqualified(Name1)
    ;
        Name = Name0
    ),
    strip_builtin_qualifiers_from_type_list(Args0, Args).
strip_builtin_qualifiers_from_type(builtin_type(BuiltinType),
        builtin_type(BuiltinType)).
strip_builtin_qualifiers_from_type(
        higher_order_type(Args0, MaybeRet0, Purity, EvalMethod),
        higher_order_type(Args, MaybeRet, Purity, EvalMethod)) :-
    strip_builtin_qualifiers_from_type_list(Args0, Args),
    (
        MaybeRet0 = yes(Ret0),
        strip_builtin_qualifiers_from_type(Ret0, Ret),
        MaybeRet = yes(Ret)
    ;
        MaybeRet0 = no,
        MaybeRet = no
    ).
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

constraint_get_tvars(constraint(_Name, Args), TVars) :-
    type_vars_list(Args, TVars).

get_unconstrained_tvars(Tvars, Constraints, Unconstrained) :-
    constraint_list_get_tvars(Constraints, ConstrainedTvars),
    list.delete_elems(Tvars, ConstrainedTvars, Unconstrained0),
    list.remove_dups(Unconstrained0, Unconstrained).

%-----------------------------------------------------------------------------%

builtin_type_ctors_with_no_hlds_type_defn =
    [ type_ctor(qualified(mercury_public_builtin_module, "int"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "string"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "character"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "float"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "pred"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "func"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "void"), 0),
      type_ctor(qualified(mercury_public_builtin_module, "tuple"), 0)
    ].

is_builtin_dummy_argument_type("io", "state", 0).    % io.state/0
is_builtin_dummy_argument_type("store", "store", 1). % store.store/1.

constructor_list_represents_dummy_argument_type([Ctor], no) :-
    Ctor = ctor([], [], _, []).

type_is_io_state(Type) :-
    type_to_ctor_and_args(Type, TypeCtor, []),
    ModuleName = mercury_std_lib_module_name("io"),
    TypeCtor = type_ctor(qualified(ModuleName, "state"), 0).

type_ctor_is_array(type_ctor(qualified(unqualified("array"), "array"), 1)).

is_introduced_type_info_type(Type) :-
    type_to_ctor_and_args(Type, TypeCtor, _),
    is_introduced_type_info_type_ctor(TypeCtor).

is_introduced_type_info_type_ctor(TypeCtor) :-
    TypeCtor = type_ctor(qualified(PrivateBuiltin, Name), 0),
    PrivateBuiltin = mercury_private_builtin_module,
    ( Name = "type_info"
    ; Name = "type_ctor_info"
    ; Name = "typeclass_info"
    ; Name = "base_typeclass_info"
    ).

is_introduced_type_info_type_category(type_cat_int) = no.
is_introduced_type_info_type_category(type_cat_char) = no.
is_introduced_type_info_type_category(type_cat_string) = no.
is_introduced_type_info_type_category(type_cat_float) = no.
is_introduced_type_info_type_category(type_cat_higher_order) = no.
is_introduced_type_info_type_category(type_cat_tuple) = no.
is_introduced_type_info_type_category(type_cat_enum) = no.
is_introduced_type_info_type_category(type_cat_dummy) = no.
is_introduced_type_info_type_category(type_cat_variable) = no.
is_introduced_type_info_type_category(type_cat_type_info) = yes.
is_introduced_type_info_type_category(type_cat_type_ctor_info) = yes.
is_introduced_type_info_type_category(type_cat_typeclass_info) = yes.
is_introduced_type_info_type_category(type_cat_base_typeclass_info) = yes.
is_introduced_type_info_type_category(type_cat_void) = no.
is_introduced_type_info_type_category(type_cat_user_ctor) = no.

%-----------------------------------------------------------------------------%

put_typeinfo_vars_first(VarsList, VarTypes) =
        TypeInfoVarsList ++ NonTypeInfoVarsList :-
    split_vars_typeinfo_no_typeinfo(VarsList, VarTypes, 
        TypeInfoVarsList, NonTypeInfoVarsList). 

remove_typeinfo_vars(VarTypes, VarsList) = NonTypeInfoVarsList :- 
    split_vars_typeinfo_no_typeinfo(VarsList, VarTypes, _, 
        NonTypeInfoVarsList).

remove_typeinfo_vars_from_set(VarTypes, VarsSet) = 
    set.from_list(remove_typeinfo_vars(VarTypes, 
        set.to_sorted_list(VarsSet))).
    
:- pred split_vars_typeinfo_no_typeinfo(list(prog_var)::in, 
    vartypes::in, list(prog_var)::out, list(prog_var)::out) is det.

split_vars_typeinfo_no_typeinfo(VarsList, VarTypes, TypeInfoVarsList, 
        NonTypeInfoVarsList) :- 
    list.filter((pred(Var::in) is semidet :-
            Type = map.lookup(VarTypes, Var),
            is_introduced_type_info_type(Type)),
        VarsList, TypeInfoVarsList, NonTypeInfoVarsList).


remove_new_prefix(unqualified(Name0), unqualified(Name)) :-
    string.append("new ", Name, Name0).
remove_new_prefix(qualified(Module, Name0), qualified(Module, Name)) :-
    string.append("new ", Name, Name0).

%-----------------------------------------------------------------------------%

int_type = builtin_type(builtin_type_int).

string_type = builtin_type(builtin_type_string).

float_type = builtin_type(builtin_type_float).

char_type = builtin_type(builtin_type_character).

void_type = defined_type(unqualified("void"), [], kind_star).

c_pointer_type = defined_type(Name, [], kind_star) :-
    BuiltinModule = mercury_public_builtin_module,
    Name = qualified(BuiltinModule, "c_pointer").

heap_pointer_type = defined_type(Name, [], kind_star) :-
    BuiltinModule = mercury_private_builtin_module,
    Name = qualified(BuiltinModule, "heap_pointer").

sample_type_info_type = defined_type(Name, [], kind_star) :-
    BuiltinModule = mercury_private_builtin_module,
    Name = qualified(BuiltinModule, "sample_type_info").

sample_typeclass_info_type = defined_type(Name, [], kind_star) :-
    BuiltinModule = mercury_private_builtin_module,
    Name = qualified(BuiltinModule, "sample_typeclass_info").

comparison_result_type = defined_type(Name, [], kind_star) :-
    BuiltinModule = mercury_public_builtin_module,
    Name = qualified(BuiltinModule, "comparison_result").

type_info_type = defined_type(Name, [], kind_star) :-
    BuiltinModule = mercury_private_builtin_module,
    Name = qualified(BuiltinModule, "type_info").

type_ctor_info_type = defined_type(Name, [], kind_star) :-
    BuiltinModule = mercury_private_builtin_module,
    Name = qualified(BuiltinModule, "type_ctor_info").

io_state_type = defined_type(Name, [], kind_star) :-
    Module = mercury_std_lib_module_name("io"),
    Name = qualified(Module, "state").

%-----------------------------------------------------------------------------%

    % Given a constant and an arity, return a type_ctor.
    % This really ought to take a name and an arity -
    % use of integers/floats/strings as type names should
    % be rejected by the parser in prog_io.m, not in module_qual.m.

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
    InstConsId = cons(qualified(PrivateBuiltin, Symbol), Arity).

%-----------------------------------------------------------------------------%

qualify_cons_id(Type, Args, ConsId0, ConsId, InstConsId) :-
    (
        ConsId0 = cons(Name0, OrigArity),
        type_to_ctor_and_args(Type, TypeCtor, _),
        TypeCtor = type_ctor(qualified(TypeModule, _), _)
    ->
        UnqualName = unqualify_name(Name0),
        Name = qualified(TypeModule, UnqualName),
        ConsId = cons(Name, OrigArity),
        InstConsId = ConsId
    ;
        ConsId0 = type_info_cell_constructor(CellCtor)
    ->
        ConsId = ConsId0,
        InstConsId = cell_inst_cons_id(type_info_cell(CellCtor),
            list.length(Args))
    ;
        ConsId0 = typeclass_info_cell_constructor
    ->
        ConsId = typeclass_info_cell_constructor,
        InstConsId = cell_inst_cons_id(typeclass_info_cell, list.length(Args))
    ;
        ConsId = ConsId0,
        InstConsId = ConsId
    ).

%-----------------------------------------------------------------------------%

type_constructors_are_no_tag_type(Ctors, Ctor, ArgType, MaybeArgName) :-
    type_is_single_ctor_single_arg(Ctors, Ctor, MaybeArgName0, ArgType),

    % We don't handle unary tuples as no_tag types -- they are rare enough
    % that it's not worth the implementation effort.
    Ctor \= unqualified("{}"),

    MaybeArgName = map_maybe(unqualify_name, MaybeArgName0).

type_constructors_are_type_info(Ctors) :-
    type_is_single_ctor_single_arg(Ctors, Ctor, _, _),
    ctor_is_type_info(Ctor).

:- pred type_is_single_ctor_single_arg(list(constructor)::in, sym_name::out,
    maybe(ctor_field_name)::out, mer_type::out) is semidet.

type_is_single_ctor_single_arg(Ctors, Ctor, MaybeArgName, ArgType) :-
    Ctors = [SingleCtor],
    SingleCtor = ctor(ExistQVars, _Constraints, Ctor,
        [MaybeArgName - ArgType]),
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

    % Assign single functor of arity one a `no_tag' tag (unless we are
    % reserving a tag, or if it is one of the dummy types).
    %
type_with_constructors_should_be_no_tag(Globals, TypeCtor, ReserveTagPragma,
        Ctors, UserEqCmp, SingleFunc, SingleArg, MaybeArgName) :-
    type_constructors_are_no_tag_type(Ctors, SingleFunc, SingleArg,
        MaybeArgName),
    (
        ReserveTagPragma = no,
        globals.lookup_bool_option(Globals, reserve_tag, no),
        globals.lookup_bool_option(Globals, unboxed_no_tag_types, yes)
    ;
        % Dummy types always need to be treated as no-tag types as the
        % low-level C back end just passes around rubbish for them. When e.g.
        % using the debugger, it is crucial that these values are treated
        % as unboxed c_pointers, not as tagged pointers to c_pointers
        % (otherwise the system winds up following a bogus pointer).
        is_dummy_argument_type_with_constructors(TypeCtor, Ctors, UserEqCmp)
    ).

:- pred is_dummy_argument_type_with_constructors(type_ctor::in,
    list(constructor)::in, maybe(unify_compare)::in) is semidet.

is_dummy_argument_type_with_constructors(TypeCtor, Ctors, UserEqCmp) :-
    % Keep this in sync with is_dummy_argument_type below.
    (
        TypeCtor = type_ctor(CtorSymName, TypeArity),
        CtorSymName = qualified(unqualified(ModuleName), TypeName),
        is_builtin_dummy_argument_type(ModuleName, TypeName, TypeArity)
    ;
        constructor_list_represents_dummy_argument_type(Ctors, UserEqCmp)
    ).

%-----------------------------------------------------------------------------%
%
% Type unification.
%

type_unify(X, Y, HeadTypeParams, !Bindings) :-
    ( X = type_variable(VarX, _) ->
        type_unify_var(VarX, Y, HeadTypeParams, !Bindings)
    ; Y = type_variable(VarY, _) ->
        type_unify_var(VarY, X, HeadTypeParams, !Bindings)
    ; type_unify_nonvar(X, Y, HeadTypeParams, !Bindings) ->
        true
    ;
        % Some special cases are not handled above. We handle them separately
        % here.
        type_unify_special(X, Y, HeadTypeParams, !Bindings)
    ).

:- pred type_unify_var(tvar::in, mer_type::in, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_var(VarX, TypeY, HeadTypeParams, !Bindings) :-
    ( TypeY = type_variable(VarY, KindY) ->
        type_unify_var_var(VarX, VarY, KindY, HeadTypeParams, !Bindings)
    ; map.search(!.Bindings, VarX, BindingOfX) ->
        % VarX has a binding. Y is not a variable.
        type_unify(BindingOfX, TypeY, HeadTypeParams, !Bindings)
    ;
        % VarX has no binding, so bind it to TypeY.
        \+ type_occurs(TypeY, VarX, !.Bindings),
        \+ list.member(VarX, HeadTypeParams),
        svmap.det_insert(VarX, TypeY, !Bindings)
    ).

:- pred type_unify_var_var(tvar::in, tvar::in, kind::in, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_var_var(X, Y, Kind, HeadTypeParams, !Bindings) :-
    ( list.member(Y, HeadTypeParams) ->
        type_unify_head_type_param(X, Y, Kind, HeadTypeParams, !Bindings)
    ; list.member(X, HeadTypeParams) ->
        type_unify_head_type_param(Y, X, Kind, HeadTypeParams, !Bindings)
    ; map.search(!.Bindings, X, BindingOfX) ->
        ( map.search(!.Bindings, Y, BindingOfY) ->
            % Both X and Y already have bindings - just unify the
            % types they are bound to.
            type_unify(BindingOfX, BindingOfY, HeadTypeParams, !Bindings)
        ;
            % Y hasn't been bound yet.
            apply_rec_subst_to_type(!.Bindings, BindingOfX, SubstBindingOfX),
            ( SubstBindingOfX = type_variable(Y, _) ->
                true
            ;
                \+ type_occurs(SubstBindingOfX, Y, !.Bindings),
                svmap.det_insert(Y, SubstBindingOfX, !Bindings)
            )
        )
    ;
        % Neither X nor Y is a head type param. X had not been bound yet.
        ( map.search(!.Bindings, Y, BindingOfY) ->
            apply_rec_subst_to_type(!.Bindings, BindingOfY, SubstBindingOfY),
            ( SubstBindingOfY = type_variable(X, _) ->
                true
            ;
                \+ type_occurs(SubstBindingOfY, X, !.Bindings),
                svmap.det_insert(X, SubstBindingOfY, !Bindings)
            )
        ;
            % Both X and Y are unbound type variables - bind one to the other.
            ( X = Y ->
                true
            ;
                svmap.det_insert(X, type_variable(Y, Kind), !Bindings)
            )
        )
    ).

:- pred type_unify_head_type_param(tvar::in, tvar::in, kind::in,
    list(tvar)::in, tsubst::in, tsubst::out) is semidet.

type_unify_head_type_param(Var, HeadVar, Kind, HeadTypeParams, !Bindings) :-
    ( map.search(!.Bindings, Var, BindingOfVar) ->
        BindingOfVar = type_variable(Var2, _),
        type_unify_head_type_param(Var2, HeadVar, Kind, HeadTypeParams,
            !Bindings)
    ;
        ( Var = HeadVar ->
            true
        ;
            \+ list.member(Var, HeadTypeParams),
            svmap.det_insert(Var, type_variable(HeadVar, Kind), !Bindings)
        )
    ).

    % Unify two types, neither of which are variables. Two special cases
    % which are not handled here are apply_n types and kinded types.
    % Those are handled below.
    %
:- pred type_unify_nonvar(mer_type::in, mer_type::in, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_nonvar(defined_type(SymName, ArgsX, _),
        defined_type(SymName, ArgsY, _), HeadTypeParams, !Bindings) :-
    % Instead of insisting that the names are equal and the arg lists
    % unify, we should consider attempting to expand equivalence types
    % first.  That would require the type table to be passed in to the
    % unification algorithm, though.
    type_unify_list(ArgsX, ArgsY, HeadTypeParams, !Bindings).
type_unify_nonvar(builtin_type(BuiltinType), builtin_type(BuiltinType), _,
        !Bindings).
type_unify_nonvar(higher_order_type(ArgsX, no, Purity, EvalMethod),
        higher_order_type(ArgsY, no, Purity, EvalMethod),
        HeadTypeParams, !Bindings) :-
    type_unify_list(ArgsX, ArgsY, HeadTypeParams, !Bindings).
type_unify_nonvar(higher_order_type(ArgsX, yes(RetX), Purity, EvalMethod),
        higher_order_type(ArgsY, yes(RetY), Purity, EvalMethod),
        HeadTypeParams, !Bindings) :-
    type_unify_list(ArgsX, ArgsY, HeadTypeParams, !Bindings),
    type_unify(RetX, RetY, HeadTypeParams, !Bindings).
type_unify_nonvar(tuple_type(ArgsX, _), tuple_type(ArgsY, _), HeadTypeParams,
        !Bindings) :-
    type_unify_list(ArgsX, ArgsY, HeadTypeParams, !Bindings).

    % Handle apply_n types and kinded types.
    %
:- pred type_unify_special(mer_type::in, mer_type::in, list(tvar)::in,
    tsubst::in, tsubst::out) is semidet.

type_unify_special(X, Y, HeadTypeParams, !Bindings) :-
    ( X = apply_n_type(VarX, ArgsX, _) ->
        type_unify_apply(Y, VarX, ArgsX, HeadTypeParams, !Bindings)
    ; Y = apply_n_type(VarY, ArgsY, _) ->
        type_unify_apply(X, VarY, ArgsY, HeadTypeParams, !Bindings)
    ; X = kinded_type(RawX, _) ->
        ( Y = kinded_type(RawY, _) ->
            type_unify(RawX, RawY, HeadTypeParams, !Bindings)
        ;
            type_unify(RawX, Y, HeadTypeParams, !Bindings)
        )
    ; Y = kinded_type(RawY, _) ->
        type_unify(X, RawY, HeadTypeParams, !Bindings)
    ;
        fail
    ).

    % The idea here is that we try to strip off arguments from Y starting
    % from the end and unify each with the corresponding argument of X.
    % If we reach an atomic type before the arguments run out then we fail.
    % If we reach a variable before the arguments run out then we unify it
    % with what remains of the apply_n expression. If we manage to unify
    % all of the arguments then we unify the apply_n variable with what
    % remains of the other expression.
    %
    % Note that Y is not a variable, since that case would have been
    % caught by type_unify.
    %
:- pred type_unify_apply(mer_type::in, tvar::in, list(mer_type)::in,
    list(tvar)::in, tsubst::in, tsubst::out) is semidet.

type_unify_apply(defined_type(NameY, ArgsY0, KindY0), VarX, ArgsX,
        HeadTypeParams, !Bindings) :-
    type_unify_args(ArgsX, ArgsY0, ArgsY, KindY0, KindY, HeadTypeParams,
        !Bindings),
    type_unify_var(VarX, defined_type(NameY, ArgsY, KindY), HeadTypeParams,
        !Bindings).
type_unify_apply(Type @ builtin_type(_), VarX, [], HeadTypeParams,
        !Bindings) :-
    type_unify_var(VarX, Type, HeadTypeParams, !Bindings).
type_unify_apply(Type @ higher_order_type(_, _, _, _), VarX, [],
        HeadTypeParams, !Bindings) :-
    type_unify_var(VarX, Type, HeadTypeParams, !Bindings).
type_unify_apply(tuple_type(ArgsY0, KindY0), VarX, ArgsX, HeadTypeParams,
        !Bindings) :-
    type_unify_args(ArgsX, ArgsY0, ArgsY, KindY0, KindY, HeadTypeParams,
        !Bindings),
    type_unify_var(VarX, tuple_type(ArgsY, KindY), HeadTypeParams, !Bindings).
type_unify_apply(apply_n_type(VarY, ArgsY0, Kind0), VarX, ArgsX0,
        HeadTypeParams, !Bindings) :-
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
    ).
type_unify_apply(kinded_type(RawY, _), VarX, ArgsX, HeadTypeParams,
        !Bindings) :-
    type_unify_apply(RawY, VarX, ArgsX, HeadTypeParams, !Bindings).

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
    % perhaps indirectly via the substitution.  (The variable must not
    % be mapped by the substitution.)
    %
:- pred type_occurs(mer_type::in, tvar::in, tsubst::in) is semidet.

type_occurs(type_variable(X, _), Y, Bindings) :-
    ( X = Y ->
        true
    ;
        map.search(Bindings, X, BindingOfX),
        type_occurs(BindingOfX, Y, Bindings)
    ).
type_occurs(defined_type(_, Args, _), Y, Bindings) :-
    type_occurs_list(Args, Y, Bindings).
type_occurs(higher_order_type(Args, MaybeRet, _, _), Y, Bindings) :-
    (
        type_occurs_list(Args, Y, Bindings)
    ;
        MaybeRet = yes(Ret),
        type_occurs(Ret, Y, Bindings)
    ).
type_occurs(tuple_type(Args, _), Y, Bindings) :-
    type_occurs_list(Args, Y, Bindings).
type_occurs(apply_n_type(X, Args, _), Y, Bindings) :-
    (
        X = Y
    ;
        type_occurs_list(Args, Y, Bindings)
    ;
        map.search(Bindings, X, BindingOfX),
        type_occurs(BindingOfX, Y, Bindings)
    ).
type_occurs(kinded_type(X, _), Y, Bindings) :-
    type_occurs(X, Y, Bindings).

:- pred type_occurs_list(list(mer_type)::in, tvar::in, tsubst::in) is semidet.

type_occurs_list([X | Xs], Y,  Bindings) :-
    (
        type_occurs(X, Y, Bindings)
    ;
        type_occurs_list(Xs, Y, Bindings)
    ).

%-----------------------------------------------------------------------------%

type_list_subsumes(TypesA, TypesB, TypeSubst) :-
    % TypesA subsumes TypesB iff TypesA can be unified with TypesB
    % without binding any of the type variables in TypesB.

    type_vars_list(TypesB, TypesBVars),
    map.init(TypeSubst0),
    type_unify_list(TypesA, TypesB, TypesBVars, TypeSubst0, TypeSubst).

type_list_subsumes_det(TypesA, TypesB, TypeSubst) :-
    ( type_list_subsumes(TypesA, TypesB, TypeSubstPrime) ->
        TypeSubst = TypeSubstPrime
    ;
        unexpected(this_file,
            "type_list_subsumes_det: type_list_subsumes failed")
    ).

arg_type_list_subsumes(TVarSet, ActualArgTypes, CalleeTVarSet, PredKindMap,
        PredExistQVars, PredArgTypes) :-
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

    type_list_subsumes(ParentArgTypes, ActualArgTypes, ParentToActualSubst),

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
    ( map.search(PartialMap, X, Y0) ->
        Y = Y0
    ;
        Y = X
    ),
    apply_partial_map_to_list(PartialMap, Xs, Ys).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "prog_type.m".

%-----------------------------------------------------------------------------%
:- end_module prog_type.
%-----------------------------------------------------------------------------%

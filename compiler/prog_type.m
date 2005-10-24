%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main author: fjh
%
% Utility predicates dealing with types that do not require access to the
% HLDS.  (The predicates that do are in type_util.m.)
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_type.

:- interface.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%
%
% Simple tests for certain properties of types.  These tests work modulo any
% kind annotations, so in the early stages of the compiler (i.e., before type
% checking) these should be used rather than direct tests.  Once we reach
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

    % type_is_higher_order(Type, Purity, PredOrFunc, ArgTypes, EvalMeth):
    % succeeds iff Type is a higher-order predicate or function type with
    % the specified argument types (for functions, the return type is
    % appended to the end of the argument types), purity, and
    % evaluation method.
    % 
:- pred type_is_higher_order(mer_type::in, purity::out, pred_or_func::out,
    lambda_eval_method::out, list(mer_type)::out) is semidet.

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
:- pred type_is_ground(mer_type::in, tsubst::in) is semidet.

    % Succeeds iff the given type with the substitution applied is not
    % ground.
    %
:- pred type_is_nonground(mer_type::in, tsubst::in) is semidet.

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

    % type_ctor_is_variable(TypeCtor) succeeds iff TypeCtor is a variable.
    %
:- pred type_ctor_is_variable(type_ctor::in) is semidet.

    % Convert a list of types to a list of vars.  Fail if any of them are
    % not variables.
    %
:- pred prog_type.type_list_to_var_list(list(mer_type)::in, list(tvar)::out)
    is semidet.

    % Convert a list of vars into a list of variable types.
    %
:- pred prog_type.var_list_to_type_list(tvar_kind_map::in, list(tvar)::in,
    list(mer_type)::out) is det.

    % Return a list of the type variables of a type, in order of their
    % first occurrence in a depth-first, left-right traversal.
    %
:- pred prog_type.vars(mer_type::in, list(tvar)::out) is det.

    % Return a list of the type variables of a list of types, in order
    % of their first occurrence in a depth-first, left-right traversal.
    %
:- pred prog_type.vars_list(list(mer_type)::in, list(tvar)::out) is det.

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

%-----------------------------------------------------------------------------%
%
% Type substitutions.
%

:- pred apply_rec_subst_to_type(tsubst::in, mer_type::in, mer_type::out)
    is det.

:- pred apply_rec_subst_to_type_list(tsubst::in, list(mer_type)::in,
    list(mer_type)::out) is det.

:- pred apply_rec_subst_to_tvar(tvar_kind_map::in, tsubst::in,
    tvar::in, mer_type::out) is det.

:- pred apply_rec_subst_to_tvar_list(tvar_kind_map::in, tsubst::in,
    list(tvar)::in, list(mer_type)::out) is det.

:- pred apply_subst_to_type(tsubst::in, mer_type::in, mer_type::out) is det.

:- pred apply_subst_to_type_list(tsubst::in, list(mer_type)::in,
    list(mer_type)::out) is det.

:- pred apply_subst_to_tvar(tvar_kind_map::in, tsubst::in,
    tvar::in, mer_type::out) is det.

:- pred apply_subst_to_tvar_list(tvar_kind_map::in, tsubst::in,
    list(tvar)::in, list(mer_type)::out) is det.

:- pred apply_variable_renaming_to_type(tvar_renaming::in, mer_type::in,
    mer_type::out) is det.

:- pred apply_variable_renaming_to_type_list(tvar_renaming::in,
    list(mer_type)::in, list(mer_type)::out) is det.

:- pred apply_variable_renaming_to_tvar(tvar_renaming::in, tvar::in, tvar::out)
    is det.

:- pred apply_variable_renaming_to_tvar_list(tvar_renaming::in, list(tvar)::in,
    list(tvar)::out) is det.

:- pred apply_variable_renaming_to_tvar_kind_map(tvar_renaming::in,
    tvar_kind_map::in, tvar_kind_map::out) is det.

%-----------------------------------------------------------------------------%
%
% Utility predicates dealing with typeclass constraints.
%

:- pred apply_rec_subst_to_prog_constraints(tsubst::in, prog_constraints::in,
    prog_constraints::out) is det.

:- pred apply_rec_subst_to_prog_constraint_list(tsubst::in,
    list(prog_constraint)::in, list(prog_constraint)::out) is det.

:- pred apply_rec_subst_to_prog_constraint(tsubst::in, prog_constraint::in,
    prog_constraint::out) is det.

:- pred apply_subst_to_prog_constraints(tsubst::in, prog_constraints::in,
    prog_constraints::out) is det.

:- pred apply_subst_to_prog_constraint_list(tsubst::in,
    list(prog_constraint)::in, list(prog_constraint)::out) is det.

:- pred apply_subst_to_prog_constraint(tsubst::in, prog_constraint::in,
    prog_constraint::out) is det.

:- pred apply_variable_renaming_to_prog_constraints(tvar_renaming::in,
    prog_constraints::in, prog_constraints::out) is det.

:- pred apply_variable_renaming_to_prog_constraint_list(tvar_renaming::in,
    list(prog_constraint)::in, list(prog_constraint)::out) is det.

:- pred apply_variable_renaming_to_prog_constraint(tvar_renaming::in,
    prog_constraint::in, prog_constraint::out) is det.

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
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module map.
:- import_module std_util.
:- import_module svmap.

%-----------------------------------------------------------------------------%

type_is_var(Type) :-
    strip_kind_annotation(Type) = variable(_, _).

type_is_nonvar(Type) :-
    \+ type_is_var(Type).

type_is_higher_order(Type) :-
    strip_kind_annotation(Type) = higher_order(_, _, _, _).

type_is_higher_order(Type0, Purity, PredOrFunc, EvalMethod, PredArgTypes) :-
    Type = strip_kind_annotation(Type0),
    Type = higher_order(ArgTypes, MaybeRetType, Purity, EvalMethod),
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
    strip_kind_annotation(Type) = tuple(ArgTypes, _).

strip_kind_annotation(Type0) = Type :-
    ( Type0 = kinded(Type1, _) ->
        Type = strip_kind_annotation(Type1)
    ;
        Type = Type0
    ).

%-----------------------------------------------------------------------------%

type_is_ground(Type) :-
    \+ type_contains_var(Type, _).

type_is_nonground(Type) :-
    type_contains_var(Type, _).

type_is_ground(Type, TSubst) :-
    \+ type_is_nonground(Type, TSubst).

type_is_nonground(Type, TSubst) :-
    type_contains_var(Type, TVar),
    ( map.search(TSubst, TVar, Binding) ->
        type_is_nonground(Binding, TSubst)
    ;
        true
    ).

type_has_variable_arity_ctor(Type, TypeCtor, TypeArgs) :-
    (
        type_is_higher_order(Type, _Purity, PredOrFunc, _,
            TypeArgs0)
    ->
        TypeArgs = TypeArgs0,
        PredOrFuncStr = prog_out.pred_or_func_to_str(PredOrFunc),
        TypeCtor = unqualified(PredOrFuncStr) - 0
    ;
        type_is_tuple(Type, TypeArgs1)
    ->
        TypeArgs = TypeArgs1,
        % XXX why tuple/0 and not {}/N ?
        TypeCtor = unqualified("tuple") - 0
    ;
        fail
    ).

type_to_ctor_and_args(defined(SymName, Args, _), SymName - Arity, Args) :-
    Arity = list.length(Args).
type_to_ctor_and_args(builtin(BuiltinType), SymName - 0, []) :-
    builtin_type_to_string(BuiltinType, Name),
    SymName = unqualified(Name).
type_to_ctor_and_args(higher_order(Args0, MaybeRet, Purity, EvalMethod), 
        SymName - Arity, Args) :-
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
        EvalMethod = lambda_aditi_bottom_up,
        insert_module_qualifier("aditi_bottom_up", SymName0, SymName1)
    ;
        EvalMethod = lambda_normal,
        SymName1 = SymName0
    ),
    (
        Purity = purity_pure,
        SymName = SymName1
    ;
        Purity = purity_semipure,
        insert_module_qualifier("semipure", SymName1, SymName)
    ;
        Purity = purity_impure,
        insert_module_qualifier("impure", SymName1, SymName)
    ).
type_to_ctor_and_args(tuple(Args, _), unqualified("{}") - Arity, Args) :-
    Arity = list.length(Args).
type_to_ctor_and_args(apply_n(_, _, _), _, _) :-
    sorry(this_file, "apply/N types").
type_to_ctor_and_args(kinded(Type, _), TypeCtor, Args) :-
    type_to_ctor_and_args(Type, TypeCtor, Args).

type_ctor_is_higher_order(SymName - _Arity, Purity, PredOrFunc, EvalMethod) :-
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
            Qualifier = "aditi_bottom_up",
            EvalMethod = lambda_aditi_bottom_up,
            Purity = purity_pure
        ;
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

type_ctor_is_tuple(unqualified("{}") - _).

type_ctor_is_variable(unqualified("") - _).

prog_type.type_list_to_var_list([], []).
prog_type.type_list_to_var_list([Type | Types], [Var | Vars]) :-
    Type = variable(Var, _),
    prog_type.type_list_to_var_list(Types, Vars).

prog_type.var_list_to_type_list(_, [], []).
prog_type.var_list_to_type_list(KindMap, [Var | Vars], [Type | Types]) :-
    get_tvar_kind(KindMap, Var, Kind),
    Type = variable(Var, Kind),
    prog_type.var_list_to_type_list(KindMap, Vars, Types).

prog_type.vars(Type, TVars) :-
    prog_type.vars_2(Type, [], RevTVars),
    list.reverse(RevTVars, TVarsDups),
    list.remove_dups(TVarsDups, TVars).

:- pred prog_type.vars_2(mer_type::in, list(tvar)::in, list(tvar)::out) is det.

prog_type.vars_2(variable(Var, _), Vs, [Var | Vs]).
prog_type.vars_2(defined(_, Args, _), !V) :-
    prog_type.vars_list_2(Args, !V).
prog_type.vars_2(builtin(_), !V).
prog_type.vars_2(higher_order(Args, MaybeRet, _, _), !V) :-
    prog_type.vars_list_2(Args, !V),
    (
        MaybeRet = yes(Ret),
        prog_type.vars_2(Ret, !V)
    ;
        MaybeRet = no
    ).
prog_type.vars_2(tuple(Args, _), !V) :-
    prog_type.vars_list_2(Args, !V).
prog_type.vars_2(apply_n(Var, Args, _), !V) :-
    !:V = [Var | !.V],
    prog_type.vars_list_2(Args, !V).
prog_type.vars_2(kinded(Type, _), !V) :-
    prog_type.vars_2(Type, !V).

prog_type.vars_list(Types, TVars) :-
    prog_type.vars_list_2(Types, [], RevTVars),
    list.reverse(RevTVars, TVarsDups),
    list.remove_dups(TVarsDups, TVars).

:- pred prog_type.vars_list_2(list(mer_type)::in, list(tvar)::in,
    list(tvar)::out) is det.

prog_type.vars_list_2([], !V).
prog_type.vars_list_2([Type | Types], !V) :-
    prog_type.vars_2(Type, !V),
    prog_type.vars_list_2(Types, !V).

type_contains_var(variable(Var, _), Var).
type_contains_var(defined(_, Args, _), Var) :-
    type_list_contains_var(Args, Var).
type_contains_var(higher_order(Args, _, _, _), Var) :-
    type_list_contains_var(Args, Var).
type_contains_var(higher_order(_, yes(Ret), _, _), Var) :-
    type_contains_var(Ret, Var).
type_contains_var(tuple(Args, _), Var) :-
    type_list_contains_var(Args, Var).
type_contains_var(apply_n(Var, _, _), Var).
type_contains_var(apply_n(_, Args, _), Var) :-
    type_list_contains_var(Args, Var).
type_contains_var(kinded(Type, _), Var) :-
    type_contains_var(Type, Var).

type_list_contains_var([Type | _], Var) :-
    type_contains_var(Type, Var).
type_list_contains_var([_ | Types], Var) :-
    type_list_contains_var(Types, Var).

construct_type(TypeCtor, Args, Type) :-
    (
        TypeCtor = unqualified(Name) - 0,
        builtin_type_to_string(BuiltinType, Name)
    ->
        Type = builtin(BuiltinType)
    ;
        type_ctor_is_higher_order(TypeCtor, Purity, PredOrFunc, EvalMethod)
    ->
        construct_higher_order_type(Purity, PredOrFunc, EvalMethod, Args, Type)
    ;
        type_ctor_is_tuple(TypeCtor)
    ->
        % XXX kind inference: we assume the kind is star.
        Type = tuple(Args, star)
    ;
        TypeCtor = SymName - _,
        % XXX kind inference: we assume the kind is star.
        Type = defined(SymName, Args, star)
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
    Type = higher_order(ArgTypes, no, Purity, EvalMethod).

construct_higher_order_func_type(Purity, EvalMethod, ArgTypes, RetType,
        Type) :-
    Type = higher_order(ArgTypes, yes(RetType), Purity, EvalMethod).

strip_builtin_qualifiers_from_type(variable(Var, Kind), variable(Var, Kind)).
strip_builtin_qualifiers_from_type(defined(Name0, Args0, Kind),
        defined(Name, Args, Kind)) :-
    (
        Name0 = qualified(Module, Name1),
        mercury_public_builtin_module(Module)
    ->
        Name = unqualified(Name1)
    ;
        Name = Name0
    ),
    strip_builtin_qualifiers_from_type_list(Args0, Args).
strip_builtin_qualifiers_from_type(builtin(BuiltinType), builtin(BuiltinType)).
strip_builtin_qualifiers_from_type(
        higher_order(Args0, MaybeRet0, Purity, EvalMethod),
        higher_order(Args, MaybeRet, Purity, EvalMethod)) :-
    strip_builtin_qualifiers_from_type_list(Args0, Args),
    (
        MaybeRet0 = yes(Ret0),
        strip_builtin_qualifiers_from_type(Ret0, Ret),
        MaybeRet = yes(Ret)
    ;
        MaybeRet0 = no,
        MaybeRet = no
    ).
strip_builtin_qualifiers_from_type(tuple(Args0, Kind), tuple(Args, Kind)) :-
    strip_builtin_qualifiers_from_type_list(Args0, Args).
strip_builtin_qualifiers_from_type(apply_n(Var, Args0, Kind),
        apply_n(Var, Args, Kind)) :-
    strip_builtin_qualifiers_from_type_list(Args0, Args).
strip_builtin_qualifiers_from_type(kinded(Type0, Kind), kinded(Type, Kind)) :-
    strip_builtin_qualifiers_from_type(Type0, Type).

strip_builtin_qualifiers_from_type_list(Types0, Types) :-
    list__map(strip_builtin_qualifiers_from_type, Types0, Types).

%-----------------------------------------------------------------------------%

apply_rec_subst_to_type(Subst, Type0 @ variable(TVar, Kind), Type) :-
    ( map__search(Subst, TVar, Type1) ->
        ensure_type_has_kind(Kind, Type1, Type2),
        apply_rec_subst_to_type(Subst, Type2, Type)
    ;
        Type = Type0
    ).
apply_rec_subst_to_type(Subst, defined(Name, Args0, Kind),
        defined(Name, Args, Kind)) :-
    apply_rec_subst_to_type_list(Subst, Args0, Args).
apply_rec_subst_to_type(_Subst, Type @ builtin(_), Type).
apply_rec_subst_to_type(Subst,
        higher_order(Args0, MaybeReturn0, Purity, EvalMethod),
        higher_order(Args, MaybeReturn, Purity, EvalMethod)) :-
    apply_rec_subst_to_type_list(Subst, Args0, Args),
    (
        MaybeReturn0 = yes(Return0),
        apply_rec_subst_to_type(Subst, Return0, Return),
        MaybeReturn = yes(Return)
    ;
        MaybeReturn0 = no,
        MaybeReturn = no
    ).
apply_rec_subst_to_type(Subst, tuple(Args0, Kind), tuple(Args, Kind)) :-
    apply_rec_subst_to_type_list(Subst, Args0, Args).
apply_rec_subst_to_type(Subst, apply_n(TVar, Args0, Kind), Type) :-
    apply_rec_subst_to_type_list(Subst, Args0, Args),
    ( map__search(Subst, TVar, AppliedType0) ->
        apply_rec_subst_to_type(Subst, AppliedType0, AppliedType),
        apply_type_args(AppliedType, Args, Type)
    ;
        Type = apply_n(TVar, Args, Kind)
    ).
apply_rec_subst_to_type(Subst, kinded(Type0, Kind), kinded(Type, Kind)) :-
    apply_rec_subst_to_type(Subst, Type0, Type).

apply_rec_subst_to_type_list(Subst, Types0, Types) :-
    list__map(apply_rec_subst_to_type(Subst), Types0, Types).

apply_rec_subst_to_tvar(KindMap, Subst, TVar, Type) :-
    ( map__search(Subst, TVar, Type0) ->
        apply_rec_subst_to_type(Subst, Type0, Type)
    ;
        get_tvar_kind(KindMap, TVar, Kind),
        Type = variable(TVar, Kind)
    ).

apply_rec_subst_to_tvar_list(KindMap, Subst, TVars, Types) :-
    list__map(apply_rec_subst_to_tvar(KindMap, Subst), TVars, Types).

apply_subst_to_type(Subst, Type0 @ variable(TVar, Kind), Type) :-
    ( map__search(Subst, TVar, Type1) ->
        ensure_type_has_kind(Kind, Type1, Type)
    ;
        Type = Type0
    ).
apply_subst_to_type(Subst, defined(Name, Args0, Kind),
        defined(Name, Args, Kind)) :-
    apply_subst_to_type_list(Subst, Args0, Args).
apply_subst_to_type(_Subst, Type @ builtin(_), Type).
apply_subst_to_type(Subst,
        higher_order(Args0, MaybeReturn0, Purity, EvalMethod),
        higher_order(Args, MaybeReturn, Purity, EvalMethod)) :-
    apply_subst_to_type_list(Subst, Args0, Args),
    (
        MaybeReturn0 = yes(Return0),
        apply_subst_to_type(Subst, Return0, Return),
        MaybeReturn = yes(Return)
    ;
        MaybeReturn0 = no,
        MaybeReturn = no
    ).
apply_subst_to_type(Subst, tuple(Args0, Kind), tuple(Args, Kind)) :-
    apply_subst_to_type_list(Subst, Args0, Args).
apply_subst_to_type(Subst, apply_n(TVar, Args0, Kind), Type) :-
    apply_subst_to_type_list(Subst, Args0, Args),
    ( map__search(Subst, TVar, AppliedType) ->
        apply_type_args(AppliedType, Args, Type)
    ;
        Type = apply_n(TVar, Args, Kind)
    ).
apply_subst_to_type(Subst, kinded(Type0, Kind), kinded(Type, Kind)) :-
    apply_subst_to_type(Subst, Type0, Type).

apply_subst_to_type_list(Subst, Types0, Types) :-
    list__map(apply_subst_to_type(Subst), Types0, Types).

apply_subst_to_tvar(KindMap, Subst, TVar, Type) :-
    ( map__search(Subst, TVar, Type0) ->
        apply_subst_to_type(Subst, Type0, Type)
    ;
        get_tvar_kind(KindMap, TVar, Kind),
        Type = variable(TVar, Kind)
    ).

apply_subst_to_tvar_list(KindMap, Subst, TVars, Types) :-
    list__map(apply_subst_to_tvar(KindMap, Subst), TVars, Types).

apply_variable_renaming_to_type(Renaming, variable(TVar0, Kind),
        variable(TVar, Kind)) :-
    apply_variable_renaming_to_tvar(Renaming, TVar0, TVar).
apply_variable_renaming_to_type(Renaming, defined(Name, Args0, Kind),
        defined(Name, Args, Kind)) :-
    apply_variable_renaming_to_type_list(Renaming, Args0, Args).
apply_variable_renaming_to_type(_Renaming, Type @ builtin(_), Type).
apply_variable_renaming_to_type(Renaming,
        higher_order(Args0, MaybeReturn0, Purity, EvalMethod),
        higher_order(Args, MaybeReturn, Purity, EvalMethod)) :-
    apply_variable_renaming_to_type_list(Renaming, Args0, Args),
    (
        MaybeReturn0 = yes(Return0),
        apply_variable_renaming_to_type(Renaming, Return0, Return),
        MaybeReturn = yes(Return)
    ;
        MaybeReturn0 = no,
        MaybeReturn = no
    ).
apply_variable_renaming_to_type(Renaming, tuple(Args0, Kind),
        tuple(Args, Kind)) :-
    apply_variable_renaming_to_type_list(Renaming, Args0, Args).
apply_variable_renaming_to_type(Renaming, apply_n(TVar0, Args0, Kind),
        apply_n(TVar, Args, Kind)) :-
    apply_variable_renaming_to_type_list(Renaming, Args0, Args),
    apply_variable_renaming_to_tvar(Renaming, TVar0, TVar).
apply_variable_renaming_to_type(Renaming, kinded(Type0, Kind),
        kinded(Type, Kind)) :-
    apply_variable_renaming_to_type(Renaming, Type0, Type).

apply_variable_renaming_to_type_list(Renaming, Types0, Types) :-
    list__map(apply_variable_renaming_to_type(Renaming), Types0, Types).

apply_variable_renaming_to_tvar(Renaming, TVar0, TVar) :-
    ( map__search(Renaming, TVar0, TVar1) ->
        TVar = TVar1
    ;
        TVar = TVar0
    ).

apply_variable_renaming_to_tvar_list(Renaming, TVars0, TVars) :-
    list__map(apply_variable_renaming_to_tvar(Renaming), TVars0, TVars).

apply_variable_renaming_to_tvar_kind_map(Renaming, KindMap0, KindMap) :-
    map__foldl(apply_variable_renaming_to_tvar_kind_map_2(Renaming),
        KindMap0, map__init, KindMap).

:- pred apply_variable_renaming_to_tvar_kind_map_2(tvar_renaming::in, tvar::in,
    kind::in, tvar_kind_map::in, tvar_kind_map::out) is det.

apply_variable_renaming_to_tvar_kind_map_2(Renaming, TVar0, Kind, !KindMap) :-
    apply_variable_renaming_to_tvar(Renaming, TVar0, TVar),
    svmap__det_insert(TVar, Kind, !KindMap).

:- pred apply_type_args(mer_type::in, list(mer_type)::in, mer_type::out)
    is det.

apply_type_args(variable(TVar, Kind0), Args, apply_n(TVar, Args, Kind)) :-
    apply_type_args_to_kind(Kind0, Args, Kind).
apply_type_args(defined(Name, Args0, Kind0), Args,
        defined(Name, Args0 ++ Args, Kind)) :-
    apply_type_args_to_kind(Kind0, Args, Kind).
apply_type_args(Type @ builtin(_), [], Type).
apply_type_args(builtin(_), [_ | _], _) :-
    unexpected(this_file, "applied type args to builtin").
apply_type_args(Type @ higher_order(_, _, _, _), [], Type).
apply_type_args(higher_order(_, _, _, _), [_ | _], _) :-
    unexpected(this_file, "applied type args to higher_order").
apply_type_args(tuple(Args0, Kind0), Args, tuple(Args0 ++ Args, Kind)) :-
    apply_type_args_to_kind(Kind0, Args, Kind).
apply_type_args(apply_n(TVar, Args0, Kind0), Args,
        apply_n(TVar, Args0 ++ Args, Kind)) :-
    apply_type_args_to_kind(Kind0, Args, Kind).
apply_type_args(kinded(Type0, _), Args, Type) :-
    % We drop the explicit kind annotation, since:
    %   - it will already have been used by kind inference, and
    %   - it no longer corresponds to any explicit annotation given.
    apply_type_args(Type0, Args, Type).

:- pred apply_type_args_to_kind(kind::in, list(mer_type)::in, kind::out)
    is det.

apply_type_args_to_kind(Kind, [], Kind).
apply_type_args_to_kind(star, [_ | _], _) :-
    unexpected(this_file, "too many args in apply_n").
apply_type_args_to_kind(arrow(Kind0, Kind1), [ArgType | ArgTypes], Kind) :-
    ( get_type_kind(ArgType) = Kind0 ->
        apply_type_args_to_kind(Kind1, ArgTypes, Kind)
    ;
        unexpected(this_file, "kind error in apply_n")
    ).
apply_type_args_to_kind(variable(_), [_ | _], _) :-
    unexpected(this_file, "unbound kind variable").

:- pred ensure_type_has_kind(kind::in, mer_type::in, mer_type::out) is det.

ensure_type_has_kind(Kind, Type0, Type) :-
    ( get_type_kind(Type0) = Kind ->
        Type = Type0
    ;
        unexpected(this_file, "substitution not kind preserving")
    ).

%-----------------------------------------------------------------------------%

apply_rec_subst_to_prog_constraints(Subst, Constraints0, Constraints) :-
    Constraints0 = constraints(UnivCs0, ExistCs0),
    apply_rec_subst_to_prog_constraint_list(Subst, UnivCs0, UnivCs),
    apply_rec_subst_to_prog_constraint_list(Subst, ExistCs0, ExistCs),
    Constraints = constraints(UnivCs, ExistCs).

apply_rec_subst_to_prog_constraint_list(Subst, !Constraints) :-
    list__map(apply_rec_subst_to_prog_constraint(Subst), !Constraints).

apply_rec_subst_to_prog_constraint(Subst, Constraint0, Constraint) :-
    Constraint0 = constraint(ClassName, Types0),
    apply_rec_subst_to_type_list(Subst, Types0, Types),
    Constraint  = constraint(ClassName, Types).

apply_subst_to_prog_constraints(Subst,
        constraints(UniversalCs0, ExistentialCs0),
        constraints(UniversalCs, ExistentialCs)) :-
    apply_subst_to_prog_constraint_list(Subst, UniversalCs0, UniversalCs),
    apply_subst_to_prog_constraint_list(Subst, ExistentialCs0,
        ExistentialCs).

apply_subst_to_prog_constraint_list(Subst, !Constraints) :-
    list__map(apply_subst_to_prog_constraint(Subst), !Constraints).

apply_subst_to_prog_constraint(Subst, Constraint0, Constraint) :-
    Constraint0 = constraint(ClassName, Types0),
    apply_subst_to_type_list(Subst, Types0, Types),
    Constraint  = constraint(ClassName, Types).

apply_variable_renaming_to_prog_constraints(Renaming, Constraints0,
        Constraints) :-
    Constraints0 = constraints(UnivConstraints0, ExistConstraints0),
    apply_variable_renaming_to_prog_constraint_list(Renaming,
        UnivConstraints0, UnivConstraints),
    apply_variable_renaming_to_prog_constraint_list(Renaming,
        ExistConstraints0, ExistConstraints),
    Constraints = constraints(UnivConstraints, ExistConstraints).

apply_variable_renaming_to_prog_constraint_list(Renaming, !Constraints) :-
    list.map(apply_variable_renaming_to_prog_constraint(Renaming),
        !Constraints).

apply_variable_renaming_to_prog_constraint(Renaming, !Constraint) :-
    !.Constraint = constraint(ClassName, ClassArgTypes0),
    apply_variable_renaming_to_type_list(Renaming,
        ClassArgTypes0, ClassArgTypes),
    !:Constraint = constraint(ClassName, ClassArgTypes).

constraint_list_get_tvars(Constraints, TVars) :-
    list.map(constraint_get_tvars, Constraints, TVarsList),
    list.condense(TVarsList, TVars).

constraint_get_tvars(constraint(_Name, Args), TVars) :-
    prog_type.vars_list(Args, TVars).

get_unconstrained_tvars(Tvars, Constraints, Unconstrained) :-
    constraint_list_get_tvars(Constraints, ConstrainedTvars),
    list.delete_elems(Tvars, ConstrainedTvars, Unconstrained0),
    list.remove_dups(Unconstrained0, Unconstrained).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "prog_type.m".

%-----------------------------------------------------------------------------%

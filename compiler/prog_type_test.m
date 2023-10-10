%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_type_test.m.
%
% Tests on types and type constructors.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_type_test.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.

:- import_module list.

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

:- pred type_is_io_state(mer_type::in) is semidet.

:- pred type_ctor_is_array(type_ctor::in) is semidet.

:- pred type_ctor_is_bitmap(type_ctor::in) is semidet.

    % type_has_variable_arity_ctor(Type, TypeCtor, ArgTypes):
    %
    % Check if the principal type constructor of Type is of variable arity.
    % If yes, return the type constructor as TypeCtor and its args as
    % ArgTypes. If not, fail.
    %
:- pred type_has_variable_arity_ctor(mer_type::in, type_ctor::out,
    list(mer_type)::out) is semidet.

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
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_scan.

:- import_module require.
:- import_module term.

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

type_is_io_state(Type) :-
    type_to_ctor_and_args(Type, TypeCtor, []),
    ModuleName = mercury_io_module,
    TypeCtor = type_ctor(qualified(ModuleName, "state"), 0).

type_ctor_is_array(type_ctor(qualified(unqualified("array"), "array"), 1)).

type_ctor_is_bitmap(type_ctor(qualified(unqualified("bitmap"), "bitmap"), 0)).

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
:- end_module parse_tree.prog_type_test.
%---------------------------------------------------------------------------%

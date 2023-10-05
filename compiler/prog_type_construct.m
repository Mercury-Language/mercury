%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_type_construct.m.
%
% Predicates that construct types.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_type_construct.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

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
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_type_test.
:- import_module parse_tree.prog_util.

%---------------------------------------------------------------------------%

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
:- end_module parse_tree.prog_type_construct.
%---------------------------------------------------------------------------%

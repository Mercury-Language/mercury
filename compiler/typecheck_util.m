%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2014-2021, 2023-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck_til.m.
%
% This file contains utility predicates used by the typechecker.
%
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck_util.
:- interface.

:- import_module check_hlds.type_assign.
:- import_module hlds.
:- import_module hlds.hlds_class.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- func empty_hlds_constraint_db = hlds_constraint_db.

%---------------------------------------------------------------------------%

:- pred type_assign_fresh_type_var(prog_var::in, mer_type::out,
    type_assign::in, type_assign::out) is det.

%---------------------------------------------------------------------------%

    % Unify (with occurs check) two types in a type assignment
    % and update the type bindings.
    %
:- pred type_assign_unify_type(mer_type::in, mer_type::in,
    type_assign::in, type_assign::out) is semidet.

%---------------------------------------------------------------------------%

:- pred type_assign_rename_apart(type_assign::in, tvarset::in,
    type_assign::out, tvar_renaming::out) is det.

%---------------------------------------------------------------------------%

:- pred keep_type_assigns_where_var_can_have_type(prog_var::in, mer_type::in,
    type_assign_set::in, type_assign_set::out) is det.

:- pred acc_type_assign_if_var_can_have_type(type_assign::in,
    prog_var::in, mer_type::in,
    type_assign_set::in, type_assign_set::out) is det.

%---------------------------------------------------------------------------%

    % higher_order_pred_type(Purity, N, EvalMethod,
    %   TypeVarSet, PredType, ArgTypes):
    %
    % Given an arity N, let TypeVarSet = {T1, T2, ..., TN},
    % PredType = `Purity EvalMethod pred(T1, T2, ..., TN)', and
    % ArgTypes = [T1, T2, ..., TN].
    %
:- pred higher_order_pred_type(purity::in, int::in,
    tvarset::out, mer_type::out, list(mer_type)::out) is det.

    % higher_order_func_type(Purity, N, EvalMethod, TypeVarSet,
    %   FuncType, ArgTypes, RetType):
    %
    % Given an arity N, let TypeVarSet = {T0, T1, T2, ..., TN},
    % FuncType = `Purity EvalMethod func(T1, T2, ..., TN) = T0',
    % ArgTypes = [T1, T2, ..., TN], and
    % RetType = T0.
    %
:- pred higher_order_func_type(purity::in, int::in,
    tvarset::out, mer_type::out, list(mer_type)::out, mer_type::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_construct.
:- import_module parse_tree.prog_type_unify.
:- import_module parse_tree.vartypes.

:- import_module map.
:- import_module maybe.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

empty_hlds_constraint_db =
    hlds_constraint_db([], [], map.init, map.init).

%---------------------------------------------------------------------------%

type_assign_fresh_type_var(Var, Type, !TypeAssign) :-
    type_assign_get_var_types(!.TypeAssign, VarTypes0),
    type_assign_get_typevarset(!.TypeAssign, TypeVarSet0),
    varset.new_var(TypeVar, TypeVarSet0, TypeVarSet),
    type_assign_set_typevarset(TypeVarSet, !TypeAssign),
    Type = type_variable(TypeVar, kind_star),
    add_var_type(Var, Type, VarTypes0, VarTypes1),
    type_assign_set_var_types(VarTypes1, !TypeAssign).

%---------------------------------------------------------------------------%

type_assign_unify_type(X, Y, TypeAssign0, TypeAssign) :-
    type_assign_get_existq_tvars(TypeAssign0, ExistQTVars),
    type_assign_get_type_bindings(TypeAssign0, TypeBindings0),
    type_unify(X, Y, ExistQTVars, TypeBindings0, TypeBindings),
    type_assign_set_type_bindings(TypeBindings, TypeAssign0, TypeAssign).

%---------------------------------------------------------------------------%

type_assign_rename_apart(TypeAssign0, PredTypeVarSet,
        TypeAssign, Renaming) :-
    type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
    tvarset_merge_renaming(TypeVarSet0, PredTypeVarSet, TypeVarSet, Renaming),
    type_assign_set_typevarset(TypeVarSet, TypeAssign0, TypeAssign).

%---------------------------------------------------------------------------%

keep_type_assigns_where_var_can_have_type(Var, Type, !TypeAssignSet) :-
    acc_type_assigns_where_var_can_have_type(!.TypeAssignSet, Var, Type,
        [], !:TypeAssignSet).

:- pred acc_type_assigns_where_var_can_have_type(type_assign_set::in,
    prog_var::in, mer_type::in,
    type_assign_set::in, type_assign_set::out) is det.

acc_type_assigns_where_var_can_have_type([], _, _, !TypeAssignSet).
acc_type_assigns_where_var_can_have_type([TypeAssign0 | TypeAssigns0],
        Var, Type, !TypeAssignSet) :-
    acc_type_assign_if_var_can_have_type(TypeAssign0, Var, Type,
        !TypeAssignSet),
    acc_type_assigns_where_var_can_have_type(TypeAssigns0,
        Var, Type, !TypeAssignSet).

acc_type_assign_if_var_can_have_type(TypeAssign0, Var, Type, !TypeAssignSet) :-
    type_assign_get_var_types(TypeAssign0, VarTypes0),
    search_insert_var_type(Var, Type, MaybeOldVarType, VarTypes0, VarTypes),
    (
        MaybeOldVarType = yes(OldVarType),
        ( if
            type_assign_unify_type(OldVarType, Type, TypeAssign0, TypeAssign1)
        then
            !:TypeAssignSet = [TypeAssign1 | !.TypeAssignSet]
        else
            !:TypeAssignSet = !.TypeAssignSet
        )
    ;
        MaybeOldVarType = no,
        type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
        !:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
    ).

%---------------------------------------------------------------------------%

higher_order_pred_type(Purity, Arity, TypeVarSet, PredType, ArgTypes) :-
    varset.init(TypeVarSet0),
    varset.new_vars(Arity, ArgTypeVars, TypeVarSet0, TypeVarSet),
    % Argument types always have kind `star'.
    prog_type.var_list_to_type_list(map.init, ArgTypeVars, ArgTypes),
    construct_higher_order_type(Purity, pf_predicate, ArgTypes, PredType).

higher_order_func_type(Purity, Arity, TypeVarSet,
        FuncType, ArgTypes, RetType) :-
    varset.init(TypeVarSet0),
    varset.new_vars(Arity, ArgTypeVars, TypeVarSet0, TypeVarSet1),
    varset.new_var(RetTypeVar, TypeVarSet1, TypeVarSet),
    % Argument and return types always have kind `star'.
    prog_type.var_list_to_type_list(map.init, ArgTypeVars, ArgTypes),
    RetType = type_variable(RetTypeVar, kind_star),
    construct_higher_order_func_type(Purity, ArgTypes, RetType, FuncType).

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_util.
%---------------------------------------------------------------------------%

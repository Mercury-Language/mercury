%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2014-2021, 2023-2026 The Mercury team.
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
:- import_module hlds.hlds_data.
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

    % higher_order_pred_type(Purity, N,
    %   TypeVarSet, PredType, ArgTypes):
    %
    % Given Purity and an arity N, return
    % TypeVarSet = {T1, T2, ..., TN},
    % PredType = `Purity pred(T1, T2, ..., TN)', and
    % ArgTypes = [T1, T2, ..., TN].
    %
:- pred general_higher_order_pred_type(purity::in, int::in,
    tvarset::out, mer_type::out, list(mer_type)::out) is det.

    % higher_order_func_type(Purity, N,
    %   TypeVarSet, FuncType, ArgTypes, RetType):
    %
    % Given Purity and an arity N, return
    % TypeVarSet = {T0, T1, T2, ..., TN},
    % FuncType = `Purity func(T1, T2, ..., TN) = T0',
    % ArgTypes = [T1, T2, ..., TN], and
    % RetType = T0.
    %
:- pred general_higher_order_func_type(purity::in, int::in,
    tvarset::out, mer_type::out, list(mer_type)::out, mer_type::out) is det.

%---------------------------------------------------------------------------%

:- type maybe_du_type
    --->    is_du_type(du_type_info)
            % The type is a du type, with the given info.
    ;       is_not_du_type(string).
            % The type is not a du type. The string describes
            % what kind of type it is. The description allows code
            % that generates diagnostics to add the article "a" in front
            % of these pieces, and the plural suffix "s" after them.

:- type du_type_info
    --->    du_type_info(type_ctor, list(mer_type),
                hlds_type_defn, type_body_du).
            % This du type has the given type_ctor and argument types.
            % The last two arguments give the whole, and the du body part,
            % of the definition of the type_ctor.

    % If the given type is a du type, return its du_type_info.
    % Otherwise, return a description of what kind of non-du type it is.
    %
    % This predicate is called from both typecheck_coerce.m, and
    % typecheck_errors.m (which constructs diagnostics for the errors
    % discovered by typecheck_coerce.m.)
    %
:- pred classify_is_du_type(type_table::in, mer_type::in,
    maybe_du_type::out) is det.

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

general_higher_order_pred_type(Purity, Arity,
        TypeVarSet, PredType, ArgTypes) :-
    varset.init(TypeVarSet0),
    varset.new_vars(Arity, ArgTypeVars, TypeVarSet0, TypeVarSet),
    % Argument types always have kind `star'.
    prog_type.var_list_to_type_list(map.init, ArgTypeVars, ArgTypes),
    construct_higher_order_type(Purity, pf_predicate, ArgTypes, PredType).

general_higher_order_func_type(Purity, Arity,
        TypeVarSet, FuncType, ArgTypes, RetType) :-
    varset.init(TypeVarSet0),
    varset.new_vars(Arity, ArgTypeVars, TypeVarSet0, TypeVarSet1),
    varset.new_var(RetTypeVar, TypeVarSet1, TypeVarSet),
    % Argument and return types always have kind `star'.
    prog_type.var_list_to_type_list(map.init, ArgTypeVars, ArgTypes),
    RetType = type_variable(RetTypeVar, kind_star),
    construct_higher_order_func_type(Purity, ArgTypes, RetType, FuncType).

%---------------------------------------------------------------------------%

classify_is_du_type(TypeTable, Type, MaybeDuType) :-
    (
        Type = type_variable(_, _),
        MaybeDuType = is_not_du_type("type variable")
    ;
        Type = defined_type(SymName, ArgTypes, _Kind),
        list.length(ArgTypes, Arity),
        TypeCtor = type_ctor(SymName, Arity),
        ( if search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn) then
            get_type_defn_body(TypeDefn, TypeBody),
            (
                TypeBody = hlds_du_type(TypeBodyDu),
                DuType = du_type_info(TypeCtor, ArgTypes,
                    TypeDefn, TypeBodyDu),
                MaybeDuType = is_du_type(DuType)
            ;
                TypeBody = hlds_eqv_type(_),
                MaybeDuType = is_not_du_type("equivalence type")
            ;
                TypeBody = hlds_foreign_type(_),
                MaybeDuType = is_not_du_type("foreign type")
            ;
                TypeBody = hlds_solver_type(_),
                MaybeDuType = is_not_du_type("solver type")
            ;
                TypeBody = hlds_abstract_type(_),
                MaybeDuType = is_not_du_type("abstract type")
            )
        else
            MaybeDuType = is_not_du_type("unknown type")
        )
    ;
        Type = builtin_type(_),
        MaybeDuType = is_not_du_type("builtin type")
    ;
        Type = tuple_type(_, _),
        % XXX This code preserves old behavior, but it prevents programs
        % from coercing one tuple type to another, even if the tuple's
        % argument types are coerceable.
        MaybeDuType = is_not_du_type("tuple type")
    ;
        Type = higher_order_type(PorF, _, _, _),
        (
            PorF = pf_function,
            MaybeDuType = is_not_du_type("function type")
        ;
            PorF = pf_predicate,
            MaybeDuType = is_not_du_type("predicate type")
        )
    ;
        Type = apply_n_type(_, _, _),
        MaybeDuType = is_not_du_type("function type")
    ;
        Type = kinded_type(SubType, _),
        classify_is_du_type(TypeTable, SubType, MaybeDuType)
    ).


%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_util.
%---------------------------------------------------------------------------%

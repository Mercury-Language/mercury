%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_type_scan.m.
%
% Predicates that scan types (and related constructs) to see
% what they contain.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_type_scan.
:- interface.

:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module set.
:- import_module term.

%---------------------------------------------------------------------------%

    % Return the set of the type variables in a type, or in a list of types.
    %
:- pred set_of_type_vars_in_type(mer_type::in, set(tvar)::out) is det.
:- pred set_of_type_vars_in_types(list(mer_type)::in, set(tvar)::out) is det.

    % Return a list of the type variables in a type, or in a list of types,
    % in order of their first occurrence in a depth-first, left-right
    % traversal.
    %
:- pred type_vars_in_type(mer_type::in, list(tvar)::out) is det.
:- pred type_vars_in_types(list(mer_type)::in, list(tvar)::out) is det.

    % Nondeterministically return the variables in a type.
    %
:- pred type_contains_var(mer_type::in, tvar::out) is nondet.

    % Nondeterministically return the variables in a list of types.
    %
:- pred type_list_contains_var(list(mer_type)::in, tvar::out) is nondet.

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
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

set_of_type_vars_in_type(Type, SetOfTVars) :-
    type_vars_in_type(Type, TVars),
    set.list_to_set(TVars, SetOfTVars).

set_of_type_vars_in_types(Types, SetOfTVars) :-
    type_vars_in_types(Types, TVars),
    set.list_to_set(TVars, SetOfTVars).

%---------------------%

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

type_vars_in_type_acc(Type, !RevTVars) :-
    (
        Type = builtin_type(_)
    ;
        Type = type_variable(Var, _),
        !:RevTVars = [Var | !.RevTVars]
    ;
        ( Type = defined_type(_, ArgTypes, _)
        ; Type = tuple_type(ArgTypes, _)
        ; Type = higher_order_type(_, ArgTypes, _, _, _)
        ),
        type_vars_in_types_acc(ArgTypes, !RevTVars)
    ;
        Type = apply_n_type(Var, ArgTypes, _),
        !:RevTVars= [Var | !.RevTVars],
        type_vars_in_types_acc(ArgTypes, !RevTVars)
    ;
        Type = kinded_type(SubType, _),
        type_vars_in_type_acc(SubType, !RevTVars)
    ).

:- pred type_vars_in_types_acc(list(mer_type)::in,
    list(tvar)::in, list(tvar)::out) is det.

type_vars_in_types_acc([], !RevTVars).
type_vars_in_types_acc([Type | Types], !RevTVars) :-
    type_vars_in_type_acc(Type, !RevTVars),
    type_vars_in_types_acc(Types, !RevTVars).

%---------------------%

type_contains_var(Type, Var) :-
    (
        Type = type_variable(Var, _)
    ;
        ( Type = defined_type(_, ArgTypes, _)
        ; Type = tuple_type(ArgTypes, _)
        ; Type = higher_order_type(_, ArgTypes, _, _, _)
        ),
        type_list_contains_var(ArgTypes, Var)
    ;
        Type = apply_n_type(V, ArgTypes, _),
        (
            Var = V
        ;
            type_list_contains_var(ArgTypes, Var)
        )
    ;
        Type = kinded_type(SubType, _),
        type_contains_var(SubType, Var)
    ).

type_list_contains_var([Type | Types], Var) :-
    (
        type_contains_var(Type, Var)
    ;
        type_list_contains_var(Types, Var)
    ).

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
:- end_module parse_tree.prog_type_scan.
%---------------------------------------------------------------------------%

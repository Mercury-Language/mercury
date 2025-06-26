%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2025 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License as described in the file LICENCE.
%-----------------------------------------------------------------------------%
%
% File: mh_scope.m
% Main author: C4Cypher.
% Stability: low.
%
%-----------------------------------------------------------------------------%
%
% This is a regression test for github issue #137.
%
% The problem is a compiler crash with the error message:
%
% Software Error: predicate
%   `check_hlds.typecheck_coerce.
%       build_type_param_variance_restrictions_in_ctor_arg_type'/6:
%   Unexpected: hlds_eqv_type
%
% The crash occurs when the compiler typechecks the coerce on the
% second last line, and its immediate cause is the fact that the
% type table entry for the root_scope function symbol, for *both*
% the supertype mh_scope and the subtype mh_root_scope, has an argument
% whose type is an equivalence type. This type is exported as an abstract
% type from gh137_helper_1.m, but gh137_helper_1.{int,int2} both contain
% its definition as an equivalence type in their implementation section.

%-----------------------------------------------------------------------------%

:- module gh137.

:- interface.

:- type mh_scope.

:- pred parent(mh_scope::in, mh_scope::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module gh137_helper_1.

%-----------------------------------------------------------------------------%

:- type mh_scope
    --->    root_scope(var_id_set)
    ;       extended_scope(mh_root_scope)
    ;       child_scope(mh_scope).

:- type mh_root_scope =< mh_scope
    --->    root_scope(var_id_set)
    ;       extended_scope(mh_root_scope).

parent(Child, Parent) :-
    require_complete_switch [Child]
    (
        Child = root_scope(_),
        fail
    ;
        Child = child_scope(Parent)
    ;
        Child = extended_scope(Root),
        Parent = coerce(Root)
    ).

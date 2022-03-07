%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2010 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: univ.m.
% Main author: fjh.
% Stability: medium.
%
% The universal type `univ', which can represent values of any type
% chosen at runtime. This type is Mercury's mechanism to allow the
% deferral of some type checks from compile time to runtime.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module univ.
:- interface.

:- import_module type_desc.

%---------------------------------------------------------------------------%

    % A variable of type `univ' can hold the type and value
    % of any other variable of any type.
    %
:- type univ.

    % type_to_univ(Object, Univ).
    %
    % True iff the type stored in `Univ' is the same as the type of Object,
    % and the value stored in Univ is equal to the value of Object.
    %
    % Operationally,
    %
    % - the forward modes (the di,uo mode and the in,out mode)
    %   convert Object to type univ;
    %
    % - the reverse mode (out,in) checks whether the value stored in Univ
    %   is of type T. If this type test succeeds, it returns that value
    %   as Object, but if the test fails, it fails as well.
    %
:- pred type_to_univ(T, univ).
:- mode type_to_univ(di, uo) is det.
:- mode type_to_univ(in, out) is det.
:- mode type_to_univ(out, in) is semidet.

    % univ_to_type(Univ, Object) :- type_to_univ(Object, Univ).
    %
:- pred univ_to_type(univ, T).
:- mode univ_to_type(in, out) is semidet.
:- mode univ_to_type(out, in) is det.
:- mode univ_to_type(uo, di) is det.

    % The function univ/1 provides the same functionality as type_to_univ/2.
    % univ(Object) = Univ :- type_to_univ(Object, Univ).
    %
:- func univ(T) = univ.
:- mode univ(in) = out is det.
:- mode univ(di) = uo is det.
:- mode univ(out) = in is semidet.

    % det_univ_to_type(Univ, Object).
    %
    % The same as the forwards mode of univ_to_type, but throws an exception
    % if univ_to_type fails.
    %
:- pred det_univ_to_type(univ::in, T::out) is det.

    % univ_type(Univ).
    %
    % Returns the type_desc for the type stored in Univ.
    %
:- func univ_type(univ) = type_desc.

    % univ_value(Univ).
    %
    % Returns the value of the object stored in Univ.
    %
:- some [T] func univ_value(univ) = T.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

    % We call the constructor for univs `univ_cons' to avoid ambiguity
    % with the univ/1 function which returns a univ.
    %
:- type univ
    --->    some [T] univ_cons(T).

:- pragma promise_equivalent_clauses(pred(type_to_univ/2)).

type_to_univ(T::di, Univ::uo) :-
    Univ0 = 'new univ_cons'(T),
    unsafe_promise_unique(Univ0, Univ).
type_to_univ(T::in, Univ::out) :-
    Univ = 'new univ_cons'(T).
type_to_univ(T::out, Univ::in) :-
    Univ = univ_cons(T0),
    private_builtin.typed_unify(T0, T).

univ_to_type(Univ, X) :-
    type_to_univ(X, Univ).

univ(X) = Univ :-
    type_to_univ(X, Univ).

det_univ_to_type(Univ, X) :-
    ( if type_to_univ(X0, Univ) then
        X = X0
    else
        UnivTypeName = type_name(univ_type(Univ)),
        ObjectTypeName = type_name(type_of(X)),
        string.append_list(["det_univ_to_type: conversion failed\n",
            "\tUniv Type: ", UnivTypeName, "\n",
            "\tObject Type: ", ObjectTypeName], ErrorString),
        error(ErrorString)
    ).

univ_type(Univ) = type_of(univ_value(Univ)).

univ_value(univ_cons(X)) = X.

:- pred construct_univ(T::in, univ::out) is det.
:- pragma foreign_export("C", construct_univ(in, out), "ML_construct_univ").
:- pragma foreign_export("C#", construct_univ(in, out), "ML_construct_univ").
:- pragma foreign_export("Java", construct_univ(in, out), "ML_construct_univ").

construct_univ(X, Univ) :-
    Univ = univ(X).

:- some [T] pred unravel_univ(univ::in, T::out) is det.
:- pragma foreign_export("C", unravel_univ(in, out), "ML_unravel_univ").
:- pragma foreign_export("C#", unravel_univ(in, out), "ML_unravel_univ").
:- pragma foreign_export("Java", unravel_univ(in, out), "ML_unravel_univ").

unravel_univ(Univ, X) :-
    univ_value(Univ) = X.

%---------------------------------------------------------------------------%
:- end_module univ.
%---------------------------------------------------------------------------%

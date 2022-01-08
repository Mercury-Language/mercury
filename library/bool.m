%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-1997,2000,2002-2007,2009-2010 The University of Melbourne.
% Copyright (C) 2014-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: bool.m.
% Main authors: fjh, zs.
% Stability: medium to high.
%
% This module exports the boolean type `bool' and some operations on bools.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module bool.
:- interface.

:- import_module enum.
:- import_module list.

%---------------------------------------------------------------------------%

    % The boolean type.
    % Unlike most languages, we use `yes' and `no' as boolean constants
    % rather than `true' and `false'.  This is to avoid confusion
    % with the predicates `true' and `fail'.
:- type bool
    --->    no
    ;       yes.

:- instance enum(bool).

    % not(A) = yes iff A = no.
    %
:- func not(bool) = bool.
:- pred not(bool::in, bool::out) is det.

    % or(A, B) = yes iff A = yes, or B = yes, or both.
    %
:- func or(bool, bool) = bool.
:- pred or(bool::in, bool::in, bool::out) is det.

    % xor(A, B) = yes iff A = yes, or B = yes, but not both.
    %
:- func xor(bool, bool) = bool.

    % and(A, B) = yes iff A = yes and B = yes.
    %
:- func and(bool, bool) = bool.
:- pred and(bool::in, bool::in, bool::out) is det.

    % or_list(As) = yes iff there exists an element of As equal to yes.
    % (Note that or_list([]) = no.)
    %
:- func or_list(list(bool)) = bool.
:- pred or_list(list(bool)::in, bool::out) is det.

    % and_list(As) = yes iff every element of As is equal to yes.
    % (Note that and_list([]) = yes.)
    %
:- func and_list(list(bool)) = bool.
:- pred and_list(list(bool)::in, bool::out) is det.

    % pred_to_bool(P) = (if P then yes else no).
    %
:- func pred_to_bool((pred)::((pred) is semidet)) = (bool::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Important:
% The representation of bool values should correspond with the definitions of
% MR_TRUE and MR_FALSE in runtime/mercury_std.h.

:- pragma foreign_export_enum("C#", bool/0, [],
    [
        no  - "NO",
        yes - "YES"
    ]).

:- pragma foreign_export_enum("Java", bool/0, [],
    [
        no  - "NO",
        yes - "YES"
    ]).

:- instance enum(bool) where [
    to_int(Bool) = bool_to_int(Bool),
    from_int(bool_to_int(Bool)) = Bool
].

:- func bool_to_int(bool) = int.
:- mode bool_to_int(in) = out is det.
:- mode bool_to_int(out) = in is semidet.

bool_to_int(no) = 0.
bool_to_int(yes) = 1.

%---------------------------------------------------------------------------%

bool.not(X) = Result :-
    bool.not(X, Result).

bool.not(no, yes).
bool.not(yes, no).

bool.or(X, Y) = Result :-
    bool.or(X, Y, Result).

bool.or(yes, _, yes).
bool.or(no, Bool, Bool).

bool.xor(no,  no)  = no.
bool.xor(no,  yes) = yes.
bool.xor(yes, no)  = yes.
bool.xor(yes, yes) = no.

bool.and(X, Y) = Result :-
    bool.and(X, Y, Result).

bool.and(no, _, no).
bool.and(yes, Bool, Bool).

bool.or_list(List) = Result :-
    bool.or_list(List, Result).

bool.or_list([], no).
bool.or_list([Bool | Bools], Result) :-
    (
        Bool = yes,
        Result = yes
    ;
        Bool = no,
        bool.or_list(Bools, Result)
    ).

bool.and_list(List) = Result :-
    bool.and_list(List, Result).

bool.and_list([], yes).
bool.and_list([Bool | Bools], Result) :-
    (
        Bool = no,
        Result = no
    ;
        Bool = yes,
        bool.and_list(Bools, Result)
    ).

pred_to_bool(P) = (if P then yes else no).

%---------------------------------------------------------------------------%
:- end_module bool.
%---------------------------------------------------------------------------%

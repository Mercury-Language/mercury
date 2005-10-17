%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1996-1997,2000,2002-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: bool.m.
% Main authors: fjh, zs.
% Stability: medium to high.

% This module exports the boolean type `bool' and some operations on bools.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module bool.

:- interface.

:- import_module enum.
:- import_module list.

%-----------------------------------------------------------------------------%

    % The boolean type.
    % Unlike most languages, we use `yes' and `no' as boolean constants
    % rather than `true' and `false'.  This is to avoid confusion
    % with the predicates `true' and `fail'.
:- type bool
    --->    no
    ;       yes.

:- instance enum(bool).

:- func bool__or(bool, bool) = bool.
:- pred bool__or(bool::in, bool::in, bool::out) is det.

:- func bool__or_list(list(bool)) = bool.
:- pred bool__or_list(list(bool)::in, bool::out) is det.

:- func bool__and(bool, bool) = bool.
:- pred bool__and(bool::in, bool::in, bool::out) is det.

:- func bool__and_list(list(bool)) = bool.
:- pred bool__and_list(list(bool)::in, bool::out) is det.

:- func bool__not(bool) = bool.
:- pred bool__not(bool::in, bool::out) is det.

:- func bool__xor(bool, bool) = bool.

    % pred_to_bool(P) = (if P then yes else no).
    %
:- func pred_to_bool((pred)::((pred) is semidet)) = (bool::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% Important:
% The representation of bool values should correspond with the definitions of
% MR_TRUE and MR_FALSE in runtime/mercury_std.h.

:- instance enum(bool) where [
    to_int(Bool) = bool_to_int(Bool),
    from_int(bool_to_int(Bool)) = Bool
].

:- func bool_to_int(bool) = int.
:- mode bool_to_int(in) = out is det.
:- mode bool_to_int(out) = in is semidet.

bool_to_int(no) = 0.
bool_to_int(yes) = 1.

bool__or(X, Y) = Result :- bool__or(X, Y, Result).

bool__or(yes, _, yes).
bool__or(no, Bool, Bool).

bool__or_list(List) = Result :- bool__or_list(List, Result).

bool__or_list([], no).
bool__or_list([Bool | Bools], Result) :-
    (
        Bool = yes,
        Result = yes
    ;
        Bool = no,
        bool__or_list(Bools, Result)
    ).

bool__and(X, Y) = Result :- bool__and(X, Y, Result).

bool__and(no, _, no).
bool__and(yes, Bool, Bool).

bool__and_list(List) = Result :- bool__and_list(List, Result).

bool__and_list([], yes).
bool__and_list([Bool | Bools], Result) :-
    (
        Bool = no,
        Result = no
    ;
        Bool = yes,
        bool__and_list(Bools, Result)
    ).

bool__not(X) = Result :- bool__not(X, Result).

bool__not(no, yes).
bool__not(yes, no).

:- func bool__return_no = bool.
:- func bool__return_yes = bool.
:- pragma export(bool__return_no = out, "ML_bool_return_no").
:- pragma export(bool__return_yes = out, "ML_bool_return_yes").

bool__return_no = no.
bool__return_yes = yes.

pred_to_bool(P) = (if P then yes else no).

bool__xor(no,  no)  = no.
bool__xor(no,  yes) = yes.
bool__xor(yes, no)  = yes.
bool__xor(yes, yes) = no.

%-----------------------------------------------------------------------------%

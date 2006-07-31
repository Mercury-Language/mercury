%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1996-1997,2000,2002-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: bool.m.
% Main authors: fjh, zs.
% Stability: medium to high.
% 
% This module exports the boolean type `bool' and some operations on bools.
% 
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
    %
:- type bool
    --->    no
    ;       yes.

:- instance enum(bool).

:- func bool.or(bool, bool) = bool.
:- pred bool.or(bool::in, bool::in, bool::out) is det.

:- func bool.or_list(list(bool)) = bool.
:- pred bool.or_list(list(bool)::in, bool::out) is det.

:- func bool.and(bool, bool) = bool.
:- pred bool.and(bool::in, bool::in, bool::out) is det.

:- func bool.and_list(list(bool)) = bool.
:- pred bool.and_list(list(bool)::in, bool::out) is det.

:- func bool.not(bool) = bool.
:- pred bool.not(bool::in, bool::out) is det.

:- func bool.xor(bool, bool) = bool.

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

bool.or(X, Y) = Result :- bool.or(X, Y, Result).

bool.or(yes, _, yes).
bool.or(no, Bool, Bool).

bool.or_list(List) = Result :- bool.or_list(List, Result).

bool.or_list([], no).
bool.or_list([Bool | Bools], Result) :-
    (
        Bool = yes,
        Result = yes
    ;
        Bool = no,
        bool.or_list(Bools, Result)
    ).

bool.and(X, Y) = Result :- bool.and(X, Y, Result).

bool.and(no, _, no).
bool.and(yes, Bool, Bool).

bool.and_list(List) = Result :- bool.and_list(List, Result).

bool.and_list([], yes).
bool.and_list([Bool | Bools], Result) :-
    (
        Bool = no,
        Result = no
    ;
        Bool = yes,
        bool.and_list(Bools, Result)
    ).

bool.not(X) = Result :- bool.not(X, Result).

bool.not(no, yes).
bool.not(yes, no).

:- func bool.return_no = bool.
:- func bool.return_yes = bool.
:- pragma export(bool.return_no = out, "ML_bool_return_no").
:- pragma export(bool.return_yes = out, "ML_bool_return_yes").

bool.return_no = no.
bool.return_yes = yes.

pred_to_bool(P) = (if P then yes else no).

bool.xor(no,  no)  = no.
bool.xor(no,  yes) = yes.
bool.xor(yes, no)  = yes.
bool.xor(yes, yes) = no.

%-----------------------------------------------------------------------------%

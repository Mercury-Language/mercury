%---------------------------------------------------------------------------%
% Copyright (C) 1996-1997,2000,2002 The University of Melbourne.
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

:- import_module enum, list.

%-----------------------------------------------------------------------------%

% The boolean type.
% Unlike most languages, we use `yes' and `no' as boolean constants
% rather than `true' and `false'.  This is to avoid confusion
% with the predicates `true' and `fail'.

:- type bool ---> no ; yes.

:- instance enum(bool).

:- func bool__or(bool, bool) = bool.
:- pred bool__or(bool, bool, bool).
:- mode bool__or(in, in, out) is det.

:- func bool__or_list(list(bool)) = bool.
:- pred bool__or_list(list(bool), bool).
:- mode bool__or_list(in, out) is det.

:- func bool__and(bool, bool) = bool.
:- pred bool__and(bool, bool, bool).
:- mode bool__and(in, in, out) is det.

:- func bool__and_list(list(bool)) = bool.
:- pred bool__and_list(list(bool), bool).
:- mode bool__and_list(in, out) is det.

:- func bool__not(bool) = bool.
:- pred bool__not(bool, bool).
:- mode bool__not(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

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
	( Bool = yes ->
		Result = yes
	;
		bool__or_list(Bools, Result)
	).

bool__and(X, Y) = Result :- bool__and(X, Y, Result).

bool__and(no, _, no).
bool__and(yes, Bool, Bool).

bool__and_list(List) = Result :- bool__and_list(List, Result).

bool__and_list([], yes).
bool__and_list([Bool | Bools], Result) :-
	( Bool = no ->
		Result = no
	;
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

%-----------------------------------------------------------------------------%

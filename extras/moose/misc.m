%----------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury Distribution.
%----------------------------------------------------------------------------%

:- module misc.
:- interface.

:- import_module map.
:- import_module pair.
:- import_module set.

:- type '' ---> ''.

:- type (T1 -> T2) == map(T1, T2).

:- type (T1 - T2) == pair(T1, T2).

:- func empty = set(T).

:- func { T } = set(T).

:- func (set(T) /\ set(T)) = set(T).

:- func (set(T) \/ set(T)) = set(T).

:- func (set(T) - set(T)) = set(T).

:- implementation.

empty = Empty :-
	set__init(Empty).

{ Elem } = Set :- set__singleton_set(Set, Elem).

A /\ B = C :- set__intersect(A, B, C).

A \/ B = C :- set__union(A, B, C).

A - B = C :- set__difference(A, B, C).

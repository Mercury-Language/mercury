%---------------------------------------------------------------------------%
% Copyright (C) 1994-1997, 1999-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: set.m.
% Authors: zs.
% Stability: high.

% This file provides an interface to the 'set' ADT that is conducive to the
% use of state variable notation. The predicates here do the same thing as
% their counterparts in the set module; the only difference is the order of
% the arguments.

%--------------------------------------------------------------------------%

:- module svset.
:- interface.
:- import_module list, set.

	% `svset__insert(X, Set0, Set)' is true iff `Set' is the union of
	% `Set0' and the set containing only `X'.

:- pred svset__insert(T::in, set(T)::in, set(T)::out) is det.

	% `svset__insert_list(Xs, Set0, Set)' is true iff `Set' is the union of
	% `Set0' and the set containing only the members of `Xs'.

:- pred svset__insert_list(list(T)::in, set(T)::in, set(T)::out) is det.

	% `svset__delete(X, Set0, Set)' is true iff `Set' is the relative
	% complement of `Set0' and the set containing only `X', i.e.
	% if `Set' is the set which contains all the elements of `Set0'
	% except `X'.

:- pred svset__delete(T::in, set(T)::in, set(T)::out) is det.

	% `svset__delete_list(Xs, Set0, Set)' is true iff `Set' is the relative
	% complement of `Set0' and the set containing only the members of
	% `Xs'.

:- pred svset__delete_list(list(T)::in, set(T)::in, set(T)::out) is det.

	% `svset__remove(X, Set0, Set)' is true iff `Set0' contains `X',
	% and `Set' is the relative complement of `Set0' and the set
	% containing only `X', i.e.  if `Set' is the set which contains
	% all the elements of `Set0' except `X'.

:- pred svset__remove(T::in, set(T)::in, set(T)::out) is semidet.

	% `svset__remove_list(Xs, Set0, Set)' is true iff `Xs' does not
	% contain any duplicates, `Set0' contains every member of `Xs',
	% and `Set' is the relative complement of `Set0' and the set
	% containing only the members of `Xs'.

:- pred svset__remove_list(list(T)::in, set(T)::in, set(T)::out) is semidet.

	% `svset__remove_least(Elem, Set0, Set)' is true iff
	% `Set0' is not empty, `Elem' is the smallest element in `Set0'
	% (with elements ordered using the standard ordering given
	% by compare/3), and `Set' is the set containing all the
	% elements of `Set0' except `Elem'.

:- pred svset__remove_least(T::out, set(T)::in, set(T)::out) is semidet.

%--------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

:- import_module term.	% for var/1.

:- pragma type_spec(svset__insert/3, T = var(_)).

:- pragma type_spec(svset__insert_list/3, T = var(_)).

%-----------------------------------------------------------------------------%

:- implementation.

svset__insert(X, Set0, Set) :-
	set__insert(Set0, X, Set).

svset__insert_list(X, Set0, Set) :-
	set__insert_list(Set0, X, Set).

svset__delete(X, Set0, Set) :-
	set__delete(Set0, X, Set).

svset__delete_list(X, Set0, Set) :-
	set__delete_list(Set0, X, Set).

svset__remove(X, Set0, Set) :-
	set__remove(Set0, X, Set).

svset__remove_list(X, Set0, Set) :-
	set__remove_list(Set0, X, Set).

svset__remove_least(X, Set0, Set) :-
	set__remove_least(Set0, X, Set).

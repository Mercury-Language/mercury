%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

% File: set.nl.
% Main authors: conway, fjh.

% This file contains a `set' ADT.
% Sets are implemented here as unsorted lists, which may contain duplicates.

%--------------------------------------------------------------------------%

:- module set.
:- interface.
:- import_module list.

:- type set(_T).

	% `set__list_to_set(List, Set)' is true iff `Set' is the set 
	% containing only the members of `List'.

:- pred set__list_to_set(list(T), set(T)).
:- mode set__list_to_set(in, out) is det.

	% `set__sorted_list_to_set(List, Set)' is true iff `Set' is the set 
	% containing only the members of `List'.  `List' must be sorted.

:- pred set__sorted_list_to_set(list(T), set(T)).
:- mode set__sorted_list_to_set(in, out) is det.

	% `set__list_to_set(Set, List)' is true iff `List' is the list
	% of all the members of `Set', in sorted order.

:- pred set__to_sorted_list(set(T), list(T)).
:- mode set__to_sorted_list(in, out) is det.

	% `set__init(Set)' is true iff `Set' is an empty set.
	% `set__init(Set)' is true iff `Set' is an empty set.

:- pred set__init(set(_T)).
:- mode set__init(out) is det.

:- pred set__singleton_set(set(T), T).
:- mode set__singleton_set(in, out) is semidet.
:- mode set__singleton_set(out, in) is det.

	% `set__equal(SetA, SetB)' is true iff
	% `SetA' and `SetB' contain the same elements.

:- pred set__equal(set(T), set(T)).
:- mode set__equal(in, in) is semidet.

	% `set__subset(SetA, SetB)' is true iff `SetA' is a subset of `SetB'.

:- pred set__subset(set(T), set(T)).
:- mode set__subset(in, in).

	% `set__superset(SetA, SetB)' is true iff `SetA' is a
	% superset of `SetB'.

:- pred set__superset(set(T), set(T)).
:- mode set__superset(in, in).

	% `set_member(X, Set)' is true iff `X' is a member of `Set'.

:- pred set__member(T, set(T)).
:- mode set__member(in, in) is semidet.
:- mode set__member(out, in) is nondet.

	% `set__insert(Set0, X, Set)' is true iff `Set' is the union of
	% `Set0' and the set containing only `X'.

:- pred set__insert(set(T), T, set(T)).
:- mode set__insert(in, in, out) is det.

	% `set__insert_list(Set0, Xs, Set)' is true iff `Set' is the union of
	% `Set0' and the set containing only the members of `Xs'.

:- pred set__insert_list(set(T), list(T), set(T)).
:- mode set__insert_list(in, in, out) is det.

	% `set__delete(Set0, X, Set)' is true iff `Set0' contains `X',
	% and `Set' is the relative complement of `Set0' and the set
	% containing only `X', i.e.  if `Set' is the set which contains
	% all the elements of `Set0' except `X'.

:- pred set__delete(set(T), T, set(T)).
:- mode set__delete(in, out, out) is nondet.
:- mode set__delete(in, in, out) is semidet.

	% `set__delete_list(Set0, Xs, Set)' is true iff Xs does not
	% contain any duplicates, `Set0' contains every member of `Xs',
	% and `Set' is the relative complement of `Set0' and the set
	% containing only the members of `Xs'.

:- pred set__delete_list(set(T), list(T), set(T)).
:- mode set__delete_list(in, in, out) is semidet.

	% `set__remove(Set0, X, Set)' is true iff `Set' is the relative
	% complement of `Set0' and the set containing only `X', i.e.
	% if `Set' is the set which contains all the elements of `Set0'
	% except `X'.

:- pred set__remove(set(T), T, set(T)).
:- mode set__remove(in, in, out) is det.

	% `set__remove_list(Set0, Xs, Set)' is true iff `Set' is the relative
	% complement of `Set0' and the set containing only the members of
	% `Xs'.

:- pred set__remove_list(set(T), list(T), set(T)).
:- mode set__remove_list(in, in, out) is det.

	% `set_union(SetA, SetB, Set)' is true iff `Set' is the union of
	% `SetA' and `SetB'.  If the sets are known to be of different
	% sizes, then for efficiency make `SetA' the larger of the two.

:- pred set__union(set(T), set(T), set(T)).
:- mode set__union(in, in, out) is det.

	% `set_intersect(SetA, SetB, Set)' is true iff `Set' is the
	% intersection of `SetA' and `SetB'.

:- pred set__intersect(set(T), set(T), set(T)).
:- mode set__intersect(in, in, out).

%--------------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- type set(T)		  ==	  list(T).

set__list_to_set(List, List).

set__sorted_list_to_set(List, List).

set__to_sorted_list(Set, List) :-
	sort(Set, List).

:- set__insert_list(_, Xs, _) when Xs.	% NU-Prolog indexing.

set__insert_list(Set0, List, Set) :-
	append(List, Set0, Set).

set__insert(S0, E, [E|S0]).

set__init([]).

set__singleton_set([X], X).

set__equal(S1, S2) :-
	set__subset(S1, S2),
	set__subset(S2, S1).

set__subset([], _).
set__subset([E|S0], S1) :-
	set__member(E, S1),
	set__subset(S0, S1).

set__superset(S0, S1) :-
	set__subset(S1, S0).

set__member(E, S) :-
	member(E, S).

:- set__delete_list(_, Xs, _) when Xs.

set__delete_list(S, [], S).
set__delete_list(S0, [X | Xs], S) :-
	set__delete(S0, X, S1),
	set__delete_list(S1, Xs, S).

set__delete(S0, E, S) :-
	member(E, S0),
	set__remove(S0, E, S).

:- set__remove_list(_, Xs, _) when Xs.

set__remove_list(S, [], S).
set__remove_list(S0, [X | Xs], S) :-
	set__remove(S0, X, S1),
	set__remove_list(S1, Xs, S).

set__remove(Set0, Elem, Set) :-
	delete_all(Set0, Elem, Set).

set__union(Set0, Set1, Set) :-
	append(Set1, Set0, Set).

set__intersect(S0, S1, S) :-
	set__intersect_2(S0, S1, [], S).

:- pred set__intersect_2(set(T), set(T), set(T), set(T)).
:- mode set__intersect_2(in, in, in, out).

set__intersect_2([], _, S, S).
set__intersect_2([E|S0], S1, S2, S) :-
	(
		member(E, S1)
	->
		S3 = [E|S2]
	;
		S3 = S2
	),
	set__intersect_2(S0, S1, S3, S).

%--------------------------------------------------------------------------%

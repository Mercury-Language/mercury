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

:- pred set__empty(set(T)).
:- mode set__empty(in) is semidet.

	% `set__subset(SetA, SetB)' is true iff `SetA' is a subset of `SetB'.

:- pred set__subset(set(T), set(T)).
:- mode set__subset(in, in) is semidet.

	% `set__superset(SetA, SetB)' is true iff `SetA' is a
	% superset of `SetB'.

:- pred set__superset(set(T), set(T)).
:- mode set__superset(in, in) is semidet.

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
:- mode set__delete(in, in, out) is semidet.
:- mode set__delete(in, out, out) is nondet.

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

:- pred set__remove_least(set(T), T, set(T)).
:- mode set__remove_least(in, out, out) is semidet.

	% `set_union(SetA, SetB, Set)' is true iff `Set' is the union of
	% `SetA' and `SetB'.  If the sets are known to be of different
	% sizes, then for efficiency make `SetA' the larger of the two.

:- pred set__union(set(T), set(T), set(T)).
:- mode set__union(in, in, out) is det.

	% `set__power_union(A, B)' is true iff `B' is the union of
	% all the sets in `A'

:- pred set__power_union(set(set(T)), set(T)).
:- mode set__power_union(in, out) is det.

	% `set_intersect(SetA, SetB, Set)' is true iff `Set' is the
	% intersection of `SetA' and `SetB'.

:- pred set__intersect(set(T), set(T), set(T)).
:- mode set__intersect(in, in, out) is det.

	% `set__difference(SetA, SetB, Set)' is true iff `Set' is the
	% set containing all the elements of `SetA' except those that
	% occur in `SetB'

:- pred set__difference(set(T), set(T), set(T)).
:- mode set__difference(in, in, out) is det.

%--------------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- type set(T)		  ==	  list(T).

set__list_to_set(List, List).

set__sorted_list_to_set(List, List).

set__to_sorted_list(Set, List) :-
	list__sort(Set, List).

:- set__insert_list(_, Xs, _) when Xs.	% NU-Prolog indexing.

set__insert_list(Set0, List, Set) :-
	list__append(List, Set0, Set).

set__insert(S0, E, [E|S0]).

set__init([]).

set__singleton_set([X], X).

set__equal(S1, S2) :-
	set__subset(S1, S2),
	set__subset(S2, S1).

set__empty([]).

set__subset([], _).
set__subset([E|S0], S1) :-
	set__member(E, S1),
	set__subset(S0, S1).

set__superset(S0, S1) :-
	set__subset(S1, S0).

set__member(E, S) :-
	list__member(E, S).

:- set__delete_list(_, Xs, _) when Xs.

set__delete_list(S, [], S).
set__delete_list(S0, [X | Xs], S) :-
	set__delete(S0, X, S1),
	set__delete_list(S1, Xs, S).

set__delete(S0, E, S) :-
	list__member(E, S0),
	set__remove(S0, E, S).

:- set__remove_list(_, Xs, _) when Xs.

set__remove_list(S, [], S).
set__remove_list(S0, [X | Xs], S) :-
	set__remove(S0, X, S1),
	set__remove_list(S1, Xs, S).

set__remove(Set0, Elem, Set) :-
	list__delete_all(Set0, Elem, Set).

set__remove_least(Set0, E, Set) :-
	Set0 = [_|_],	% fail early on an empty set
	set__to_sorted_list(Set0, [E|Set]).

set__union(Set0, Set1, Set) :-
	list__append(Set1, Set0, Set).

set__power_union(PS, S) :-
	set__to_sorted_list(PS, SL),
	set__init(S0),
	set__power_union_2(SL, S0, S).

:- pred set__power_union_2(list(set(T)), set(T), set(T)).
:- mode set__power_union_2(in, in, out) is det.

set__power_union_2([], S, S).
set__power_union_2([T|Ts], S0, S) :-
	set__union(T, S0, S1),
	set__power_union_2(Ts, S1, S).

set__intersect(S0, S1, S) :-
	set__intersect_2(S0, S1, [], S).

:- pred set__intersect_2(set(T), set(T), set(T), set(T)).
:- mode set__intersect_2(in, in, in, out).

set__intersect_2([], _, S, S).
set__intersect_2([E|S0], S1, S2, S) :-
	(
		list__member(E, S1)
	->
		S3 = [E|S2]
	;
		S3 = S2
	),
	set__intersect_2(S0, S1, S3, S).

%--------------------------------------------------------------------------%

set__difference(A, B, C) :-
	set__difference_2(B, A, C).

:- pred set__difference_2(set(T), set(T), set(T)).
:- mode set__difference_2(in, in, out) is det.

set__difference_2([], C, C).
set__difference_2([E|Es], A, C) :-
	set__remove(A, E, B),
	set__difference_2(Es, B, C).

%--------------------------------------------------------------------------%

%---------------------------------------------------------------------------%
% Copyright (C) 1994-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: set.m.
% Main authors: conway, fjh, benyi.
% Stability: high.

% This module provides a set ADT. 
% The implementation represents sets using ordered lists.
% This file just calls the equivalent predicates in set_ordlist.

%--------------------------------------------------------------------------%

:- module set.
:- interface.
:- import_module bool, list.

:- type set(T).

	% `set__list_to_set(List, Set)' is true iff `Set' is the set 
	% containing only the members of `List'.

:- pred set__list_to_set(list(T), set(T)).
:- mode set__list_to_set(in, out) is det.

	% `set__sorted_list_to_set(List, Set)' is true iff `Set' is the set 
	% containing only the members of `List'.  `List' must be sorted
	% and must not contain any duplicates.

:- pred set__sorted_list_to_set(list(T), set(T)).
:- mode set__sorted_list_to_set(in, out) is det.

	% `set__to_sorted_list(Set, List)' is true iff `List' is the list
	% of all the members of `Set', in sorted order without any
	% duplicates.

:- pred set__to_sorted_list(set(T), list(T)).
:- mode set__to_sorted_list(in, out) is det.

	% `set__init(Set)' is true iff `Set' is an empty set.

:- pred set__init(set(T)).
:- mode set__init(uo) is det.

	% `set__singleton_set(Set, Elem)' is true iff `Set' is the set
	% containing just the single element `Elem'.

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

	% `set__member(X, Set)' is true iff `X' is a member of `Set'.

:- pred set__member(T, set(T)).
:- mode set__member(in, in) is semidet.
:- mode set__member(out, in) is nondet.

	% `set_is_member(X, Set, Result)' returns
	% `Result = yes' iff `X' is a member of `Set'.

:- pred set__is_member(T, set(T), bool).
:- mode set__is_member(in, in, out) is det.

	% `set__insert(Set0, X, Set)' is true iff `Set' is the union of
	% `Set0' and the set containing only `X'.

:- pred set__insert(set(T), T, set(T)).
:- mode set__insert(di, di, uo) is det.
:- mode set__insert(in, in, out) is det.

	% `set__insert_list(Set0, Xs, Set)' is true iff `Set' is the union of
	% `Set0' and the set containing only the members of `Xs'.

:- pred set__insert_list(set(T), list(T), set(T)).
:- mode set__insert_list(in, in, out) is det.

	% `set__delete(Set0, X, Set)' is true iff `Set' is the relative
	% complement of `Set0' and the set containing only `X', i.e.
	% if `Set' is the set which contains all the elements of `Set0'
	% except `X'.

:- pred set__delete(set(T), T, set(T)).
% :- mode set__delete(di, in, uo) is det.
:- mode set__delete(in, in, out) is det.

	% `set__delete_list(Set0, Xs, Set)' is true iff `Set' is the relative
	% complement of `Set0' and the set containing only the members of
	% `Xs'.

:- pred set__delete_list(set(T), list(T), set(T)).
:- mode set__delete_list(in, in, out) is det.

	% `set__remove(Set0, X, Set)' is true iff `Set0' contains `X',
	% and `Set' is the relative complement of `Set0' and the set
	% containing only `X', i.e.  if `Set' is the set which contains
	% all the elements of `Set0' except `X'.

:- pred set__remove(set(T), T, set(T)).
:- mode set__remove(in, in, out) is semidet.

	% `set__remove_list(Set0, Xs, Set)' is true iff `Xs' does not
	% contain any duplicates, `Set0' contains every member of `Xs',
	% and `Set' is the relative complement of `Set0' and the set
	% containing only the members of `Xs'.

:- pred set__remove_list(set(T), list(T), set(T)).
:- mode set__remove_list(in, in, out) is semidet.

	% `set__remove_least(Set0, Elem, Set)' is true iff
	% `Set0' is not empty, `Elem' is the smallest element in `Set0'
	% (with elements ordered using the standard ordering given
	% by compare/3), and `Set' is the set containing all the
	% elements of `Set0' except `Elem'.

:- pred set__remove_least(set(T), T, set(T)).
:- mode set__remove_least(in, out, out) is semidet.

	% `set_union(SetA, SetB, Set)' is true iff `Set' is the union of
	% `SetA' and `SetB'.  If the sets are known to be of different
	% sizes, then for efficiency make `SetA' the larger of the two.
	% (The current implementation using sorted lists with duplicates
	% removed is not sensitive to the ordering of the input arguments,
	% but other set implementations may be, so observing this convention
	% will make it less likely that you will encounter problems if
	% the implementation is changed.)

:- pred set__union(set(T), set(T), set(T)).
:- mode set__union(in, in, out) is det.

	% `set__power_union(A, B)' is true iff `B' is the union of
	% all the sets in `A'

:- pred set__power_union(set(set(T)), set(T)).
:- mode set__power_union(in, out) is det.

	% `set__intersect(SetA, SetB, Set)' is true iff `Set' is the
	% intersection of `SetA' and `SetB'. If the two sets are
	% known to be unequal in size, then making SetA be the larger
	% set will usually be more efficient.
	% (The current implementation, using sorted lists with duplicates
	% removed is not sensitive to the ordering of the input arguments
	% but other set implementations may be, so observing this convention
	% will make it less likely that you will encounter problems if
	% the implementation is changed.)

:- pred set__intersect(set(T), set(T), set(T)).
:- mode set__intersect(in, in, out) is det.

	% `set__power_intersect(A, B)' is true iff `B' is the intersection of
	% all the sets in `A'

:- pred set__power_intersect(set(set(T)), set(T)).
:- mode set__power_intersect(in, out) is det.

	% `set__difference(SetA, SetB, Set)' is true iff `Set' is the
	% set containing all the elements of `SetA' except those that
	% occur in `SetB'

:- pred set__difference(set(T), set(T), set(T)).
:- mode set__difference(in, in, out) is det.

	% `set__count(Set, Count)' is true iff `Set' has `Count' elements.

:- pred set__count(set(T), int).
:- mode set__count(in, out) is det.

%--------------------------------------------------------------------------%

:- implementation.

:- import_module set_ordlist, set_unordlist, require.

:- type set(T)		  ==	  set_ordlist(T).

set__list_to_set(List, Set) :-
	set_ordlist__list_to_set(List, Set).

set__sorted_list_to_set(List, Set) :-
	set_ordlist__sorted_list_to_set(List, Set).

set__to_sorted_list(Set, List) :-
	set_ordlist__to_sorted_list(Set, List).

set__insert_list(Set0, List, Set) :-
	set_ordlist__insert_list(Set0, List, Set).

set__insert(Set0, X, Set) :-
	set_ordlist__insert(Set0, X, Set).

set__init(Set) :-
	set_ordlist__init(Set).

set__singleton_set(Set, X) :-
	set_ordlist__singleton_set(Set, X).

set__equal(SetA, SetB) :-
	set_ordlist__equal(SetA, SetB).

set__empty(Set) :-
	set_ordlist__empty(Set).

set__subset(SetA, SetB) :-
	set_ordlist__subset(SetA, SetB).

set__superset(SetA, SetB) :-
	set_ordlist__superset(SetA, SetB).

set__member(X, Set) :-
	set_ordlist__member(X, Set).

set__is_member(X, Set, Result) :-
	set_ordlist__is_member(X, Set, Result).

set__delete_list(Set0, List, Set) :-
	set_ordlist__delete_list(Set0, List, Set).

set__delete(Set0, X, Set) :-
	set_ordlist__delete(Set0, X, Set).

set__remove_list(Set0, List, Set) :-
	set_ordlist__remove_list(Set0, List, Set).

set__remove(Set0, X, Set) :-
	set_ordlist__remove(Set0, X, Set).

set__remove_least(Set0, X, Set) :-
	set_ordlist__remove_least(Set0, X, Set).

set__union(SetA, SetB, Set) :-
	set_ordlist__union(SetA, SetB, Set).

set__power_union(Sets, Set) :-
	set_ordlist__power_union(Sets, Set).

set__intersect(SetA, SetB, Set) :-
	set_ordlist__intersect(SetA, SetB, Set).

set__power_intersect(Sets, Set) :-
	set_ordlist__power_intersect(Sets, Set).

set__difference(SetA, SetB, Set) :-
	set_ordlist__difference(SetA, SetB, Set).

set__count(Set, Count) :-
	set_ordlist__count(Set, Count).

%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

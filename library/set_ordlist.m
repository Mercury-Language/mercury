%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: set_ordlist.m.
% Main authors: conway, fjh.
% Stability: medium.

% This file contains a `set' ADT.
% Sets are implemented here as sorted lists without duplicates.

%--------------------------------------------------------------------------%

:- module set_ordlist.
:- interface.
:- import_module bool, list.

:- type set_ordlist(_T).

	% `set_ordlist__list_to_set(List, Set)' is true iff `Set' is the set 
	% containing only the members of `List'.

:- pred set_ordlist__list_to_set(list(T), set_ordlist(T)).
:- mode set_ordlist__list_to_set(in, out) is det.

	% `set_ordlist__sorted_list_to_set(List, Set)' is true iff `Set' is
	% the set containing only the members of `List'.  `List' must be sorted.

:- pred set_ordlist__sorted_list_to_set(list(T), set_ordlist(T)).
:- mode set_ordlist__sorted_list_to_set(in, out) is det.

	% `set_ordlist__to_sorted_list(Set, List)' is true iff `List' is the
	% list of all the members of `Set', in sorted order.

:- pred set_ordlist__to_sorted_list(set_ordlist(T), list(T)).
:- mode set_ordlist__to_sorted_list(in, out) is det.

	% `set_ordlist__init(Set)' is true iff `Set' is an empty set.

:- pred set_ordlist__init(set_ordlist(_T)).
:- mode set_ordlist__init(uo) is det.

	% `set_ordlist__singleton_set(Set, Elem)' is true iff `Set' is the set
	% containing just the single element `Elem'.

:- pred set_ordlist__singleton_set(set_ordlist(T), T).
:- mode set_ordlist__singleton_set(in, out) is semidet.
:- mode set_ordlist__singleton_set(out, in) is det.

	% `set_ordlist__equal(SetA, SetB)' is true iff
	% `SetA' and `SetB' contain the same elements.

:- pred set_ordlist__equal(set_ordlist(T), set_ordlist(T)).
:- mode set_ordlist__equal(in, in) is semidet.

	% `set_ordlist__empty(Set)' is true iff `Set' is an empty set.

:- pred set_ordlist__empty(set_ordlist(_T)).
:- mode set_ordlist__empty(in) is semidet.

	% `set_ordlist__subset(SetA, SetB)' is true iff `SetA' is a subset of
	% `SetB'.

:- pred set_ordlist__subset(set_ordlist(T), set_ordlist(T)).
:- mode set_ordlist__subset(in, in) is semidet.

	% `set_ordlist__superset(SetA, SetB)' is true iff `SetA' is a
	% superset of `SetB'.

:- pred set_ordlist__superset(set_ordlist(T), set_ordlist(T)).
:- mode set_ordlist__superset(in, in) is semidet.

	% `set_ordlist__member(X, Set)' is true iff `X' is a member of `Set'.

:- pred set_ordlist__member(T, set_ordlist(T)).
:- mode set_ordlist__member(in, in) is semidet.
:- mode set_ordlist__member(out, in) is nondet.

	% `set_ordlist__is_member(X, Set, Result)' returns
	% `Result = yes' iff `X' is a member of `Set'.

:- pred set_ordlist__is_member(T, set_ordlist(T), bool).
:- mode set_ordlist__is_member(in, in, out) is det.

	% `set_ordlist__insert(Set0, X, Set)' is true iff `Set' is the union
	% of `Set0' and the set containing only `X'.

:- pred set_ordlist__insert(set_ordlist(T), T, set_ordlist(T)).
:- mode set_ordlist__insert(di, di, uo) is det.
:- mode set_ordlist__insert(in, in, out) is det.

	% `set_ordlist__insert_list(Set0, Xs, Set)' is true iff `Set' is the
	% union of `Set0' and the set containing only the members of `Xs'.

:- pred set_ordlist__insert_list(set_ordlist(T), list(T), set_ordlist(T)).
:- mode set_ordlist__insert_list(in, in, out) is det.

	% `set_ordlist__delete(Set0, X, Set)' is true iff `Set' is the
	% relative complement of `Set0' and the set containing only `X', i.e.
	% if `Set' is the set which contains all the elements of `Set0'
	% except `X'.

:- pred set_ordlist__delete(set_ordlist(T), T, set_ordlist(T)).
% :- mode set_ordlist__delete(di, in, uo) is det.
:- mode set_ordlist__delete(in, in, out) is det.

	% `set_ordlist__delete_list(Set0, Xs, Set)' is true iff `Set' is the
	% relative complement of `Set0' and the set containing only the members
	% of `Xs'.

:- pred set_ordlist__delete_list(set_ordlist(T), list(T), set_ordlist(T)).
:- mode set_ordlist__delete_list(in, in, out) is det.

	% `set_ordlist__remove(Set0, X, Set)' is true iff `Set0' contains `X',
	% and `Set' is the relative complement of `Set0' and the set
	% containing only `X', i.e.  if `Set' is the set which contains
	% all the elements of `Set0' except `X'.

:- pred set_ordlist__remove(set_ordlist(T), T, set_ordlist(T)).
:- mode set_ordlist__remove(in, in, out) is semidet.

	% `set_ordlist__remove_list(Set0, Xs, Set)' is true iff Xs does not
	% contain any duplicates, `Set0' contains every member of `Xs',
	% and `Set' is the relative complement of `Set0' and the set
	% containing only the members of `Xs'.

:- pred set_ordlist__remove_list(set_ordlist(T), list(T), set_ordlist(T)).
:- mode set_ordlist__remove_list(in, in, out) is semidet.

	% `set_ordlist__remove_least(Set0, X, Set)' is true iff `X' is the
	% least element in `Set0', and `Set' is the set which contains all the
	% elements of `Set0' except `X'.

:- pred set_ordlist__remove_least(set_ordlist(T), T, set_ordlist(T)).
:- mode set_ordlist__remove_least(in, out, out) is semidet.

	% `set_ordlist_union(SetA, SetB, Set)' is true iff `Set' is the union
	% of `SetA' and `SetB'. The efficiency of the union operation is
	% O(card(SetA)+card(SetB)) and is not sensitive to the argument
	% ordering.

:- pred set_ordlist__union(set_ordlist(T), set_ordlist(T),
							set_ordlist(T)).
:- mode set_ordlist__union(in, in, out) is det.

	% `set_ordlist__power_union(A, B)' is true iff `B' is the union of
	% all the sets in `A'

:- pred set_ordlist__power_union(set_ordlist(set_ordlist(T)),
							set_ordlist(T)).
:- mode set_ordlist__power_union(in, out) is det.

	% `set_ordlist__intersect(SetA, SetB, Set)' is true iff `Set' is the
	% intersection of `SetA' and `SetB'. The efficiency of the intersection
	% operation is not influenced by the argument order.

:- pred set_ordlist__intersect(set_ordlist(T), set_ordlist(T),
							set_ordlist(T)).
:- mode set_ordlist__intersect(in, in, out) is det.
:- mode set_ordlist__intersect(in, in, in) is semidet.

	% `set_ordlist__power_intersect(A, B)' is true iff `B' is the
	% intersection of all the sets in `A'

:- pred set_ordlist__power_intersect(set_ordlist(set_ordlist(T)),
							set_ordlist(T)).
:- mode set_ordlist__power_intersect(in, out) is det.

	% `set_ordlist__difference(SetA, SetB, Set)' is true iff `Set' is the
	% set containing all the elements of `SetA' except those that
	% occur in `SetB'

:- pred set_ordlist__difference(set_ordlist(T), set_ordlist(T),
							set_ordlist(T)).
:- mode set_ordlist__difference(in, in, out) is det.

%--------------------------------------------------------------------------%

:- implementation.

:- import_module list, std_util.

:- type set_ordlist(T)		  ==	  list(T).

set_ordlist__list_to_set(List0, List) :-
	list__sort_and_remove_dups(List0, List).

set_ordlist__sorted_list_to_set(List0, List) :-
	list__remove_adjacent_dups(List0, List).

set_ordlist__to_sorted_list(List, List).

set_ordlist__insert_list(Set0, List0, Set) :-
	list__sort_and_remove_dups(List0, List),
	set_ordlist__union(List, Set0, Set).

set_ordlist__insert([], E, [E]).
set_ordlist__insert([I|Is], E, Js) :-
	compare(R, I, E),
	(
		R = (<),
		set_ordlist__insert(Is, E, Ks),
		Js = [I|Ks]
	;
		R = (=),
		Js = [I|Is]
	;
		R = (>),
		Js = [E,I|Is]
	).

set_ordlist__init([]).

set_ordlist__singleton_set([X], X).

set_ordlist__equal(Set, Set).

set_ordlist__empty([]).

set_ordlist__subset(Subset, Set) :-
	set_ordlist__intersect(Set, Subset, Subset).

set_ordlist__superset(Superset, Set) :-
	set_ordlist__subset(Set, Superset).

set_ordlist__member(E, S) :-
	list__member(E, S).

set_ordlist__is_member(E, S, R) :-
	( set_ordlist__member(E, S) ->
		R = yes
	;
		R = no
	).

set_ordlist__delete_list(S0, D, S) :-
	list__sort_and_remove_dups(D, DS),
	set_ordlist__difference(S0, DS, S).

set_ordlist__delete(Set0, Elem, Set) :-
	set_ordlist__difference(Set0, [Elem], Set).

set_ordlist__remove_list(Set0, Elems, Set) :-
	set_ordlist__sort_no_dups(Elems, ElemSet),
	set_ordlist__subset(ElemSet, Set0),
	set_ordlist__difference(Set0, ElemSet, Set).

	% set_ordlist__sort_no_dups(List, Set) is true iff
	% List is a list with the same elements as Set and
	% List contains no duplicates.
:- pred set_ordlist__sort_no_dups(list(T), set_ordlist(T)).
:- mode set_ordlist__sort_no_dups(in, out) is semidet.

set_ordlist__sort_no_dups(List, Set) :-
	list__sort(List, Set),
	(
		Set = []
	;
		Set = [Elem|Elems],
		set_ordlist__no_dups(Elem, Elems)
	).

	% set_ordlist__no_dups(Elem, Set) is true iff Set does not
	% contain Elem, and Set does not contains duplicates.
:- pred set_ordlist__no_dups(T::in, list(T)::in) is semidet.

set_ordlist__no_dups(_, []).
set_ordlist__no_dups(Elem, [Elem0|Elems]) :-
	Elem \= Elem0,
	set_ordlist__no_dups(Elem0, Elems).

set_ordlist__remove(Set0, Elem, Set) :-
	list__delete_first(Set0, Elem, Set).

set_ordlist__remove_least([Elem|Set], Elem, Set).

set_ordlist__union(Set0, Set1, Set) :-
	list__merge_and_remove_dups(Set0, Set1, Set).

set_ordlist__power_union(SetofSets, Set) :-
	set_ordlist__init(Set0),
	set_ordlist__power_union_2(SetofSets, Set0, Set).

:- pred set_ordlist__power_union_2(list(set_ordlist(T)), set_ordlist(T),
							set_ordlist(T)).
:- mode set_ordlist__power_union_2(in, in, out) is det.

set_ordlist__power_union_2([], Set, Set).
set_ordlist__power_union_2([NextSet|SetofSets], Set0, Set) :-
	set_ordlist__union(Set0, NextSet, Set1),
	set_ordlist__power_union_2(SetofSets, Set1, Set).

set_ordlist__intersect([], _, []).
set_ordlist__intersect([_|_], [], []).
set_ordlist__intersect([X|Xs], [Y|Ys], Set) :-
	compare(R, X, Y),
	(
		R = (<),
		set_ordlist__intersect(Xs, [Y|Ys], Set)
	;
		R = (=),
		set_ordlist__intersect(Xs, Ys, Set0),
		Set = [X|Set0]
	;
		R = (>),
		set_ordlist__intersect([X|Xs], Ys, Set)
	).

set_ordlist__power_intersect([], []).
set_ordlist__power_intersect([S0|Ss], S) :-
	(
		Ss = []
	->
		S = S0
	;
		set_ordlist__power_intersect(Ss, S1),
		set_ordlist__intersect(S1, S0, S)
	).

%--------------------------------------------------------------------------%

set_ordlist__difference([], _, []).
set_ordlist__difference([X|Xs], [], [X|Xs]).
set_ordlist__difference([X|Xs], [Y|Ys], Set) :-
	compare(R, X, Y),
	(
		R = (<),
		set_ordlist__difference(Xs, [Y|Ys], Set0),
		Set = [X|Set0]
	;
		R = (=),
		set_ordlist__difference(Xs, Ys, Set)
	;
		R = (>),
		set_ordlist__difference([X|Xs], Ys, Set)
	).

%--------------------------------------------------------------------------%

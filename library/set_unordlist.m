%---------------------------------------------------------------------------%
% Copyright (C) 1995-1997,1999-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: set_unordlist.m.
% Main authors: conway, fjh.
% Stability: medium.

% This file contains a `set' ADT.
% Sets are implemented here as unsorted lists, which may contain duplicates.

%--------------------------------------------------------------------------%

:- module set_unordlist.
:- interface.
:- import_module bool, list.

:- type set_unordlist(_T).

	% `set_unordlist__list_to_set(List, Set)' is true iff `Set' is the set 
	% containing only the members of `List'.

:- pred set_unordlist__list_to_set(list(T), set_unordlist(T)).
:- mode set_unordlist__list_to_set(in, out) is det.

:- func set_unordlist__list_to_set(list(T)) = set_unordlist(T).
	% `set_unordlist__sorted_list_to_set(List, Set)' is true iff `Set' is
	% the set containing only the members of `List'.  `List' must be sorted.

:- pred set_unordlist__sorted_list_to_set(list(T), set_unordlist(T)).
:- mode set_unordlist__sorted_list_to_set(in, out) is det.

:- func set_unordlist__sorted_list_to_set(list(T)) = set_unordlist(T).

	% `set_unordlist__to_sorted_list(Set, List)' is true iff `List' is the
	% list of all the members of `Set', in sorted order.

:- pred set_unordlist__to_sorted_list(set_unordlist(T), list(T)).
:- mode set_unordlist__to_sorted_list(in, out) is det.

:- func set_unordlist__to_sorted_list(set_unordlist(T)) = list(T).

	% `set_unordlist__init(Set)' is true iff `Set' is an empty set.

:- pred set_unordlist__init(set_unordlist(_T)).
:- mode set_unordlist__init(uo) is det.

:- func set_unordlist__init = set_unordlist(T).

	% `set_unordlist__singleton_set(Set, Elem)' is true iff `Set' is the set
	% containing just the single element `Elem'.

:- pred set_unordlist__singleton_set(set_unordlist(T), T).
:- mode set_unordlist__singleton_set(in, out) is semidet.
:- mode set_unordlist__singleton_set(out, in) is det.

:- func set_unordlist__make_singleton_set(T) = set_unordlist(T).

	% `set_unordlist__equal(SetA, SetB)' is true iff
	% `SetA' and `SetB' contain the same elements.

:- pred set_unordlist__equal(set_unordlist(T), set_unordlist(T)).
:- mode set_unordlist__equal(in, in) is semidet.

	% `set_unordlist__empty(Set)' is true iff `Set' is an empty set.

:- pred set_unordlist__empty(set_unordlist(_T)).
:- mode set_unordlist__empty(in) is semidet.

	% `set_unordlist__subset(SetA, SetB)' is true iff `SetA' is a subset of
	% `SetB'.

:- pred set_unordlist__subset(set_unordlist(T), set_unordlist(T)).
:- mode set_unordlist__subset(in, in) is semidet.

	% `set_unordlist__superset(SetA, SetB)' is true iff `SetA' is a
	% superset of `SetB'.

:- pred set_unordlist__superset(set_unordlist(T), set_unordlist(T)).
:- mode set_unordlist__superset(in, in) is semidet.

	% `set_unordlist__member(X, Set)' is true iff `X' is a member of `Set'.

:- pred set_unordlist__member(T, set_unordlist(T)).
:- mode set_unordlist__member(in, in) is semidet.
:- mode set_unordlist__member(out, in) is nondet.

	% `set_unordlist__is_member(X, Set, Result)' returns
	% `Result = yes' iff `X' is a member of `Set'.

:- pred set_unordlist__is_member(T, set_unordlist(T), bool).
:- mode set_unordlist__is_member(in, in, out) is det.

	% `set_unordlist__contains(Set, X)' is true iff
	% `X' is a member of `Set'.

:- pred set_unordlist__contains(set_unordlist(T), T).
:- mode set_unordlist__contains(in, in) is semidet.

	% `set_unordlist__insert(Set0, X, Set)' is true iff `Set' is the union
	% of `Set0' and the set containing only `X'.

:- pred set_unordlist__insert(set_unordlist(T), T, set_unordlist(T)).
:- mode set_unordlist__insert(di, di, uo) is det.
:- mode set_unordlist__insert(in, in, out) is det.

:- func set_unordlist__insert(set_unordlist(T), T) = set_unordlist(T).

	% `set_unordlist__insert_list(Set0, Xs, Set)' is true iff `Set' is the
	% union of `Set0' and the set containing only the members of `Xs'.

:- pred set_unordlist__insert_list(set_unordlist(T), list(T),
					set_unordlist(T)).
:- mode set_unordlist__insert_list(in, in, out) is det.

:- func set_unordlist__insert_list(set_unordlist(T), list(T))
		= set_unordlist(T).

	% `set_unordlist__delete(Set0, X, Set)' is true iff `Set' is the
	% relative complement of `Set0' and the set containing only `X', i.e.
	% if `Set' is the set which contains all the elements of `Set0'
	% except `X'.

:- pred set_unordlist__delete(set_unordlist(T), T, set_unordlist(T)).
:- mode set_unordlist__delete(di, in, uo) is det.
:- mode set_unordlist__delete(in, in, out) is det.

:- func set_unordlist__delete(set_unordlist(T), T) = set_unordlist(T).

	% `set_unordlist__delete_list(Set0, Xs, Set)' is true iff `Set' is the
	% relative complement of `Set0' and the set containing only the members
	% of `Xs'.

:- pred set_unordlist__delete_list(set_unordlist(T), list(T),
					set_unordlist(T)).
:- mode set_unordlist__delete_list(in, in, out) is det.

:- func set_unordlist__delete_list(set_unordlist(T), list(T))
		= set_unordlist(T).

	% `set_unordlist__remove(Set0, X, Set)' is true iff `Set0' contains `X',
	% and `Set' is the relative complement of `Set0' and the set
	% containing only `X', i.e.  if `Set' is the set which contains
	% all the elements of `Set0' except `X'.

:- pred set_unordlist__remove(set_unordlist(T), T, set_unordlist(T)).
:- mode set_unordlist__remove(in, in, out) is semidet.

	% `set_unordlist__remove_list(Set0, Xs, Set)' is true iff Xs does not
	% contain any duplicates, `Set0' contains every member of `Xs',
	% and `Set' is the relative complement of `Set0' and the set
	% containing only the members of `Xs'.

:- pred set_unordlist__remove_list(set_unordlist(T), list(T),
					set_unordlist(T)).
:- mode set_unordlist__remove_list(in, in, out) is semidet.

	% `set_unordlist__remove_least(Set0, X, Set)' is true iff `X' is the
	% least element in `Set0', and `Set' is the set which contains all the
	% elements of `Set0' except `X'.

:- pred set_unordlist__remove_least(set_unordlist(T), T, set_unordlist(T)).
:- mode set_unordlist__remove_least(in, out, out) is semidet.

	% `set_unordlist_union(SetA, SetB, Set)' is true iff `Set' is the union
	% of `SetA' and `SetB'.  If the sets are known to be of different
	% sizes, then for efficiency make `SetA' the larger of the two.

:- pred set_unordlist__union(set_unordlist(T), set_unordlist(T),
							set_unordlist(T)).
:- mode set_unordlist__union(in, in, out) is det.

:- func set_unordlist__union(set_unordlist(T), set_unordlist(T))
		= set_unordlist(T).

	% `set_unordlist__union_list(A) = B' is true iff `B' is the union of
	% all the sets in `A'

:- func set_unordlist__union_list(list(set_unordlist(T)))
		= set_unordlist(T).

	% `set_unordlist__power_union(A, B)' is true iff `B' is the union of
	% all the sets in `A'

:- pred set_unordlist__power_union(set_unordlist(set_unordlist(T)),
							set_unordlist(T)).
:- mode set_unordlist__power_union(in, out) is det.

:- func set_unordlist__power_union(set_unordlist(set_unordlist(T)))
		= set_unordlist(T).

	% `set_unordlist__intersect(SetA, SetB, Set)' is true iff `Set' is the
	% intersection of `SetA' and `SetB'.

:- pred set_unordlist__intersect(set_unordlist(T), set_unordlist(T),
							set_unordlist(T)).
:- mode set_unordlist__intersect(in, in, out) is det.

:- func set_unordlist__intersect(set_unordlist(T), set_unordlist(T))
		= set_unordlist(T).

	% `set_unordlist__power_intersect(A, B)' is true iff `B' is the
	% intersection of all the sets in `A'

:- pred set_unordlist__power_intersect(set_unordlist(set_unordlist(T)),
							set_unordlist(T)).
:- mode set_unordlist__power_intersect(in, out) is det.

:- func set_unordlist__power_intersect(set_unordlist(set_unordlist(T)))
		= set_unordlist(T).

	% `set_unordlist__intersect_list(A, B)' is true iff `B' is the
	% intersection of all the sets in `A'

:- func set_unordlist__intersect_list(list(set_unordlist(T)))
		= set_unordlist(T).

	% `set_unordlist__difference(SetA, SetB, Set)' is true iff `Set' is the
	% set containing all the elements of `SetA' except those that
	% occur in `SetB'

:- pred set_unordlist__difference(set_unordlist(T), set_unordlist(T),
							set_unordlist(T)).
:- mode set_unordlist__difference(in, in, out) is det.

:- func set_unordlist__difference(set_unordlist(T), set_unordlist(T))
		= set_unordlist(T).


:- func set_unordlist__map(func(T1) = T2, set_unordlist(T1))
		= set_unordlist(T2).

:- func set_unordlist__filter_map(func(T1) = T2, set_unordlist(T1))
		= set_unordlist(T2).
:- mode set_unordlist__filter_map(func(in) = out is semidet, in) = out is det.

:- func set_unordlist__fold(func(T1, T2) = T2, set_unordlist(T1), T2) = T2.

	% set_unordlist__divide(Pred, Set, TruePart, FalsePart):
	% TruePart consists of those elements of Set for which Pred succeeds;
	% FalsePart consists of those elements of Set for which Pred fails.
:- pred set_unordlist__divide(pred(T1), set_unordlist(T1), set_unordlist(T1),
	set_unordlist(T1)).
:- mode set_unordlist__divide(pred(in) is semidet, in, out, out) is det.

%--------------------------------------------------------------------------%

:- implementation.

:- import_module list, std_util.

:- type set_unordlist(T)		  ==	  list(T).

set_unordlist__list_to_set(List, List).

set_unordlist__sorted_list_to_set(List, List).

set_unordlist__to_sorted_list(Set, List) :-
	list__sort_and_remove_dups(Set, List).

set_unordlist__insert_list(Set0, List, Set) :-
	list__append(List, Set0, Set).

set_unordlist__insert(S0, E, [E|S0]).

set_unordlist__init([]).

set_unordlist__singleton_set([X], X).

set_unordlist__equal(SetA, SetB) :-
	set_unordlist__subset(SetA, SetB),
	set_unordlist__subset(SetB, SetA).

set_unordlist__empty([]).

set_unordlist__subset([], _).
set_unordlist__subset([E|S0], S1) :-
	set_unordlist__member(E, S1),
	set_unordlist__subset(S0, S1).

set_unordlist__superset(S0, S1) :-
	set_unordlist__subset(S1, S0).

set_unordlist__member(E, S) :-
	list__member(E, S).

set_unordlist__is_member(E, S, R) :-
	( set_unordlist__member(E, S) ->
		R = yes
	;
		R = no
	).

set_unordlist__contains(S, E) :-
	set_unordlist__member(E, S).

set_unordlist__delete_list(S, [], S).
set_unordlist__delete_list(S0, [X | Xs], S) :-
	set_unordlist__delete(S0, X, S1),
	set_unordlist__delete_list(S1, Xs, S).

set_unordlist__delete(S0, E, S) :-
	list__delete_all(S0, E, S).

set_unordlist__remove_list(S, [], S).
set_unordlist__remove_list(S0, [X | Xs], S) :-
	set_unordlist__remove(S0, X, S1),
	set_unordlist__remove_list(S1, Xs, S).

set_unordlist__remove(S0, E, S) :-
	list__member(E, S0),
	set_unordlist__delete(S0, E, S).

set_unordlist__remove_least(Set0, E, Set) :-
	Set0 = [_|_],	% fail early on an empty set
	set_unordlist__to_sorted_list(Set0, [E|Set]).

set_unordlist__union(Set0, Set1, Set) :-
	list__append(Set1, Set0, Set).

set_unordlist__union_list(LS) = S :-
	set_unordlist__power_union(LS, S).

set_unordlist__power_union(PS, S) :-
	set_unordlist__init(S0),
	set_unordlist__power_union_2(PS, S0, S1),
	list__sort_and_remove_dups(S1, S).

:- pred set_unordlist__power_union_2(list(set_unordlist(T)), set_unordlist(T),
							set_unordlist(T)).
:- mode set_unordlist__power_union_2(in, in, out) is det.

set_unordlist__power_union_2([], S, S).
set_unordlist__power_union_2([T|Ts], S0, S) :-
	set_unordlist__union(S0, T, S1),
	set_unordlist__power_union_2(Ts, S1, S).

set_unordlist__intersect(S0, S1, S) :-
	set_unordlist__intersect_2(S0, S1, [], S).

:- pred set_unordlist__intersect_2(set_unordlist(T), set_unordlist(T),
					set_unordlist(T), set_unordlist(T)).
:- mode set_unordlist__intersect_2(in, in, in, out) is det.

set_unordlist__intersect_2([], _, S, S).
set_unordlist__intersect_2([E|S0], S1, S2, S) :-
	(
		list__member(E, S1)
	->
		S3 = [E|S2]
	;
		S3 = S2
	),
	set_unordlist__intersect_2(S0, S1, S3, S).

set_unordlist__power_intersect([], []).
set_unordlist__power_intersect([S0|Ss], S) :-
	(
		Ss = []
	->
		S = S0
	;
		set_unordlist__power_intersect(Ss, S1),
		set_unordlist__intersect(S1, S0, S)
	).

set_unordlist__intersect_list(Sets) = 
	set_unordlist__power_intersect(Sets).

%--------------------------------------------------------------------------%

set_unordlist__difference(A, B, C) :-
	set_unordlist__difference_2(B, A, C).

:- pred set_unordlist__difference_2(set_unordlist(T), set_unordlist(T),
							set_unordlist(T)).
:- mode set_unordlist__difference_2(in, in, out) is det.

set_unordlist__difference_2([], C, C).
set_unordlist__difference_2([E|Es], A, C) :-
	set_unordlist__delete(A, E, B),
	set_unordlist__difference_2(Es, B, C).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cam.sri.com> 24/04/99
%	Function forms added.

set_unordlist__list_to_set(Xs) = S :-
	set_unordlist__list_to_set(Xs, S).

set_unordlist__sorted_list_to_set(Xs) = S :-
	set_unordlist__sorted_list_to_set(Xs, S).

set_unordlist__to_sorted_list(S) = Xs :-
	set_unordlist__to_sorted_list(S, Xs).

set_unordlist__init = S :-
	set_unordlist__init(S).

set_unordlist__make_singleton_set(T) = S :-
	set_unordlist__singleton_set(S, T).

set_unordlist__insert(S1, T) = S2 :-
	set_unordlist__insert(S1, T, S2).

set_unordlist__insert_list(S1, Xs) = S2 :-
	set_unordlist__insert_list(S1, Xs, S2).

set_unordlist__delete(S1, T) = S2 :-
	set_unordlist__delete(S1, T, S2).

set_unordlist__delete_list(S1, Xs) = S2 :-
	set_unordlist__delete_list(S1, Xs, S2).

set_unordlist__union(S1, S2) = S3 :-
	set_unordlist__union(S1, S2, S3).

set_unordlist__power_union(SS) = S :-
	set_unordlist__power_union(SS, S).

set_unordlist__intersect(S1, S2) = S3 :-
	set_unordlist__intersect(S1, S2, S3).

set_unordlist__power_intersect(SS) = S :-
	set_unordlist__power_intersect(SS, S).

set_unordlist__difference(S1, S2) = S3 :-
	set_unordlist__difference(S1, S2, S3).

set_unordlist__map(F, S1) = S2 :-
	S2 = set_unordlist__list_to_set(list__map(F,
			set_unordlist__to_sorted_list(S1))).

set_unordlist__filter_map(PF, S1) = S2 :-
	S2 = set_unordlist__list_to_set(list__filter_map(PF,
			set_unordlist__to_sorted_list(S1))).

set_unordlist__fold(F, S, A) = B :-
	B = list__foldl(F, set_unordlist__to_sorted_list(S), A).

set_unordlist__divide(Pred, Set, RevTruePart, RevFalsePart) :-
	set_unordlist__divide_2(Pred, Set, [], RevTruePart, [], RevFalsePart).

:- pred set_unordlist__divide_2(pred(T1), set_unordlist(T1),
	set_unordlist(T1), set_unordlist(T1),
	set_unordlist(T1), set_unordlist(T1)).
:- mode set_unordlist__divide_2(pred(in) is semidet, in, in, out, in, out)
	is det.

set_unordlist__divide_2(_Pred, [], RevTrue, RevTrue, RevFalse, RevFalse).
set_unordlist__divide_2(Pred, [H | T],
		RevTrue0, RevTrue, RevFalse0, RevFalse) :-
	( call(Pred, H) ->
		RevTrue1 = [H | RevTrue0],
		RevFalse1 = RevFalse0
	;
		RevTrue1 = RevTrue0,
		RevFalse1 = [H | RevFalse0]
	),
	set_unordlist__divide_2(Pred, T,
		RevTrue1, RevTrue, RevFalse1, RevFalse).

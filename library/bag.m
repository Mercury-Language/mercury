%---------------------------------------------------------------------------%
% Copyright (C) 1994-1999, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% file: bag.m
%	An implementation of multisets.
% main author: conway, crs.
% stability: medium
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module bag.

:- interface.

:- import_module list, assoc_list, set.

:- type bag(T).

	% Create an empty bag.
	%
:- pred bag__init(bag(T)::out) is det.
:- func bag__init = bag(T).

	% Insert a particular value in a bag.
	%
:- pred bag__insert(bag(T)::in, T::in, bag(T)::out) is det.
:- func bag__insert(bag(T), T) = bag(T).

	% Insert a list of values into a bag.
	%
:- pred bag__insert_list(bag(T)::in, list(T)::in, bag(T)::out) is det.
:- func bag__insert_list(bag(T), list(T)) = bag(T).

	% Insert a list of values into a bag.
	%
:- pred bag__insert_set(bag(T)::in, set(T)::in, bag(T)::out) is det.
:- func bag__insert_set(bag(T), set(T)) = bag(T).

	% Make a bag from a list.
	%
:- pred bag__from_list(list(T)::in, bag(T)::out) is det.
:- func bag__from_list(list(T)) = bag(T).

	% Make a bag from a set.
	%
:- pred bag__from_set(set(T)::in, bag(T)::out) is det.
:- func bag__from_set(set(T)) = bag(T).

	% Given a bag, produce a sorted list containing all the values in
	% the bag.  Each value will appear in the list the same number of
	% times that it appears in the bag.
	%
:- pred bag__to_list(bag(T)::in, list(T)::out) is det.
:- func bag__to_list(bag(T)) = list(T).

	% Given a bag, produce a sorted list containing all the values in
	% the bag.  Each value will appear in the list once, with the
	% associated integer giving the number of times that it appears
	% in the bag.
	%
:- pred bag__to_assoc_list(bag(T)::in, assoc_list(T, int)::out) is det.
:- func bag__to_assoc_list(bag(T)) = assoc_list(T, int).

	% Given a bag, produce a sorted list with no duplicates
	% containing all the values in the bag.
	%
:- pred bag__to_list_without_duplicates(bag(T)::in, list(T)::out) is det.
:- func bag__to_list_without_duplicates(bag(T)) = list(T).

	% Given a bag, produce a set containing all the values in the bag.
	%
:- pred bag__to_set_without_duplicates(bag(T)::in, set(T)::out) is det.
:- func bag__to_set_without_duplicates(bag(T)) = set(T).

	% Remove one occurrence of a particular value from a bag.
	% Fail if the item does not exist in the bag.
	%
:- pred bag__remove(bag(T)::in, T::in, bag(T)::out) is semidet.

	% Remove one occurrence of a particular value from a bag.
	% Abort if the item does not exist in the bag.
	%
:- pred bag__det_remove(bag(T)::in, T::in, bag(T)::out) is det.
:- func bag__det_remove(bag(T), T) = bag(T).

	% Remove a list of values from a bag.  Duplicates are removed
	% from the bag the appropriate number of times.  Fail if any
	% of the items in the list do not exist in the bag.
	%
	% This call is logically equivalent to:
	%
	%	bag__remove_list(Bag0, RemoveList, Bag) :-
	%		bag__from_list(RemoveList, RemoveBag),
	%		bag__is_subbag(RemoveBag, Bag0),
	%		bag__subtract(Bag0, RemoveBag, Bag).
	%
:- pred bag__remove_list(bag(T)::in, list(T)::in, bag(T)::out) is semidet.

	% Remove a list of values from a bag.  Duplicates are removed
	% from the bag the appropriate number of times.  Abort if any
	% of the items in the list do not exist in the bag.
	%
:- pred bag__det_remove_list(bag(T)::in, list(T)::in, bag(T)::out) is det.
:- func bag__det_remove_list(bag(T), list(T)) = bag(T).

	% Remove a set of values from a bag. Each value is removed once.
	% Fail if any of the items in the set do not exist in the bag.
	%
:- pred bag__remove_set(bag(T)::in, set(T)::in, bag(T)::out) is semidet.

	% Remove a set of values from a bag. Each value is removed once.
	% Abort if any of the items in the set do not exist in the bag.
	%
:- pred bag__det_remove_set(bag(T)::in, set(T)::in, bag(T)::out) is det.
:- func bag__det_remove_set(bag(T), set(T)) = bag(T).

	% Delete one occurrence of a particular value from a bag.
	% If the key is not present, leave the bag unchanged.
	%
:- pred bag__delete(bag(T)::in, T::in, bag(T)::out) is det.
:- func bag__delete(bag(T), T) = bag(T).

	% Remove all occurrences of a particular value from a bag.
	% Fail if the item does not exist in the bag.
	%
:- pred bag__remove_all(bag(T)::in, T::in, bag(T)::out) is semidet.

:- func bag__delete_all(bag(T), T) = bag(T).

	% Delete all occurrences of a particular value from a bag.
	%
:- pred bag__delete_all(bag(T)::in, T::in, bag(T)::out) is det.

	% Check whether a bag contains a particular value.
	%
:- pred bag__contains(bag(T)::in, T::in) is semidet.

	% Count how many occurrences of the value the bag contains.
	%
:- pred bag__count_value(bag(T)::in, T::in, int::out) is det.
:- func bag__count_value(bag(T), T) = int.

	% bag__subtract(Bag0, SubBag, Bag)
	% subtracts SubBag from Bag0 to produce Bag
	% each element in SubBag is removed from Bag0 to produce Bag.
	% If an element exists in SubBag, but not in Bag, then that
	% element is not removed.
	% e.g. bag__subtract({1, 1, 2, 2, 3 }, {1, 1, 2, 3, 3, 3}, {2}).
	%
:- pred bag__subtract(bag(T)::in, bag(T)::in, bag(T)::out) is det.
:- func bag__subtract(bag(T), bag(T)) = bag(T).

	% The third bag is the union of the first 2 bags.
	% e.g. {1, 1, 2, 2} U {2, 2, 3, 3} = {1, 1, 2, 2, 2, 2, 3, 3}
	% If the two input bags are known to be unequal in size, then
	% making the first bag the larger bag will usually be more
	% efficient.
	%
:- pred bag__union(bag(T)::in, bag(T)::in, bag(T)::out) is det.
:- func bag__union(bag(T), bag(T)) = bag(T).

	% The third bag is the intersection of the first 2 bags.  Every
	% element in the third bag exists in both of the first 2 bags.
	% e.g. bag__intersect({1, 2, 2, 3, 3}, {2, 2, 3, 4}, {2, 2, 3}).
	%
:- pred bag__intersect(bag(T)::in, bag(T)::in, bag(T)::out) is det.
:- func bag__intersect(bag(T), bag(T)) = bag(T).

	% Fails if there is no intersection between the 2 bags.
	% bag__intersect(A, B) :- bag__intersect(A, B, C), not bag__is_empty(C).
	%
:- pred bag__intersect(bag(T)::in, bag(T)::in) is semidet.

	% The third bag is the smallest bag that has both the first two bags
	% as subbags. If an element X is present N times in one of the first
	% two bags, X will be present at least N times in the third bag.
	% E.g. {1, 1, 2} upper_bound {2, 2, 3} = {1, 1, 2, 2, 3}
	% If the two input bags are known to be unequal in size, then
	% making the first bag the larger bag will usually be more
	% efficient.
	%
:- pred bag__least_upper_bound(bag(T)::in, bag(T)::in, bag(T)::out) is det.
:- func bag__least_upper_bound(bag(T), bag(T)) = bag(T).

	% Fails if the first bag is not a subbag of the second.
	% bag__is_subbag(A, B). implies that every element in the bag A
	% is also in the bag B.  If an element is in bag A multiple times, it
	% must be in bag B at least as many times.
	% e.g. bag__is_subbag({1, 1, 2}, {1, 1, 2, 2, 3}).
	% e.g. bag__is_subbag({1, 1, 2}, {1, 2, 3}) :- fail.
	%
:- pred bag__is_subbag(bag(T)::in, bag(T)::in) is semidet.

	% Check whether a bag is empty.
	%
:- pred bag__is_empty(bag(T)::in) is semidet.

	% Fails if the bag is empty.
	%
:- pred bag__remove_smallest(bag(T)::in, T::out, bag(T)::out) is semidet.

	% Compares the two bags, and returns whether the first bag is a
	% subset (<), is equal (=), or is a superset (>) of the second.
	% bag__subset_compare(<, {apple, orange}, {apple, apple, orange}).
	% bag__subset_compare(=, {apple, orange}, {apple, orange}).
	% bag__subset_compare(>, {apple, apple, orange}, {apple, orange}).
	% bag__subset_compare(_, {apple, apple}, {orange, orange}) :- fail.
	%
:- pred bag__subset_compare(comparison_result::out, bag(T)::in, bag(T)::in)
	is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module map, int, require, std_util.

:- type bag(T)		==	map(T, int).

%---------------------------------------------------------------------------%

bag__init(Bag) :-
	map__init(Bag).

%---------------------------------------------------------------------------%

bag__insert(Bag0, Item, Bag) :-
	( map__search(Bag0, Item, Count0) ->
		Count = Count0 + 1
	;
		Count = 1
	),
	map__set(Bag0, Item, Count, Bag).

%---------------------------------------------------------------------------%

bag__insert_list(Bag, [], Bag).
bag__insert_list(Bag0, [Item|Items], Bag) :-
	bag__insert(Bag0, Item, Bag1),
	bag__insert_list(Bag1, Items, Bag).

bag__insert_set(Bag0, Set, Bag) :-
	set__to_sorted_list(Set, List),
		% XXX We should exploit the sortedness of List.
	bag__insert_list(Bag0, List, Bag).

bag__from_list(List, Bag) :-
	bag__init(Bag0),
	bag__insert_list(Bag0, List, Bag).

bag__from_set(Set, Bag) :-
	set__to_sorted_list(Set, List),
	bag__init(Bag0),
		% XXX We should exploit the sortedness of List.
	bag__insert_list(Bag0, List, Bag).

bag__to_list(Bag, List) :-
	map__to_assoc_list(Bag, AssocList),
	bag__to_list_2(AssocList, List).

:- pred bag__to_list_2(assoc_list(T, int)::in, list(T)::out) is det.

bag__to_list_2([], []).
bag__to_list_2([X - Int | Xs ], Out) :-
	( Int =< 0 ->
		bag__to_list_2(Xs, Out)
	;
		NewInt = Int - 1,
		bag__to_list_2([X - NewInt | Xs], Out0),
		Out = [X | Out0]
	).

bag__to_assoc_list(Bag, AssocList) :-
	map__to_assoc_list(Bag, AssocList).

bag__to_list_without_duplicates(Bag, List) :-
	map__keys(Bag, List).

bag__to_set_without_duplicates(Bag, Set) :-
	map__keys(Bag, List),
	set__sorted_list_to_set(List, Set).

%---------------------------------------------------------------------------%

bag__delete(Bag0, Item, Bag) :-
	( bag__remove(Bag0, Item, Bag1) ->
		Bag = Bag1
	;
		Bag = Bag0
	).

bag__remove(Bag0, Item, Bag) :-
	map__search(Bag0, Item, Count0),
	( Count0 > 1 ->
		Count = Count0 - 1,
		map__set(Bag0, Item, Count, Bag)
	;
		map__delete(Bag0, Item, Bag)
	).

bag__det_remove(Bag0, Item, Bag) :-
	( bag__remove(Bag0, Item, Bag1) ->
		Bag = Bag1
	;
		error("bag__det_remove: Missing item in bag.")
	).

bag__remove_list(Bag, [], Bag).
bag__remove_list(Bag0, [X | Xs], Bag) :-
	bag__remove(Bag0, X, Bag1),
	bag__remove_list(Bag1, Xs, Bag).

bag__det_remove_list(Bag0, List, Bag) :-
	( bag__remove_list(Bag0, List, Bag1) ->
		Bag = Bag1
	;
		error("bag__det_remove_list: Missing item in bag.")
	).

bag__remove_set(Bag0, Set, Bag) :-
	set__to_sorted_list(Set, List),
		% XXX We should exploit the sortedness of List.
	bag__remove_list(Bag0, List, Bag).

bag__det_remove_set(Bag0, Set, Bag) :-
	set__to_sorted_list(Set, List),
		% XXX We should exploit the sortedness of List.
	bag__det_remove_list(Bag0, List, Bag).

bag__remove_all(Bag0, Item, Bag) :- 	% semidet
	map__remove(Bag0, Item, _Val, Bag).

bag__delete_all(Bag0, Item, Bag) :-	% det
	map__delete(Bag0, Item, Bag).

%---------------------------------------------------------------------------%

bag__contains(Bag, Item) :-
	map__contains(Bag, Item).

%---------------------------------------------------------------------------%

bag__count_value(Bag, Item, Count) :-
	( map__search(Bag, Item, Count0) ->
		Count = Count0
	;
		Count = 0
	).

%---------------------------------------------------------------------------%

bag__subtract(Bag0, SubBag, Bag) :-
	( map__remove_smallest(SubBag, SubKey, SubVal, SubBag0) ->
		( map__search(Bag0, SubKey, Val) ->
			NewVal = Val - SubVal,
			( NewVal > 0 ->
				map__det_update(Bag0, SubKey, NewVal, Bag1)
			;
				map__det_remove(Bag0, SubKey, _Val, Bag1)
			)
		;
			Bag1 = Bag0
		),
		bag__subtract(Bag1, SubBag0, Bag)
	;
		Bag = Bag0
	).

bag__union(A, B, Out) :-
	( map__remove_smallest(B, Key, BVal, B0) ->
		( map__search(A, Key, AVal) ->
			NewVal = AVal + BVal,
			map__det_update(A, Key, NewVal, A0)
		;
			map__det_insert(A, Key, BVal, A0)
		),
		bag__union(A0, B0, Out)
	;
		Out = A
	).

bag__intersect(A, B, Out) :-
	bag__init(Out0),
	bag__intersect_2(A, B, Out0, Out).

:- pred bag__intersect_2(bag(T)::in, bag(T)::in, bag(T)::in, bag(T)::out)
	is det.

bag__intersect_2(A, B, Out0, Out) :-
	( map__remove_smallest(A, Key, AVal,A0) ->
		( map__search(B, Key, BVal) ->
			int__min(AVal, BVal, Val),
			map__det_insert(Out0, Key, Val, Out1)
		;
			Out1 = Out0
		),
		bag__intersect_2(A0, B, Out1, Out)
	;
		Out = Out0
	).

bag__intersect(A, B) :-
	map__remove_smallest(A, Key, _AVal,A0),
	( map__contains(B, Key) ->
		true
	;
		bag__intersect(A0, B)
	).

bag__least_upper_bound(A, B, Out) :-
	( map__remove_smallest(B, Key, BVal, B0) ->
		( map__search(A, Key, AVal) ->
			int__max(AVal, BVal, NewVal),
			map__det_update(A, Key, NewVal, A0)
		;
			map__det_insert(A, Key, BVal, A0)
		),
		bag__least_upper_bound(A0, B0, Out)
	;
		Out = A
	).

%---------------------------------------------------------------------------%

bag__is_subbag(SubBag, BigBag) :-
	bag__subtract(SubBag, BigBag, SubBag0),
	bag__is_empty(SubBag0).

%---------------------------------------------------------------------------%

bag__is_empty(Bag) :-
	map__is_empty(Bag).

%---------------------------------------------------------------------------%

bag__remove_smallest(Bag0, Item, Bag) :-
	map__remove_smallest(Bag0, Item, Val, Bag1),
	( Val > 1 ->
		NewVal = Val - 1,
		map__det_insert(Bag1, Item, NewVal, Bag)
	;
		Bag = Bag1
	).

	% compares the two bags, and returns whether the first bag is a
	% subset (<), is equal (=), or is a superset (>) of the second
	% bag__subset_compare(<, {apple, orange}, {apple, apple, orange}).
	% bag__subset_compare(=, {apple, orange}, {apple, orange}).
	% bag__subset_compare(>, {apple, apple, orange}, {apple, orange}).
	% bag__subset_compare(_, {apple, apple}, {orange, orange}) :- fail.
	% :- pred bag__subset_compare(comparison_result, bag(T), bag(T)).
	% :- mode bag__subset_compare(out, in, in) is semidet.
	%
bag__subset_compare(Res, A, B) :-
	( map__remove_smallest(A, Key, AVal, A0) ->
		( map__remove(B, Key, BVal, B0) ->
			compare(ValRes, AVal, BVal),
			(
				ValRes = (>),
				bag__is_subbag(B0, A0),
				Res = (>)
			;
				ValRes = (=),
				bag__subset_compare(Res, A0, B0)
			;
				ValRes = (<),
				bag__is_subbag(A0, B0),
				Res = (<)
			)
		;
			% B is empty, but A is not
			Res = (>)
		)
	;
		% A is empty
		( map__is_empty(B) ->
			Res = (=)
		;
			Res = (<)
		)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
% 	Function forms added.

bag__init = B :-
	bag__init(B).

bag__insert(B1, X) = B2 :-
	bag__insert(B1, X, B2).

bag__insert_list(B1, Xs) = B2 :-
	bag__insert_list(B1, Xs, B2).

bag__insert_set(B1, Xs) = B2 :-
	bag__insert_set(B1, Xs, B2).

bag__from_list(Xs) = B :-
	bag__from_list(Xs, B).

bag__from_set(Xs) = B :-
	bag__from_set(Xs, B).

bag__to_list(B) = Xs :-
	bag__to_list(B, Xs).

bag__to_assoc_list(B) = AL :-
	bag__to_assoc_list(B, AL).

bag__to_list_without_duplicates(B) = Xs :-
	bag__to_list_without_duplicates(B, Xs).

bag__to_set_without_duplicates(B) = Xs :-
	bag__to_set_without_duplicates(B, Xs).

bag__det_remove(B1, X) = B2 :-
	bag__det_remove(B1, X, B2).

bag__det_remove_list(B1, Xs) = B2 :-
	bag__det_remove_list(B1, Xs, B2).

bag__det_remove_set(B1, Xs) = B2 :-
	bag__det_remove_set(B1, Xs, B2).

bag__delete(B1, X) = B2 :-
	bag__delete(B1, X, B2).

bag__delete_all(B1, X) = B2 :-
	bag__delete_all(B1, X, B2).

bag__count_value(B, X) = N :-
	bag__count_value(B, X, N).

bag__subtract(B1, B2) = B3 :-
	bag__subtract(B1, B2, B3).

bag__union(B1, B2) = B3 :-
	bag__union(B1, B2, B3).

bag__intersect(B1, B2) = B3 :-
	bag__intersect(B1, B2, B3).

bag__least_upper_bound(B1, B2) = B3 :-
	bag__least_upper_bound(B1, B2, B3).

%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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

:- import_module list.

:- type bag(T).

	% create an empty bag
:- pred bag__init(bag(T)).
:- mode bag__init(out) is det.

	% insert a particular value in a bag
:- pred bag__insert(bag(T), T, bag(T)).
:- mode bag__insert(in, in, out) is det.

	% insert a list of values into a bag
:- pred bag__insert_list(bag(T), list(T), bag(T)).
:- mode bag__insert_list(in, in, out) is det.

	% make a bag from a list
:- pred bag__from_list(list(T), bag(T)).
:- mode bag__from_list(in, out) is det.

	% given a bag, produce a sorted list with no duplicates 
	% containing all the values in the bag
:- pred bag__to_list_without_duplicates(bag(T), list(T)).
:- mode bag__to_list_without_duplicates(in, out) is det.

	% remove one occurrence of a particular value from a bag
	% fail if the item does not exist in the bag
:- pred bag__remove(bag(T), T, bag(T)).
:- mode bag__remove(in, in, out) is semidet.

	% remove one occurrence of a particular value from a bag
	% abort if the item does not exist in the bag
:- pred bag__det_remove(bag(T), T, bag(T)).
:- mode bag__det_remove(in, in, out) is det.

	% delete one occurrence of a particular value from a bag
	% if the key is not present, leave the map unchanged
:- pred bag__delete(bag(T), T, bag(T)).
:- mode bag__delete(in, in, out) is det.

	% remove all occurrences of a particular value from a bag
	% fail if the item does not exist in the bag
:- pred bag__remove_all(bag(T), T, bag(T)).
:- mode bag__remove_all(in, in, out) is semidet.

	% delete all occurrences of a particular value from a bag
:- pred bag__delete_all(bag(T), T, bag(T)).
:- mode bag__delete_all(in, in, out) is det.

	% check whether a bag contains a particular value
:- pred bag__contains(bag(T), T).
:- mode bag__contains(in, in) is semidet.

	% bag__subtract(Bag0, SubBag, Bag)
	% subtracts SubBag from Bag0 to produce Bag
	% each element in SubBag is removed from Bag0 to produce Bag
	% if an element exists in SubBag, but not in Bag, then that
	% element is not removed.
	% e.g. bag__subtract({1, 1, 2, 2, 3 }, {1, 1, 2, 3, 3, 3}, {2}). 
:- pred bag__subtract(bag(T), bag(T), bag(T)).
:- mode bag__subtract(in, in, out) is det.

	% The third bag is the union of the first 2 bags.
	% e.g. {1, 1, 2, 2} U {2, 2, 3, 3} = {1, 1, 2, 2, 2, 2, 3, 3}
	% If the two input bags are known to be unequal in size, then
	% making the first bag the larger bag will usually be more
	% efficient
:- pred bag__union(bag(T), bag(T), bag(T)).
:- mode bag__union(in, in, out) is det.

	% The third bag is the intersection of the first 2 bags
	% every element in the third bag exists in both of the first 2 bags
	% e.g. bag__intersect({1, 2, 2, 3, 3}, {2, 2, 3, 4}, {2, 2, 3}).
:- pred bag__intersect(bag(T), bag(T), bag(T)).
:- mode bag__intersect(in, in, out) is det.

	% fails if there is no intersection between the 2 bags
	% bag__intersect(A, B) :- bag__intersect(A, B, C), not bag__is_empty(C).
:- pred bag__intersect(bag(T), bag(T)).
:- mode bag__intersect(in, in) is semidet.

	% fails if the first bag is not a subbag of the second.
	% bag__is_subbag(A, B). implies that every element in the bag A
	% is also in the bag B.  If an element is in bag A multiple times, it
	% must be in bag B at least as many times.
	% e.g. bag__is_subbag({1, 1, 2}, {1, 1, 2, 2, 3}).
	% e.g. bag__is_subbag({1, 1, 2}, {1, 2, 3}) :- fail.
:- pred bag__is_subbag(bag(T), bag(T)).
:- mode bag__is_subbag(in, in) is semidet.

	% Check whether a bag is empty.
:- pred bag__is_empty(bag(T)).
:- mode bag__is_empty(in) is semidet.

	% fails if the bag is empty
:- pred bag__remove_smallest(bag(T), T, bag(T)).
:- mode bag__remove_smallest(in, out, out) is semidet.

	% compares the two bags, and returns whether the first bag is a 
	% subset (<), is equal (=), or is a superset (>) of the second
	% bag__subset_compare(<, {apple, orange}, {apple, apple, orange}).
	% bag__subset_compare(=, {apple, orange}, {apple, orange}).
	% bag__subset_compare(>, {apple, apple, orange}, {apple, orange}).
	% bag__subset_compare(_, {apple, apple}, {orange, orange}) :- fail.
:- pred bag__subset_compare(comparison_result, bag(T), bag(T)).
:- mode bag__subset_compare(out, in, in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
:- implementation.

:- import_module map, int, require.

:- type bag(T)		==	map(T, int).

%---------------------------------------------------------------------------%

bag__init(Bag) :-
	map__init(Bag).

%---------------------------------------------------------------------------%

bag__insert(Bag0, Item, Bag) :-
	(
		map__search(Bag0, Item, Count0)
	->
		Count is Count0 + 1
	;
		Count = 1
	),
	map__set(Bag0, Item, Count, Bag).

%---------------------------------------------------------------------------%

:- bag__insert_list(_, List, _) when List.

bag__insert_list(Bag, [], Bag).
bag__insert_list(Bag0, [Item|Items], Bag) :-
	bag__insert(Bag0, Item, Bag1),
	bag__insert_list(Bag1, Items, Bag).

bag__from_list(List, Bag) :-
	bag__init(Bag0),
	bag__insert_list(Bag0, List, Bag).

%---------------------------------------------------------------------------%

bag__delete(Bag0, Item, Bag) :- 	% det
	( bag__remove(Bag0, Item, Bag1) ->
		Bag = Bag1
	;
		Bag = Bag0
	).

bag__remove(Bag0, Item, Bag) :- 	% semidet
	map__search(Bag0, Item, Count0),
	(
		Count0 > 1
	->
		Count is Count0 - 1,
		map__set(Bag0, Item, Count, Bag)
	;
		map__delete(Bag0, Item, Bag)
	).

bag__det_remove(Bag0, Item, Bag) :-	% det
	( bag__remove(Bag0, Item, Bag1) ->
		Bag = Bag1
	;
		error("bag__det_remove: Missing item in bag."),
		Bag = Bag0
	).

bag__remove_all(Bag0, Item, Bag) :- 	% semidet
	map__remove(Bag0, Item, _Val, Bag).

bag__delete_all(Bag0, Item, Bag) :-	% det
	map__delete(Bag0, Item, Bag).

%---------------------------------------------------------------------------%

bag__contains(Bag, Item) :-
	map__contains(Bag, Item).

%---------------------------------------------------------------------------%

bag__to_list_without_duplicates(Bag, List) :-
	map__keys(Bag, List).

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

:- pred bag__intersect_2(bag(T), bag(T), bag(T), bag(T)).
:- mode bag__intersect_2(in, in, in, out) is det.
bag__intersect_2(A, B, Out0, Out) :-
	( map__remove_smallest(A, Key, AVal,A0) ->
		( map__search(B, Key, BVal) ->
			int__max(AVal, BVal, Val),
			map__det_insert(Out0, Key, Val, Out1)
		;
			map__det_insert(Out0, Key, AVal, Out1)
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

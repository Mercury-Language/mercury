%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% file: bag.m
%	An implementation of multisets.
% main author: conway.
% stability: medium
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module bag.

:- interface.

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

	% remove one occurrence of a particular value from a bag
:- pred bag__remove(bag(T), T, bag(T)).
:- mode bag__remove(in, in, out) is det.

	% remove all occurrences of a particular value from a bag
:- pred bag__remove_all(bag(T), T, bag(T)).
:- mode bag__remove_all(in, in, out) is det.

	% check whether a bag contains a particular value
:- pred bag__contains(T, bag(T)).
:- mode bag__contains(in, in) is semidet.

	% given a bag, produce a sorted list with no duplicates 
	% containing all the values in the bag
:- pred bag__to_list_without_duplicates(bag(T), list(T)).
:- mode bag__to_list_without_duplicates(in, out) is det.

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

%---------------------------------------------------------------------------%

bag__remove(Bag0, Item, Bag) :-
	(
		map__search(Bag0, Item, Count0)
	->
		Count is Count0 - 1,
		(
			Count > 0
		->
			map__set(Bag0, Item, Count, Bag)
		;
			map__delete(Bag0, Item, Bag)
		)
	;
		Bag = Bag0
	).

%---------------------------------------------------------------------------%

bag__remove_all(Bag0, Item, Bag) :-
	map__delete(Bag0, Item, Bag).

%---------------------------------------------------------------------------%

bag__contains(Item, Bag) :-
	map__contains(Bag, Item).

%---------------------------------------------------------------------------%

bag__to_list_without_duplicates(Bag, List) :-
	map__keys(Bag, List).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%


%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% array.m

% Main author: conway.

% this file contains a set of predicates for generating an manipulating
% an array data structure. The implementation uses 2-3 trees to yield
% O(logN) performance for accesses and updates. Array creation is an
% O(NlogN) process.

% arrays are created with any two integer bounds on the indices.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module array.
:- interface.
:- import_module int, list.

:- type array(T).

	% array__init creates an array with bounds from Low to High, with each
	% element initialized to Init.
:- pred array__init(int, int, T, array(T)).
:- mode array__init(in, in, in, out) is det. % want an array_skeleton?

	% array__bounds returns the upper and lower bounds of an array.
:- pred array__bounds(array(_T), int, int).
:- mode array__bounds(in, out, out) is det.

	% array__lookup returns the Nth element of an array 
	% or fails if the index is out of bounds.
:- pred array__lookup(array(T), int, T).
:- mode array__lookup(in, in, out) is det.

	% array__set sets the nth element of an array, and returns the resulting
	% array (good oppertunity for destructive update ;-). It fails if the
	% index is out of bounds.
:- pred array__set(array(T), int, T, array(T)).
:- mode array__set(in, in, in, out) is det.

	% array__resize takes an array and new lower and upper bounds.
	% the array is expanded or shrunk at each end to make it fit
	% the new bounds.
:- pred array__resize(array(T), int, int, array(T)).
:- mode array__resize(in, in, in, out) is det.

	% array__from_list takes a list (of nonzero length),
	% and returns an array containing those elements in
	% the same order that they occured in the list.
:- pred array__from_list(list(T), array(T)).
:- mode array__from_list(in, out) is det.

	% array__to_list takes an array and returns a list containing the
	% elements of the array in the same order that they
	% occured in the array.
:- pred array__to_list(array(T), list(T)).
:- mode array__to_list(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- type array(T)	--->	empty
			;	node(T)
			;	two(
					int,
					int,
					array(T),
					array(T)
				)
			;	three(
					int,
					int,
					array(T),
					array(T),
					array(T)
				).

array__init(Low, High, Init, Array) :-
	Size0 is High - Low,
	Size is Size0 + 1,
	(
		Size = 0
	->
		Array = empty
	;
		Size = 1
	->
		Array = node(Init)
	, empty;
		Size = 2
	->
		Array = two(Low, High, node(Init), node(Init))
	;
		Size = 3
	->
		Array = three(Low, High, node(Init), node(Init), node(Init))
	;
		Mod2 is Size mod 2,
		Mod2 = 0,
		Mod3 is Size mod 3,
		Mod3 \= 0
	->
		N is Size // 2,
		L2 is Low + N,
		H1 is L2 - 1,
		array__init(Low, H1, Init, A1),
		array__init(L2, High, Init, A2),
		Array = two(Low, High, A1, A2)
	;
		N is Size // 3,
		L2 is Low + N,
		L3 is L2 + N,
		H1 is L2 - 1,
		H2 is L3 - 1,
		array__init(Low, H1, Init, A1),
		array__init(L2, H2, Init, A2),
		array__init(L3, High, Init, A3),
		Array = three(Low, High, A1, A2, A3)
	).

%-----------------------------------------------------------------------------%

array__bounds(empty, 1, 0).
array__bounds(node(_), 0, 0).
array__bounds(two(Low, High, _, _), Low, High).
array__bounds(three(Low, High, _, _, _), Low, High).

%-----------------------------------------------------------------------------%

array__lookup(Array, Index, Item) :-
	array__bounds(Array, Low, High),
	(
		Index < Low
	->
		error("Array index below lower bound")
	;
		Index > High
	->
		error("Array index above upper bound")
	;
		array__search_2(Array, Index, Item)
	).

:- pred array__search_2(array(T), int, T).
:- mode array__search_2(in, in, out) is det.

array__search_2(empty, _, _) :-
	error("Attempt to access element of non-existent array").
array__search_2(node(Item), _Index, Item).
array__search_2(two(Low, High, Left, Right), Index, Item) :-
	Size is High - Low,
	Half is Size // 2,
	Mid is Low + Half,
	(
		Index =< Mid
	->
		array__search_2(Left, Index, Item)
	;
		array__search_2(Right, Index, Item)
	).
array__search_2(three(Low, High, Left, Middle, Right), Index, Item) :-
	Size is High - Low,
	Size1 is Size + 1,
	Third is Size1 // 3,
	Mid1 is Low + Third,
	Mid2 is Mid1 + Third,
	(
		Index < Mid1
	->
		array__search_2(Left, Index, Item)
	;
		Index < Mid2
	->
		array__search_2(Middle, Index, Item)
	;
		array__search_2(Right, Index, Item)
	).

%-----------------------------------------------------------------------------%

array__set(empty, _, _, empty) :-
	error("Attempt to access element of non-existent array").
array__set(node(_), _Index, Item, node(Item)).
array__set(two(Low, High, Left, Right), Index, Item, A) :-
	Size is High - Low,
	Half is Size // 2,
	Mid is Low + Half,
	(
		Index < Mid
	->
		array__set(Left, Index, Item, Left1),
		A = two(Low, High, Left1, Right)
	;
		array__set(Right, Index, Item, Right1),
		A = two(Low, High, Left, Right1)
	).
array__set(three(Low, High, Left, Middle, Right), Index, Item, A) :-
	Size is High - Low,
	Size1 is Size + 1,
	Third is Size1 // 3,
	Mid1 is Low + Third,
	Mid2 is Mid1 + Third,
	(
		Index < Mid1
	->
		array__set(Left, Index, Item, Left1),
		A = three(Low, High, Left1, Middle, Right)
	;
		Index < Mid2
	->
		array__set(Middle, Index, Item, Middle1),
		A = three(Low, High, Left, Middle1, Right)
	;
		array__set(Right, Index, Item, Right1),
		A = three(Low, High, Left, Middle, Right1)
	).

%-----------------------------------------------------------------------------%

array__resize(Array0, L, H, Array) :-
	array__bounds(Array0, L0, H0),
	array__lookup(Array0, L0, Item),
	int__max(L, L0, L1),
	int__min(H, H0, H1),
	array__fetch_items(Array0, L1, H1, Items),
	array__init(L, H, Item, Array1),
	array__insert_items(Array1, L1, Items, Array).

%-----------------------------------------------------------------------------%

array__from_list([], empty).
array__from_list([Head | Tail], Array) :-
	list__length(Tail, Len1),
	array__init(0, Len1, Head, Array0),
	array__insert_items(Array0, 1, Tail, Array).

%-----------------------------------------------------------------------------%

:- pred array__insert_items(array(T), int, list(T), array(T)).
:- mode array__insert_items(in, in, in, out) is det.

array__insert_items(Array, _N, [], Array).
array__insert_items(Array0, N, [Head|Tail], Array) :-
	array__set(Array0, N, Head, Array1),
	N1 is N + 1,
	array__insert_items(Array1, N1, Tail, Array).

%-----------------------------------------------------------------------------%

array__to_list(Array, List) :-
	array__bounds(Array, Low, High),
	array__fetch_items(Array, Low, High, List).

%-----------------------------------------------------------------------------%

:- pred array__fetch_items(array(T), int, int, list(T)).
:- mode array__fetch_items(in, in, in, out) is det.

array__fetch_items(Array, Low, High, List) :-
	(
		Low > High
	->
		List = []
	;
		Low1 is Low + 1,
		array__fetch_items(Array, Low1, High, List0),
		array__search_2(Array, Low, Item),
		List = [Item|List0]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

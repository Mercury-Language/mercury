%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% array.m

% Main author: bromage.

% this file contains a set of predicates for generating an manipulating
% an array data structure. It is based on the original module by conway,
% which used 2-3 trees to store the array.  This uses the map module 
% instead, because it's less likely to contain bugs.  :-)
%
% Arrays will be internal soon anyway, so it doesn't really matter how
% efficient this code is, as long as it works.

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

:- import_module require, map.

:- type array(T)	--->	array(int, int, map(int,T)).

%-----------------------------------------------------------------------------%

array__init(Low, High, Item, array(Low, High, MapOut)) :-
	map__init(MapIn),
	array__init_2(Low, High, Item, MapIn, MapOut).

:- pred array__init_2(int, int, T, map(int,T), map(int,T)).
:- mode array__init_2(in, in, in, in, out) is det.

array__init_2(Low, High, Item, ArrayIn, ArrayOut) :-
	( Low > High ->
	    ArrayIn = ArrayOut
	;
	    map__det_insert(ArrayIn, Low, Item, Array1),
	    Low1 is Low+1,
	    array__init_2(Low1, High, Item, Array1, ArrayOut)
	).

%-----------------------------------------------------------------------------%

array__bounds(array(Low, High, _), Low, High).

%-----------------------------------------------------------------------------%

array__lookup(array(Low, High, Map), Index, Item) :-
	( ( Low =< Index, Index =< High ) ->
	    map__lookup(Map, Index, Item)
	;
	    error("Array subscript out of bounds")
	).

%-----------------------------------------------------------------------------%

array__set(array(Low, High, MapIn), Index, Item, array(Low, High, MapOut)) :-
	( ( Low =< Index, Index =< High ) ->
	    map__set(MapIn, Index, Item, MapOut)
	;
	    error("Array subscript out of bounds")
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

array__from_list([], array(1, 0, Map)) :-
	map__init(Map).
array__from_list(List, Array) :-
        List = [ Head | Tail ],
        list__length(List, Len),
        array__init(1, Len, Head, Array0),
        array__insert_items(Array0, 2, Tail, Array).

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
                array__lookup(Array, Low, Item),
                List = [Item|List0]
        ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

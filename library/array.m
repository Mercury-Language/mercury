%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: array.m
% Main author: bromage.
% Based on the original version using 2-3 trees by conway.
% Stability: low

% This file contains a set of predicates for generating an manipulating
% an array data structure.  The current implementation does not actually
% use an array - instead we use a map with integer keys, which is implemented
% using a tree data structure.
%
% The current interface will eventually be replaced by a version using
% unique modes, and the implementation will be replaced by one which
% uses real arrays.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module array.
:- interface.
:- import_module list.

:- type array(T).

	% array__init creates an array with bounds from Low to High, with each
	% element initialized to Init.
:- pred array__init(int, int, T, array(T)).
:- mode array__init(in, in, in, out) is det. % want an array_skeleton?

	% array__bounds returns the upper and lower bounds of an array.
:- pred array__bounds(array(_T), int, int).
:- mode array__bounds(in, out, out) is det.

	% array__lookup returns the Nth element of an array.
	% It is an error if the index is out of bounds.
:- pred array__lookup(array(T), int, T).
:- mode array__lookup(in, in, out) is det.

	% array__semidet_lookup is like array__lookup except that
	% it fails if the index is out of bounds.
:- pred array__semidet_lookup(array(T), int, T).
:- mode array__semidet_lookup(in, in, out) is semidet.

	% array__set sets the nth element of an array, and returns the
	% resulting array (good opportunity for destructive update ;-).  
	% It is an error if the index is out of bounds.
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

	% array__to_list takes an array and returns a list containing
	% the elements of the array in the same order that they
	% occurred in the array.
:- pred array__to_list(array(T), list(T)).
:- mode array__to_list(in, out) is det.

	% array__fetch_items takes an array and a lower and upper
	% index, and places those items in the array between these
	% indices into a list.  It is an error if either index is
	% out of bounds.
:- pred array__fetch_items(array(T), int, int, list(T)).
:- mode array__fetch_items(in, in, in, out) is det.

	% array__bsearch takes an array, an element to be found
	% and a comparison predicate and returns the position of
	% the element in the array.  Assumes the array is in sorted
	% order.  Fails if the element is not present.  If the
	% element to be found appears multiple times, the index of
	% the first occurrence is returned.
:- pred array__bsearch(array(T), T, pred(T, T, comparison_result), int).
:- mode array__bsearch(in, in, pred(in, in, out) is det, out) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, require, map.

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
	    Low1 is Low + 1,
	    array__init_2(Low1, High, Item, Array1, ArrayOut)
	).

%-----------------------------------------------------------------------------%

array__bounds(array(Low, High, _), Low, High).

%-----------------------------------------------------------------------------%

array__lookup(array(Low, High, Map), Index, Item) :-
	( ( Low =< Index, Index =< High ) ->
	    map__lookup(Map, Index, Item)
	;
	    error("array__lookup: Array subscript out of bounds")
	).

array__semidet_lookup(array(Low, High, Map), Index, Item) :-
	Low =< Index, Index =< High,
	map__lookup(Map, Index, Item).

%-----------------------------------------------------------------------------%

array__set(array(Low, High, MapIn), Index, Item, array(Low, High, MapOut)) :-
	( ( Low =< Index, Index =< High ) ->
	    map__set(MapIn, Index, Item, MapOut)
	;
	    error("array__set: Array subscript out of bounds")
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

array__bsearch(A, El, Compare, I) :-
	array__bounds(A, Lo, Hi),
	Lo =< Hi,
	array__bsearch_2(A, Lo, Hi, El, Compare, I).

:- pred array__bsearch_2(array(T), int, int, T,
			pred(T, T, comparison_result), int).
:- mode array__bsearch_2(in, in, in, in, pred(in, in, out) is det,
				out) is semidet.
array__bsearch_2(A, Lo, Hi, El, Compare, I) :-
	Width is Hi - Lo,

	% If Width < 0, there is no range left.
	Width >= 0,

	% If Width == 0, we may just have found our element.
	% Do a Compare to check.
	( Width = 0 ->
	    array__lookup(A, Lo, X),
	    call(Compare, El, X, (=)),
	    I = Lo
	;
	    % Otherwise find the middle element of the range
	    % and check against that.  NOTE: I used ">> 1"
	    % rather than "// 2" because division always
	    % rounds towards zero whereas shift right always
	    % rounds down.  (Indices can be negative.)
	    Mid is (Lo + Hi) >> 1,
	    array__lookup(A, Mid, XMid),
	    call(Compare, XMid, El, Comp),
	    ( Comp = (<),
		Mid1 is Mid + 1,
		array__bsearch_2(A, Mid1, Hi, El, Compare, I)
	    ; Comp = (=),
		array__bsearch_2(A, Lo, Mid, El, Compare, I)
	    ; Comp = (>),
		Mid1 is Mid - 1,
		array__bsearch_2(A, Lo, Mid1, El, Compare, I)
	    )
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

%---------------------------------------------------------------------------%
% Copyright (C) 1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: array.nu.nl.
% Main author: bromage.

%-----------------------------------------------------------------------------%

% The purpose of this module is to provide a version of array.m which
% works for Prolog implementations.  For simplicitly, the implementation
% is a thin layer on top of bt_array.m.

%-----------------------------------------------------------------------------%


array__make_empty_array(Array) :-
	bt_array__make_empty_array(0, Array).

array__init(Size, Item, Array) :-
	Max is Size - 1,
	bt_array__init(0, Max, Item, Array).

	% The function array/1 can't be implemented because Prolog
	% doesn't have functions. :-(

%-----------------------------------------------------------------------------%

array__min(Array, Min) :-
	bt_array__min(Array, Min).

array__max(Array, Max) :-
	bt_array__max(Array, Max).

array__size(Array, Size) :-
	bt_array__size(Array, Size).

array__bounds(Array, Min, Max) :-
	bt_array__bounds(Array, Min, Max).

array__in_bounds(Array, Index) :-
	bt_array__in_bounds(Array, Index).

%-----------------------------------------------------------------------------%

array__lookup(Array, Index, Item) :-
	bt_array__lookup(Array, Index, Item).

array__semidet_lookup(Array, Index, Item) :-
	bt_array__semidet_lookup(Array, Index, Item).

array__set(Array0, Index, Item, Array) :-
	bt_array__set(Array0, Index, Item, Array).

array__semidet_set(Array0, Index, Item, Array) :-
	bt_array__semidet_set(Array0, Index, Item, Array).

array__slow_set(Array0, Index, Item, Array) :-
	bt_array__set(Array0, Index, Item, Array).

array__semidet_slow_set(Array0, Index, Item, Array) :-
	bt_array__semidet_set(Array0, Index, Item, Array).

array__copy(Array, Array).

array__resize(Array0, NewSize, Item, Array) :-
	NewMax is NewSize - 1,
	bt_array__resize(Array0, 0, NewMax, Item, Array).

array__shrink(Array0, NewSize, Array) :-
	NewMax is NewSize - 1,
	bt_array__shrink(Array0, 0, NewMax, Array).

array__from_list(List, Array) :-
	bt_array__from_list(0, List, Array).

array__to_list(Array, List) :-
	bt_array__to_list(Array, List).

array__fetch_items(Array0, Low, High, List) :-
	bt_array__fetch_items(Array0, Low, High, List).

array__bsearch(Array, Item, Cmp, MaybeResult) :-
	( bt_array__bsearch(Array, Item, Cmp, Result) ->
		MaybeResult = yes(Result)
	;
		MaybeResult = no
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

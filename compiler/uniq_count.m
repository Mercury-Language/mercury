%-----------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: uniq_count.m
% Main author: dmo
%
% This module contains the uniq_count type and operations on it.
%
%-----------------------------------------------------------------------------%

:- module uniq_count.
:- interface.

:- import_module map, int.
  
:- type uniq_count
	--->	known(int)
	;	many.

:- type uniq_counts(T) == map(T, uniq_count).

:- pred inc_uniq_count(T, uniq_counts(T), uniq_counts(T)).
:- mode inc_uniq_count(in, in, out) is det.

:- pred dec_uniq_count(T, uniq_counts(T), uniq_counts(T)).
:- mode dec_uniq_count(in, in, out) is det.

:- pred has_count_zero(uniq_counts(T), T).
:- mode has_count_zero(in, in) is semidet.

:- pred has_count_one(uniq_counts(T), T).
:- mode has_count_one(in, in) is semidet.

:- pred has_count_greater_than_one(uniq_counts(T), T).
:- mode has_count_greater_than_one(in, in) is semidet.

:- pred set_count_many(T, uniq_counts(T), uniq_counts(T)).
:- mode set_count_many(in, in, out) is det.

:- pred uniq_count_max(uniq_count, uniq_count, uniq_count).
:- mode uniq_count_max(in, in, out) is det.

	% Merge two uniq_counts maps.  If an element occurs in both maps
	% then use the maximum of the two values for that element.
:- pred uniq_counts_max_merge(uniq_counts(T), uniq_counts(T), uniq_counts(T)).
:- mode uniq_counts_max_merge(in, in, out) is det.

:- implementation.

:- import_module int.

inc_uniq_count(Item, Map0, Map) :-
	( map__search(Map0, Item, C0) ->
		(
			C0 = known(N),
			map__det_update(Map0, Item, known(N + 1), Map)
		;
			C0 = many,
			Map = Map0
		)
	;
		map__det_insert(Map0, Item, known(1), Map)
	).

dec_uniq_count(Item, Map0, Map) :-
	( map__search(Map0, Item, C0) ->
		(
			C0 = known(N0),
			int__max(N0 - 1, 0, N),
			map__det_update(Map0, Item, known(N), Map)
		;
			C0 = many,
			Map = Map0
		)
	;
		Map = Map0
	).

has_count_zero(Map, Item) :-
	map__search(Map, Item, Count) => Count = known(0).

has_count_one(Map, Item) :-
	map__search(Map, Item, known(1)).

has_count_greater_than_one(Map, Item) :-
	map__search(Map, Item, Count),
	( Count = known(N), N > 1
	; Count = many
	).

set_count_many(Item, Map0, Map) :-
	map__set(Map0, Item, many, Map).

uniq_count_max(many, _, many).
uniq_count_max(known(_), many, many).
uniq_count_max(known(A), known(B), known(C)) :-
	int__max(A, B, C).

uniq_counts_max_merge(MapA, MapB, Map) :-
	map__foldl(lambda([Item::in, CountA::in, M0::in, M::out] is det,
		( map__search(M0, Item, CountB) ->
			uniq_count_max(CountA, CountB, Count),
			( Count = CountB ->
				M = M0
			;
				map__det_update(M0, Item, Count, M)
			)
		;
			map__det_insert(M0, Item, CountA, M)
		)), MapA, MapB, Map).

%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% file: bag.m
%	An implementation of multisets.
% main author: conway.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module bag.

:- interface.

:- type bag(T).

:- pred bag__init(bag(T)).
:- mode bag__init(out) is det.

:- pred bag__insert(bag(T), T, bag(T)).
:- mode bag__insert(in, in, out) is det.

:- pred bag__remove(bag(T), T, bag(T)).
:- mode bag__remove(in, in, out) is det.

:- pred bag__remove_all(bag(T), T, bag(T)).
:- mode bag__remove_all(in, in, out) is det.

:- pred bag__contains(T, bag(T)).
:- mode bag__contains(in, in) is semidet.

:- pred bag__to_list_without_duplicates(bag(T), list(T)).
:- mode bag__to_list_without_duplicates(in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
:- implementation.

:- import_module map, int, require.

:- type bag(T)		==	map(T, int).

%---------------------------------------------------------------------------%

bag__init(B) :-
	map__init(B).

%---------------------------------------------------------------------------%

bag__insert(B0, I, B) :-
	(
		map__search(B0, I, C0)
	->
		C is C0 + 1
	;
		C = 1
	),
	map__set(B0, I, C, B).

%---------------------------------------------------------------------------%

bag__remove(B0, I, B) :-
	(
		map__search(B0, I, C0)
	->
		C is C0 - 1,
		(
			C > 0
		->
			map__set(B0, I, C, B)
		;
			map__delete(B0, I, B)
		)
	;
		B = B0
	).

%---------------------------------------------------------------------------%

bag__remove_all(B0, I, B) :-
	map__delete(B0, I, B).

%---------------------------------------------------------------------------%

bag__contains(I, B) :-
	map__contains(B, I).

%---------------------------------------------------------------------------%

bag__to_list_without_duplicates(Bag, List) :-
	map__keys(Bag, List).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%


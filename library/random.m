%---------------------------------------------------------------------------%
% Copyright (C) 1994-1998,2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% file: rand.m
% main author: conway
% stability: low
%
% Define a set of random number generator predicates. This implementation
% uses a threaded random-number supply. It could be made non-unique, but
% since each thread returns the same list of random numbers, in the interests
% of safety, it is declared with (backtrackable) unique modes.
% The coefficients used in the implementation were taken from Numerical
% Recipes in C (Press et al), and are originally due to Knuth.
%
%---------------------------------------------------------------------------%

:- module random.

:- interface.

:- import_module list.

	% The type `random__supply' represents a supply of random numbers.
:- type random__supply.

	% random__init(Seed, RS): creates a supply of random numbers RS
	% using the specified Seed.
:- pred random__init(int, random__supply).
:- mode random__init(in, uo) is det.

	% random__random(Num, RS0, RS): extracts a number Num in the
	% range 0 .. RandMax from the random number supply RS0, and
	% binds RS to the new state of the random number supply.
:- pred random__random(int, random__supply, random__supply).
:- mode random__random(out, mdi, muo) is det.

	% random__randmax(RandMax, RS0, RS): binds Randax to the maximum
	% random number that can be returned from the random number
	% supply RS0, and returns RS = RS0.
:- pred random__randmax(int, random__supply, random__supply).
:- mode random__randmax(out, mdi, muo) is det.

	% random__permutation(Num, List, RS0, RS): binds List to a
	% random permutation of the numbers in the range 0 .. Num - 1,
	% and binds RS to the new state of the random number supply.
:- pred random__permutation(int, list(int), random__supply, random__supply).
:- mode random__permutation(in, out, mdi, muo) is det.

%---------------------------------------------------------------------------%

:- implementation.
	% Everything after the first `:- implementation' does not appear
	% in the Mercury Library Reference Manual.
:- interface.

	% The following predicate was just for test purposes.
	% It should not be used by user programs.
:- pragma obsolete(random__test/4).
:- pred random__test(int, int, list(int), int).
:- mode random__test(in, in, out, out) is det.

%---------------------------------------------------------------------------%

:- implementation.
:- import_module int, map.

:- type random__supply		==	int.	% I(j)

:- pred random__params(int, int, int).	% a, c, m
:- mode random__params(out, out, out) is det.

random__params(9301, 49297, 233280).

random__init(I0, RS) :-
	copy(I0, RS).

random__random(I, RS0, RS) :-
	RS0 = I0,
	random__params(A, C, M),
	I is ((I0 * A) + C) mod M,
	copy(I, RS).

random__randmax(M1, Rs, Rs) :-
	random__params(_A, _C, M),
	M1 is M - 1.

%---------------------------------------------------------------------------%

	% The random permutation is implemented via a "sampling without
	% replacement" method. In init_record, we build up a map in which
	% every integer in the range 0 .. Num - 1 is mapped to itself. The
	% sampling stage iterates from Num - 1 down to 0. The invariant being
	% maintained is that at iteration I, the numbers in the image of
	% the part of the map indexed by 0 .. I-1 are the numbers that have
	% not been selected yet. At each iteration, perform_sampling generates
	% a random number Index in the range 0 .. I-1, adds the number that
	% Index is mapped to, Next, to the permutation, and then ensures that
	% Next is not generated again by swapping it with the image of I-1.

random__permutation(Num, List, RS0, RS) :-
	map__init(Empty),
	init_record(Num, Empty, Samples0),
	perform_sampling(Num, Samples0, [], List, RS0, RS).

:- pred init_record(int::in, map(int, int)::in, map(int, int)::out) is det.

init_record(N, Record0, Record) :-
	( N =< 0 ->
		Record = Record0
	;
		N1 = N - 1,
		map__det_insert(Record0, N1, N1, Record1),
		init_record(N1, Record1, Record)
	).

:- pred perform_sampling(int::in, map(int, int)::in,
	list(int)::in, list(int)::out,
	random__supply::mdi, random__supply::muo) is det.

perform_sampling(I, Record0, Order0, Order, RS0, RS) :-
	( I =< 0 ->
		Order = Order0,
		RS = RS0
	;
		I1 = I - 1,
		random__random(Random, RS0, RS1),
		Index = Random mod I,
		map__lookup(Record0, Index, Next),
		map__lookup(Record0, I1, MaxImage),
		Order1 = [Next | Order0],
		map__det_update(Record0, Index, MaxImage, Record1),
		map__det_update(Record1, I1, Next, Record2),
		perform_sampling(I1, Record2, Order1, Order, RS1, RS)
	).

%---------------------------------------------------------------------------%

random__test(Seed, N, Nums, Max) :-
	random__init(Seed, RS),
	random__randmax(Max, RS, RS1),
	random__test_2(N, Nums, RS1, _RS2).

:- pred random__test_2(int, list(int), random__supply, random__supply).
:- mode random__test_2(in, out, mdi, muo) is det.

random__test_2(N, Is, RS0, RS) :-
	(
		N > 0
	->
		N1 is N - 1,
		random__random(I, RS0, RS1),
		random__test_2(N1, Is0, RS1, RS),
		Is = [I|Is0]
	;
		Is = [],
		RS = RS0
	).

%---------------------------------------------------------------------------%

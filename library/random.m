%---------------------------------------------------------------------------%
% Copyright (C) 1994-1998,2001 The University of Melbourne.
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
:- mode random__random(out, in, out) is det.

	% random__randmax(RandMax, RS0, RS): binds Randax to the maximum
	% random number that can be returned from the random number
	% supply RS0, and returns RS = RS0.
:- pred random__randmax(int, random__supply, random__supply).
:- mode random__randmax(out, mdi, muo) is det.
:- mode random__randmax(out, in, out) is det.

	% random__permutation(List0, List, RS0, RS):
	% binds List to a random permutation of List0,
	% and binds RS to the new state of the random number supply.
:- pred random__permutation(list(T), list(T), random__supply, random__supply).
:- mode random__permutation(in, out, mdi, muo) is det.
:- mode random__permutation(in, out, in, out) is det.

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
:- import_module int, array.

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
	% replacement" method. In init_record, we build up an array in which
	% every integer in the range 0 .. Length - 1 is mapped to the
	% corresponding element in the list.  The sampling stage
	% iterates from Length - 1 down to 0. The invariant being
	% maintained is that at iteration I, the elements in the image of
	% the part of the map indexed by 0 .. I-1 are the elements that have
	% not been selected yet. At each iteration, perform_sampling generates
	% a random number Index in the range 0 .. I-1, adds the element that
	% Index is mapped to, Next, to the permutation, and then ensures that
	% Next is not generated again by swapping it with the image of I-1.

random__permutation(List0, List, RS0, RS) :-
	Samples = array(List0),
	Len = array__size(Samples),
	perform_sampling(Len, Samples, [], List, RS0, RS).

:- pred perform_sampling(int, array(T), list(T), list(T),
	random__supply, random__supply) is det.

:- mode perform_sampling(in, array_di, in, out, mdi, muo) is det.
:- mode perform_sampling(in, array_di, in, out, in, out) is det.

perform_sampling(I, Record0, Order0, Order, RS0, RS) :-
	( I =< 0 ->
		Order = Order0,
		RS = RS0
	;
		I1 = I - 1,
		random__random(Random, RS0, RS1),
		% we know that Random >= 0, so it's safe to use rem
		% rather than mod, and rem is typically more efficient
		Index = Random rem I,
		array__lookup(Record0, Index, Next),
		array__lookup(Record0, I1, MaxImage),
		Order1 = [Next | Order0],
		array__set(Record0, Index, MaxImage, Record1),
		array__set(Record1, I1, Next, Record2),
		perform_sampling(I1, Record2, Order1, Order, RS1, RS)
	).

%---------------------------------------------------------------------------%

random__test(Seed, N, Nums, Max) :-
	random__init(Seed, RS),
	random__randmax(Max, RS, RS1),
	random__test_2(N, Nums, RS1, _RS2).

:- pred random__test_2(int, list(int), random__supply, random__supply).
:- mode random__test_2(in, out, mdi, muo) is det.
:- mode random__test_2(in, out, in, out) is det.

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

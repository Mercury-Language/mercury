%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% rand.nl
%
% main author: conway
%
% Define a set of random number generator predicates. This implementation
% uses a threaded random-number supply. It could be made non-unique, but
% since each thread returns the same list of random numbers, in the interests
% of safety, it is declared with unique modes.
%
%---------------------------------------------------------------------------%

:- module random.

:- interface.

:- import_module int, list.

:- type random__supply.

:- pred random__init(int, random__supply).
:- mode random__init(in, uo) is det.

:- pred random__random(int, random__supply, random__supply).
:- mode random__random(out, di, uo) is det.

:- pred random__randmax(int, random__supply, random__supply).
:- mode random__randmax(out, di, uo) is det.

:- pred random__test(int, int, list(int), int).
:- mode random__test(in, in, out, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- type random__supply		--->	random__supply(
						int,	% I(j)
						int,	% a
						int,	% c
						int	% m
					).

random__init(I0, RS) :-
	RS = random__supply(I0, 2416, 374441, 1771875).

random__random(I, RS0, RS) :-
	RS0 = random__supply(I0, A, C, M),
	I1 is I0 * A,
	I2 is I1 + C,
	I is I2 mod M,
	RS = random__supply(I, A, C, M).

random__randmax(M1, Rs0, Rs) :-
	Rs0 = random__supply(I, A, C, M),
	M1 is M - 1,
	Rs = random__supply(I, A, C, M).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

random__test(Seed, N, Nums, Max) :-
	random__init(Seed, RS),
	random__randmax(Max, RS, RS1),
	random__test_2(N, Nums, RS1, _RS2).

:- pred random__test_2(int, list(int), random__supply, random__supply).
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

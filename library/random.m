%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1998,2001-2006, 2011 The University of Melbourne.
% Copyright (C) 2015-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: random.m
% Main author: conway
% Stability: low
%
% Define a set of random number generator predicates. This implementation
% uses a threaded random-number supply.  The supply can be used in a
% non-unique way, which means that each thread returns the same list of
% random numbers.  However, this may not be desired so in the interests
% of safety it is also declared with (backtrackable) unique modes.
%
% The coefficients used in the implementation were taken from Numerical
% Recipes in C (Press et al), and are originally due to Knuth.  These
% coefficients are described as producing a "Quick and Dirty" random number
% generator, which generates the numbers very quickly but not necessarily
% with a high degree of quality.  As with all random number generators,
% the user is advised to consider carefully whether this generator meets
% their requirements in terms of "randomness".  For applications which have
% special needs (e.g. cryptographic key generation), a generator such as
% this is unlikely to be suitable.
%
% Note that random number generators of this type have several known
% pitfalls which the user may need to avoid:
%
%   1) The high bits tend to be more random than the low bits.  If
%   you wish to generate a random integer within a given range, you
%   should something like 'div' to reduce the random numbers to the
%   required range rather than something like 'mod' (or just use
%   random.random/5).
%
%   2) Similarly, you should not try to break a random number up into
%   components.  Instead, you should generate each number with a
%   separate call to this module.
%
%   3) There can be sequential correlation between successive calls,
%   so you shouldn't try to generate tuples of random numbers, for
%   example, by generating each component of the tuple in sequential
%   order.  If you do, it is likely that the resulting sequence will
%   not cover the full range of possible tuples.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module random.
:- interface.

:- import_module list.

%---------------------------------------------------------------------------%

    % The type `supply' represents a supply of random numbers.
    %
:- type supply.

    % init(Seed, RS).
    %
    % Creates a supply of random numbers RS using the specified Seed.
    %
:- pred init(int::in, supply::uo) is det.

    % random(Num, !RS).
    %
    % Extracts a number Num in the range 0 .. RandMax from the random number
    % supply !RS.
    %
:- pred random(int, supply, supply).
:- mode random(out, in, out) is det.
:- mode random(out, mdi, muo) is det.

    % random(Low, Range, Num, !RS).
    %
    % Extracts a number Num in the range Low .. (Low + Range - 1) from the
    % random number supply !RS.  For best results, the value of Range should be
    % no greater than about 100.
    %
:- pred random(int, int, int, supply, supply).
:- mode random(in, in, out, in, out) is det.
:- mode random(in, in, out, mdi, muo) is det.

    % randmax(RandMax, !RS).
    %
    % Binds RandMax to the maximum random number that can be returned from the
    % random number supply !RS, the state of the supply is unchanged.
    %
:- pred randmax(int, supply, supply).
:- mode randmax(out, in, out) is det.
:- mode randmax(out, mdi, muo) is det.

    % randcount(RandCount, !RS).
    %
    % Binds RandCount to the number of distinct random numbers that can be
    % returned from the random number supply !RS.  The state of the supply is
    % unchanged.  This will be one more than the number returned by randmax/3.
    %
:- pred randcount(int, supply, supply).
:- mode randcount(out, in, out) is det.
:- mode randcount(out, mdi, muo) is det.

    % permutation(List0, List, !RS).
    %
    % Binds List to a random permutation of List0.
    %
:- pred permutation(list(T), list(T), supply, supply).
:- mode permutation(in, out, in, out) is det.
:- mode permutation(in, out, mdi, muo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.
    % Everything after the first `:- implementation' does not appear
    % in the Mercury Library Reference Manual.
:- interface.

    % The following predicate was just for test purposes.
    % It should not be used by user programs.
:- pragma obsolete(test/4).
:- pred test(int::in, int::in, list(int)::out, int::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module int.

:- type supply
    --->    rs(int). % I(j)

:- pred params(int::out, int::out, int::out) is det. % a, c, m

params(9301, 49297, 233280).

init(I0, rs(RS)) :-
    copy(I0, RS).

random(I, rs(RS0), rs(RS)) :-
    RS0 = I0,
    random.params(A, C, M),
    I = ((I0 * A) + C) mod M,
    copy(I, RS).

    % We could make this more robust by checking whether the range is
    % less than a certain threshold, and using a more sophisticated
    % algorithm if the threshold is exceeded.  But that would defeat
    % the purpose of having a "quick and dirty" random number generator,
    % so we don't do that.
random(Low, Range, Num, !RandomSupply) :-
    random(R, !RandomSupply),
    randcount(M, !RandomSupply),
    % With our current set of parameters and a reasonable choice of Range,
    % the following should never overflow.
    Num = Low + (Range * R) // M.

randmax(M1, RS, RS) :-
    params(_A, _C, M),
    M1 = M - 1.

randcount(M, RS, RS) :-
    params(_A, _C, M).

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

permutation(List0, List, !RS) :-
    Samples = array(List0),
    Len = array.size(Samples),
    perform_sampling(Len, Samples, [], List, !RS).

:- pred perform_sampling(int, array(T), list(T), list(T),
    random.supply, random.supply).
:- mode perform_sampling(in, array_di, in, out, in, out) is det.
:- mode perform_sampling(in, array_di, in, out, mdi, muo) is det.

perform_sampling(I, !.Record, !Order, !RS) :-
    ( if I =< 0 then
        true
    else
        I1 = I - 1,
        random.random(0, I, Index, !RS),
        array.lookup(!.Record, Index, Next),
        array.lookup(!.Record, I1, MaxImage),
        !:Order = [Next | !.Order],
        array.set(Index, MaxImage, !Record),
        array.set(I1, Next, !Record),
        perform_sampling(I1, !.Record, !Order, !RS)
    ).

%---------------------------------------------------------------------------%

test(Seed, N, Nums, Max) :-
    init(Seed, RS),
    randmax(Max, RS, RS1),
    test_2(N, Nums, RS1, _RS2).

:- pred test_2(int, list(int), supply, supply).
:- mode test_2(in, out, in, out) is det.
:- mode test_2(in, out, mdi, muo) is det.

random.test_2(N, Is, !RS) :-
    ( if N > 0 then
        N1 = N - 1,
        random(I, !RS),
        test_2(N1, Is0, !RS),
        Is = [I | Is0]
    else
        Is = []
    ).

%---------------------------------------------------------------------------%

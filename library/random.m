%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1998,2001-2006, 2011 The University of Melbourne.
% Copyright (C) 2015-2016, 2018-2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: random.m
% Main author: Mark Brown
%
% This module provides interfaces to several random number generators,
% implementations of which can be found in the submodules.
%
% The interfaces can be used in three styles:
%
%   - In the "ground" style, an instance of the random/1 typeclass is
%   passed through the code using 'in' and 'out' modes. This can be used
%   to generate random numbers, and since the value is ground it can also
%   easily be stored in larger data structures. The major drawback is that
%   generators in this style tend to be either fast or of good quality,
%   but not both.
%
%   - In the "unique" style, the urandom/2 typeclass is used instead. Each
%   instance consists of a "params" type which is passed into the code
%   using an 'in' mode, and a "state" type which is passed through the
%   code using modes 'di' and 'uo'. The uniqueness allows destructive
%   update, which means that these generators can be both fast and good.
%
%   - A generator can be attached to the I/O state. In this case, the
%   interface is the same as the unique style, with 'io' being used as
%   the unique state.
%
% Each generator defined in the submodules is natively one of the first
% two styles. Adaptors are defined below for converting between these,
% or from either of these to the third style.
%
%
% Example, ground style:
%
%   main(!IO) :-
%       R0 = sfc16.init,
%       roll(R0, R1, !IO),
%       roll(R1, _, !IO).
%
%   :- pred roll(R::in, R::out, io::di, io::uo) is det <= random(R).
%
%   roll(!R, !IO) :-
%       uniform_int_in_range(1, 6, N, !R),
%       io.format("You rolled a %d\n", [i(N)], !IO).
%
%
% Example, unique style:
%
%   main(!IO) :-
%       sfc64.init(P, S0),
%       roll(P, S0, S1, !IO),
%       roll(P, S1, _, !IO).
%
%   :- pred roll(P::in, S::di, S::uo, io::di, io::uo) is det <= urandom(P, S).
%
%   roll(P, !S, !IO) :-
%       uniform_int_in_range(P, 1, 6, N, !S),
%       io.format("You rolled a %d\n", [i(N)], !IO).
%
%
% Example, attached to I/O state:
%
%   main(!IO) :-
%       % Using a ground generator.
%       R = sfc16.init,
%       make_io_random(R, M1, !IO),
%       roll(M1, !IO),
%       roll(M1, !IO),
%
%       % Using a unique generator.
%       sfc64.init(P, S),
%       make_io_urandom(P, S, M2, !IO),
%       roll(M2, !IO),
%       roll(M2, !IO).
%
%   :- pred roll(M::in, io::di, io::uo) is det <= urandom(M, io).
%
%   roll(M, !IO) :-
%       uniform_int_in_range(M, 1, 6, N, !IO),
%       io.format("You rolled a %d\n", [i(N)], !IO).
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module random.
:- interface.

:- include_module sfc16.
:- include_module sfc32.
:- include_module sfc64.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % Interface to random number generators.
    %
:- typeclass random(R) where [

        % Generate a uniformly distributed pseudo-random unsigned integer
        % of 8, 16, 32 or 64 bits, respectively.
        %
    pred generate_uint8(uint8::out, R::in, R::out) is det,
    pred generate_uint16(uint16::out, R::in, R::out) is det,
    pred generate_uint32(uint32::out, R::in, R::out) is det,
    pred generate_uint64(uint64::out, R::in, R::out) is det

].

    % uniform_int_in_range(Start, Range, N, !R)
    %
    % Generate a pseudo-random integer that is uniformly distributed
    % in the range Start to (Start + Range - 1), inclusive.
    %
    % Throws an exception if Range < 1 or Range > uint32_max.
    %
:- pred uniform_int_in_range(int::in, int::in, int::out, R::in, R::out)
    is det <= random(R).

    % uniform_uint_in_range(Start, Range, N, !R)
    %
    % Generate a pseudo-random unsigned integer that is uniformly
    % distributed in the range Start to (Start + Range - 1), inclusive.
    %
    % Throws an exception if Range < 1 or Range > uint32_max.
    %
:- pred uniform_uint_in_range(uint::in, uint::in, uint::out, R::in, R::out)
    is det <= random(R).

    % uniform_float_in_range(Start, Range, N, !R)
    %
    % Generate a pseudo-random float that is uniformly distributed
    % in the interval [Start, Start + Range).
    %
:- pred uniform_float_in_range(float::in, float::in, float::out, R::in, R::out)
    is det <= random(R).

    % uniform_float_around_mid(Mid, Delta, N, !R)
    %
    % Generate a pseudo-random float that is uniformly distributed
    % in the interval (Mid - Delta, Mid + Delta).
    %
:- pred uniform_float_around_mid(float::in, float::in, float::out,
    R::in, R::out) is det <= random(R).

    % uniform_float_in_01(N, !R)
    %
    % Generate a pseudo-random float that is uniformly distributed
    % in the interval [0.0, 1.0).
    %
:- pred uniform_float_in_01(float::out, R::in, R::out) is det <= random(R).

    % normal_floats(M, SD, U, V, !R)
    %
    % Generate two pseudo-random floats from a normal (i.e., Gaussian)
    % distribution with mean M and standard deviation SD, using the
    % Box-Muller method.
    %
    % We generate two at a time for efficiency; they are independent of
    % each other.
    %
:- pred normal_floats(float::in, float::in, float::out, float::out,
    R::in, R::out) is det <= random(R).

    % normal_floats(U, V, !R)
    %
    % Generate two pseudo-random floats from a normal (i.e., Gaussian)
    % distribution with mean 0.0 and standard deviation 1.0, using the
    % Nox-Muller method.
    %
    % We generate two at a time for efficiency; they are independent of
    % each other.
    %
:- pred normal_floats(float::out, float::out, R::in, R::out) is det
    <= random(R).

%---------------------------------------------------------------------------%

    % Interface to unique random number generators. Callers need to
    % ensure they preserve the uniqueness of the random state, and in
    % turn instances can use destructive update on it.
    %
:- typeclass urandom(P, S) <= (P -> S) where [

        % Generate a uniformly distributed pseudo-random unsigned integer
        % of 8, 16, 32 or 64 bits, respectively.
        %
    pred generate_uint8(P::in, uint8::out, S::di, S::uo) is det,
    pred generate_uint16(P::in, uint16::out, S::di, S::uo) is det,
    pred generate_uint32(P::in, uint32::out, S::di, S::uo) is det,
    pred generate_uint64(P::in, uint64::out, S::di, S::uo) is det

].

:- typeclass urandom_dup(S) where [

        % urandom_dup(!S, !:Sdup)
        %
        % Create a duplicate random state that will generate the same
        % sequence of integers.
        %
    pred urandom_dup(S::di, S::uo, S::uo) is det

].

    % uniform_int_in_range(P, Start, Range, N, !S)
    %
    % Generate a pseudo-random integer that is uniformly distributed
    % in the range Start to (Start + Range - 1), inclusive.
    %
    % Throws an exception if Range < 1 or Range > uint32_max.
    %
:- pred uniform_int_in_range(P::in, int::in, int::in, int::out, S::di, S::uo)
    is det <= urandom(P, S).

    % uniform_uint_in_range(P, Start, Range, N, !S)
    %
    % Generate a pseudo-random unsigned integer that is uniformly
    % distributed in the range Start to (Start + Range - 1), inclusive.
    %
    % Throws an exception if Range < 1 or Range > uint32_max.
    %
:- pred uniform_uint_in_range(P::in, uint::in, uint::in, uint::out,
    S::di, S::uo) is det <= urandom(P, S).

    % uniform_float_in_range(P, Start, Range, N, !S)
    %
    % Generate a pseudo-random float that is uniformly distributed
    % in the interval [Start, Start + Range).
    %
:- pred uniform_float_in_range(P::in, float::in, float::in, float::out,
    S::di, S::uo) is det <= urandom(P, S).

    % uniform_float_around_mid(P, Mid, Delta, N, !S)
    %
    % Generate a pseudo-random float that is uniformly distributed
    % in the interval (Mid - Delta, Mid + Delta).
    %
:- pred uniform_float_around_mid(P::in, float::in, float::in, float::out,
    S::di, S::uo) is det <= urandom(P, S).

    % uniform_float_in_01(P, N, !S)
    %
    % Generate a pseudo-random float that is uniformly distributed
    % in the interval [0.0, 1.0).
    %
:- pred uniform_float_in_01(P::in, float::out, S::di, S::uo) is det
    <= urandom(P, S).

    % normal_floats(P, M, S, U, V, !S)
    %
    % Generate two pseudo-random floats from a normal (i.e., Gaussian)
    % distribution with mean M and standard deviation S, using the
    % Box-Muller method.
    %
    % We generate two at a time for efficiency; they are independent of
    % each other.
    %
:- pred normal_floats(P::in, float::in, float::in, float::out, float::out,
    S::di, S::uo) is det <= urandom(P, S).

    % normal_floats(P, U, V, !S)
    %
    % Generate two pseudo-random floats from a normal (i.e., Gaussian)
    % distribution with mean 0.0 and standard deviation 1.0, using the
    % Nox-Muller method.
    %
    % We generate two at a time for efficiency; they are independent of
    % each other.
    %
:- pred normal_floats(P::in, float::out, float::out, S::di, S::uo) is det
    <= urandom(P, S).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Convert any instance of random/1 into an instance of urandom/2.
    % This creates additional overhead in the form of additional
    % typeclass method calls.
    %
:- type urandom_params(R).
:- type urandom_state(R).

:- instance urandom(urandom_params(R), urandom_state(R)) <= random(R).
:- instance urandom_dup(urandom_state(R)) <= random(R).

:- pred make_urandom(R::in, urandom_params(R)::out, urandom_state(R)::uo)
    is det.

%---------------------------------------------------------------------------%

    % Convert any instance of urandom/2 and urandom_dup/1 into an
    % instance of random/1. This duplicates the state every time a
    % random number is generated, hence may use significantly more
    % memory than if the unique version were used directly.
    %
:- type shared_random(P, S).

:- instance random(shared_random(P, S)) <= (urandom(P, S), urandom_dup(S)).

:- func make_shared_random(P::in, S::di) = (shared_random(P, S)::out) is det.

%---------------------------------------------------------------------------%

    % Convert any instance of random/1 into an instance of urandom/2
    % where the state is the I/O state.
    %
:- type io_random(R).

:- instance urandom(io_random(R), io) <= random(R).

:- pred make_io_random(R::in, io_random(R)::out, io::di, io::uo) is det
    <= random(R).

%---------------------------------------------------------------------------%

    % Convert any instance of urandom/2 into an instance of urandom/2
    % where the state is the I/O state.
    %
:- type io_urandom(P, S).

:- instance urandom(io_urandom(P, S), io) <= urandom(P, S).

:- pred make_io_urandom(P::in, S::di, io_urandom(P, S)::out, io::di, io::uo)
    is det <= urandom(P, S).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Interface to the older random number generator. This is now deprecated.
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

    % The type `supply' represents a supply of random numbers.
    %
:- type supply.

    % init(Seed, RS).
    %
    % Creates a supply of random numbers RS using the specified Seed.
    %
:- pragma obsolete(init/2).
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
:- import_module float.
:- import_module int.
:- import_module math.
:- import_module mutvar.
:- import_module uint.
:- import_module uint32.

%---------------------------------------------------------------------------%

uniform_int_in_range(Start, Range0, N, !R) :-
    Range = uint32.det_from_int(Range0),
    Max = uint32.max_uint32,
    generate_uint32(N0, !R),
    N1 = N0 // (Max // Range),
    ( if N1 < Range then
        N = Start + uint32.cast_to_int(N1)
    else
        uniform_int_in_range(Start, Range0, N, !R)
    ).

uniform_uint_in_range(Start, Range0, N, !R) :-
    Range = uint32.cast_from_uint(Range0),
    Max = uint32.max_uint32,
    generate_uint32(N0, !R),
    N1 = N0 // (Max // Range),
    ( if N1 < Range then
        N = Start + uint32.cast_to_uint(N1)
    else
        uniform_uint_in_range(Start, Range0, N, !R)
    ).

uniform_float_in_range(Start, Range, N, !R) :-
    uniform_float_in_01(N0, !R),
    N = Start + (N0 * Range).

uniform_float_around_mid(Mid, Delta, N, !R) :-
    uniform_float_in_01(N0, !R),
    ( if N0 = 0.0 then
        uniform_float_around_mid(Mid, Delta, N, !R)
    else
        N = Mid + Delta * (2.0 * N0 - 1.0)
    ).

uniform_float_in_01(N, !R) :-
    generate_uint64(N0, !R),
    D = 18_446_744_073_709_551_616.0,       % 2^64
    N = float.cast_from_uint64(N0) / D.

normal_floats(M, SD, U, V, !R) :-
    normal_floats(U0, V0, !R),
    U = M + SD * U0,
    V = M + SD * V0.

normal_floats(U, V, !R) :-
    uniform_float_in_range(-1.0, 2.0, X, !R),
    uniform_float_in_range(-1.0, 2.0, Y, !R),
    ( if uniform_to_normal(X, Y, U0, V0) then
        U = U0,
        V = V0
    else
        normal_floats(U, V, !R)
    ).

%---------------------------------------------------------------------------%

uniform_int_in_range(P, Start, Range0, N, !S) :-
    Range = uint32.det_from_int(Range0),
    Max = uint32.max_uint32,
    generate_uint32(P, N0, !S),
    N1 = N0 // (Max // Range),
    ( if N1 < Range then
        N = Start + uint32.cast_to_int(N1)
    else
        uniform_int_in_range(P, Start, Range0, N, !S)
    ).

uniform_uint_in_range(P, Start, Range0, N, !S) :-
    Range = uint32.cast_from_uint(Range0),
    Max = uint32.max_uint32,
    generate_uint32(P, N0, !S),
    N1 = N0 // (Max // Range),
    ( if N1 < Range then
        N = Start + uint32.cast_to_uint(N1)
    else
        uniform_uint_in_range(P, Start, Range0, N, !S)
    ).

uniform_float_in_range(P, Start, Range, N, !S) :-
    uniform_float_in_01(P, N0, !S),
    N = Start + (N0 * Range).

uniform_float_around_mid(P, Mid, Delta, N, !S) :-
    uniform_float_in_01(P, N0, !S),
    ( if N0 = 0.0 then
        uniform_float_around_mid(P, Mid, Delta, N, !S)
    else
        N = Mid + Delta * (2.0 * N0 - 1.0)
    ).

uniform_float_in_01(P, N, !S) :-
    generate_uint64(P, N0, !S),
    D = 18_446_744_073_709_551_616.0,       % 2^64
    N = float.cast_from_uint64(N0) / D.

normal_floats(P, M, SD, U, V, !S) :-
    normal_floats(P, U0, V0, !S),
    U = M + SD * U0,
    V = M + SD * V0.

normal_floats(P, U, V, !S) :-
    uniform_float_in_range(P, -1.0, 2.0, X, !S),
    uniform_float_in_range(P, -1.0, 2.0, Y, !S),
    ( if uniform_to_normal(X, Y, U0, V0) then
        U = U0,
        V = V0
    else
        normal_floats(P, U, V, !S)
    ).

%---------------------------------------------------------------------------%

:- pred uniform_to_normal(float::in, float::in, float::out, float::out)
    is semidet.

uniform_to_normal(X, Y, U, V) :-
    S = X * X + Y * Y,
    S > 0.0,
    S < 1.0,
    Fac = math.sqrt(-2.0 * math.ln(S) / S),
    U = X * Fac,
    V = Y * Fac.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type urandom_params(R)
    --->    urandom_params.

:- type urandom_state(R)
    --->    urandom_state(R).

:- instance urandom(urandom_params(R), urandom_state(R)) <= random(R) where [
    ( generate_uint8(_, N, S0, S) :-
        S0 = urandom_state(R0),
        generate_uint8(N, R0, R),
        S = unsafe_promise_unique(urandom_state(R))
    ),
    ( generate_uint16(_, N, S0, S) :-
        S0 = urandom_state(R0),
        generate_uint16(N, R0, R),
        S = unsafe_promise_unique(urandom_state(R))
    ),
    ( generate_uint32(_, N, S0, S) :-
        S0 = urandom_state(R0),
        generate_uint32(N, R0, R),
        S = unsafe_promise_unique(urandom_state(R))
    ),
    ( generate_uint64(_, N, S0, S) :-
        S0 = urandom_state(R0),
        generate_uint64(N, R0, R),
        S = unsafe_promise_unique(urandom_state(R))
    )
].

:- instance urandom_dup(urandom_state(R)) <= random(R) where [
    ( urandom_dup(S, S1, S2) :-
        S1 = unsafe_promise_unique(S),
        S2 = unsafe_promise_unique(S)
    )
].

make_urandom(R, P, S) :-
    P = urandom_params,
    S = unsafe_promise_unique(urandom_state(R)).

%---------------------------------------------------------------------------%

:- type shared_random(P, S)
    --->    shared_random(
                shared_random_params :: P,
                shared_random_state :: S
            ).

:- instance random(shared_random(P, S)) <= (urandom(P, S), urandom_dup(S))
        where [
    ( generate_uint8(N, R0, R) :-
        R0 = shared_random(P, S0),
        S1 = unsafe_promise_unique(S0),
        urandom_dup(S1, _, S2),
        generate_uint8(P, N, S2, S),
        R = shared_random(P, S)
    ),
    ( generate_uint16(N, R0, R) :-
        R0 = shared_random(P, S0),
        S1 = unsafe_promise_unique(S0),
        urandom_dup(S1, _, S2),
        generate_uint16(P, N, S2, S),
        R = shared_random(P, S)
    ),
    ( generate_uint32(N, R0, R) :-
        R0 = shared_random(P, S0),
        S1 = unsafe_promise_unique(S0),
        urandom_dup(S1, _, S2),
        generate_uint32(P, N, S2, S),
        R = shared_random(P, S)
    ),
    ( generate_uint64(N, R0, R) :-
        R0 = shared_random(P, S0),
        S1 = unsafe_promise_unique(S0),
        urandom_dup(S1, _, S2),
        generate_uint64(P, N, S2, S),
        R = shared_random(P, S)
    )
].

make_shared_random(P, S) = shared_random(P, S).

%---------------------------------------------------------------------------%

:- type io_random(R)
    --->    io_random(mutvar(R)).

:- instance urandom(io_random(R), io) <= random(R) where [
    pred(generate_uint8/4) is io_random_gen_uint8,
    pred(generate_uint16/4) is io_random_gen_uint16,
    pred(generate_uint32/4) is io_random_gen_uint32,
    pred(generate_uint64/4) is io_random_gen_uint64
].

:- pred io_random_gen_uint8(io_random(R)::in, uint8::out, io::di, io::uo)
    is det <= random(R).
:- pragma promise_pure(io_random_gen_uint8/4).

io_random_gen_uint8(io_random(V), N, !IO) :-
    impure get_mutvar(V, R0),
    generate_uint8(N, R0, R),
    impure set_mutvar(V, R).

:- pred io_random_gen_uint16(io_random(R)::in, uint16::out, io::di, io::uo)
    is det <= random(R).
:- pragma promise_pure(io_random_gen_uint16/4).

io_random_gen_uint16(io_random(V), N, !IO) :-
    impure get_mutvar(V, R0),
    generate_uint16(N, R0, R),
    impure set_mutvar(V, R).

:- pred io_random_gen_uint32(io_random(R)::in, uint32::out, io::di, io::uo)
    is det <= random(R).
:- pragma promise_pure(io_random_gen_uint32/4).

io_random_gen_uint32(io_random(V), N, !IO) :-
    impure get_mutvar(V, R0),
    generate_uint32(N, R0, R),
    impure set_mutvar(V, R).

:- pred io_random_gen_uint64(io_random(R)::in, uint64::out, io::di, io::uo)
    is det <= random(R).
:- pragma promise_pure(io_random_gen_uint64/4).

io_random_gen_uint64(io_random(V), N, !IO) :-
    impure get_mutvar(V, R0),
    generate_uint64(N, R0, R),
    impure set_mutvar(V, R).

:- pragma promise_pure(make_io_random/4).

make_io_random(R, Pio, !IO) :-
    impure new_mutvar(R, V),
    Pio = io_random(V).

%---------------------------------------------------------------------------%

:- type io_urandom(P, S)
    --->    io_urandom(P, mutvar(S)).

:- instance urandom(io_urandom(P, S), io) <= urandom(P, S) where [
    pred(generate_uint8/4) is io_urandom_gen_uint8,
    pred(generate_uint16/4) is io_urandom_gen_uint16,
    pred(generate_uint32/4) is io_urandom_gen_uint32,
    pred(generate_uint64/4) is io_urandom_gen_uint64
].

:- pred io_urandom_gen_uint8(io_urandom(P, S)::in, uint8::out, io::di, io::uo)
    is det <= urandom(P, S).
:- pragma promise_pure(io_urandom_gen_uint8/4).

io_urandom_gen_uint8(io_urandom(P, V), N, !IO) :-
    impure get_mutvar(V, S0),
    S1 = unsafe_promise_unique(S0),
    generate_uint8(P, N, S1, S),
    impure set_mutvar(V, S).

:- pred io_urandom_gen_uint16(io_urandom(P, S)::in, uint16::out, io::di, io::uo)
    is det <= urandom(P, S).
:- pragma promise_pure(io_urandom_gen_uint16/4).

io_urandom_gen_uint16(io_urandom(P, V), N, !IO) :-
    impure get_mutvar(V, S0),
    S1 = unsafe_promise_unique(S0),
    generate_uint16(P, N, S1, S),
    impure set_mutvar(V, S).

:- pred io_urandom_gen_uint32(io_urandom(P, S)::in, uint32::out, io::di, io::uo)
    is det <= urandom(P, S).
:- pragma promise_pure(io_urandom_gen_uint32/4).

io_urandom_gen_uint32(io_urandom(P, V), N, !IO) :-
    impure get_mutvar(V, S0),
    S1 = unsafe_promise_unique(S0),
    generate_uint32(P, N, S1, S),
    impure set_mutvar(V, S).

:- pred io_urandom_gen_uint64(io_urandom(P, S)::in, uint64::out, io::di, io::uo)
    is det <= urandom(P, S).
:- pragma promise_pure(io_urandom_gen_uint64/4).

io_urandom_gen_uint64(io_urandom(P, V), N, !IO) :-
    impure get_mutvar(V, S0),
    S1 = unsafe_promise_unique(S0),
    generate_uint64(P, N, S1, S),
    impure set_mutvar(V, S).

:- pragma promise_pure(make_io_urandom/5).

make_io_urandom(P, S, Pio, !IO) :-
    impure new_mutvar(S, V),
    Pio = io_urandom(P, V).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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

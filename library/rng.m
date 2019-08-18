%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: rng.m
% Main author: Mark Brown
%
% This module provides an interface to several random number generators,
% which can be found in the submodules.
%
% Two styles of the interface are provided, a ground style and a
% unique style. Each has its own advantages and disadvantages:
%
%   - Ground RNGs are easier to use; for example they can be easily
%     stored in larger data structures.
%   - Ground RNGs are easier to implement instances for.
%   - Unique RNGs are able to use destructive update, and therefore
%     are often able to operate more efficiently.
%   - Unique RNGs need to be explicitly duplicated (i.e., to produce
%     a new generator that will generate the same sequence of numbers).
%     This may be regarded as an advantage or a disadvantage.
%   - Some RNGs, for example the binfile generator that reads data from
%     a file, use the IO state and therefore must use the unique interface.
%
% Each RNG defined in the submodules is natively one of these two styles.
% Conversion between the two styles can be done with make_urng/3 and
% make_shared_rng/2, below, although this incurs additional overhead.
%
%
% Example, ground style:
%
%   main(!IO) :-
%       RNG0 = rng.marsaglia.init,
%       roll(RNG0, RNG1, !IO),
%       roll(RNG1, _, !IO).
%
%   :- pred roll(RNG, RNG, io, io) <= rng(RNG).
%   :- mode roll(in, out, di, uo) is det.
%
%   roll(!RNG, !IO) :-
%       random_int(1, 6, N, !RNG),
%       io.format("You rolled a %d\n", [i(N)], !IO).
%
%
% Example, unique style:
%
%   main(!IO) :-
%       rng.tausworthe.init_t3(RP, RS0),
%       roll(RP, RS0, RS1, !IO),
%       roll(RP, RS1, _, !IO).
%
%   :- pred roll(RP, RS, RS, io, io) <= urng(RP, RS).
%   :- mode roll(in, di, uo, di, uo) is det.            % note unique modes
%
%   roll(RP, !RS, !IO) :-
%       urandom_int(RP, 1, 6, N, !RS),
%       io.format("You rolled a %d\n", [i(N)], !IO).
%
%
% Example, converting style:
%
%   main(!IO) :-
%       rng.tausworthe.init_t3(RP, RS),
%       RNG0 = make_shared_rng(RP, RS),
%       random_int(1, 6, N, RNG0, RNG1),
%       ...
%
%   main(!IO) :-
%       RNG = rng.marsaglia.init,
%       make_urng(RNG, RP, RS0),
%       urandom_int(RP, 1, 6, N, RS0, RS1),
%       ...
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module rng.
:- interface.

:- include_module binfile.
:- include_module marsaglia.
:- include_module sfc.
:- include_module tausworthe.

%---------------------------------------------------------------------------%

    % random_int(Start, Range, N, !RNG)
    %
    % Generate a random integer between Start and Start+Range-1 inclusive.
    % Throws an exception if Range < 1 or Range > random_max.
    %
:- pred random_int(int, int, int, RNG, RNG) <= rng(RNG).
:- mode random_int(in, in, out, in, out) is det.

    % Generate a random float between 0.0 and 1.0, inclusive.
    %
:- pred random_float(float, RNG, RNG) <= rng(RNG).
:- mode random_float(out, in, out) is det.

    % Generate two random floats from a normal distribution with
    % mean 0 and standard deviation 1, using the Box-Muller method.
    %
    % We generate two at a time for efficiency; they are independent of
    % each other.
    %
:- pred random_gauss(float, float, RNG, RNG) <= rng(RNG).
:- mode random_gauss(out, out, in, out) is det.

%---------------------------------------------------------------------------%

    % Interface to random number generators.
    %
:- typeclass rng(RNG) where [

        % Generate a random integer between 0 and random_max, inclusive.
        %
    pred random(uint64, RNG, RNG),
    mode random(out, in, out) is det,

        % Return the largest integer that can be generated.
        %
    func random_max(RNG) = uint64
].

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % urandom_int(RP, Start, Range, N, !RS)
    %
    % Generate a random integer between Start and Start+Range-1 inclusive.
    % Throws an exception if Range < 1 or Range > urandom_max.
    %
:- pred urandom_int(RP, int, int, int, RS, RS) <= urng(RP, RS).
:- mode urandom_int(in, in, in, out, di, uo) is det.

    % Generate a random float between 0.0 and 1.0, inclusive.
    %
:- pred urandom_float(RP, float, RS, RS) <= urng(RP, RS).
:- mode urandom_float(in, out, di, uo) is det.

    % Generate two random floats from a normal distribution with
    % mean 0 and standard deviation 1, using the Box-Muller method.
    %
    % We generate two at a time for efficiency; they are independent of
    % each other.
    %
:- pred urandom_gauss(RP, float, float, RS, RS) <= urng(RP, RS).
:- mode urandom_gauss(in, out, out, di, uo) is det.

%---------------------------------------------------------------------------%

    % Interface to unique random number generators. Callers need to
    % ensure they preserve the uniqueness of the random state, and in
    % turn instances can use destructive update on it.
    %
:- typeclass urng(RP, RS) <= (RP -> RS) where [

        % Generate a random integer between 0 and random_max, inclusive.
        %
    pred urandom(RP, uint64, RS, RS),
    mode urandom(in, out, di, uo) is det,

        % Return the largest integer that can be generated.
        %
    func urandom_max(RP) = uint64
].

:- typeclass urng_dup(RS) where [

        % urandom_dup(!RS, !:RSdup)
        %
        % Create a duplicate random state that will generate the
        % same sequence of integers.
        %
    pred urandom_dup(RS, RS, RS),
    mode urandom_dup(di, uo, uo) is det
].

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Convert any rng into a urng. This creates some additional overhead
    % in the form of additional typeclass method calls.
    %
:- type urng_params(RNG).
:- type urng_state(RNG).

:- instance urng(urng_params(RNG), urng_state(RNG)) <= rng(RNG).
:- instance urng_dup(urng_state(RNG)) <= rng(RNG).

:- pred make_urng(RNG, urng_params(RNG), urng_state(RNG)) <= rng(RNG).
:- mode make_urng(in, out, uo) is det.

%---------------------------------------------------------------------------%

    % Convert any urng into an rng. This duplicates the state every time
    % a random number is generated, hence may use significantly more
    % memory than if the unique version is used directly.
    %
:- type shared_rng(RP, RS).

:- instance rng(shared_rng(RP, RS)) <= (urng(RP, RS), urng_dup(RS)).

:- func make_shared_rng(RP, RS) = shared_rng(RP, RS).
:- mode make_shared_rng(in, di) = out is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module float.
:- import_module math.
:- import_module uint64.

%---------------------------------------------------------------------------%

random_int(Start, Range0, N, !RNG) :-
    Range = uint64.det_from_int(Range0),
    random(N0, !RNG),
    Max = random_max(!.RNG),
    N1 = N0 // (Max // Range),
    ( if N1 < Range then
        N = Start + uint64.cast_to_int(N1)
    else
        random_int(Start, Range0, N, !RNG)
    ).

random_float(F, !RNG) :-
    random(N, !RNG),
    Max = random_max(!.RNG),
    F = float.from_uint64(N) / float.from_uint64(Max).

random_gauss(U, V, !RNG) :-
    random_float(X, !RNG),
    random_float(Y, !RNG),
    ( if gauss(X, Y, U0, V0) then
        U = U0,
        V = V0
    else
        random_gauss(U, V, !RNG)
    ).

%---------------------------------------------------------------------------%

urandom_int(RP, Start, Range0, N, !RS) :-
    Range = uint64.det_from_int(Range0),
    urandom(RP, N0, !RS),
    Max = urandom_max(RP),
    N1 = N0 // (Max // Range),
    ( if N1 < Range then
        N = Start + uint64.cast_to_int(N1)
    else
        urandom_int(RP, Start, Range0, N, !RS)
    ).

urandom_float(RP, F, !RS) :-
    urandom(RP, N, !RS),
    Max = urandom_max(RP),
    F = float.from_uint64(N) / float.from_uint64(Max).

urandom_gauss(RP, U, V, !RS) :-
    urandom_float(RP, X, !RS),
    urandom_float(RP, Y, !RS),
    ( if gauss(X, Y, U0, V0) then
        U = U0,
        V = V0
    else
        urandom_gauss(RP, U, V, !RS)
    ).

%---------------------------------------------------------------------------%

:- pred gauss(float, float, float, float).
:- mode gauss(in, in, out, out) is semidet.

gauss(X0, Y0, U, V) :-
    X = 2.0 * X0 - 1.0,
    Y = 2.0 * Y0 - 1.0,
    S = X * X + Y * Y,
    S > 0.0,
    S < 1.0,
    Fac = math.sqrt(-2.0 * math.ln(S) / S),
    U = X * Fac,
    V = Y * Fac.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type urng_params(RNG)
    --->    urng_params(
                urng_max :: uint64
            ).

:- type urng_state(RNG)
    --->    urng_state(
                urng_rng :: RNG
            ).

:- instance urng(urng_params(RNG), urng_state(RNG)) <= rng(RNG) where [
    ( urandom(_, N, RS0, RS) :-
        RS0 = urng_state(RNG0),
        random(N, RNG0, RNG),
        RS = unsafe_promise_unique(urng_state(RNG))
    ),
    ( urandom_max(RP) = RP ^ urng_max )
].

:- instance urng_dup(urng_state(RNG)) <= rng(RNG) where [
    ( urandom_dup(RS, RS1, RS2) :-
        RS1 = unsafe_promise_unique(RS),
        RS2 = unsafe_promise_unique(RS)
    )
].

make_urng(RNG, RP, RS) :-
    RP = urng_params(random_max(RNG)),
    RS = unsafe_promise_unique(urng_state(RNG)).

%---------------------------------------------------------------------------%

:- type shared_rng(RP, RS)
    --->    shared_rng(
                shared_rng_params :: RP,
                shared_rng_state :: RS
            ).

:- instance rng(shared_rng(RP, RS)) <= (urng(RP, RS), urng_dup(RS)) where [
    ( random(N, RNG0, RNG) :-
        RNG0 = shared_rng(RP, RS0),
        RS1 = unsafe_promise_unique(RS0),
        urandom_dup(RS1, _, RS2),
        urandom(RP, N, RS2, RS),
        RNG = shared_rng(RP, RS)
    ),
    ( random_max(RNG) = urandom_max(RNG ^ shared_rng_params) )
].

make_shared_rng(RP, RS) = shared_rng(RP, RS).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

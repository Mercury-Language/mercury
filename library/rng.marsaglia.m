%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: rng.marsaglia.m
% Main author: Mark Brown
%
% Very fast concatenation of two 16-bit MWC generators.
%
% http://gcrhoads.byethost4.com/Code/Random/marsaglia.c
%
% "Algorithm recommended by Marsaglia."
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module rng.marsaglia.
:- interface.

%---------------------------------------------------------------------------%

:- type marsaglia.

:- instance rng(marsaglia).

    % Initialise a marsaglia RNG with the default seed.
    %
:- func init = marsaglia.

    % Initialise a marsaglia RNG with the given seed.
    %
:- func seed(uint32, uint32) = marsaglia.

%---------------------------------------------------------------------------%

    % Generate a random number between 0 and max_uint32.
    %
:- pred rand(uint32, marsaglia, marsaglia).
:- mode rand(out, in, out) is det.

    % Return max_uint32, the maximum number that can be returned by this
    % generator.
    %
:- func rand_max(marsaglia) = uint32.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint32.

%---------------------------------------------------------------------------%

:- type marsaglia
    --->    marsaglia(uint64).

:- instance rng(marsaglia) where [
    ( random(N, !RNG) :-
        rand(N0, !RNG),
        N = uint32.cast_to_uint64(N0)
    ),
    ( random_max(RNG) = uint32.cast_to_uint64(rand_max(RNG)) )
].

%---------------------------------------------------------------------------%

init = seed(0u32, 0u32).

seed(SX0, SY0) = RNG :-
    SX = ( if SX0 = 0u32 then 521288629u32 else SX0 ),
    SY = ( if SY0 = 0u32 then 362436069u32 else SY0 ),
    RNG = marsaglia(pack_uint64(SX, SY)).

%---------------------------------------------------------------------------%

rand(N, RNG0, RNG) :-
    RNG0 = marsaglia(S0),
    unpack_uint64(S0, SX0, SY0),
    A = 18000u32,
    B = 30903u32,
    M = 0xffffu32,
    SX = A * (SX0 /\ M) + (SX0 >> 16),
    SY = B * (SY0 /\ M) + (SY0 >> 16),
    N = (SX << 16) + (SY /\ M),
    S = pack_uint64(SX, SY),
    RNG = marsaglia(S).

rand_max(_) = uint32.max_uint32.

%---------------------------------------------------------------------------%

:- func pack_uint64(uint32, uint32) = uint64.

pack_uint64(Hi, Lo) =
    (uint32.cast_to_uint64(Hi) << 32) + uint32.cast_to_uint64(Lo).

:- pred unpack_uint64(uint64, uint32, uint32).
:- mode unpack_uint64(in, out, out) is det.

unpack_uint64(S, Hi, Lo) :-
    Hi = uint32.cast_from_uint64(S >> 32),
    Lo = uint32.cast_from_uint64(S /\ 0xffffffffu64).

%---------------------------------------------------------------------------%

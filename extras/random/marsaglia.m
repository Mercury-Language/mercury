%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: marsaglia.m
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

:- module marsaglia.
:- interface.

:- import_module random.

%---------------------------------------------------------------------------%

:- type random.

:- instance random(random).

    % Initialise a marsaglia generator with the default seed.
    %
:- func init = random.

    % Initialise a marsaglia generator with the given seed.
    %
:- func seed(uint32, uint32) = random.

    % Generate a uniformly distributed pseudo-random unsigned integer
    % of 8, 16, 32 or 64 bytes, respectively.
    %
:- pred gen_uint8(uint8::out, random::in, random::out) is det.
:- pred gen_uint16(uint16::out, random::in, random::out) is det.
:- pred gen_uint32(uint32::out, random::in, random::out) is det.
:- pred gen_uint64(uint64::out, random::in, random::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint8.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.

%---------------------------------------------------------------------------%

:- type random
    --->    random(uint64).

:- instance random(random) where [
    pred(gen_uint8/3) is marsaglia.gen_uint8,
    pred(gen_uint16/3) is marsaglia.gen_uint16,
    pred(gen_uint32/3) is marsaglia.gen_uint32,
    pred(gen_uint64/3) is marsaglia.gen_uint64
].

init = seed(0u32, 0u32).

seed(SX0, SY0) = R :-
    SX = ( if SX0 = 0u32 then 521288629u32 else SX0 ),
    SY = ( if SY0 = 0u32 then 362436069u32 else SY0 ),
    R = random(pack_uint64(SX, SY)).

%---------------------------------------------------------------------------%

gen_uint8(N, !R) :-
    marsaglia.gen_uint32(N0, !R),
    N1 = uint32.cast_to_int(N0 >> 24),
    N = uint8.cast_from_int(N1).

gen_uint16(N, !R) :-
    marsaglia.gen_uint32(N0, !R),
    N1 = uint32.cast_to_int(N0 >> 16),
    N = uint16.cast_from_int(N1).

gen_uint64(N, !R) :-
    marsaglia.gen_uint32(A0, !R),
    marsaglia.gen_uint32(B0, !R),
    A = uint32.cast_to_uint64(A0),
    B = uint32.cast_to_uint64(B0),
    N = A + (B << 32).

%---------------------------------------------------------------------------%

gen_uint32(N, R0, R) :-
    R0 = random(S0),
    unpack_uint64(S0, SX0, SY0),
    A = 18000u32,
    B = 30903u32,
    M = 0xffffu32,
    SX = A * (SX0 /\ M) + (SX0 >> 16),
    SY = B * (SY0 /\ M) + (SY0 >> 16),
    N = (SX << 16) + (SY /\ M),
    S = pack_uint64(SX, SY),
    R = random(S).

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

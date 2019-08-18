%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: rng.sfc.m
% Main author: Mark Brown
%
% Small Fast Counting generators, by Chris Doty-Humphrey.
%
% http://pracrand.sourceforge.net/
%
% From the above:
% "[A] good small chaotic RNG driven by a bad smaller linear RNG. The
% combination gives it the strengths of each - good chaotic behavior,
% but enough structure to avoid short cycles."
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module rng.sfc.
:- interface.

%---------------------------------------------------------------------------%

    % A fast, 16-bit SFC generator.
    %
:- type sfc.

:- instance rng(sfc).

    % Initialise a 16-bit SFC RNG with the default seed.
    %
:- func init16 = sfc.

    % Initialise a 16-bit SFC RNG with the given seed.
    %
:- func seed16(uint64) = sfc.

    % Generate a random number between 0 and max_uint16.
    %
:- pred rand16(uint16, sfc, sfc).
:- mode rand16(out, in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % A fast, 64-bit SFC generator with unique state.
    %
:- type params.
:- type state.

:- instance urng(params, state).
:- instance urng_dup(state).

    % Initialise a 64-bit SFC RNG with the default seed.
    %
:- pred init(params, state).
:- mode init(out, uo) is det.

    % Initialise a 64-bit SFC RNG with the given seed.
    %
:- pred seed(uint64, uint64, uint64, params, state).
:- mode seed(in, in, in, out, uo) is det.

%---------------------------------------------------------------------------%

    % Generate a random number between 0 and max_uint64. Note that the
    % params are not required for this RNG unless calling via the
    % typeclass interface.
    %
:- pred rand(uint64, state, state).
:- mode rand(out, di, uo) is det.

    % Duplicate a 64-bit SFC state.
    %
:- pred dup(state, state, state).
:- mode dup(di, uo, uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % A fast, 32-bit SFC generator with unique state. This may achieve
    % better performance on 32-bit architectures, but generally does not
    % have the quality of the 64-bit generator or the low heap usage of
    % the 16-bit generator.
    %
:- type params32.
:- type state32.

:- instance urng(params32, state32).
:- instance urng_dup(state32).

    % Initialise a 32-bit SFC RNG with the default seed.
    %
:- pred init32(params32, state32).
:- mode init32(out, uo) is det.

    % Initialise a 32-bit SFC RNG with the given seed.
    %
:- pred seed32(uint32, uint32, uint32, params32, state32).
:- mode seed32(in, in, in, out, uo) is det.

%---------------------------------------------------------------------------%

    % Generate a random number between 0 and max_uint32. Note that the
    % params are not required for this RNG unless calling via the
    % typeclass interface.
    %
:- pred rand32(uint32, state32, state32).
:- mode rand32(out, di, uo) is det.

    % Duplicate a 32-bit SFC state.
    %
:- pred dup32(state32, state32, state32).
:- mode dup32(di, uo, uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module int.
:- import_module list.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.

%---------------------------------------------------------------------------%

:- type sfc
    --->    sfc(uint64).

:- instance rng(sfc) where [
    ( random(N, !RNG) :-
        rand16(N0, !RNG),
        N = uint16.cast_to_uint64(N0)
    ),
    ( random_max(_) = uint16.cast_to_uint64(uint16.max_uint16) )
].

init16 = seed16(0x6048_5623_5e79_371e_u64).

seed16(Seed) = RNG :-
    skip16(10, sfc(Seed), RNG).

:- pred skip16(int, sfc, sfc).
:- mode skip16(in, in, out) is det.

skip16(N, !RNG) :-
    ( if N > 0 then
        rand16(_, !RNG),
        skip16(N - 1, !RNG)
    else
        true
    ).

%---------------------------------------------------------------------------%

rand16(N, sfc(S0), sfc(S)) :-
    unpack_uint64(S0, A0, B0, C0, Counter0),
    N = A0 + B0 + Counter0,
    A = B0 `xor` (B0 >> 5),
    B = C0 + (C0 << 3),
    C = ((C0 << 6) \/ (C0 >> 10)) + N,
    Counter = Counter0 + 1u16,
    S = pack_uint64(A, B, C, Counter).

:- func pack_uint64(uint16, uint16, uint16, uint16) = uint64.

pack_uint64(P1, P2, P3, P4) =
    (uint16.cast_to_uint64(P1) << 48) +
    (uint16.cast_to_uint64(P2) << 32) +
    (uint16.cast_to_uint64(P3) << 16) +
    uint16.cast_to_uint64(P4).

:- pred unpack_uint64(uint64, uint16, uint16, uint16, uint16).
:- mode unpack_uint64(in, out, out, out, out) is det.

unpack_uint64(S, P1, P2, P3, P4) :-
    Mask = 0xffffu64,
    P1 = uint16.cast_from_uint64(S >> 48),
    P2 = uint16.cast_from_uint64((S >> 32) /\ Mask),
    P3 = uint16.cast_from_uint64((S >> 16) /\ Mask),
    P4 = uint16.cast_from_uint64(S /\ Mask).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type params
    --->    params.

:- type state
    --->    state(array(uint64)).

:- instance urng(params, state) where [
    ( urandom(_, N, !RS) :-
        rand(N, !RS)
    ),
    ( urandom_max(_) = uint64.max_uint64 )
].

:- instance urng_dup(state) where [
    pred(urandom_dup/3) is dup
].

dup(S, S1, S2) :-
    S = state(A),
    Sc = state(array.copy(A)),
    S1 = unsafe_promise_unique(S),
    S2 = unsafe_promise_unique(Sc).

%---------------------------------------------------------------------------%

init(RP, RS) :-
    sfc.seed(
        0x9578_32f2_b9e1_43b1_u64,
        0x9578_32f2_b9e1_43b1_u64,
        0x9578_32f2_b9e1_43b1_u64,
        RP, RS).

seed(A, B, C, params, RS) :-
    Counter = 1u64,
    S0 = array([A, B, C, Counter]),
    RS0 = unsafe_promise_unique(state(S0)),
    skip(18, RS0, RS).

:- pred skip(int, state, state).
:- mode skip(in, di, uo) is det.

skip(N, !RS) :-
    ( if N > 0 then
        rand(_, !RS),
        skip(N - 1, !RS)
    else
        true
    ).

%---------------------------------------------------------------------------%

rand(N, RS0, RS) :-
    RS0 = state(S0),
    array.unsafe_lookup(S0, 0, A0),
    array.unsafe_lookup(S0, 1, B0),
    array.unsafe_lookup(S0, 2, C0),
    array.unsafe_lookup(S0, 3, Counter0),
    N = A0 + B0 + Counter0,
    A = B0 `xor` (B0 >> 11),
    B = C0 + (C0 << 3),
    C = ((C0 << 24) \/ (C0 >> 40)) + N,
    Counter = Counter0 + 1u64,
    array.unsafe_set(0, A, S0, S1),
    array.unsafe_set(1, B, S1, S2),
    array.unsafe_set(2, C, S2, S3),
    array.unsafe_set(3, Counter, S3, S),
    RS = unsafe_promise_unique(state(S)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type params32
    --->    params32.

:- type state32
    --->    state32(array(uint32)).

:- instance urng(params32, state32) where [
    ( urandom(_, N, !RS) :-
        rand32(N0, !RS),
        N = uint32.cast_to_uint64(N0)
    ),
    ( urandom_max(_) = uint32.cast_to_uint64(uint32.max_uint32) )
].

:- instance urng_dup(state32) where [
    pred(urandom_dup/3) is dup32
].

dup32(S, S1, S2) :-
    S = state32(A),
    Sc = state32(array.copy(A)),
    S1 = unsafe_promise_unique(S),
    S2 = unsafe_promise_unique(Sc).

%---------------------------------------------------------------------------%

init32(RP, RS) :-
    sfc.seed32(
        0x0_u32,
        0xf16c_a8bb_u32,
        0x20a3_6f2d_u32,
        RP, RS).

seed32(A, B, C, params32, RS) :-
    Counter = 1u32,
    S0 = array([A, B, C, Counter]),
    RS0 = unsafe_promise_unique(state32(S0)),
    skip32(15, RS0, RS).

:- pred skip32(int, state32, state32).
:- mode skip32(in, di, uo) is det.

skip32(N, !RS) :-
    ( if N > 0 then
        rand32(_, !RS),
        skip32(N - 1, !RS)
    else
        true
    ).

%---------------------------------------------------------------------------%

rand32(N, RS0, RS) :-
    RS0 = state32(S0),
    array.unsafe_lookup(S0, 0, A0),
    array.unsafe_lookup(S0, 1, B0),
    array.unsafe_lookup(S0, 2, C0),
    array.unsafe_lookup(S0, 3, Counter0),
    N = A0 + B0 + Counter0,
    A = B0 `xor` (B0 >> 9),
    B = C0 + (C0 << 3),
    C = ((C0 << 21) \/ (C0 >> 11)) + N,
    Counter = Counter0 + 1u32,
    array.unsafe_set(0, A, S0, S1),
    array.unsafe_set(1, B, S1, S2),
    array.unsafe_set(2, C, S2, S3),
    array.unsafe_set(3, Counter, S3, S),
    RS = unsafe_promise_unique(state32(S)).

%---------------------------------------------------------------------------%

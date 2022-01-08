%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: random.sfc16.m
% Main author: Mark Brown
%
% 16-bit Small Fast Counting generator, by Chris Doty-Humphrey.
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

:- module random.sfc16.
:- interface.

%---------------------------------------------------------------------------%

    % A fast, 16-bit SFC generator.
    %
:- type random.

:- instance random(random).

    % Initialise a 16-bit SFC generator with the default seed. The
    % resulting generator produces the same sequence every time.
    %
:- func init = random.

    % Initialise a 16-bit SFC generator with the given seed.
    %
:- func seed(uint64) = random.

    % Generate a uniformly distributed pseudo-random unsigned integer
    % of 8, 16, 32 or 64 bits, respectively.
    %
:- pred generate_uint8(uint8::out, random::in, random::out) is det.
:- pred generate_uint16(uint16::out, random::in, random::out) is det.
:- pred generate_uint32(uint32::out, random::in, random::out) is det.
:- pred generate_uint64(uint64::out, random::in, random::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module uint.
:- import_module uint8.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.

%---------------------------------------------------------------------------%

:- type random
    --->    random(uint64).

:- instance random(random) where [
    pred(generate_uint8/3) is sfc16.generate_uint8,
    pred(generate_uint16/3) is sfc16.generate_uint16,
    pred(generate_uint32/3) is sfc16.generate_uint32,
    pred(generate_uint64/3) is sfc16.generate_uint64
].

init = seed(0x6048_5623_5e79_371e_u64).

seed(Seed) = R :-
    skip(10, random(Seed), R).

:- pred skip(int::in, random::in, random::out) is det.

skip(N, !R) :-
    ( if N > 0 then
        sfc16.generate_uint16(_, !R),
        skip(N - 1, !R)
    else
        true
    ).

%---------------------------------------------------------------------------%

generate_uint8(N, !R) :-
    sfc16.generate_uint16(N0, !R),
    N1 = uint16.to_int(N0 >> 8),
    N = uint8.cast_from_int(N1).

generate_uint16(N, random(S0), random(S)) :-
    unpack_uint64(S0, A0, B0, C0, Counter0),
    N = A0 + B0 + Counter0,
    A = B0 `xor` (B0 >> 5),
    B = C0 + (C0 << 3),
    C = ((C0 << 6) \/ (C0 >> 10)) + N,
    Counter = Counter0 + 1u16,
    S = pack_uint64(A, B, C, Counter).

generate_uint32(N, !R) :-
    sfc16.generate_uint16(A0, !R),
    sfc16.generate_uint16(B0, !R),
    A = uint16.cast_to_uint(A0),
    B = uint16.cast_to_uint(B0),
    N = uint32.cast_from_uint(A + (B << 16)).

generate_uint64(N, !R) :-
    sfc16.generate_uint16(A, !R),
    sfc16.generate_uint16(B, !R),
    sfc16.generate_uint16(C, !R),
    sfc16.generate_uint16(D, !R),
    N = pack_uint64(A, B, C, D).

%---------------------------------------------------------------------------%

:- func pack_uint64(uint16, uint16, uint16, uint16) = uint64.

pack_uint64(P1, P2, P3, P4) =
    uint16.cast_to_uint64(P1) +
    (uint16.cast_to_uint64(P2) << 16) +
    (uint16.cast_to_uint64(P3) << 32) +
    (uint16.cast_to_uint64(P4) << 48).

:- pred unpack_uint64(uint64::in, uint16::out, uint16::out, uint16::out,
    uint16::out) is det.

unpack_uint64(S, P1, P2, P3, P4) :-
    Mask = 0xffffu64,
    P1 = uint16.cast_from_uint64(S /\ Mask),
    P2 = uint16.cast_from_uint64((S >> 16) /\ Mask),
    P3 = uint16.cast_from_uint64((S >> 32) /\ Mask),
    P4 = uint16.cast_from_uint64(S >> 48).

%---------------------------------------------------------------------------%
:- end_module random.sfc16.
%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: random.sfc64.m
% Main author: Mark Brown
%
% 64-bit Small Fast Counting generator, by Chris Doty-Humphrey.
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

:- module random.sfc64.
:- interface.

%---------------------------------------------------------------------------%

    % A fast, 64-bit SFC generator with unique state.
    %
:- type params.
:- type ustate.

:- instance urandom(params, ustate).
:- instance urandom_dup(ustate).

    % Initialise a 64-bit SFC generator with the default seed. The
    % resulting generator produces the same sequence every time.
    %
:- pred init(params::out, ustate::uo) is det.

    % Initialise a 64-bit SFC generator with the given seed.
    %
:- pred seed(uint64::in, uint64::in, uint64::in, params::out, ustate::uo)
    is det.

    % Generate a uniformly distributed pseudo-random unsigned integer
    % of 8, 16, 32 or 64 bits, respectively.
    %
:- pred generate_uint8(params::in, uint8::out,
    ustate::di, ustate::uo) is det.
:- pred generate_uint16(params::in, uint16::out,
    ustate::di, ustate::uo) is det.
:- pred generate_uint32(params::in, uint32::out,
    ustate::di, ustate::uo) is det.
:- pred generate_uint64(params::in, uint64::out,
    ustate::di, ustate::uo) is det.

    % Duplicate a 64-bit SFC state.
    %
:- pred urandom_dup(ustate::di, ustate::uo, ustate::uo) is det.

%---------------------------------------------------------------------------%

    % Generate a uniformly distributed pseudo-random unsigned integer
    % of 8, 16, 32 or 64 bits, respectively.
    %
    % As above, but does not require the params argument (which is a dummy
    % type only needed to satisfy the typeclass interface).
    %
:- pred generate_uint8(uint8::out, ustate::di, ustate::uo) is det.
:- pred generate_uint16(uint16::out, ustate::di, ustate::uo) is det.
:- pred generate_uint32(uint32::out, ustate::di, ustate::uo) is det.
:- pred generate_uint64(uint64::out, ustate::di, ustate::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module int.
:- import_module uint8.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.

%---------------------------------------------------------------------------%

:- type params
    --->    params.

:- type ustate
    --->    ustate(array(uint64)).

:- instance urandom(params, ustate) where [
    pred(generate_uint8/4) is sfc64.generate_uint8,
    pred(generate_uint16/4) is sfc64.generate_uint16,
    pred(generate_uint32/4) is sfc64.generate_uint32,
    pred(generate_uint64/4) is sfc64.generate_uint64
].

:- instance urandom_dup(ustate) where [
    pred(urandom_dup/3) is sfc64.urandom_dup
].

%---------------------------------------------------------------------------%

init(P, S) :-
    seed(
        0x9578_32f2_b9e1_43b1_u64,
        0x9578_32f2_b9e1_43b1_u64,
        0x9578_32f2_b9e1_43b1_u64,
        P, S).

seed(A, B, C, params, S) :-
    Counter = 1u64,
    Seed0 = array([A, B, C, Counter]),
    S0 = unsafe_promise_unique(ustate(Seed0)),
    skip(18, S0, S).

:- pred skip(int::in, ustate::di, ustate::uo) is det.

skip(N, !S) :-
    ( if N > 0 then
        sfc64.generate_uint64(_, !S),
        skip(N - 1, !S)
    else
        true
    ).

%---------------------------------------------------------------------------%

generate_uint8(_, N, !S) :-
    sfc64.generate_uint8(N, !S).

generate_uint16(_, N, !S) :-
    sfc64.generate_uint16(N, !S).

generate_uint32(_, N, !S) :-
    sfc64.generate_uint32(N, !S).

generate_uint64(_, N, !S) :-
    sfc64.generate_uint64(N, !S).

urandom_dup(S, S1, S2) :-
    S = ustate(A),
    Sc = ustate(array.copy(A)),
    S1 = unsafe_promise_unique(S),
    S2 = unsafe_promise_unique(Sc).

%---------------------------------------------------------------------------%

generate_uint8(N, !S) :-
    sfc64.generate_uint64(N0, !S),
    N1 = uint64.cast_to_int(N0 >> 56),
    N = uint8.cast_from_int(N1).

generate_uint16(N, !S) :-
    sfc64.generate_uint64(N0, !S),
    N1 = uint64.cast_to_int(N0 >> 48),
    N = uint16.cast_from_int(N1).

generate_uint32(N, !S) :-
    sfc64.generate_uint64(N0, !S),
    N = uint32.cast_from_uint64(N0 >> 32).

generate_uint64(N, RS0, RS) :-
    RS0 = ustate(S0),
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
    RS = unsafe_promise_unique(ustate(S)).

%---------------------------------------------------------------------------%
:- end_module random.sfc64.
%---------------------------------------------------------------------------%

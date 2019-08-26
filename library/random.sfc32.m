%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: random.sfc32.m
% Main author: Mark Brown
%
% 32-bit Small Fast Counting generator, by Chris Doty-Humphrey.
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

:- module random.sfc32.
:- interface.

%---------------------------------------------------------------------------%

    % A fast, 32-bit SFC generator with unique state. This may achieve
    % better performance on 32-bit architectures, but generally does not
    % have the quality of the 64-bit generator or the low heap usage of
    % the 16-bit generator.
    %
:- type params.
:- type ustate.

:- instance urandom(params, ustate).
:- instance urandom_dup(ustate).

    % Initialise a 32-bit SFC generator with the default seed.
    %
:- pred init(params::out, ustate::uo) is det.

    % Initialise a 32-bit SFC generator with the given seed.
    %
:- pred seed(uint32::in, uint32::in, uint32::in, params::out, ustate::uo)
    is det.

    % Generate a uniformly distributed pseudo-random unsigned integer
    % of 8, 16, 32 or 64 bits, respectively.
    %
:- pred gen_uint8(params::in, uint8::out, ustate::di, ustate::uo) is det.
:- pred gen_uint16(params::in, uint16::out, ustate::di, ustate::uo) is det.
:- pred gen_uint32(params::in, uint32::out, ustate::di, ustate::uo) is det.
:- pred gen_uint64(params::in, uint64::out, ustate::di, ustate::uo) is det.

    % Duplicate a 32-bit SFC state.
    %
:- pred urandom_dup(ustate::di, ustate::uo, ustate::uo) is det.

%---------------------------------------------------------------------------%

    % Generate a uniformly distributed pseudo-random unsigned integer
    % of 8, 16, 32 or 64 bits, respectively.
    %
    % As above, but does not require the params argument (which is a dummy
    % type only needed to satisfy the typeclass interface).
    %
:- pred gen_uint8(uint8::out, ustate::di, ustate::uo) is det.
:- pred gen_uint16(uint16::out, ustate::di, ustate::uo) is det.
:- pred gen_uint32(uint32::out, ustate::di, ustate::uo) is det.
:- pred gen_uint64(uint64::out, ustate::di, ustate::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module int.
:- import_module list.
:- import_module uint8.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.

%---------------------------------------------------------------------------%

:- type params
    --->    params.

:- type ustate
    --->    ustate(array(uint32)).

:- instance urandom(params, ustate) where [
    pred(gen_uint8/4) is sfc32.gen_uint8,
    pred(gen_uint16/4) is sfc32.gen_uint16,
    pred(gen_uint32/4) is sfc32.gen_uint32,
    pred(gen_uint64/4) is sfc32.gen_uint64
].

:- instance urandom_dup(ustate) where [
    pred(urandom_dup/3) is sfc32.urandom_dup
].

urandom_dup(S, S1, S2) :-
    S = ustate(A),
    Sc = ustate(array.copy(A)),
    S1 = unsafe_promise_unique(S),
    S2 = unsafe_promise_unique(Sc).

%---------------------------------------------------------------------------%

init(P, S) :-
    seed(0x0_u32, 0xf16c_a8bb_u32, 0x20a3_6f2d_u32, P, S).

seed(A, B, C, params, S) :-
    Counter = 1u32,
    Seed0 = array([A, B, C, Counter]),
    S0 = unsafe_promise_unique(ustate(Seed0)),
    skip(15, S0, S).

:- pred skip(int::in, ustate::di, ustate::uo) is det.

skip(N, !S) :-
    ( if N > 0 then
        sfc32.gen_uint32(_, !S),
        skip(N - 1, !S)
    else
        true
    ).

%---------------------------------------------------------------------------%

gen_uint8(_, N, !S) :-
    sfc32.gen_uint8(N, !S).

gen_uint16(_, N, !S) :-
    sfc32.gen_uint16(N, !S).

gen_uint32(_, N, !S) :-
    sfc32.gen_uint32(N, !S).

gen_uint64(_, N, !S) :-
    sfc32.gen_uint64(N, !S).

%---------------------------------------------------------------------------%

gen_uint8(N, !S) :-
    sfc32.gen_uint32(N0, !S),
    N1 = uint32.cast_to_int(N0 >> 24),
    N = uint8.cast_from_int(N1).

gen_uint16(N, !S) :-
    sfc32.gen_uint32(N0, !S),
    N1 = uint32.cast_to_int(N0 >> 16),
    N = uint16.cast_from_int(N1).

gen_uint64(N, !S) :-
    sfc32.gen_uint32(A0, !S),
    sfc32.gen_uint32(B0, !S),
    A = uint32.cast_to_uint64(A0),
    B = uint32.cast_to_uint64(B0),
    N = A + (B << 32).

%---------------------------------------------------------------------------%

gen_uint32(N, RS0, RS) :-
    RS0 = ustate(S0),
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
    RS = unsafe_promise_unique(ustate(S)).

%---------------------------------------------------------------------------%

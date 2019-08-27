%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: tausworthe.m
% Main author: Mark Brown
%
% Combined Tausworthe-type generators. See:
%
% Pierre L'Ecuyer, "Maximally Equidistributed Combined Tausworthe Generators",
%   Mathematics of Computation, vol. 65, no. 213 (1996)
% Pierre L'Ecuyer, "Tables of Maximally-Equidistributed Combined LFSR
%   Generators", Mathematics of Computation, vol. 68, no. 225 (1999)
%
% http://gcrhoads.byethost4.com/Code/Random/tausworth.c
% http://gcrhoads.byethost4.com/Code/Random/tausworth4.c
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module tausworthe.
:- interface.

:- import_module maybe.
:- import_module random.

%---------------------------------------------------------------------------%

:- type params.
:- type ustate.

:- instance urandom(params, ustate).
:- instance urandom_dup(ustate).

    % Initialise a 3-combo tausworthe generator with the default seed
    % and parameters.
    %
:- pred init_t3(params::out, ustate::uo) is det.

    % Initialise a 4-combo tausworthe generator with the default seed
    % and parameters.
    %
:- pred init_t4(params::out, ustate::uo) is det.

    % Initialise a 3-combo tausworthe generator with the given seed.
    % If given, the first argument selects from one of two sets of
    % parameters, depending on its value modulo 2.
    %
:- pred seed_t3(maybe(int)::in, uint32::in, uint32::in, uint32::in,
    params::out, ustate::uo) is det.

    % Initialise a 4-combo tausworthe generator with the given seed.
    % If given, the first argument selects from one of 62 sets of
    % parameters, depending on its value modulo 62.
    %
:- pred seed_t4(maybe(int)::in, uint32::in, uint32::in, uint32::in, uint32::in,
    params::out, ustate::uo) is det.

%---------------------------------------------------------------------------%

    % Generate a uniformly distributed pseudo-random unsigned integer
    % of 8, 16, 32 or 64 bits, respectively.
    %
    % Throws an exception if the params and ustate are not the same size
    % (i.e., both 3-combo or both 4-combo).
    %
:- pred generate_uint8(params::in, uint8::out, ustate::di, ustate::uo) is det.
:- pred generate_uint16(params::in, uint16::out, ustate::di, ustate::uo) is det.
:- pred generate_uint32(params::in, uint32::out, ustate::di, ustate::uo) is det.
:- pred generate_uint64(params::in, uint64::out, ustate::di, ustate::uo) is det.

    % Duplicate a tausworthe RNG state.
    %
:- pred urandom_dup(ustate::di, ustate::uo, ustate::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module uint8.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.

%---------------------------------------------------------------------------%

:- type params
    --->    params(
                qs :: array(int),
                ps :: array(int),
                shft :: array(int),
                mask :: array(uint32)
            ).

:- type ustate
    --->    ustate(
                seed :: array(uint32)
            ).

:- instance urandom(params, ustate) where [
    pred(generate_uint8/4) is tausworthe.generate_uint8,
    pred(generate_uint16/4) is tausworthe.generate_uint16,
    pred(generate_uint32/4) is tausworthe.generate_uint32,
    pred(generate_uint64/4) is tausworthe.generate_uint64
].

:- instance urandom_dup(ustate) where [
    pred(urandom_dup/3) is tausworthe.urandom_dup
].

urandom_dup(S, S1, S2) :-
    S = ustate(A),
    Sc = ustate(array.copy(A)),
    S1 = unsafe_promise_unique(S),
    S2 = unsafe_promise_unique(Sc).

%---------------------------------------------------------------------------%

generate_uint8(RP, N, !RS) :-
    tausworthe.generate_uint32(RP, N0, !RS),
    N1 = uint32.cast_to_int(N0 >> 24),
    N = uint8.cast_from_int(N1).

generate_uint16(RP, N, !RS) :-
    tausworthe.generate_uint32(RP, N0, !RS),
    N1 = uint32.cast_to_int(N0 >> 16),
    N = uint16.cast_from_int(N1).

generate_uint64(RP, N, !RS) :-
    tausworthe.generate_uint32(RP, A0, !RS),
    tausworthe.generate_uint32(RP, B0, !RS),
    A = uint32.cast_to_uint64(A0),
    B = uint32.cast_to_uint64(B0),
    N = A + (B << 32).

%---------------------------------------------------------------------------%

generate_uint32(RP, N, RS0, RS) :-
    RS0 = ustate(Seed0),
    Size = array.size(Seed0),
    rand(RP, 0, Size, 0u32, N, Seed0, Seed),
    RS = unsafe_promise_unique(ustate(Seed)).

:- pred rand(params::in, int::in, int::in, uint32::in, uint32::out,
    array(uint32)::array_di, array(uint32)::array_uo) is det.

rand(RP, I, Size, N0, N, !Seed) :-
    ( if I < Size then
        array.lookup(RP ^ qs, I, Q),
        array.lookup(RP ^ ps, I, P),
        array.lookup(RP ^ shft, I, Shft),
        array.lookup(RP ^ mask, I, Mask),
        array.lookup(!.Seed, I, S0),
        B = ((S0 << Q) `xor` S0) >> Shft,
        S = ((S0 /\ Mask) << P) `xor` B,
        array.set(I, S, !Seed),
        N1 = N0 `xor` S,
        rand(RP, I + 1, Size, N1, N, !Seed)
    else
        N = N0
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred seed(array(int)::in, array(int)::in, array(uint32)::array_di,
    params::out, ustate::uo) is det.

seed(Qs, Ps, Seed0, RP, RS) :-
    Size = array.size(Seed0),
    Ks = array([31, 29, 28, 25]),
    Ds = array([390451501u32, 613566701u32, 858993401u32, 943651322u32]),
    Shft0 = array.init(Size, 0),
    Mask0 = array.init(Size, 0u32),
    seed_2(0, Size, Ks, Ps, Ds, Shft0, Shft, Mask0, Mask, Seed0, Seed),
    RP = params(Qs, Ps, Shft, Mask),
    RS0 = unsafe_promise_unique(ustate(Seed)),
    tausworthe.generate_uint32(RP, _, RS0, RS).

:- pred seed_2(int::in, int::in, array(int)::in, array(int)::in,
    array(uint32)::in, array(int)::array_di, array(int)::array_uo,
    array(uint32)::array_di, array(uint32)::array_uo,
    array(uint32)::array_di, array(uint32)::array_uo) is det.

seed_2(I, Size, Ks, Ps, Ds, !Shft, !Mask, !Seed) :-
    ( if I < Size then
        array.lookup(Ks, I, K),
        array.lookup(Ps, I, P),
        array.lookup(!.Seed, I, S),
        J = 32 - K,
        array.set(I, K - P, !Shft),
        array.set(I, uint32.max_uint32 << J, !Mask),
        ( if S > (1u32 << J) then
            true
        else
            array.lookup(Ds, I, D),
            array.set(I, D, !Seed)
        ),
        seed_2(I + 1, Size, Ks, Ps, Ds, !Shft, !Mask, !Seed)
    else
        true
    ).

%---------------------------------------------------------------------------%

init_t3(RP, RS) :-
    seed_t3(no, 0u32, 0u32, 0u32, RP, RS).

seed_t3(MZ, A, B, C, RP, RS) :-
    (
        MZ = yes(Z)
    ;
        MZ = no,
        Z = 0
    ),
    ( if params_t3(Z mod 2, Q1, Q2, Q3, P1, P2, P3) then
        Qs = array([Q1, Q2, Q3]),
        Ps = array([P1, P2, P3])
    else
        unexpected($pred, "unexpected failure")
    ),
    Seed = array([A, B, C]),
    seed(Qs, Ps, Seed, RP, RS).

:- pred params_t3(int::in, int::out, int::out, int::out, int::out, int::out,
    int::out) is semidet.

params_t3(0, 13, 2, 3, 12, 4, 17).
params_t3(1, 3, 2, 13, 20, 16, 7).

%---------------------------------------------------------------------------%

init_t4(RP, RS) :-
    seed_t4(no, 0u32, 0u32, 0u32, 0u32, RP, RS).

seed_t4(MZ, A, B, C, D, RP, RS) :-
    (
        MZ = yes(Z)
    ;
        MZ = no,
        Z = 58
    ),
    ( if params_t4(Z mod 62, P1, P2, P3, P4) then
        Qs = array([6, 2, 13, 3]),
        Ps = array([P1, P2, P3, P4])
    else
        unexpected($pred, "unexpected failure")
    ),
    Seed = array([A, B, C, D]),
    seed(Qs, Ps, Seed, RP, RS).

:- pred params_t4(int::in, int::out, int::out, int::out, int::out) is semidet.

params_t4(0,  18, 2,  7,  13).
params_t4(1,  13, 3,  4,  9).
params_t4(2,  24, 3,  11, 12).
params_t4(3,  10, 4,  2,  6).
params_t4(4,  16, 4,  2,  12).
params_t4(5,  11, 5,  4,  3).
params_t4(6,  17, 5,  4,  6).
params_t4(7,  12, 5,  11, 9).
params_t4(8,  23, 5,  11, 12).
params_t4(9,  23, 6,  7,  8).
params_t4(10, 14, 8,  2,  9).
params_t4(11, 22, 8,  7,  4).
params_t4(12, 21, 8,  11, 4).
params_t4(13, 10, 9,  8,  2).
params_t4(14, 22, 9,  11, 9).
params_t4(15, 3,  10, 4,  15).
params_t4(16, 24, 10, 7,  8).
params_t4(17, 21, 10, 8,  4).
params_t4(18, 12, 10, 8,  15).
params_t4(19, 17, 10, 11, 6).
params_t4(20, 3,  11, 4,  12).
params_t4(21, 9,  11, 4,  13).
params_t4(22, 9,  11, 7,  4).
params_t4(23, 11, 12, 4,  10).
params_t4(24, 20, 12, 7,  15).
params_t4(25, 17, 12, 11, 11).
params_t4(26, 21, 13, 4,  14).
params_t4(27, 11, 14, 8,  7).
params_t4(28, 6,  14, 8,  13).
params_t4(29, 20, 15, 7,  13).
params_t4(30, 12, 16, 2,  10).
params_t4(31, 4,  16, 8,  3).
params_t4(32, 22, 17, 4,  6).
params_t4(33, 21, 17, 4,  13).
params_t4(34, 20, 17, 7,  8).
params_t4(35, 19, 17, 11, 6).
params_t4(36, 4,  17, 11, 7).
params_t4(37, 12, 17, 11, 15).
params_t4(38, 15, 18, 4,  9).
params_t4(39, 17, 18, 4,  15).
params_t4(40, 12, 18, 7,  4).
params_t4(41, 15, 18, 8,  11).
params_t4(42, 6,  18, 11, 13).
params_t4(43, 8,  19, 2,  9).
params_t4(44, 13, 19, 4,  2).
params_t4(45, 5,  19, 8,  3).
params_t4(46, 6,  19, 8,  11).
params_t4(47, 24, 19, 11, 5).
params_t4(48, 6,  20, 2,  10).
params_t4(49, 13, 20, 4,  10).
params_t4(50, 24, 21, 2,  7).
params_t4(51, 14, 21, 8,  13).
params_t4(52, 10, 22, 8,  13).
params_t4(53, 7,  22, 8,  14).
params_t4(54, 15, 23, 8,  5).
params_t4(55, 9,  23, 11, 4).
params_t4(56, 20, 24, 4,  8).
params_t4(57, 16, 24, 4,  14).
params_t4(58, 20, 24, 4,  14).
params_t4(59, 23, 24, 7,  3).
params_t4(60, 14, 24, 8,  10).
params_t4(61, 16, 24, 11, 12).

%---------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% tausworthe3.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Tue Feb  1 11:44:19 EST 2005
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
%-----------------------------------------------------------------------------%

:- module tausworthe3.

:- interface.

:- import_module uint32.

:- type tausworthe3.

:- func init_tausworthe3 = tausworthe3.

:- func seed_tausworthe3(uint32, uint32, uint32) = tausworthe3.

:- pred rand_tausworthe3(uint32::out, tausworthe3::in, tausworthe3::out) is det.

%-----------------------------------------------------------------------------%
%
% Typeclasses for encapsulating RNG functionality
%

:- typeclass random(RNG, Seed) <= (RNG -> Seed) where [

        % (Re)seed the random number generator.
        %
    pred seed(Seed, RNG,  RNG),
    mode seed(in,   in,   out) is det,

        % Return the next random number. This must be no less than zero
        % and no greater than the number returned by the max/3 method.
        %
    pred next(int, RNG, RNG),
    mode next(out, in,  out) is det,

        % Return the maximum integer that can be
        % returned by this random number generator.
        %
    pred max(int, RNG, RNG),
    mode max(out, in,  out) is det
].

:- type tausworthe3_seed ---> tausworthe3_seed(int, int, int).

:- instance random(tausworthe3.tausworthe3, tausworthe3_seed).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Adapted from http://remus.rutgers.edu/~rhoads/Code/tausworth.c
% (Now at http://gcrhoads.byethost4.com/Code/Random/tausworth.c)
%

:- implementation.

:- import_module int.

:- instance random(tausworthe3.tausworthe3, tausworthe3_seed) where [
    ( seed(tausworthe3_seed(A, B, C), _, RNG) :-
        RNG = tausworthe3.seed_tausworthe3(
                uint32.cast_from_int(A),
                uint32.cast_from_int(B),
                uint32.cast_from_int(C))
    ),
    ( next(int.abs(uint32.cast_to_int(I)), !RNG) :-
        tausworthe3.rand_tausworthe3(I, !RNG)
    ),
    max(int.max_int, !RNG)
].

:- type tausworthe3
    --->    state(
                s1  ::  uint32,
                s2  ::  uint32,
                s3  ::  uint32,
                tausworthe3_consts
            ).

:- type tausworthe3_consts
    --->    consts(
                shft1   ::  int,
                shft2   ::  int,
                shft3   ::  int,
                mask1   ::  uint32,
                mask2   ::  uint32,
                mask3   ::  uint32
            ).

%-----------------------------------------------------------------------------%

init_tausworthe3 = state(0u32, 0u32, 0u32, consts(0, 0, 0, 0u32, 0u32, 0u32)).

seed_tausworthe3(A, B, C) = R :-
    P1     = 12,
    P2     =  4,
    P3     = 17,

    K1     = 31,
    K2     = 29,
    K3     = 28,

    X      = 4294967295u32,

    Shft1  = K1 - P1,
    Shft2  = K2 - P2,
    Shft3  = K3 - P3,

    Mask1  = X << (32 - K1),
    Mask2  = X << (32 - K2),
    Mask3  = X << (32 - K3),

    S1     = ( if A > (1u32 << (32 - K1)) then A else 390451501u32 ),
    S2     = ( if B > (1u32 << (32 - K2)) then B else 613566701u32 ),
    S3     = ( if C > (1u32 << (32 - K3)) then C else 858993401u32 ),

    Consts = consts(Shft1, Shft2, Shft3, Mask1, Mask2, Mask3),
    R0     = state(S1, S2, S3, Consts),
    rand_tausworthe3(_, R0, R).

%-----------------------------------------------------------------------------%

rand_tausworthe3(I, R0, R) :-
    R0     = state(S1_0, S2_0, S3_0, Consts),
    Consts = consts(Shft1, Shft2, Shft3, Mask1, Mask2, Mask3),

    P1     = 12,
    P2     =  4,
    P3     = 17,

    Q1     = 13,
    Q2     =  2,
    Q3     =  3,

    B1     = ((S1_0 << Q1)`xor`S1_0) >> Shft1,
    S1     = ((S1_0 /\ Mask1) << P1)`xor`B1,

    B2     = ((S2_0 << Q2)`xor`S2_0) >> Shft2,
    S2     = ((S2_0 /\ Mask2) << P2)`xor`B2,

    B3     = ((S3_0 << Q3)`xor`S3_0) >> Shft3,
    S3     = ((S3_0 /\ Mask3) << P3)`xor`B3,

    I      = S1`xor`S2`xor`S3,
    R      = state(S1,   S2,   S3,   Consts).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

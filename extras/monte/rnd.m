%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% file: rnd.m
% main author: conway.
%
% This pseudo-random number generator is derived from C source due to
% George Marsaglia, geo@stat.fsu.edu.
%
% The original source is included in the file ORIGINAL as a reference.
%
%-----------------------------------------------------------------------------%

:- module rnd.

:- interface.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module pair.

:- type rnd.

    % Initialize a random number supply.
    %
:- pred rnd.init(int::in, rnd::out) is det.

    % rnd(Random, !Rnd):
    %
    % Get a random float on the range [0, 1).
    %
:- pred rnd(float::out, rnd::in, rnd::out) is det.

    % irange(Lower, Upper, Random, !Rnd):
    %
    % Get a random int on the range [Lower, Upper].
    %
:- pred irange(int::in, int::in, int::out, rnd::in, rnd::out) is det.

    % frange(Lower, Upper, Random, !Rnd):
    %
    % Get a random float on the range [Lower, Upper).
    %
:- pred frange(float::in, float::in, float::out, rnd::in, rnd::out) is det.

    % Generate a random permutation of a list.
    %
:- pred shuffle(list(T)::in, list(T)::out, rnd::in, rnd::out) is det.

    % Get a random element of a list.
    %
:- pred oneof(list(T)::in, T::out, rnd::in, rnd::out) is det.

    % get a random element of a weighted list.
    %
:- pred wghtd_oneof(list(pair(int, T))::in, T::out, rnd::in, rnd::out) is det.

    % gaussian(X, Y, !Rnd):
    %
    % Generate a pair of gaussian deviates `X' and `Y'.
    %
:- pred gaussian(float::out, float::out, rnd::in, rnd::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module math.
:- import_module require.

irange(Min, Max, Val, !R) :-
    frange(rfloat(Min), rfloat(Max+1), FVal, !R),
    Val = rint(FVal).

frange(Min, Max, Val, !R) :-
    rnd(J, !R),
    Val = J*(Max - Min)+Min.

shuffle(Ins, Outs, !R) :-
    list.length(Ins, N),
    shuffle2(N, Ins, [], T0, !R),
    shuffle2(N, T0, [], T1, !R),
    shuffle2(N, T1, [], T2, !R),
    shuffle2(N, T2, [], T3, !R),
    shuffle2(N, T3, [], U, !R),
    shuffle2(N, U, [], Outs, !R).

:- pred shuffle2(int::in, list(T)::in, list(T)::in, list(T)::out,
    rnd::in, rnd::out) is det.

shuffle2(N, Ins, !Acc, !R) :-
    ( N > 0 ->
        irange(0, N - 1, J, !R),
        delnth(Ins, J, Rest, T),
        shuffle2(N - 1, Rest, [T | !.Acc], !:Acc, !R)
    ;
        true
    ).

:- pred delnth(list(T)::in, int::in, list(T)::out, T::out) is det.

delnth([], _, _, _) :-
    error("delnth: no enough elems!").
delnth([X | Xs], N, Zs, Z) :-
    ( N =< 0 ->
        Z = X,
        Zs = Xs
    ;
        Zs = [X|Ys],
        delnth(Xs, N-1, Ys, Z)
    ).

oneof(Things, Thing, !R) :-
    list.length(Things, Num),
    irange(0, Num-1, X, !R),
    list.det_index0(Things, X, Thing).

wghtd_oneof(WghtdThings, Thing, !R) :-
    cumu(WghtdThings, 0, Sum),
    irange(0, Sum, X, !R),
    pick(WghtdThings, X, Thing).

:- pred cumu(list(pair(int, T))::in, int::in, int::out) is det.

cumu([], Sum, Sum).
cumu([X - _T | Rest0], Sum, Sum1) :-
    cumu(Rest0, X+Sum, Sum1).

:- pred pick(list(pair(int, T))::in, int::in, T::out) is det.

pick([], _, _) :-
    error("pick: no things to pick from!").
pick([N - T | Rest], P, Thing) :-
    ( N >= P ->
        Thing = T
    ;
        pick(Rest, P - N, Thing)
    ).

gaussian(X, Y, !Rnd) :-
    frange(-1.0, 1.0, V1, !Rnd),
    frange(-1.0, 1.0, V2, !Rnd),
    R = V1*V1 + V2*V2,
    ( R >= 1.0, R \= 0.0  ->
        gaussian(X, Y, !Rnd)
    ;
        Fac = sqrt(-2.0 * ln(R) / R),
        X = V1 * Fac,
        Y = V2 * Fac
    ).

%-----------------------------------------------------------------------------%

:- type vec
    --->    vec(int, int, int, int, int, int, int, int, int, int).

:- type rnd
    --->    rnd(
                vec,
                vec,
                int
            ).

rnd.init(Seed, rnd(M1, M2, Seed)) :-
    SN = Seed /\ ((1 `unchecked_left_shift` 15) - 1),
    N  = Seed /\ ((1 `unchecked_left_shift` 30) - 1),
    M1a = vec(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    M2a = vec(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    seed1(17, SN, N, M1a, M2a, M1b, M2b),
    set(M1b, 0, (M1b ** 0) /\ ((1 `unchecked_left_shift` 15) - 1), M1),
    set(M2b, 0, (M2b ** 0) /\ ((1 `unchecked_left_shift` 15) - 1), M2).

:- pred seed1(int, int, int, vec, vec, vec, vec).
:- mode seed1(in, in, in, in, in, out, out) is det.

seed1(N, SNum0, Num0, M1a, M2a, M1, M2) :-
    (N > 0 ->
        Num1 = 30903 * SNum0 + (Num0 `unchecked_right_shift` 15),
        SNum1 = Num1 /\ ((1 `unchecked_left_shift` 15) - 1),
        ( N >= 9 ->
            M2b = M2a,
            set(M1a, 17 - N, SNum1, M1b)
        ;
            M1b = M1a,
            set(M2a, 8 - N, SNum1, M2b)
        ),
        seed1(N-1, SNum1, Num1, M1b, M2b, M1, M2)
    ;
        M1 = M1a,
        M2 = M2a
    ).

rnd(Res, rnd(M1a, M2a, _Seed0), rnd(M1d, M2d, Seed1)) :-
    shift(M1a, M1b),
    shift(M2a, M2b),
    N1a = (M1b ** 0),
    N2a = (M2b ** 0),

    N1b = N1a + 1941 * (M1b ** 2) + 1860 * (M1b ** 3) + 1812 * (M1b ** 4)
        + 1776 * (M1b ** 5) + 1492 * (M1b ** 6) + 1215 * (M1b ** 7)
        + 1066 * (M1b ** 8) + 12013 * (M1b ** 9),

    N2b = N2a + 1111 * (M2b ** 2) + 2222 * (M2b ** 3) + 3333 * (M2b ** 4)
        + 4444 * (M2b ** 5) + 5555 * (M2b ** 6) + 6666 * (M2b ** 7)
        + 7777 * (M2b ** 8) + 9272 * (M2b ** 9),

    set(M1b, 0, (N1b `unchecked_right_shift` 15) /\
        ((1 `unchecked_left_shift` 15) - 1), M1c),
    set(M2b, 0, (N2b `unchecked_right_shift` 15) /\
        ((1 `unchecked_left_shift` 15) - 1), M2c),

    set(M1c, 1, N1b /\ ((1 `unchecked_left_shift` 15) - 1), M1d),
    set(M2c, 1, N2b /\ ((1 `unchecked_left_shift` 15) - 1), M2d),

    Seed1 = ((M1d ** 1) `unchecked_left_shift` 15) + (M2d ** 1),

    Res = rfloat(Seed1)/rfloat((1 `unchecked_left_shift` 30) - 1).

:- pred shift(vec::in, vec::out) is det.
:- pragma inline(pred(shift/2)).

shift(Vec0, Vec1) :-
    Vec0 = vec(A, B, C, D, E, F, G, H, I, _),
    Vec1 = vec(A, B, B, C, D, E, F, G, H, I).

%-----------------------------------------------------------------------------%

:- func (vec ** int) = int.
:- mode ((in ** in) = out) is det.
:- mode ((in ** in(bound(0))) = out) is det.
:- mode ((in ** in(bound(1))) = out) is det.
:- mode ((in ** in(bound(2))) = out) is det.
:- mode ((in ** in(bound(3))) = out) is det.
:- mode ((in ** in(bound(4))) = out) is det.
:- mode ((in ** in(bound(5))) = out) is det.
:- mode ((in ** in(bound(6))) = out) is det.
:- mode ((in ** in(bound(7))) = out) is det.
:- mode ((in ** in(bound(8))) = out) is det.
:- mode ((in ** in(bound(9))) = out) is det.
:- pragma inline(func((**)/2)).

( Vec ** Ind ) = Res :-
    Vec = vec(A, B, C, D, E, F, G, H, I, J),
    (
        ( Ind = 0, Res0 = A
        ; Ind = 1, Res0 = B
        ; Ind = 2, Res0 = C
        ; Ind = 3, Res0 = D
        ; Ind = 4, Res0 = E
        ; Ind = 5, Res0 = F
        ; Ind = 6, Res0 = G
        ; Ind = 7, Res0 = H
        ; Ind = 8, Res0 = I
        ; Ind = 9, Res0 = J
        )
    ->
        Res = Res0
    ;
        error("**: out of range")
    ).

:- pred set(vec, int, int, vec).
:- mode set(in, in, in, out) is det.
:- mode set(in, in(bound(0)), in, out) is det.
:- mode set(in, in(bound(1)), in, out) is det.
:- mode set(in, in(bound(2)), in, out) is det.
:- mode set(in, in(bound(3)), in, out) is det.
:- mode set(in, in(bound(4)), in, out) is det.
:- mode set(in, in(bound(5)), in, out) is det.
:- mode set(in, in(bound(6)), in, out) is det.
:- mode set(in, in(bound(7)), in, out) is det.
:- mode set(in, in(bound(8)), in, out) is det.
:- mode set(in, in(bound(9)), in, out) is det.
:- pragma inline(pred(rnd.set/4)).

set(Vec0, Ind, V, Vec) :-
    Vec0 = vec(A, B, C, D, E, F, G, H, I, J),
    (
        ( Ind = 0, Vec1 = vec(V, B, C, D, E, F, G, H, I, J)
        ; Ind = 1, Vec1 = vec(A, V, C, D, E, F, G, H, I, J)
        ; Ind = 2, Vec1 = vec(A, B, V, D, E, F, G, H, I, J)
        ; Ind = 3, Vec1 = vec(A, B, C, V, E, F, G, H, I, J)
        ; Ind = 4, Vec1 = vec(A, B, C, D, V, F, G, H, I, J)
        ; Ind = 5, Vec1 = vec(A, B, C, D, E, V, G, H, I, J)
        ; Ind = 6, Vec1 = vec(A, B, C, D, E, F, V, H, I, J)
        ; Ind = 7, Vec1 = vec(A, B, C, D, E, F, G, V, I, J)
        ; Ind = 8, Vec1 = vec(A, B, C, D, E, F, G, H, V, J)
        ; Ind = 9, Vec1 = vec(A, B, C, D, E, F, G, H, I, V)
        )
    ->
        Vec = Vec1
    ;
        error("set: out of range")
    ).

%-----------------------------------------------------------------------------%

:- func rfloat(int) = float.
:- pragma foreign_proc("C",
    rfloat(I::in) = (F::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    F = I;
").

:- func rint(float) = int.
:- pragma foreign_proc("C",
    rint(F::in) = (I::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    I = F;
").

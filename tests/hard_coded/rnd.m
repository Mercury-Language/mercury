%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module rnd.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module float.
:- import_module int.
:- import_module list.

main -->
    { rnd.init(17, Rnd) },
    { gen_nums(10, Rnd, [], Nums) },
    foldl(
        ( pred(Num::in, di, uo) is det -->
            io.print(Num), nl
        ), Nums).

:- pred gen_nums(int, rnd, list(int), list(int)).
:- mode gen_nums(in, in, in, out) is det.

gen_nums(N, Rnd0, Acc0, Acc) :-
    ( N =< 0 ->
        Acc = Acc0
    ;
        irange(0, 100, Num, Rnd0, Rnd1),
        gen_nums(N-1, Rnd1, [Num | Acc0], Acc)
    ).

%---------------------------------------------------------------------------%

:- pred rnd.init(int, rnd).
:- mode rnd.init(in, out) is det.

:- pred rnd(float, rnd, rnd).
:- mode rnd(out, in, out) is det.

:- pred irange(int, int, int, rnd, rnd).
:- mode irange(in, in, out, in, out) is det.

:- pred frange(float, float, float, rnd, rnd).
:- mode frange(in, in, out, in, out) is det.

:- pred shuffle(list(T), list(T), rnd, rnd).
:- mode shuffle(in, out, in, out) is det.

:- pred oneof(list(T), T, rnd, rnd).
:- mode oneof(in, out, in, out) is det.

%---------------------------------------------------------------------------%

:- import_module require.

irange(Min, Max, Val, R0, R) :-
    frange(rfloat(Min), rfloat(Max+1), FVal, R0, R),
    Val = rint(FVal).

frange(Min, Max, Val, R0, R) :-
    rnd(J, R0, R),
    Val = J*(Max - Min)+Min.

shuffle(Ins, Outs, R0, R) :-
    list.length(Ins, N),
    shuffle2(N, Ins, [], T0, R0, R1),
    shuffle2(N, T0, [], T1, R1, R2),
    shuffle2(N, T1, [], T2, R2, R3),
    shuffle2(N, T2, [], T3, R3, R4),
    shuffle2(N, T3, [], U, R4, R5),
    shuffle2(N, U, [], Outs, R5, R).

:- pred shuffle2(int, list(T), list(T), list(T), rnd, rnd).
:- mode shuffle2(in, in, in, out, in, out) is det.

shuffle2(N, Ins, Acc0, Acc, R0, R) :-
    ( if N > 0 then
        irange(0, N-1, J, R0, R1),
        delnth(Ins, J, Rest, T),
        shuffle2(N-1, Rest, [T | Acc0], Acc, R1, R)
    else
        Acc = Acc0,
        R = R0
    ).

:- pred delnth(list(T), int, list(T), T).
:- mode delnth(in, in, out, out) is det.

delnth([], _, _, _) :-
    error("delnth: no enough elems!").
delnth([X | Xs], N, Zs, Z) :-
    ( if N =< 0 then
        Z = X,
        Zs = Xs
    else
        Zs = [X | Ys],
        delnth(Xs, N-1, Ys, Z)
    ).

oneof(Things, Thing, R0, R) :-
    list.length(Things, Num),
    irange(0, Num-1, X, R0, R),
    list.det_index0(Things, X, Thing).

%---------------------------------------------------------------------------%

:- type vec
    --->    vec(int, int, int, int, int, int, int, int, int, int).

:- type rnd
    --->    rnd(
                vec,
                vec,
                int
            ).

rnd.init(Seed, rnd(M1, M2, Seed)) :-
    SN = Seed /\ ((1 << 15) - 1),
    N  = Seed /\ ((1 << 30) - 1),
    M1a = vec(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    M2a = vec(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    seed1(17, SN, N, M1a, M2a, M1b, M2b),
    set(M1b, 0, (M1b ** 0) /\ ((1 << 15) - 1), M1),
    set(M2b, 0, (M2b ** 0) /\ ((1 << 15) - 1), M2).

:- pred seed1(int, int, int, vec, vec, vec, vec).
:- mode seed1(in, in, in, in, in, out, out) is det.

seed1(N, SNum0, Num0, M1a, M2a, M1, M2) :-
    ( if N > 0 then
        Num1 = 30903 * SNum0 + (Num0 >> 15),
        SNum1 = Num1 /\ ((1 << 15) - 1),
        ( if N >= 9 then
            M2b = M2a,
            set(M1a, 17 - N, SNum1, M1b)
        else
            M1b = M1a,
            set(M2a, 8 - N, SNum1, M2b)
        ),
        seed1(N-1, SNum1, Num1, M1b, M2b, M1, M2)
    else
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

    set(M1b, 0, (N1b >> 15) /\ ((1 << 15) - 1), M1c),
    set(M2b, 0, (N2b >> 15) /\ ((1 << 15) - 1), M2c),

    set(M1c, 1, N1b /\ ((1 << 15) - 1), M1d),
    set(M2c, 1, N2b /\ ((1 << 15) - 1), M2d),

    Seed1 = ((M1d ** 1) << 15) + (M2d ** 1),

    Res = rfloat(Seed1)/rfloat((1 << 30) - 1).

:- pred shift(vec, vec).
:- mode shift(in, out) is det.

shift(Vec0, Vec1) :-
    Vec0 = vec(A, B, C, D, E, F, G, H, I, _),
    Vec1 = vec(A, B, B, C, D, E, F, G, H, I).

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

( Vec ** Ind ) = Res :-
    Vec = vec(A, B, C, D, E, F, G, H, I, J),
    ( if
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
    then
        Res = Res0
    else
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

set(Vec0, Ind, V, Vec) :-
    Vec0 = vec(A, B, C, D, E, F, G, H, I, J),
    ( if
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
    then
        Vec = Vec1
    else
        error("set: out of range")
    ).

:- func rfloat(int) = float.
:- pragma foreign_proc("C",
    rfloat(I::in) = (F::out),
    [will_not_call_mercury, promise_pure],
"
    F = I;
").
:- pragma foreign_proc("C#",
    rfloat(I::in) = (F::out),
    [promise_pure],
"
    F = I;
").
:- pragma foreign_proc("Java",
    rfloat(I::in) = (F::out),
    [promise_pure],
"
    F = I;
").
:- pragma foreign_proc("Erlang",
    rfloat(I::in) = (F::out),
    [promise_pure],
"
    F = float(I)
").

:- func rint(float) = int.
:- pragma foreign_proc("C",
    rint(F::in) = (I::out),
    [will_not_call_mercury, promise_pure],
"
    I = F;
").
:- pragma foreign_proc("C#",
    rint(F::in) = (I::out),
    [promise_pure],
"
    I = (int) F;"
).
:- pragma foreign_proc("Java",
    rint(F::in) = (I::out),
    [promise_pure],
"
    I = (int) F;"
).
:- pragma foreign_proc("Erlang",
    rint(F::in) = (I::out),
    [promise_pure],
"
    I = trunc(F)"
).

:- pred for(int, int, pred(int, T, T), T, T).
:- mode for(in, in, pred(in, in, out) is det, in, out) is det.

for(Min, Max, Pred, Acc0, Acc) :-
    ( if Min =< Max then
        call(Pred, Min, Acc0, Acc1),
        for(Min+1, Max, Pred, Acc1, Acc)
    else
        Acc = Acc0
    ).

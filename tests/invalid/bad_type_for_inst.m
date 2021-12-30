%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bad_type_for_inst.

:- interface.

:- type t1(A, B)
    --->    f1(int, A, B)
    ;       f2(B, A, float).

:- type t2(A, B)
    --->    f1(int, A, B)
    ;       f2(B, A, float).

:- inst i1_a(AI, BI) for t1/2
    --->    f1(bound(11), AI, BI)
    ;       f2(ground, BI, ground).

:- inst i1_b(AI) for t1/2
    --->    f1(bound(11), ground, AI)
    ;       f2(ground, AI, ground).

:- pred p1(t1(t2(int, string), float), int).
:- mode p1(in(i1_a(ground, ground)), out(i1_a(ground, ground))) is det.

:- pred p2(t1(t2(int, string), float), int).
:- mode p2(in(i1_a(i1_a(ground, ground), ground)), out) is det.

:- pred p3(t2(A, int), int).
:- mode p3(in(i1_b(ground)), out) is det.

:- func f1(A, int) = t2(A, int).
:- mode f1(in, in) = out(i1_b(ground)) is det.

:- implementation.

:- pragma external_pred(p1/2).
:- pragma external_pred(p2/2).
:- pragma external_func(f1/2).

p3(T2, N) :-
    Pred =
        ( pred(Xs::in(bound({ground, i1_b(ground)})), Y::out) is det :-
            Xs = {X, _},
            (
                X = f1(_, _, Y)
            ;
                X = f2(Y, _, _)
            )
        ),
    Pred({T2, T2}, N).

%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module det_errors_cc.

:- interface.

:- pred p(int::in, int::out) is cc_nondet.

:- implementation.
:- import_module int.

:- type t
    --->    fa
    ;       fb
    ;       fc
    ;       fd.

p(A, X) :-
    p1(A, B),
    p2(B, C),
    p3(C, D),
    p4(D, E),
    p5(E, F),
    p6(F, X).

:- pred p1(int::in, int::out) is cc_nondet.

p1(A, X) :-
    p3(A, B),
    p4(B, C),
    C = 10,
    B = C,
    X = C.

:- pred p2(int::in, int::out) is cc_nondet.

p2(A, X) :-
    p3(A, B),
    ( if B < 5 then
        fail
    else
        X = B
    ).

:- pred p3(int::in, int::out) is cc_nondet.

p3(A, X) :-
    p2(A, X),
    not (X = 10).

:- pred p4(int::in, int::out) is cc_nondet.

p4(A, X) :-
    p3(A, B),
    P = r(B),
    P(A, X).

:- pred p5(int::in, int::out) is cc_nondet.

p5(A, X) :-
    p1(A, B),
    ( if B < 5 then
        C = fa
    else if B < 10 then
        C = fb
    else
        C = fc
    ),
    (
        C = fa,
        X = 5
    ;
        C = fb,
        X = 6
    ).

:- pred p6(int::in, int::out) is cc_nondet.

p6(A, X) :-
    p1(A, B),
    ( if B < 5 then
        C = fa
    else
        C = fb
    ),
    (
        C = fa,
        X = 5
    ;
        C = fb,
        X = 6
    ).

:- pred q(int::in, int::out) is multi.

q(N, N).
q(N, N + 1).

:- pred r(int::in, int::in, int::out) is semidet.

r(N, N, N).

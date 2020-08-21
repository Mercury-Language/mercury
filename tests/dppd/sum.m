%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module sum.

:- interface.

:- pred sumsumsum(list(int)::in, int::out) is det.

:- implementation.

:- import_module int.
:- import_module list.

sumsumsum(A, B) :-
    sum1(A, 0, C),
    sum2(A, 0, D),
    sum3(A, 0, E),
    B = C + D + E.

:- pred sum1(list(int)::in, int::in, int::out) is det.

sum1([], Sum, Sum).
sum1([X | Xs], Sum0, Sum) :-
    Sum1 is Sum0 + X,
    sum1(Xs, Sum1, Sum).

:- pred sum2(list(int)::in, int::in, int::out) is det.

sum2([], Sum, Sum).
sum2([X | Xs], Sum0, Sum) :-
    Sum1 = Sum0 + X,
    sum2(Xs, Sum1, Sum).

:- pred sum3(list(int)::in, int::in, int::out) is det.

sum3([], Sum, Sum).
sum3([X | Xs], Sum0, Sum) :-
    Sum1 = Sum0 + X,
    sum3(Xs, Sum1, Sum).

%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module followcode_det_problem.

:- interface.

:- pred p(int::out) is semidet.

:- implementation.

p(X) :-
    Z = 4,
    (
        Z = 3
    ;
        Z = 4
    ),
    q(X).

:- pred q(int::out) is det.

q(42).

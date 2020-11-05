%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bind_in_negated.
:- interface.

:- type foo
    --->    f(int, int).

:- pred p(foo, foo, foo).
:- mode p(bound(f(free, ground)) >> ground,
    bound(f(ground, free)) >> ground,
    out) is det.

:- implementation.

p(A, B, C) :-
    ( if A = B then
        C = A
    else
        C = f(1, 1),
        A = f(1, _),
        B = f(_, 1)
    ).

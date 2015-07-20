%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module stack_alloc.

:- interface.

:- pred p is semidet.

:- implementation.

:- pred in(int::in) is semidet.
:- pred out(int::out) is det.

:- external(in/1).
:- external(out/1).

p :-
    (
        out(X),
        out(Y),
        p,
        in(X),
        in(Y)
    ;
        out(A),
        out(B),
        p,
        in(A),
        in(B)
    ).

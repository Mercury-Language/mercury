%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module loop_in_disj.
:- interface.

:- pred p(int::out) is det.

:- pred q(int::out) is det.

:- implementation.

:- pred loop is erroneous.

loop :-
    loop.

p(X) :-
    (
        loop
    ;
        X = 42
    ).

q(X) :-
    (
        loop,
        X = 41
    ;
        X = 42
    ).

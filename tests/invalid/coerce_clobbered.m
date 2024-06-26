%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_clobbered.
:- interface.

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

%---------------------------------------------------------------------------%

:- implementation.

:- pred bad(fruit, fruit).
:- mode bad(clobbered >> clobbered, out) is det.

bad(X, Y) :-
    Y = coerce(X).

%---------------------------------------------------------------------------%

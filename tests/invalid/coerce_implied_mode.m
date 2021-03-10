%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_implied_mode.
:- interface.

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- type citrus =< fruit
    --->    orange
    ;       lemon.

%---------------------------------------------------------------------------%

:- implementation.

:- pred coerce_implied_mode(fruit::in) is semidet.

coerce_implied_mode(Y) :-
    Y = coerce(orange : citrus).

:- pred coerce_implied_mode2(fruit).
:- mode coerce_implied_mode2(in(bound(orange))) is det.

coerce_implied_mode2(Y) :-
    Y = coerce(orange : citrus).

:- pred coerce_implied_mode3(fruit).
:- mode coerce_implied_mode3(in(bound(orange))) is failure.

coerce_implied_mode3(Y) :-
    Y = coerce(lemon : citrus).

%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_unreachable.
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

:- import_module exception.

:- pred p1(fruit::in, citrus::out) is det.

p1(X, Y) :-
    % This coercion is invalid.
    Y = coerce(X),
    ( if Y = orange then
        true
    else
        throw(apple)
    ).

:- pred p2(fruit::in, citrus::out) is erroneous.

p2(X, Y) :-
    % This coercion is invalid, but can be moved after the exception is thrown.
    Y = coerce(X),
    throw(apple).

%---------------------------------------------------------------------------%

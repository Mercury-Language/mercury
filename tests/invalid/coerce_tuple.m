%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_tuple.
:- interface.

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- type citrus =< fruit
    --->    orange
    ;       lemon.

:- type basket(T)
    --->    basket({int, string}, {T, T}).

:- type citrus_basket =< basket(fruit)
    --->    basket({int, string}, {citrus, citrus}).

%---------------------------------------------------------------------------%

:- implementation.

:- pred bad1 is det.

bad1 :-
    X = basket({1, "two"}, {lemon, apple}),
    coerce(X) = _ : basket(citrus).

:- pred bad2 is det.

bad2 :-
    X = basket({1, "two"}, {lemon, apple}),
    coerce(X) = _ : citrus_basket.

:- pred ok3 is det.

ok3 :-
    X = basket({1, "two"}, {orange, lemon : fruit}),
    coerce(X) = _ : basket(citrus).

:- pred ok4 is det.

ok4 :-
    X = basket({1, "two"}, {orange, lemon}) : citrus_basket,
    coerce(X) = _ : basket(fruit).

:- pred ok5 is det.

ok5 :-
    X = basket({1, "two"}, {orange, lemon}) : citrus_basket,
    coerce(X) = _ : basket(citrus).

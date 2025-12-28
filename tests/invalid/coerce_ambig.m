%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_ambig.
:- interface.

:- type list(T)
    --->    []
    ;       [T | list(T)].

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- type citrus =< fruit
    --->    orange
    ;       lemon.

:- type another_fruit
    --->    apple.

%---------------------------------------------------------------------------%

:- implementation.

:- pred ambig1 is semidet.

ambig1 :-
    X = orange,
    _Y = coerce(X).

:- pred ambig2 is semidet.

ambig2 :-
    X1 = apple,
    _Y1 = coerce(X1),
    X2 = apple,
    _Y2 = coerce(X2).

:- pred ambig3(list(fruit)::out) is det.

ambig3(Xs) :-
    Xs = coerce([]).

:- pred ambig4 is det.

ambig4 :-
    X = [],
    coerce(X) = _ : list(citrus),
    coerce(X) = _ : list(fruit).

%---------------------------------------------------------------------------%

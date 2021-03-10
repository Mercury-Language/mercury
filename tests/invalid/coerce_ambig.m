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
    X = apple,
    _Y = coerce(X).

:- pred ambig3(list(fruit)::out) is det.

ambig3(Xs) :-
    % Not ambiguous after we unify list(_T) with list(fruit).
    Xs = coerce([]).

%---------------------------------------------------------------------------%

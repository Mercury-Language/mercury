%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_uniq.
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

:- pred coerce_ui(fruit::ui, fruit::out) is det.

coerce_ui(X, Y) :-
    Y = coerce(X).

:- pred coerce_di(fruit::di, fruit::uo) is det.

coerce_di(X, Y) :-
    Y = coerce(X).

:- pred coerce_become_shared(fruit::unique>>ground, fruit::out) is det.

coerce_become_shared(X, Y) :-
    Y = coerce(X).

%---------------------------------------------------------------------------%

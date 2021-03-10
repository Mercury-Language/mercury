%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_instvar.
:- interface.

:- type foobar
    --->    foo
    ;       bar.

:- type foo =< foobar
    --->    foo.

:- type fruit
    --->    apple
    ;       orange
    ;       lemon(foobar).

:- type citrus =< fruit
    --->    orange
    ;       lemon(foo).

%---------------------------------------------------------------------------%

:- implementation.

:- pred good(citrus, fruit).
:- mode good(in(I), out(I)) is det.

good(X, Y) :-
    Y = coerce(X),
    (
        Y = orange
    ;
        Y = lemon(foo)
    ).

:- pred bad(fruit, citrus).
:- mode bad(in(I), out(I)) is det.

bad(X, Y) :-
    Y = coerce(X).

%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_typecheck_eqv.
:- interface.

:- type fruit
    --->    apple
    ;       lemon.

:- type citrus =< fruit
    --->    lemon.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module coerce_typecheck_eqv_helper_1.

:- type foo(T)
    --->    foo(first(int, T)).     % first(int, T) == second(T, int).

:- type bar(T)
    --->    bar(first(T, int)).     % first(T, int) == second(int, T).

:- pred test1(foo(citrus)::in, foo(fruit)::out) is det.

test1(X, Y) :-
    Y = coerce(X).

:- pred test2(bar(citrus)::in, bar(fruit)::out) is det.

test2(X, Y) :-
    Y = coerce(X).

%---------------------------------------------------------------------------%

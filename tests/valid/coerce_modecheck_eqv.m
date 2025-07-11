%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module coerce_modecheck_eqv.
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

:- import_module coerce_modecheck_eqv_helper_1.

:- type wrap_foo(T)
    --->    wrap_foo(foo(T)).

:- type wrap_bar(T)
    --->    wrap_bar({T, bar}).

:- pred test1(wrap_foo(citrus)::in, wrap_foo(fruit)::out) is det.

test1(X, Y) :-
    Y = coerce(X).

:- pred test2(wrap_bar(citrus)::in, wrap_bar(fruit)::out) is det.

test2(X, Y) :-
    Y = coerce(X).

%---------------------------------------------------------------------------%

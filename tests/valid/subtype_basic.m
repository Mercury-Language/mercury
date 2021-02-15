%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_basic.
:- interface.

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- type citrus =< fruit
    --->    orange
    ;       lemon.

:- type citrus(C) =< fruit
    --->    orange
    ;       lemon.

:- type lemon.  % abstract

:- type lemon_eqv == lemon.

%---------------------------------------------------------------------------%

:- implementation.

:- type lemon =< citrus
    --->    lemon.

:- type list(T)
    --->    []
    ;       [T | list(T)].

:- type nonempty_list(T) =< list(T)
    --->    [T | list(T)].

:- type nonempty_list_of_fruit =< list(fruit)
    --->    [fruit | list(fruit)].

:- type nonempty_list_of_fruit_eqv == nonempty_list(fruit).

:- type nonempty_list_of_citrus =< nonempty_list_of_fruit_eqv
    --->    [citrus | list(citrus)].

:- type nonempty_list_of_lemon =< nonempty_list_of_citrus
    --->    [lemon | list(lemon_eqv)].

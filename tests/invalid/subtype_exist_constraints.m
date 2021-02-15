%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_exist_constraints.
:- interface.

:- typeclass sweet(T) where [].
:- typeclass sour(T) where [].

:- type fruit
    --->    apple
    ;       orange
    ;       some [L] lemon(L) => sour(L).

:- type lemon =< fruit
    --->    some [L] lemon(L) => sour(L).

:- type missing_constraint =< fruit
    --->    some [L] lemon(L).

:- type wrong_constraint =< fruit
    --->    some [L] lemon(L) => sweet(L).

:- type extra_constraint =< fruit
    --->    some [L] lemon(L) => (sour(L), sweet(L)).

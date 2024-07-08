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

:- type foo
    --->    some [A, B, C, D] foo(A, B, C, D) => (sweet(B), sour(C)).

:- type constraints_same_order =< foo
    --->    some [X4, X3, X2, X1] foo(X4, X3, X2, X1) => (sweet(X3), sour(X2)).

:- type constraints_differ =< foo
    --->    some [X4, X3, X2, X1] foo(X4, X3, X2, X1) => (sweet(X2), sour(X3)).

:- type constraints_different_order =< foo
    --->    some [X4, X3, X2, X1] foo(X4, X3, X2, X1) => (sour(X2), sweet(X3)).

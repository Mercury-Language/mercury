%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_exist_vars.
:- interface.

:- type fruit == fruit(int).

:- type fruit(A)
    --->    apple(A)
    ;       some [O] orange(O)
    ;       some [T, U] lemon(T, T, foo(U)).

:- type foo(T)
    --->    foo(T).

:- type univ_vs_exist_mismatch(T) =< fruit(T)
    --->    orange(T).

:- type exist_vs_univ_mismatch =< fruit
    --->    some [T] apple(T).

:- type exist_vars_match =< fruit
    --->    some [A, B] lemon(A, A, foo(B)).

:- type exist_vars_mismatch1 =< fruit
    --->    some [A, B] lemon(A, B, foo(B)).

:- type exist_vars_mismatch2 =< fruit
    --->    some [A, B] lemon(B, A, foo(A)).

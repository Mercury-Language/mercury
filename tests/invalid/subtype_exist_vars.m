%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_exist_vars.
:- interface.

:- type fruit == fruit(int).

:- type fruit(A)
    --->    apple(A)
    ;       some [O] orange(O)
    ;       some [T, U] lemon(T, T, foo(U))
    ;       some [T, U] pear(T, U).
%    ;       some [T, T] nondistinct(T, T). % weird

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

:- type exist_vars_mismatch3 =< fruit
    --->    some [A] pear(A, A).

:- type exist_vars_mismatch4 =< fruit
    --->    some [A, B] pear(B, A). % order of quantified vars is significant

%:- type nondistinct_allowed =< fruit
%    --->    some [A, A] nondistinct(A, A).

%:- type nondistinct_mismatch =< fruit
%    --->    some [A, B] nondistinct(A, B).

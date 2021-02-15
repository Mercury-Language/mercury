%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_eqv.
:- interface.

:- type fruit(A, O, L)
    --->    apple(A)
    ;       orange(O)
    ;       lemon(L).

:- type fruit_rev(L, O, A) == fruit(A, O, L).

:- type citrus(O, L) =< fruit_rev(L, O, apples)
    --->    orange(O)
    ;       lemon(L).

:- type citrus_bad(O, L) =< fruit_rev(O, L, apples)
    --->    orange(O)
    ;       lemon(L).

:- type apples
    --->    apples.

:- type oranges
    --->    oranges.

:- type lemons
    --->    lemons.

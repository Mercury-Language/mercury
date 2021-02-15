%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_circular.
:- interface.

:- type loop =< loop
    --->    loop.

:- type loop1 =< loop2_eqv
    --->    loop.

:- type loop2 =< loop1_eqv
    --->    loop.

:- type loop1_eqv == loop1.
:- type loop2_eqv == loop2.

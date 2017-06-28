%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module abstract_solver_type.

:- interface.

:- solver type foo1.
:- solver type foo2.
:- solver type foo3.

:- implementation.

:- type foo1 == int.

:- type foo2
    --->    foo2a
    ;       foo2b(int).

:- pragma foreign_type("C", foo3, "struct solver_store_t *").

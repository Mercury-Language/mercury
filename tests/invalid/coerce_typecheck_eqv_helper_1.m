%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_typecheck_eqv_helper_1.
:- interface.

:- type first(A, B) == second(B, A).

:- type second(T, U).

:- implementation.

:- import_module list.

:- type second(T, U) == list(U).

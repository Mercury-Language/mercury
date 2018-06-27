%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module invalid_instance_declarations.
:- interface.

:- type t(T) ---> f(T).
:- type t(T, U) ---> f(T, U).

:- typeclass tc(T, U) where [].

:- type f(T) == ((func) = T).
:- type h == t(int).

:- implementation.
:- instance tc(f(T), int) where [].
:- instance tc(h, int) where [].
:- instance tc(h, t(int, string)) where [].

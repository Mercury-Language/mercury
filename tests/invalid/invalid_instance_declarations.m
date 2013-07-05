:- module invalid_instance_declarations.
:- interface.

:- type t(T) ---> f(T).
:- type t(T, U) ---> f(T, U).

:- typeclass tc(T) where [].

:- type f(T) == ((func) = T).
:- type h == t(int).

:- implementation.
:- instance tc(f(T)) where [].
:- instance tc(h) where [].

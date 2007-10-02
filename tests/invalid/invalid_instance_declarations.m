:- module invalid_instance_declarations.
:- interface.

:- type t(T) ---> f(T).
:- type t(T, U) ---> f(T, U).

:- typeclass tc(T) where [].

:- type e(T) == t(T, T).
:- type f(T) == ((func) = T).
:- type g(T) == {T, T}.
:- type h == t(int).

:- implementation.
:- instance tc(e(T)) where [].
:- instance tc(f(T)) where [].
:- instance tc(g(T)) where [].
:- instance tc(h) where [].

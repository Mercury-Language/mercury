:- module fundeps_coverage.
:- interface.

:- typeclass foo(A, B, C) <= (A -> B) where [].
:- typeclass bar(A, B, C) <= (A -> B, C) where [].

:- implementation.

:- type t(T) ---> t(T).

:- instance bar(t(A), t(B), t(C)) <= foo(A, B, C) where [].


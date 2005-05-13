:- module fundeps.
:- interface.

:- typeclass foo(A, B) <= (A -> B) where [].

:- type bar ---> some [A, B] bar(A) => foo(A, B).

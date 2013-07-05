:- module instance_dup_var.
:- interface.

:- type foo(A, B) ---> foo(A, B).
:- type bar ---> bar.

:- typeclass baz(X, Y) where [].

:- implementation.

:- instance baz(foo(T, T), bar) where [].

:- instance baz(foo(A, B), foo(C, A)) where [].


:- module instance_dup_var.
:- interface.

:- type foo(A, B) ---> foo(A, B).
:- type bar ---> bar.

:- typeclass baz(X, Y) where [].

:- implementation.

	% Error: T is duplicated.
:- instance baz(foo(T, T), bar) where [].

	% Error: A is duplicated.
:- instance baz(foo(A, B), foo(C, A)) where [].


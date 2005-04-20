:- module inconsistent_instances.
:- interface.

:- typeclass foo(A, B) <= (A -> B) where [].

:- implementation.
:- import_module list.

	% Inconsistent.

:- instance foo(list(T), int) where [].
:- instance foo(list(X), string) where [].


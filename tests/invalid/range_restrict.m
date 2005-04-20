:- module range_restrict.
:- interface.

:- typeclass foo(A, B) <= (A -> B) where [].

:- implementation.
:- import_module list, map.

	% Errors: range-restrictedness

:- instance foo(list(X), map(X, Y)) where [].
:- instance foo(map(W, X), map(Y, Z)) where [].


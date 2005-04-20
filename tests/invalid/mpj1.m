% Compile with '--infer-all'

:- module mpj1.
:- interface.

:- typeclass coll(E, C) <= (C -> E) where [
	func insert(E, C) = C
].

:- implementation.

f(A, B, C) = insert(A, insert(B, C)).

g(L) = f("foo", 2, L).


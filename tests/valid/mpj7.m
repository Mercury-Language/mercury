% Compile with '--infer-all'

:- module mpj7.
:- interface.

:- typeclass coll(E, C) where [
	func i(E, C) = C
].

:- type intlist ---> [] ; [int | intlist].
:- type stringlist ---> [] ; [string | stringlist].

:- func g(intlist) = intlist.

:- implementation.

:- instance coll(string, intlist) where [ (i(_S, L) = [1 | L]) ].
:- instance coll(int, intlist) where [ (i(N, L) = [N | L]) ].
:- instance coll(string, stringlist) where [ (i(S, L) = [S | L]) ].

g(L) = h(f, 1, 2, L).

:- func h(func(A, A, C) = C, A, A, C) = C <= coll(A, C).

h(F, A1, A2, L) = F(A1, A2, L).

:- func f(A, B, C) = C <= (coll(A, C), coll(B, C)).
f(A, B, C) = i(A, i(B, C)).


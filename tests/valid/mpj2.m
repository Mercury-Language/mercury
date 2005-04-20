:- module mpj2.
:- interface.

:- typeclass coll(E, C) <= (C -> E) where [
	func i(E, C) = C
].

:- type intlist ---> [] ; [int | intlist].
:- type stringlist ---> [] ; [string | stringlist].

:- func g(intlist) = intlist.

:- implementation.

:- instance coll(int, intlist) where [ (i(N, L) = [N | L]) ].
:- instance coll(string, stringlist) where [ (i(S, L) = [S | L]) ].

g(L) = f(1, 2, L).

% This probably should be an error, or at least a warning, since the
% constraints could be tighter.
:- func f(A, B, C) = C <= (coll(A, C), coll(B, C)).

f(A, B, C) = i(A, i(B, C)).


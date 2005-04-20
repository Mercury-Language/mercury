:- module mpj4.
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

	% Error: call to f/3 has unsatisfiable constraints

g(L) = f(1, "foo", L).

% This probably should be an error, or at least a warning, since the
% constraints could be tighter.
:- func f(A, B, C) = C <= (coll(A, C), coll(B, C)).

f(A, B, C) = i(A, i(B, C)).


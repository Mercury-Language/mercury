% Compile with '--infer-all'

:- module no_fundep_infer.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
	io.write(g([]), !IO),
	io.nl(!IO).

:- typeclass coll(E, C) where [
	func i(E, C) = C
].

:- type intlist ---> [] ; [int | intlist].
:- type stringlist ---> [] ; [string | stringlist].

:- instance coll(string, intlist) where [ (i(_S, L) = [1 | L]) ].
:- instance coll(int, intlist) where [ (i(N, L) = [N | L]) ].
:- instance coll(string, stringlist) where [ (i(S, L) = [S | L]) ].

:- func g(intlist) = intlist.

g(L) = f("foo", 2, L).

f(A, B, C) = i(A, i(B, C)).


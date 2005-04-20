:- module fundeps_3.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module list.

main(!S) :-
	(
		e = 1
	->
		write_string("yes\n", !S)
	;
		write_string("no\n", !S)
	).

:- typeclass coll(C, E) <= (C -> E) where [
	func e = C
].

:- instance coll(int, int) where [
	(e = 1)
].


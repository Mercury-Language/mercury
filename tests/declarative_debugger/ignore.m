:- module ignore.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module ignore_1, int, list.

main(!IO) :-
	p(X),
	p(Y),
	write_int(X+Y, !IO),
	nl(!IO).

:- pred p(int::out) is det.

p(ignore_1.fold(q, [1, 2, 3, 4, 5], 0)).

:- func q(int, int) = int.

q(X, Y) = X+Y.

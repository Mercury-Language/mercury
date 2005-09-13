:- module partial.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
	p(X),
	io.write(X, !IO),
	nl(!IO).

:- type t
	--->	t(
			a :: int,
			b :: int
		).

:- pred p(t::out) is det.

p(X) :-
	a(A),
	b(B),
	X = t(A, _),
	X = t(_, B).

:- pred a(int::out) is det.

a(1).

:- pred b(int::out) is det.

b(2).

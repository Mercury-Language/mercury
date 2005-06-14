:- module priv_builtin_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
	q(1, X),
	io.write_int(X, !IO),
	io.nl(!IO).

:- pred q(int::in, int::out) is det.

q(X, Y) :-
	( if
		private_builtin.builtin_unify_int(X, 1),
		Z1 = 2
	then
		p(Z1, Y)
	else
		Y = 2
	).

:- pred p(int::in, int::out) is det.

p(X, X).

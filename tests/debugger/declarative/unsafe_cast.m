:- module unsafe_cast.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main(!IO) :-
	p(Y),
	io__write_int(Y, !IO),
	io__nl(!IO).

:- pred p(int::out) is det.

p(Y) :-
	X = 42,
	private_builtin__unsafe_type_cast(X, Y).

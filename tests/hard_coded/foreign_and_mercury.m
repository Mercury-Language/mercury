% Test that we can use non-mode specific mercury implementations in
% conjunction with foreign code procs.
:- module foreign_and_mercury.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	{ f(2, Y) },
	io__write_int(Y),
	io__nl,
	{ g(2, Z) },
	io__write_int(Z),
	io__nl.

:- pred f(int::in, int::out) is det.
f(X, X).
:- pragma foreign_proc(c, f(X::in, Y::out), [promise_pure], "
	Y = X;
").

:- pred g(int::in, int::out) is det.
:- pragma foreign_proc(c, g(X::in, Y::out), [promise_pure], "
	Y = X;
").
g(X, X).

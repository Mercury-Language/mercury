:- module empty_command.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.

main -->
	{ p(53, P) },
	io__write_int(P),
	io__nl.

:- pred p(int::in, int::out) is det.
:- pred q(int::in, int::out) is det.
:- pred r(int::in, int::out) is det.
:- pred s(int::in, int::out) is det.

p --> q, r, s.
q --> [].
r --> [].
s --> [].


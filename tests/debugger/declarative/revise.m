:- module revise.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

main -->
	{ p("foo", S) },
	io__write_string(S),
	io__nl.

:- pred p(string::in, string::out) is multi.
:- pred q(string::in, string::out) is det.
:- pred r(string::in, string::out) is multi.
:- pred s(string::in, string::out) is det.
:- pred a(string::in, string::out) is det.
:- pred b(string::in, string::out) is det.
:- pred c(string::in, string::out) is det.

p --> q, r, s.

q --> [].
r --> a, b.
r --> c.
s --> [].

a --> [].
b --> [].
c --> :=("bar").


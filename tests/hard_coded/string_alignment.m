% This module tests for possible problems that unaligned string literals
% would cause if tagged.

:- module string_alignment.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module require.

:- type t ---> f1(string) ; f2(string) ; f3(string) ; f4(string).

main -->
	show(f1("foo")),
	show(f2("foo")),
	show(f1("oo")),
	show(f2("oo")).

:- pred show(t::in, io__state::di, io__state::uo) is det.

show(f1(S)) --> io__write_string("f1: "), io__write_string(S), io__nl.
show(f2(S)) --> io__write_string("f2: "), io__write_string(S), io__nl.
show(f3(S)) --> io__write_string("f3: "), io__write_string(S), io__nl.
show(f4(S)) --> io__write_string("f4: "), io__write_string(S), io__nl.


% This is a test case to test that duplicate call elimination
% takes purity into account.

:- module dupcall_impurity.
:- interface.
:- import_module io.

:- impure pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module string.

:- impure pred test1(io__state::di, io__state::uo) is det.
:- impure pred test2(io__state::di, io__state::uo) is det.

main -->
	impure test1,
	impure test2.

test1 -->
	{ impure next_x(X0) },
	{ impure next_x(X1) },
	print(X0), nl,
	print(X1), nl.

test2 -->
	{ semipure get_x(X0) },
	{ impure incr_x },
	{ semipure get_x(X1) },
	print(X0), nl,
	print(X1), nl.

:- semipure pred get_x(int::out) is det.
:- impure pred next_x(int::out) is det.
:- impure pred incr_x is det.

:- pragma c_header_code("extern int my_global;").
:- pragma c_code("int my_global;").

:- pragma c_code(get_x(X::out), "X = my_global;").
:- pragma c_code(next_x(X::out), "X = my_global++;").
:- pragma c_code(incr_x, "my_global++;").


:- module foreign_import_module.

:- interface.

:- import_module int, io.

:- pred main(io__state::di, io__state::uo) is det.

:- pred bar(int::in, int::out) is det.

:- implementation.

main -->
	{ bar(41, X) },
	io__write(X),
	io__write_char('\n').

:- pragma foreign_import_module("C", foreign_import_module_2).

:- pragma c_code(bar(X::in, Y::out), may_call_mercury,
"
	foo(X, &Y);
").

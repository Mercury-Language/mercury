:- module foreign_import_module.

:- interface.

:- import_module int, io.

:- pred main(io__state::di, io__state::uo) is det.

:- pred bar(int::in, int::out) is det.

:- implementation.

main -->
	{ bar(41, X) },
	io__write(X),
	io__nl,

	{ bar2(41, Y) },
	io__write(Y),
	io__nl.

:- pragma foreign_import_module(c, foreign_import_module_2).
:- pragma foreign_import_module(il, foreign_import_module_2).
:- pragma foreign_import_module("MC++", foreign_import_module_2).
% :- pragma foreign_import_module(il, std_util).

:- pragma c_code(bar(X::in, Y::out), may_call_mercury,
"
	foo(X, &Y);
").
:- pragma foreign_proc("MC++", bar(X::in, Y::out),
		[may_call_mercury, promise_pure], "
	MR_Integer Y1, Y2;

	foreign_import_module_2::mercury_code::foo(X, &Y1);
	foreign_import_module_2__cpp_code::mercury_code::foo2(X, &Y2);

	if (Y1 == Y2) {
		Y = Y1;
	} else {
		throw new System::Exception(""Y1 != Y2"");
	}
").

:- pred bar2(int::in, int::out) is det.
:- pragma c_code(bar2(X::in, Y::out), may_call_mercury,
"
	foo(X, &Y);
").
:- pragma foreign_proc("C#", bar2(X::in, Y::out),
		[may_call_mercury, promise_pure], "
	int Y1 = 0, Y2 = 0;

	foreign_import_module_2.mercury_code.foo(X, ref Y1);
	foreign_import_module_2__cpp_code.mercury_code.foo2(X, ref Y2);

	if (Y1 == Y2) {
		Y = Y1;
	} else {
		throw new System.Exception(""Y1 != Y2"");
	}
").


:- module no_inline.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list.

:- pragma promise_pure(main/2).
main -->
	{
	impure bar(A),
	impure bar(B),
	impure bar(C),
	impure bar(D)
	},
	io__write([A, B, C, D]),
	io__write_string("\n").

:- pragma no_inline(bar/1).
:- impure pred bar(int::out) is det.

:- pragma c_code(bar(Value::out), "
{
	static int counter = 0;

	Value = counter++;
}
").

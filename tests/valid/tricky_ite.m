:- module tricky_ite.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module int, require.

main -->
	( { p(42, X) } ->
		{ error("blah") },
		write(X)
	;
		io__write_string("No.\n")
	).

:- pred p(int::in, int::out) is nondet.
p(42, 1).
p(42, 2).
p(42, 3).

:- module label_layout.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	(
		{
			a(1, X)
		;
			a(2, X)
		},
		{ X = 0 }
	->
		io__write_string("yes\n")
	;
		io__write_string("no\n")
	).

:- pred a(int::in, int::out) is det.

a(X, X).


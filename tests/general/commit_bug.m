:- module commit_bug.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is multi.

:- implementation.

:- import_module int, list.

main -->
	(
		{ list__member(X, [1,2,3,4,5]) }
	->
		(
			{ some [Y] foo(X, Y) },
			{ foo(X,Z) }
		->
			io__write_int(Z),
			io__write_string("\n")
		;
			io__write_string("No.\n")
		)
	;
		[]
	).

:- pred foo(int, int).
:- mode foo(in, out) is nondet.

foo(X, X).
foo(_, 7).



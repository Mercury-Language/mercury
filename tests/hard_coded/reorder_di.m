% regression test - Mercury 0.7 failed this test

:- module reorder_di.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module require.

main -->
	{ r(Y) },
	{ q(X) },
	io__write_string(Str),
	( { Y = 1 } ->
		io__write_string(Str),
		io__write_string("quux\n")
	;
		{ error("qux") }
	),
	io__write_string("bar\n"),
	{ X = 1, Str = "foo\n"
	; X = 2, Str = "baz\n"
	}.

:- pred q(int::(free >> bound(1 ; 2))) is det.

q(1).

:- pred r(int::(free >> bound(1 ; 2))) is det.

r(1).

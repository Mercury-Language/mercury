
:- module dense_lookup_switch.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- type foo ---> a;b;c;d;e;f;g;h.

main --> bar(e).

:- pragma no_inline(bar/3).

:- pred bar(foo::in, io__state::di, io__state::uo) is det.
bar(X) -->
	(
		{ X = a ; X = b ; X = c ; X = d }
	->
		io__write_string("a or b or c or d\n")
	;
		io__write_string("something else\n")
	).

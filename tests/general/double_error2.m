:- module double_error2.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module std_util, require.

main -->
	(
		{ semidet_succeed }
	->
		io__write_string("yes\n")
	;
		io__progname("foo", Name),
		{ error(Name) }
	),
	(
		{ semidet_succeed }
	->
		io__write_string("yes\n")
	;
		io__progname("bar", Name),
		{ error(Name) }
	).


:- module term_io_test.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, string, term_io.

main -->
	see("term_io_test.inp", Res0),
	(
		{ Res0 = ok },
		doit,
		seen
	;
		{ Res0 = error(Err) },
		{ error_message(Err, Msg) },
		stderr_stream(StdErr),
		format(StdErr, "error opening term_io_test.inp: %s\n", [s(Msg)])
	).

:- pred doit(io__state::di, io__state::uo) is det.

doit -->
	read_term(Res0),
	(
		{ Res0 = term(VarSet, Term) },
		write_term_nl(VarSet, Term),
		doit
	;
		{ Res0 = eof }
	;
		{ Res0 = error(Msg, Line) },
		stderr_stream(StdErr),
		format(StdErr, "%d: %s\n", [i(Line), s(Msg)]),
		doit
	).


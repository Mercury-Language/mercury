:- module term_io_test.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, string, term_io.

main -->
	read_term(Res0),
	(
		{ Res0 = term(VarSet, Term) },
		write_term_nl(VarSet, Term),
		main
	;
		{ Res0 = eof }
	;
		{ Res0 = error(Msg, Line) },
		stderr_stream(StdErr),
		format(StdErr, "%d: %s\n", [i(Line), s(Msg)]),
		main
	).


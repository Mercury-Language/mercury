:- module double_error.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, require.

main -->
	io__command_line_arguments(Args),
	{ list__length(Args, Num) },
	(
		{ p(Num) }
	->
		io__write_string("1\n")
	;
		{ Arg = "not 1\n" },
		{ error(Arg) }
	),
	(
		{ q(Num) }
	->
		io__write_string("2\n")
	;
		{ Arg = "not 2\n" },
		{ error(Arg) }
	).

:- pred p(int::in) is semidet.

p(0).
p(1).
p(2).
p(3).

:- pred q(int::in) is semidet.

q(0).
q(4).
q(5).
q(6).


% The top-level

:- module main.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module globals, gml, eval, eval_util, std_util, exception, peephole.

main -->
	try_io(
		(pred(unit::out, di, uo) is det --> 
			globals__init,
			tokenize(BasicTokens),
			{ parse(BasicTokens, Program) },
			{ peephole(Program, OptProgram) },
%			write_prog(0, OptProgram),
			interpret(OptProgram)
		),
		ExceptionResult
	),
	(
		{ ExceptionResult = exception(E) }
	->
		write_nice_exception(E),
		io__set_exit_status(1)
	;
		[]
	).

%-----------------------------------------------------------------------------%

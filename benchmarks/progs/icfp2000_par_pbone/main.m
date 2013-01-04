% The top-level

:- module main.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module globals, gml, eval, eval_util, exception, peephole.
:- import_module unit.
:- import_module renderer.

main -->
	try_io(
		(pred(unit::out, di, uo) is det --> 
			globals__init,
			tokenize(BasicTokens),
			{ parse(BasicTokens, Program) },
			{ peephole(Program, OptProgram) },
%			write_prog(0, OptProgram),
			{ State0 = new_interpreter_state },
			{ interpret(OptProgram, State0, State) },
            do_rendering(State)
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

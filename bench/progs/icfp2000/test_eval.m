:- module test_eval.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module std_util, exception, eval_util, eval, gml, peephole.
:- import_module globals.

main -->
	try_io(
		interpret_tester,
		ExceptionResult
	),
	( { ExceptionResult = succeeded((Env - Stack) - (OptEnv - OptStack)) },
		io__write_string("Success.\n"),
		io__write_string("---   Normal  prog ---.\n"),
		write_env(Env),
		io__write_string("Stack: "),
		io__write(Stack),
		io__nl,
		io__write_string("--- Optimized prog ---.\n"),
		write_env(OptEnv),
		io__write_string("Stack: "),
		io__write(OptStack),
		io__nl
	; { ExceptionResult = exception(E) },
		write_nice_exception(E)
	).

:- pred interpret_tester(pair(pair(env, stack))::out, 
		io__state::di, io__state::uo) is det.

interpret_tester((Env - Stack) - (OptEnv - OptStack)) -->
	globals__init,
	tokenize(Toks),
	{ parse(Toks, Prog) },
	{ peephole(Prog, OptProg) },

	initial_setup(Env0, Stack0),
	interpret(Prog, Env0, Stack0, Env, Stack),

	interpret(OptProg, Env0, Stack0, OptEnv, OptStack).

	/*
	( { OptStack = Stack } ->
		[]
	;
		{ throw(unequal_stacks_exception("Unequal stacks:",
				Stack, OptStack)) }
	).
	*/

	% foo is declared semidet but is actually erroneous.
	% Unused argument elimination used to generate a call to the
	% unused version in a model_det context instead of the correct
	% model_semi context.
:- module unused_arg_determinism.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is cc_multi.

:- type t.
:- func foo(t) = t.
:- mode foo(in) = out is semidet.

:- implementation.
:- import_module exception.
:- type t ---> t ; u.

main -->
	try_io(main_2, Result),
	( { Result = succeeded(_) },
		io__write_string("No exception.\n")
	; { Result = exception(_) },
		io__write_string("Exception.\n")
	).

:- pred main_2(int::out, io__state::di, io__state::uo) is det.
main_2(0) -->
	( { X = foo(t) } ->
		io__write(X)
	;
		io__write_string("failure")
	),
	io__nl.

foo(_) = _ :-
	private_builtin__sorry("foo").

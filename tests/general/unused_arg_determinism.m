	% foo is declared semidet but is actually erroneous.
	% Unused argument elimination used to generate a call to the
	% unused version in a model_det context instead of the correct
	% model_semi context.
:- module unused_arg_determinism.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.

:- type t.
:- func foo(t) = t.
:- mode foo(in) = out is semidet.

:- implementation.
:- type t ---> t ; u.
main -->
	( { X = foo(t) } ->
		io__write(X)
	;
		io__write_string("failure")
	),
	io__nl.

foo(_) = _ :-
	private_builtin__sorry("foo").

:- module arbitrary_constraint_class.
:- interface.

:- import_module float, string, io.

:- typeclass solver_for(B, S) where [
	func coerce(B) = S
].

:- typeclass solver_for_float(U) <= solver_for(float, U) where [].

:- instance solver_for(float, string) where [
	coerce(Float) = string__float_to_string(Float)
].

:- instance solver_for_float(string) where [].

:- pred mg(T, T) <= solver_for_float(T).
:- mode mg(in, out) is det.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module std_util.

mg(S0, S) :-
	( semidet_succeed ->
		S = coerce(0.0)
	;
		S = S0
	).


main -->
	{ mg("1.0", S) },
	io__write_string(S),
	io__nl.

% This module tests the case of calling a semidet pred in
% a nondet lambda expression; Mercury-0.4 got this case wrong.

:- module semidet_lambda.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module list, std_util.

main -->
	{ q(Y) },
	( { Y = [Z] } ->
		io__write_int(Z),
		io__write_string("\n")
	;
		io__write_string("Hello, world\n")
	).

:- pred p(int::out) is semidet.
p(42).

:- pred q(list(int)::out) is det.

q(Y) :-
	solutions(lambda([X::out] is nondet,p(X)), Y).


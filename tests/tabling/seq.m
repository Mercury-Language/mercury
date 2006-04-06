% This test case checks the correctness of the code that performs
% the fixpoint loop returning answers to consumers.

:- module seq.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module solutions, int, list.

main(!IO) :-
	solutions(p, Solns),
	io__write(Solns, !IO),
	io__write_string("\n", !IO).

:- pred p(int).
:- mode p(out) is nondet.

:- pragma minimal_model(p/1).

p(X) :-
	(
		p(Y),
		X is Y + 1,
		X < 10
	;
		X = 0
	).

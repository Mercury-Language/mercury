% This test case checks the correctness of the code that performs
% the fixpoint loop returning answers to consumers.

:- module seq.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module std_util, int, list.

main -->
	{ solutions(p, Solns) },
	io__write(Solns),
	io__write_string("\n").

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

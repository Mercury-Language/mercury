% This test case checks the correctness of the code that performs
% the fixpoint loop returning answers to consumers.  The fixpoint
% computation has to repeatedly switch from one consumer to the
% other to obtain all answers for p/1.

:- module seq2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module solutions, int, list.

main(!IO) :-
	solutions(p, Solns),
	io__write(Solns, !IO),
	io__write_string("\n", !IO).

:- pred p(int::out) is nondet.

:- pragma minimal_model(p/1).

p(X) :-
	(
		p(Y),
		between(0, Y, 9),
		X = -Y - 1
	;
		p(Y),
		between(-9, Y, 0),
		X = -Y + 1
	;
		X = 0
	).

:- pred between(int::in, int::in, int::in) is semidet.

between(A, B, C) :-
	A =< B,
	B =< C.

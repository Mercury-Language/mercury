% This test case checks the correctness of the code that performs
% the fixpoint loop returning answers to consumers.  The fixpoint
% computation has to repeatedly switch from one consumer to the
% other to obtain all answers for p/1.

:- module seq2.

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
		0 =< Y, Y < 10,
		X is -Y - 1
	;
		p(Y),
		-10 < Y, Y =< 10,
		X is -Y + 1
	;
		X = 0
	).

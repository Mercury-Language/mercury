% This ix supposed to be the same as "coup2', albeit without a coup.
% 
% Currently (12 Mar 2004), it does not work correctly for q/1 (some
% answers are missing perhaps due to a bug in the completion algorithm.

:- module seq4.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module std_util, int, list.

main(!IO) :-
	solutions(p, SolnsP),
	io__write_string("P = ", !IO),
	io__write(SolnsP, !IO),
	io__write_string("\n", !IO),
	solutions(q, SolnsQ),
	io__write_string("Q = ", !IO),
	io__write(SolnsQ, !IO),
	io__write_string("\n", !IO).

:- pred p(int::out) is nondet.
:- pragma minimal_model(p/1).

p(X) :-
	(
		p(Y),
		X = 3 * Y,
		X < 20
	;
		q(X)
	).

:- pred q(int::out) is nondet.
:- pragma minimal_model(q/1).

q(X) :-
	(
		X = 1
	;
		q(Y),
		X = 2 * Y,
		X < 20
	).

% This ix supposed to be the same as "coup2', albeit without a coup.
% 
% Currently (12 Mar 2004), it does not work correctly for q/1 (some
% answers are missing perhaps due to a bug in the completion algorithm.

:- module seq4.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module std_util, int, list.

main -->
	{ solutions(p, SolnsP) },
	io__write_string("P = "),
	io__write(SolnsP),
	io__write_string("\n"),
	{ solutions(q, SolnsQ) },
	io__write_string("Q = "),
	io__write(SolnsQ),
	io__write_string("\n").

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

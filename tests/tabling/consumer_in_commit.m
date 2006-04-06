% This test case checks whether we get incorrect answers
% when a consumer gets suspended inside a commit.

:- module consumer_in_commit.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, list.
:- import_module solutions.

main(!IO) :-
	solutions(p, SolnsP),
	io__write(SolnsP, !IO),
	io__write_string("\n", !IO),
	solutions(q, SolnsQ),
	io__write(SolnsQ, !IO),
	io__write_string("\n", !IO).

:- pred p(int).
:- mode p(out) is nondet.

p(X) :-
	q(X),
	r(X).

:- pragma minimal_model(q/1).
:- pred q(int).
:- mode q(out) is nondet.

q(X) :-
	(
		X = 1
	;
		q(Y),
		X = Y + 1,
		X < 10
	).

:- pred r(int).
:- mode r(in) is semidet.

r(X) :-
	X < 5,
	q(_).

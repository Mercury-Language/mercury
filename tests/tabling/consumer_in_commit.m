% This test case checks whether we get incorrect answers
% when a consumer gets suspended inside a commit.

:- module consumer_in_commit.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, list.
:- import_module std_util.

main --> 
	{ solutions(p, SolnsP) },
	io__write(SolnsP),
	io__write_string("\n"),
	{ solutions(q, SolnsQ) },
	io__write(SolnsQ),
	io__write_string("\n").

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

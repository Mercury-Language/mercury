% This test case checks whether we get incorrect answers
% when a generator gets started but not finished inside a commit.
% One possible problem it tests for is not cleaning up the consumers
% of such generators properly.

:- module generator_in_commit.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, list.
:- import_module std_util.

main(!IO) :-
	solutions(p, Solns),
	io__write(Solns, !IO),
	io__write_string("\n", !IO).

:- pred p(int).
:- mode p(out) is nondet.

p(X) :-
	(
		q(_),
		X = 42
	;
		q(Y),
		X = Y + 20
	).

:- pragma minimal_model(q/1).
:- pred q(int).
:- mode q(out) is nondet.

q(X) :-
	(
		q(Y),
		X = Y + 1,
		X < 4
	;
		X = 1
	).

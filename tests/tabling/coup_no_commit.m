% This test case is a variant of coup. It does not use commits,
% but does use the output value of every tabled subgoal.

:- module coup_no_commit.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, list.
:- import_module std_util.

main --> 
	{ solutions(p, Solns) },
	io__write(Solns),
	io__write_string("\n").

:- pragma minimal_model(p/1).
:- pred p(int).
:- mode p(out) is nondet.

p(X) :-
	(
		q(X)
	;
		X = 1
	).

:- pragma minimal_model(q/1).
:- pred q(int).
:- mode q(out) is nondet.

q(Y) :-
	p(Z),
	Y = Z + 3,
	Y < 20.

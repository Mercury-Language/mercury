% This test case checks whether we get incorrect answers
% when a consumer gets suspended inside solutions.

:- module consumer_in_solutions.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, list.
:- import_module std_util.

main --> 
	{ solutions(q, Solns) },
	io__write(Solns),
	io__write_string("\n").

:- pragma minimal_model(q/1).
:- pred q(pair(int, list(int))).
:- mode q(out) is nondet.

q(X - L) :-
	p(X),
	solutions(p, L).

:- pragma minimal_model(p/1).
:- pred p(int).
:- mode p(out) is nondet.

p(1).
p(2).
p(3).

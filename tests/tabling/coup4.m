% This is a test for whether a single coup that takes leadership away from
% multiple generators is correctly handled.
%
% The code in this file is the same as coup3, except the order of clauses in r.

:- module coup4.

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
	io__write_string("\n"),
	{ solutions(r, SolnsR) },
	io__write_string("R = "),
	io__write(SolnsR),
	io__write_string("\n").

:- pred p(int::out) is nondet.
:- pragma minimal_model(p/1).

p(X) :-
	(
		p(Y),
		X = 2 * Y,
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
	        q(Y),		% suspend execution -- q is its own leader here
	        X = 3 * Y,
	        X < 20
	;
	        r(X)
	).

:- pred r(int::out) is nondet.
:- pragma minimal_model(r/1).

r(X) :-
	(
	        X = 1		% r is its own leader at this point
	;
		p(Y),		% here a coup takes place -- p becomes leader
				% of both q and r
		X = 5 * Y,
		X < 20
	;
	        q(Y),
	        X = 4 * Y,
	        X < 20
	).

% This is a test for whether a single coup that takes leadership away from
% multiple generators is correctly handled.
%
% The code in this file is the same as coup3, except the order of clauses in r.

:- module coup4.

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
	io__write_string("\n", !IO),
	solutions(r, SolnsR),
	io__write_string("R = ", !IO),
	io__write(SolnsR, !IO),
	io__write_string("\n", !IO).

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

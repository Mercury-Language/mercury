% This is a test for whether multiple coups (change of leaders)
% are correctly handled.

:- module coup3.

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
	        q(Y), % suspend execution -- q is its own leader here
	        X = 3 * Y,
	        X < 20
	;
	        r(X)
	).

:- pred r(int::out) is nondet.
:- pragma minimal_model(r/1).

r(X) :-
	(
	        X = 1 % r is its own leader at this point
	;
	        q(Y), % here a coup takes place -- leader becomes q
	        X = 4 * Y,
	        X < 20
	;
		p(Y), % another coup takes place -- leader becomes p
		X = 5 * Y,
		X < 20
	).

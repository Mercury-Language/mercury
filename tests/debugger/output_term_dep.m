:- module output_term_dep.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module list.

main -->
	%
	% Test cases which track an output subterm.
	%
	test1,		% basic det conjunction
	test2,		% construction unification
	test3,		% if-then-else
	test4,		% switch and disjunction
	test5.		% negation

%-----------------------------------------------------------------------------%

:- pred test1(io__state::di, io__state::uo) is det.
test1 -->
	{ p(A, B, C) },
	io__write_int(A),
	io__nl,
	io__write_int(B),
	io__nl,
	io__write_int(C),
	io__nl.

:- pred p(int, int, int).
:- mode p(out, out, out) is det.

p(A, B, C) :-		% tracking subterm B
	pa(A),
	pb(B),
	pc(C).

:- pred pa(int).
:- mode pa(out) is det.

pa(5).

:- pred pb(int).
:- mode pb(out) is det.

pb(8).

:- pred pc(int).
:- mode pc(out) is det.

pc(13).

%-----------------------------------------------------------------------------%

:- pred test2(io__state::di, io__state::uo) is det.
test2 -->
	{ q(X) },
	io__write(X),
	io__nl.

:- pred q(list(list(int))).
:- mode q(out) is det.

q([A, B, C]) :-		% tracking subterm B
	qa(A),
	qb(B),
	qc(C).

:- pred qa(list(int)).
:- mode qa(out) is det.

qa([1, 2, 3]).

:- pred qb(list(int)).
:- mode qb(out) is det.

qb([]).

:- pred qc(list(int)).
:- mode qc(out) is det.

qc([99]).

%-----------------------------------------------------------------------------%

:- pred test3(io__state::di, io__state::uo) is det.
test3 -->
	{ r(1, W) },
	io__write(W),
	io__nl,
	{ r(2, X) },
	io__write(X),
	io__nl,
	{ r(3, Y) },
	io__write(Y),
	io__nl,
	{ r(4, Z) },
	io__write(Z),
	io__nl.

:- pred r(int, int).
:- mode r(in, out) is det.

r(N, P) :-
	(
		N = 1
	->
		P = 999
	;
		ra(N)
	->
		(
			rb(N)
		->
			rc(P)
		;
			P = 43
		)
	;
		rd(P)
	).

:- pred ra(int).
:- mode ra(in) is semidet.

ra(2).
ra(3).

:- pred rb(int).
:- mode rb(in) is semidet.

rb(3).

:- pred rc(int).
:- mode rc(out) is det.

rc(57).

:- pred rd(int).
:- mode rd(out) is det.

rd(-1).

%-----------------------------------------------------------------------------%

:- pred test4(io__state::di, io__state::uo) is det.
test4 -->
	(
		{ s(1, _, X) },
		{ sd(X) }
	->
		io__write_string("yes\n")
	;
		io__write_string("no\n")
	),
	(
		{ s(2, _, Y) },
		{ sd(Y) }
	->
		io__write_string("yes\n")
	;
		io__write_string("no\n")
	).

:- pred s(int, int, int).
:- mode s(in, out, out) is nondet.

s(1, J, K) :-
	(
		sa(J)
	;
		sb(J)
	),
	(
		sa(K)
	;
		sc(K)
	).
s(2, J, K) :-
	(
		sa(J),
		sb(K)
	;
		sb(J),
		sc(K)
	).

:- pred sa(int).
:- mode sa(out) is det.

sa(7).

:- pred sb(int).
:- mode sb(out) is det.

sb(38).

:- pred sc(int).
:- mode sc(out) is det.

sc(155).

:- pred sd(int).
:- mode sd(in) is semidet.

sd(-3).

%-----------------------------------------------------------------------------%

:- pred test5(io__state::di, io__state::uo) is det.
test5 -->
	(
		{ t(1, K) }
	->
		io__write_int(K),
		io__nl
	;
		io__write_string("no\n")
	).

:- pred t(int, int).
:- mode t(in, out) is semidet.

t(J, K) :-
	\+ ta(J),
	tb(K),
	\+ tc(K).

:- pred ta(int).
:- mode ta(in) is semidet.

ta(0).

:- pred tb(int).
:- mode tb(out) is det.

tb(77).

:- pred tc(int).
:- mode tc(in) is semidet.

tc(-654).

%-----------------------------------------------------------------------------%


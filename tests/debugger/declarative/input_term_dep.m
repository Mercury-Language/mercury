:- module input_term_dep.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module list.

main -->
	%
	% Test cases which track an input subterm.
	%
	test1,		% basic det conjunction
	test2,		% construction unification
	test3,		% if-then-else
	test4.		% negation

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

p(A, B, C) :-
	pa(A),
	pb(B),
	pc(A, C).

:- pred pa(int).
:- mode pa(out) is det.

pa(5).

:- pred pb(int).
:- mode pb(out) is det.

pb(8).

:- pred pc(int, int).
:- mode pc(in, out) is det.

pc(_, 13).

%-----------------------------------------------------------------------------%

:- pred test2(io__state::di, io__state::uo) is det.
test2 -->
	(
		{ q(X) }
	->
		io__write(X),
		io__nl
	;
		io__write_string("no\n")
	).

:- pred q(list(list(int))).
:- mode q(out) is semidet.

q(L) :-
	qa([C, A]),
	qb(B),
	qc([A, B, C], L).

:- pred qa(list(list(int))).
:- mode qa(out) is det.

qa([[1], [2, 3]]).

:- pred qb(list(int)).
:- mode qb(out) is det.

qb([]).

:- pred qc(list(list(int)), list(list(int))).
:- mode qc(in, out) is det.

qc(L, L).

%-----------------------------------------------------------------------------%

:- pred test3(io__state::di, io__state::uo) is det.
test3 -->
	{ r(1, Z) },
	io__write(Z),
	io__nl.

:- pred r(int, int).
:- mode r(in, out) is det.

r(N, P) :-
	(
		ra(N, A)
	->
		(
			rb(A)
		->
			rc(A, P)
		;
			P = 1
		)
	;
		P = 99
	).

:- pred ra(int, int).
:- mode ra(in, out) is semidet.

ra(1, 3).

:- pred rb(int).
:- mode rb(in) is semidet.

rb(3).

:- pred rc(int, int).
:- mode rc(in, out) is det.

rc(_, 33).

%-----------------------------------------------------------------------------%

:- pred test4(io__state::di, io__state::uo) is det.
test4 -->
	(
		{ s(1) }
	->
		io__write_string("yes\n")
	;
		io__write_string("no\n")
	).

:- pred s(int).
:- mode s(in) is semidet.

s(N) :-
	\+ (
		sa(N, A),
		sb(A),
		\+ sc(A)
	).

:- pred sa(int, int).
:- mode sa(in, out) is semidet.

sa(1, 7).

:- pred sb(int).
:- mode sb(in) is semidet.

sb(7).

:- pred sc(int).
:- mode sc(in) is semidet.

sc(7).

%-----------------------------------------------------------------------------%

:- module special_term_dep.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.

%
% Term dependencies in the presence of calls to special predicates.
%

:- implementation.
:- import_module list.

main -->
	test1,			% compare
	test2.			% unify

%-----------------------------------------------------------------------------%

:- pred test1(io__state::di, io__state::uo) is det.

test1 -->
	{ p(L) },
	io__write(L),
	io__nl.

:- pred p(list(int)).
:- mode p(out) is det.

p(L) :-
	pa(L0),
	(
		compare('>', L0, [1])
	->
		L = L0
	;
		L = []
	).

:- pred pa(list(int)).
:- mode pa(out) is det.

pa([2, 3]).

%-----------------------------------------------------------------------------%

:- pred test2(io__state::di, io__state::uo) is det.

test2 -->
	(
		{ q([1, 2], L) }
	->
		io__write(L),
		io__nl
	;
		io__write_string("no\n")
	).

:- pred q(list(int), list(int)).
:- mode q(in, out) is semidet.

q(A, B) :-
	qa(A),
	qb(A, B).

:- pred qa(list(int)).
:- mode qa(out) is det.

qa([1, 2]).

:- pred qb(list(int), list(int)).
:- mode qb(in, out) is det.

qb(_, [3]).

%-----------------------------------------------------------------------------%


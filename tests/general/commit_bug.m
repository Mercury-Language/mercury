% This tests the case of committting across a nondet goal in a nondet
% context. There was a bug in this, which this test case exercised.

:- module commit_bug.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, list, std_util.

main -->
	{ solutions(test, List) },
	print_intlist(List).

:- pred test(int::out) is multi.
test(Val) :-
	(if some [X]
		list__member(X, [1,2,3,4,5])
	then
		(if some [Z] (
			some [Y] foo(X, Y),
			foo(X,Z)
			)
		then
			Val = Z		
		else
			Val = -1
		)
	else
		Val = -2
	).

:- pred foo(int, int).
:- mode foo(in, out) is nondet.

foo(X, X).
foo(_, 7).

:- pred nl(io__state::di, io__state::uo) is det.
nl --> io__write_string("\n").

:- pred print_intlist(list(int)::in,io__state::di, io__state::uo) is det.
print_intlist([])--> [].
print_intlist([X|L])--> io__write_int(X), nl, print_intlist(L).


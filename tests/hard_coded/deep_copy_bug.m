% This is a regression test.
% The Mercury compiler of Apr 11 1997 failed for this in non-gc grades,
% because of a bug in deep_copy() of equivalence types.

:- module deep_copy_bug.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, solutions, std_util, list, term, varset.

main --> test1, test2, test3.

:- pred test1(io__state::di, io__state::uo) is det.
test1 -->
	{ Lambda = (pred(X::out) is nondet :-
		varset__init(Varset0),
		varset__new_vars(Varset0, 10, Vars, _),
		list__member(X, Vars)
	) },
	{ solutions(Lambda, List) },
	io__write(List),
	io__write_string("\n").

:- pred test2(io__state::di, io__state::uo) is det.
test2 -->
	test2b("blahblah").

:- pred test2b(T::in, io__state::di, io__state::uo) is det.
test2b(S) -->
	{ F = foo(S) },
	{ solutions(F, List) },
	io__write(List),
	io__write_string("\n").

:- pred foo(T, var).
:- mode foo(in, out) is nondet.
foo(Blah, X) :-
	varset__init(Varset0),
	varset__new_vars(Varset0, 10, Vars, _),
	list__member(X, Vars).

:- pred test3(io__state::di, io__state::uo) is det.
test3 -->
	{ solutions((pred(X::out) is nondet :- bar(X)), List) },
	io__write(List),
	io__write_string("\n").

:- pred bar(int).
:- mode bar(out) is det.
bar(42).

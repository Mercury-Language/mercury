% This is a regression test.
% The Mercury compiler of Apr 11 1997 failed for this in non-gc grades,
% because of a bug in deep_copy() of equivalence types.

:- module deep_copy_bug.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, std_util, list, term, varset.

main -->
	{ Lambda = lambda([X::out] is nondet,
	(
		varset__init(Varset0),
		varset__new_vars(Varset0, 10, Vars, _),
		list__member(X, Vars)
	)) },
	{ solutions(Lambda, List) },
	io__write(List),
	io__write_string("\n").

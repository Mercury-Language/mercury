% This test case checks that we correctly handle the trivial case of the same
% subgoal being called twice, without recursion.

:- module repeat.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, list.
:- import_module std_util.

main --> 
	{ solutions(p, Solns1) },
	io__write(Solns1),
	io__write_string("\n"),
	{ solutions(p, Solns2) },
	io__write(Solns2),
	io__write_string("\n").

:- pragma minimal_model(p/1).
:- pred p(int).
:- mode p(out) is nondet.

p(1).
p(2).
p(3).

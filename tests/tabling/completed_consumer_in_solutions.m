% This test case checks whether we get incorrect answers
% when a consumer accesses a completed generator inside solutions.
% The first call to solutions completes the generator for p,
% the second call accesses the completed generator.

:- module completed_consumer_in_solutions.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, list.
:- import_module std_util.

main --> 
	{ q(Solns1, Solns2) },
	io__write(Solns1),
	io__write_string("\n"),
	io__write(Solns2),
	io__write_string("\n").

:- pred q(list(int)::out, list(int)::out) is det.

q(L1, L2) :-
	solutions(p, L1),
	solutions(p, L2).

:- pragma minimal_model(p/1).
:- pred p(int::out) is nondet.

p(1).
p(2).
p(3).

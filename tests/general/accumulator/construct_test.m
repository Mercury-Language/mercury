	%
	% Tests that any construction unifications get handled properly.
	%
:- module construct_test.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list.

main -->
	io__write_string("p1: "),
	{ p([1,10,100], ListA) },
	io__write(ListA),
	io__nl,
	io__write_string("pb: "),
	{ p2([5,6,7], ListB) },
	io__write(ListB),
	io__nl.

:- pred p(list(T), list(T)).
:- mode p(in, out) is det.

	%
	% Direct construction unification.
	%
p([], []).
p(X,Y) :-
	X = [H|T],
	p(T,T0),
	Y = [H|T0].

	%
	% Hide the construction by introducing some intermediate
	% variables.
	%
	% This will introduce accumulators provided 
	% --optimize-constructor-last-call is turned on.
	%
:- pred p2(list(int), list(int)).
:- mode p2(in, out) is det.

p2([], []).
p2(X,Y) :-
	X = [H|T],
	p2(T, T0),
	append(T0, [1], T1),
	Y = [H|T1].

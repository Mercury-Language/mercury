	%
	% Tests that any deconstruction unifications get handled properly.
	%
:- module deconstruct_test.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, list.

:- type wrapper ---> wrapper(int, list(int)).

main -->
	io__write_string("p1: "),
	(
		{ p([1,10,100], ListA) }
	->
		io__write(ListA)
	;
		io__write_string("failed")
	),
	io__nl,
	io__write_string("pb: "),
	(
		{ p2([5,6,7], ListB) }
	->
		io__write(ListB)
	;
		io__write_string("failed")
	),
	io__nl.

:- pred p(list(int), list(int)).
:- mode p(in, out) is semidet.

	%
	% Direct deconstruction unification.
	%
p([], [1000]).
p(X,Y) :-
	X = [H|T],
	p(T,T0),
	T0 = [Ht | Tt],
	append([Ht], [H], NewH),
	append(NewH, Tt, Y).

	%
	% Using a deconstruction as a wrapper.  Should introduce
	% accumlator recursion, doesn't.
	%
:- pred p2(list(int), wrapper).
:- mode p2(in, out) is semidet.

p2([], wrapper(0, [])).
p2(X,W) :-
	X = [H|T],
	p2(T, W0),
	W0 = wrapper(L0, R0),
	L is L0 + 1,
	append(R0, [H], R),
	W = wrapper(L, R).

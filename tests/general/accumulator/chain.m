	%
	% Tests chained calls to a predicate that requires
	% rearrangement.
	%
:- module chain.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, list.

main -->
	io__write_string("pa: "),
	{ pa([5,6,7], ListA) },
	io__write(ListA),
	io__nl,
	io__write_string("pb: "),
	{ pb([5,6,7], ListB) },
	io__write(ListB),
	io__nl,
	io__write_string("pc: "),
	{ pc([1,3,5], ValC) },
	io__write(ValC),
	io__nl,
	io__write_string("pd: "),
	{ pd([2,4,5], ValD) },
	io__write(ValD),
	io__nl.

	%
	% append([H], [1], NewH) is static so we can introduce
	% accumulator recursion.
	%
:- pred pa(list(int)::in, list(int)::out) is det.

pa([], []).
pa(X, Y) :-
	X = [H | T],
	pa(T, T0),
	append([H], [1], NewH),
	append(T0, NewH, Y).

	%
	% We have two calls to append with dynamic variables in them
	% that require rearrangement.  Hence we can't introduce
	% accumulator recursion.
	%
:- pred pb(list(int)::in, list(int)::out) is det.

pb([], []).
pb(X, Y) :-
	X = [H | T],
	pb(T, T0),
	append([1], T0, NewT),
	append([H], NewT, Y).

	%
	% We have two calls to append with dynamic variables in them
	% that don't require rearrangement.  Hence we CAN introduce
	% accumulator recursion.
	%
:- pred pc(list(int)::in, int::out) is det.

pc([], 0).
pc(X, Y) :-
	X = [H | T],
	pc(T, Y0),
	Tmp is Y0 + (2*H),
	Y is Tmp + H.

	%
	% We CANNOT introduce accumulators because the chain of calls
	% are to different predicates.
	%
:- pred pd(list(int)::in, int::out) is det.

pd([], 0).
pd(X, Y) :-
	X = [H | T],
	pd(T, Y0),
	Tmp is 2*Y0,
	Y is Tmp + H.

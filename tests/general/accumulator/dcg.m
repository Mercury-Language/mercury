	%
	% Tests the case where the base case contains some goals which
	% must be left in the base case of the introduced predicate.
	%
:- module dcg.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list.

main -->
	io__write_string("p A: "),
	{ p([1,10,100,9,0], ListA, [], ListB) },
	io__write(ListA),
	io__nl,
	io__write_string("p B: "),
	io__write(ListB),
	io__nl,
	io__write_string("p2 A2: "),
	{ p2([1,10,100,9,0], ListA2, [], ListB2) },
	io__write(ListA2),
	io__nl,
	io__write_string("p2 B2: "),
	io__write(ListB2),
	io__nl.

	%
	% We can introduce accumulators, but the DCG goals must be left
	% in the base case of the accumulator version of the predicate.
	%
:- pred p(list(T), list(T), list(T), list(T)).
:- mode p(in, out, in, out) is det.

p([], []) --> [].
p(X,Y) -->
	{ X = [H|T] },
	q(H),
	p(T,T0),
	{ list__append(T0, [H], Y) }.


	%
	% We cannot introduce accumulators because the second call to q
	% can't be moved before p2.
	%
:- pred p2(list(T), list(T), list(T), list(T)).
:- mode p2(in, out, in, out) is det.

p2([], []) --> [].
p2(X,Y) -->
	{ X = [H|T] },
	q(H),
	p2(T,T0),
	q(H),
	{ list__append(T0, [H], Y) }.


:- pred q(T, list(T), list(T)).
:- mode q(in, in, out) is det.
:- pragma no_inline(q/3).

q(H, DCG0, DCG) :-
	DCG = [H | DCG0].

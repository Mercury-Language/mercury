	%
	% This is an interleaved version of reverse and length.
	% Tests if we can introduce more then one accumulator.
	%
:- module inter.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, int.

main -->
	io__write_string("rl: "),
	{ rl([1,10,100], Length, Reverse) },
	io__write(Length),
	io__write_string(" "),
	io__write(Reverse),
	io__nl.

:- pred rl(list(T), int, list(T)).
:- mode rl(in, out, out) is det.

rl(X, L, R) :-
	X = [],
	L = 0,
	R = [].
rl(X, L, R) :-
	X = [H | T],
	rl(T, L0, R0),
	L is L0 + 1,
	Tmp = [H],
	append(R0, Tmp, R).

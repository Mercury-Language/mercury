% This program tests whether the debugger is able to properly handle
% label layout structures which have variables whose locations (lvals)
% use long as well as short encodings.

:- module lval_desc_array.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int.

main -->
	{ A0 = 0 },
	{ perform_increments(A0, A) },
	io__write_int(A),
	io__write_string("\n").

:- pred perform_increments(int::in, int::out) is det.

perform_increments -->
	increment, increment, increment, increment, increment, increment,
	increment, increment, increment, increment, increment, increment,
	increment, increment, increment, increment, increment, increment,
	increment, increment, increment, increment, increment, increment,
	increment, increment, increment, increment, increment, increment,
	increment, increment, increment, increment, increment, increment,
	increment, increment, increment, increment, increment, increment,
	increment, increment, increment, increment, increment, increment,
	increment, increment, increment, increment, increment, increment,
	increment, increment, increment, increment, increment, increment,
	increment, increment, increment, increment, increment, increment,
	increment, increment, increment, increment, increment, increment,
	increment, increment, increment, increment, increment, increment,
	increment, increment, increment, increment, increment, increment,
	increment, increment, increment, increment, increment, increment,
	increment, increment, increment, increment, increment, increment,
	increment, increment, increment, increment, increment, increment,
	increment, increment, increment, increment, increment, increment.

:- pragma no_inline(increment/2).

:- pred increment(int::in, int::out) is det.

increment(N, N + 1).

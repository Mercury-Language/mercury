% A test to see whether backwards arithmetic works.

:- module rev_arith.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module int, std_util.

main --> 
	{
	3 is 1 + A,
	3 is B + 2,
	C is 1 + 2,
	10 is 2 * D,
	10 is E * 5,
	F is 2 * 5,
	20 is 30 - G,
	20 is H - 10,
	I is 30 - 10,
	15 is 90 // J,
	15 is K // 6,
	L is 90 // 6
	},
	io__write_int(A),
	io__write_string("\n"),
	io__write_int(B),
	io__write_string("\n"),
	io__write_int(C),
	io__write_string("\n"),
	io__write_int(D),
	io__write_string("\n"),
	io__write_int(E),
	io__write_string("\n"),
	io__write_int(F),
	io__write_string("\n"),
	io__write_int(G),
	io__write_string("\n"),
	io__write_int(H),
	io__write_string("\n"),
	io__write_int(I),
	io__write_string("\n"),
	io__write_int(J),
	io__write_string("\n"),
	io__write_int(K),
	io__write_string("\n"),
	io__write_int(L),
	io__write_string("\n").

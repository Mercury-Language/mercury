:- module compare_rep_array.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is cc_multi.
:- implementation.
:- import_module array, list, std_util.

main -->
	test(d1, d2),
	test(d2, d3),
	test(d3, d3).

:- type val == pair(string, univ).

:- func d1 = val.
d1 = "{1, 2, 3} : array(int)" - univ(array([1, 2, 3])).

:- func d2 = val.
d2 = "{1, 4, 9} : array(int)" - univ(array([1, 4, 9])).

:- func d3 = val.
d3 = "{1.0, 1.1, 1.2} : array(float)" - univ(array([1.0, 1.1, 1.2])).

:- pred test(val::in, val::in, io__state::di, io__state::uo) is cc_multi.
test(SA - A, SB - B) -->
	io__write_string(SA),
	io__nl,
	io__write_string(SB),
	io__nl,
	{ std_util__compare_representation(Res, A, B) },
	io__write_string("Result = "),
	io__write(Res),
	io__write_string(".\n\n").


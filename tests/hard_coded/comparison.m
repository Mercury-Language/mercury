% This is a test to check the correctness of the way we handle specialized
% comparison predicates for types with three or fewer function symbols.

:- module comparison.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module list, std_util, exception.

:- type t1 ---> a1(int, int).

:- type t2 ---> a2(int, int) ; b2(int).

:- type t3 ---> a3(int, int) ; b3(int) ; c3.

main -->
	perform_comparison_test(a1(10, 20), a1(10, 21)),
	perform_comparison_test(a1(10, 20), a1(10, 20)),
	perform_comparison_test(a1(10, 20), a1( 9, 20)),

	perform_comparison_test(a2(10, 20), a2(10, 19)),
	perform_comparison_test(a2(10, 20), a2(10, 20)),
	perform_comparison_test(a2(10, 20), a2(11, 20)),
	perform_comparison_test(a2(10, 20), b2(10)),

	perform_comparison_test(b2(30), a2(50, 40)),
	perform_comparison_test(b2(30), b2(29)),
	perform_comparison_test(b2(30), b2(30)),
	perform_comparison_test(b2(30), b2(31)),

	perform_comparison_test(a3(10, 20), a3(10, 19)),
	perform_comparison_test(a3(10, 20), a3(10, 20)),
	perform_comparison_test(a3(10, 20), a3(11, 20)),
	perform_comparison_test(a3(10, 20), b3(10)),
	perform_comparison_test(a3(10, 20), c3),

	perform_comparison_test(b3(30), a3(50, 40)),
	perform_comparison_test(b3(30), b3(29)),
	perform_comparison_test(b3(30), b3(30)),
	perform_comparison_test(b3(30), b3(31)),
	perform_comparison_test(b3(30), c3),

	perform_comparison_test(c3, a3(50, 40)),
	perform_comparison_test(c3, b3(50)),
	perform_comparison_test(c3, c3).

:- pred perform_comparison_test(T::in, T::in, io__state::di, io__state::uo)
	is det.

perform_comparison_test(X, Y) -->
	{ compare(R, X, Y) },
	io__write(X),
	(
		{ R = (<) },
		io__write_string(" < ")
	;
		{ R = (=) },
		io__write_string(" = ")
	;
		{ R = (>) },
		io__write_string(" > ")
	),
	io__write(Y),
	io__write_string("\n").

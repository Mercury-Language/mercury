:- module solve_quadratic.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module float, math.

main -->
	% Two roots (i.e. B^2 > 4AC)
	check_quad(1.0, 3.0, -2.0),
	check_quad(1.0, -5.0, 3.0),
	check_quad(2.0, -5.0, 3.0),
	check_quad(2.0, -5.0, -6.0),
	check_quad(-15.0, 68.0, 34.5),
	check_quad(-4.0, -17.0, 3.5),

	% One root (i.e. B^2 = 4AC)
	check_quad(5.0, 10.0, 5.0),
	check_quad(4.0, -8.0, 4.0),
	check_quad(-3.0, -18.0, -27.0),

	% No roots (i.e. B^2 < 4AC)
	check_quad(4.0, 3.0, 2.0),
	check_quad(1.0, 1.0, 1.0),
	check_quad(-1.0, -2.0, -2.0),

	% Special cases
	check_quad(1.0, -2.0, 0.0),
	check_quad(1.0, 0.0, -2.0),
	check_quad(2.0, 0.0, 0.0),
	check_quad(-100.0, 0.0, 0.0001).


:- pred check_quad(float, float, float, io__state, io__state).
:- mode check_quad(in, in, in, di, uo) is det.

check_quad(A, B, C) -->
	{ Ss = solve_quadratic(A, B, C) },
	(
		{ Ss = no_roots },
		io__write_string("No roots.\n")
	;
		{ Ss = one_root(R) },
		io__write_string("One root: "),
		check_result(A, B, C, R),
		io__write_string(".\n")
	;
		{ Ss = two_roots(R1, R2) },
		io__write_string("Two roots: "),
		check_result(A, B, C, R1),
		io__write_string(", "),
		check_result(A, B, C, R2),
		io__write_string(".\n")
	).

:- pred check_result(float, float, float, float, io__state, io__state).
:- mode check_result(in, in, in, in, di, uo) is det.

check_result(A, B, C, R) -->
	{ Val = ((A * R + B) * R) + C },
	(
		%
		% This test is pretty conservative, since I don't know
		% how much error should be expected.
		%
		{ abs(Val) < 1E-6 }
	->
		io__write_string("ok")
	;
		io__write_string("problem: Val = "),
		io__write_float(Val)
	).


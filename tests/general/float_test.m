:- module test.
:- interface.
:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.
:- import_module float, string, list.

main -->
	( { string__to_float("1234.5678", F) } ->
		write_message("1234.5678: ", F)
	;
		io__write_string("can't parse 1234.5678")
	),
	test(3.0, 4.0),
	test(41.0, -3.0).

:- pred test(float, float, io__state, io__state).
:- mode test(in, in, di, uo) is det.

test(X, Y) -->
	{
		builtin_float_plus(X, Y, Plus),
		builtin_float_times(X, Y, Times),
		builtin_float_minus(X, Y, Minus),
		builtin_float_divide(X, Y, Divide)
	},
	write_message("X: ", X),
	write_message("Y: ", Y),
	write_message("X + Y: ", Plus),
	write_message("X * Y: ", Times),
	write_message("X - Y: ", Minus),
	write_message("X / Y: ", Divide).

:- pred write_message(string, float, io__state, io__state).
:- mode write_message(in, in, di, uo) is det.

write_message(String, Float) -->
	io__write_string(String),
	io__write_float(Float),
	io__write_string("\n").


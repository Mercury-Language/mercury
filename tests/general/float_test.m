% A very basic check of floating point arithmetic and string__to_float.
% Now tests maths library stuff too.

:- module float_test.
:- interface.
:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.
:- import_module float, math, string, list.

main -->
	( { string__to_float("1234.5678", F) } ->
		write_message("1234.5678: ", F)
	;
		io__write_string("can't parse 1234.5678")
	),
	test(3.0, 4.0),
	test(41.0, -3.0),
	test_constants,
	test_math_constants,
	test_rounding(2.7),
	test_rounding(-3.6),
	test_power(2.2),
	test_trig(0.5),
	test_inv_trig(0.6).

:- pred test(float, float, io__state, io__state).
:- mode test(in, in, di, uo) is det.

test(X, Y) -->
	{
		Plus = X + Y,
		Times = X * Y,
		Minus = X - Y,
		Divide = X / Y,
		Pow = math__pow(X, Y)
	},
	write_message("X: ", X),
	write_message("Y: ", Y),
	write_message("X + Y: ", Plus),
	write_message("X * Y: ", Times),
	write_message("X - Y: ", Minus),
	write_message("X / Y: ", Divide),
	write_message("X ^ Y: ", Pow).

:- pred write_message(string, float, io__state, io__state).
:- mode write_message(in, in, di, uo) is det.

write_message(String, Float) -->
	{ string__format("%s%6.3g\n", [s(String), f(Float)], Message) },
	io__write_string(Message).

:- pred test_constants(io__state :: di, io__state :: uo) is det.
test_constants -->
	write_message("Float max: ", float__max),
	write_message("Float min: ", float__min),
	write_message("Float epsilon: ", float__epsilon).

:- pred test_math_constants(io__state :: di, io__state :: uo) is det.
test_math_constants -->
	write_message("Pi: ", math__pi),
	write_message("e: ", math__e).

:- pred test_rounding(float :: in, io__state :: di, io__state :: uo) is det.
test_rounding(X) -->
	write_message("X: ", X),
	write_message("ceil(X): ", math__ceiling(X)),
	write_message("floor(X): ", math__floor(X)),
	write_message("round(X): ", math__round(X)),
	write_message("truncate(X): ", math__truncate(X)).

:- pred test_power(float :: in, io__state :: di, io__state :: uo) is det.
test_power(X) -->
	write_message("X: ", X),
	write_message("sqrt(X): ", math__sqrt(X)),
	write_message("ln(X): ", math__ln(X)),
	write_message("log2(X): ", math__log2(X)),
	write_message("log10(X): ", math__log10(X)),
	write_message("log(2.1,X): ", math__log(2.1, X)),
	write_message("exp(X): ", math__exp(X)).

:- pred test_trig(float :: in, io__state :: di, io__state :: uo) is det.
test_trig(X) -->
	write_message("X: ", X),
	write_message("sin(X): ", math__sin(X)),
	write_message("cos(X): ", math__cos(X)),
	write_message("tan(X): ", math__tan(X)),
	write_message("sinh(X): ", math__sinh(X)),
	write_message("cosh(X): ", math__cosh(X)),
	write_message("tanh(X): ", math__tanh(X)),
	write_message("atan2(sin(X),cos(X)): ",
		math__atan2(math__sin(X), math__cos(X))).

:- pred test_inv_trig(float :: in, io__state :: di, io__state :: uo) is det.
test_inv_trig(X) -->
	write_message("X: ", X),
	write_message("asin(X): ", math__asin(X)),
	write_message("acos(X): ", math__acos(X)),
	write_message("atan(X): ", math__atan(X)).

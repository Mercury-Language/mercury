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
	% test_math_constants,
	% test_rounding(2.7),
	% test_rounding(-3.6),
	% test_power(2.2),
	% test_trig(0.5),
	% test_inv_trig(0.6).
	[].

:- pred test(float, float, io__state, io__state).
:- mode test(in, in, di, uo) is det.

test(X, Y) -->
	{
		Plus = X + Y,
		Times = X * Y,
		Minus = X - Y,
		Divide = X / Y,
		% math__pow(X, Y, Pow)
		true
	},
	write_message("X: ", X),
	write_message("Y: ", Y),
	write_message("X + Y: ", Plus),
	write_message("X * Y: ", Times),
	write_message("X - Y: ", Minus),
	write_message("X / Y: ", Divide),
	% write_message("X ^ Y: ", Pow).
	[].

:- pred write_message(string, float, io__state, io__state).
:- mode write_message(in, in, di, uo) is det.

write_message(String, Float) -->
	{ string__format("%s%6.3g\n", [s(String), f(Float)], Message) },
	io__write_string(Message).

:- pred test_constants(io__state :: di, io__state :: uo) is det.
test_constants -->
	{ float__max(FMax) },
	write_message("Float max: ", FMax),
	{ float__min(FMin) },
	write_message("Float min: ", FMin),
	{ float__epsilon(FEps) },
	write_message("Float epsilon: ", FEps).

:- pred test_math_constants(io__state :: di, io__state :: uo) is det.
test_math_constants -->
	{ math__pi(Pi) },
	write_message("Pi: ", Pi),
	{ math__e(E) },
	write_message("e: ", E).

:- pred test_rounding(float :: in, io__state :: di, io__state :: uo) is det.
test_rounding(X) -->
	{
	    math__ceiling(X, Ceil),
	    math__floor(X, Floor),
	    math__round(X, Round),
	    math__truncate(X, Truncate)
	},
	write_message("X: ", X),
	write_message("ceil(X): ", Ceil),
	write_message("floor(X): ", Floor),
	write_message("round(X): ", Round),
	write_message("truncate(X): ", Truncate).

:- pred test_power(float :: in, io__state :: di, io__state :: uo) is det.
test_power(X) -->
	{
	    math__sqrt(X, Sqrt),
	    math__ln(X, Ln),
	    math__log2(X, Log2),
	    math__log10(X, Log10),
	    math__log(2.1, X, Log2_1),
	    math__exp(X, Exp)
	},
	write_message("X: ", X),
	write_message("sqrt(X): ", Sqrt),
	write_message("ln(X): ", Ln),
	write_message("log2(X): ", Log2),
	write_message("log10(X): ", Log10),
	write_message("log(2.1,X): ", Log2_1),
	write_message("exp(X): ", Exp).

:- pred test_trig(float :: in, io__state :: di, io__state :: uo) is det.
test_trig(X) -->
	{
	    math__sin(X, Sin),
	    math__cos(X, Cos),
	    math__tan(X, Tan),
	    math__sinh(X, Sinh),
	    math__cosh(X, Cosh),
	    math__tanh(X, Tanh),
	    math__atan2(Sin, Cos, Atan2)
	},
	write_message("X: ", X),
	write_message("sin(X): ", Sin),
	write_message("cos(X): ", Cos),
	write_message("tan(X): ", Tan),
	write_message("sinh(X): ", Sinh),
	write_message("cosh(X): ", Cosh),
	write_message("tanh(X): ", Tanh),
	write_message("atan2(sin(X),cos(X)): ", Atan2).

:- pred test_inv_trig(float :: in, io__state :: di, io__state :: uo) is det.
test_inv_trig(X) -->
	{
	    math__asin(X, Asin),
	    math__acos(X, Acos),
	    math__atan(X, Atan)
	},
	write_message("X: ", X),
	write_message("asin(X): ", Asin),
	write_message("acos(X): ", Acos),
	write_message("atan(X): ", Atan).


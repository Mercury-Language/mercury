% A very basic check of arithmetic on big integers.

:- module integer_test.
:- interface.
:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.
:- import_module integer, string, list, int, require.

main -->
	{ X = integer.det_from_string("1234567890987654321") },
	{ Y = integer.det_from_string(
		"98765432101234567890123400000009999111") },
	{ Z = integer(200) },
	test(X, Y, Z).

:- pred test(integer, integer, integer, io__state, io__state).
:- mode test(in, in, in, di, uo) is det.

test(X, Y, Z) -->
	{
		Plus = X + Y,
		Times = X * Y,
		Minus = X - Y,
		Div = Y // X,
		Rem = Y rem X,
		Pow = integer.pow(X, Z),
		fac(Z,Fac)
	},
	write_message("X: ", X),
	write_message("Y: ", Y),
	write_message("Z: ", Z),
	write_message("X + Y: ", Plus),
	write_message("X * Y: ", Times),
	write_message("X - Y: ", Minus),
	write_message("Y // X: ", Div),
	write_message("Y rem X: ", Rem),
	write_message("fac(Z): ", Fac),
	write_message("pow(X,Z): ", Pow),
	{ X0 = integer(100000), X1 = integer(3) },
	write_integer(X0), io.write_string(" div mod "),
		write_integer(X1), io.write_string(" = "),
		write_integer(X0 div X1), io.write_string(" "),
		write_integer(X0 mod X1), io.nl,
	write_integer(-X0), io.write_string(" div mod "),
		write_integer(X1), io.write_string(" = "),
		write_integer(X0 div -X1), io.write_string(" "),
		write_integer(X0 mod -X1), io.nl,
	write_integer(X0), io.write_string(" div mod "),
		write_integer(-X1), io.write_string(" = "),
		write_integer(-X0 div X1), io.write_string(" "),
		write_integer(-X0 mod X1), io.nl,
	write_integer(-X0), io.write_string(" div mod "),
		write_integer(-X1), io.write_string(" = "),
		write_integer(-X0 div -X1), io.write_string(" "),
		write_integer(-X0 mod -X1), io.nl,
	write_integer(X0), io.write_string(" // rem "),
		write_integer(X1), io.write_string(" = "),
		write_integer(X0 // X1), io.write_string(" "),
		write_integer(X0 rem X1), io.nl,
	write_integer(-X0), io.write_string(" // rem "),
		write_integer(X1), io.write_string(" = "),
		write_integer(X0 // -X1), io.write_string(" "),
		write_integer(X0 rem -X1), io.nl,
	write_integer(X0), io.write_string(" // rem "),
		write_integer(-X1), io.write_string(" = "),
		write_integer(-X0 // X1), io.write_string(" "),
		write_integer(-X0 rem X1), io.nl,
	write_integer(-X0), io.write_string(" // rem "),
		write_integer(-X1), io.write_string(" = "),
		write_integer(-X0 // -X1), io.write_string(" "),
		write_integer(-X0 rem -X1), io.nl,
	{ int.min_int(Minint) },
		( { integer(Minint) < integer(0) } ->
			io.write_string("integer(min_int) ok\n")
		;
			io.write_string("integer(min_int) failed\n")
		).

:- pred write_message(string, integer, io__state, io__state).
:- mode write_message(in, in, di, uo) is det.

write_message(String, Int) -->
	io__write_string(String),
	{ Str = integer.to_string(Int) }, 
	io__write_string(Str),
	io__nl.

:- pred fac(integer, integer).
:- mode fac(in, out) is det.

fac(X,F) :-
	( X =< integer.zero ->
		F = integer.one
	;
		fac(X - integer.one,F1),
		F = F1 * X
	).

:- pred write_integer(integer, io.state, io.state).
:- mode write_integer(in, di, uo) is det.
write_integer(X) -->
	{ S = integer.to_string(X) },
	io.write_string(S).



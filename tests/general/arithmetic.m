% A very basic check of integer arithmetic.

% Note: this test makes use of Mercury-specific features (specifically
% the use of "`xor`" rather than "^" for the exclusive or operator,
% and the use of the reverse modes of xor) so it really belongs in
% the `tests/hard_coded' directory, rather than the `tests/general'
% directory... but that distinction is pretty much obsolete now that we
% don't support compiling things with Prolog.

:- module arithmetic.
:- interface.
:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.
:- import_module int, string, list.

main -->
	test(3, 4).

:- pred test(int, int, io__state, io__state).
:- mode test(in, in, di, uo) is det.

test(X, Y) -->
	{
		Plus = X + Y,
		Times = X * Y,
		Minus = X - Y,
		Div = X // Y,
		Mod = X mod Y,
		LeftShift = X << Y,
		RightShift = X >> Y,
		BitAnd = X /\ Y,
		BitOr = X \/ Y,
		BitXor = X `xor` Y,
		X = BitXor2 `xor` Y,
		Y = X `xor` BitXor3,
		BitNeg = \ X
	},
	write_message("X: ", X),
	write_message("Y: ", Y),
	write_message("X + Y: ", Plus),
	write_message("X * Y: ", Times),
	write_message("X - Y: ", Minus),
	write_message("X / Y: ", Div),
	write_message("X mod Y: ", Mod),
	write_message("X << Y: ", LeftShift),
	write_message("X >> Y: ", RightShift),
	write_message("X /\\ Y: ", BitAnd),
	write_message("X \\/ Y: ", BitOr),
	write_message("X `xor` Y: ", BitXor),
	write_message("Z such that X = Z `xor` Y: ", BitXor2),
	write_message("Z such that Y = X `xor` Z: ", BitXor3),
	write_message("\\ X: ", BitNeg).

:- pred write_message(string, int, io__state, io__state).
:- mode write_message(in, in, di, uo) is det.

write_message(String, Int) -->
	io__write_string(String), io__write_int(Int), io__write_string("\n").

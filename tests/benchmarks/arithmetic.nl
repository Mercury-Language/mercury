:- module arithmetic.
:- import_module int, io.

:- pred main_predicate(list(string), io__state, io__state).
:- mode main_predicate(in, di, uo) is det.

main_predicate(_) -->
	test(3, 4).

:- pred test(int, int, io__state, io__state).
:- mode test(in, in, di, uo) is det.

test(X, Y) -->
	{
		Plus is X + Y,
		Times is X * Y,
		Minus is X - Y,
		Div is X // Y,
		Mod is X mod Y,
		LeftShift is X << Y,
		RightShift is X >> Y,
		BitAnd is X /\ Y,
		BitOr is X \/ Y,
		BitXor is X ^ Y,
		BitNeg is \ X
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
	write_message("X ^ Y: ", BitXor),
	write_message("\\ X: ", BitNeg).

:- pred write_message(string, int, io__state, io__state).
:- mode write_message(in, in, di, uo) is det.

write_message(String, Int) -->
	io__write_string(String), io__write_int(Int), io__write_string("\n").


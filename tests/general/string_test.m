:- module string_test.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.
:- import_module string, list, require.

main -->
	test("foo", "bar").

:- pred test(string, string, io__state, io__state).
:- mode test(in, in, di, uo) is det.

test(X, Y) -->
	write_message("X: ", X),
	write_message("Y: ", Y),
	{ string__append(X, Y, Z) },
	write_message("X append Y: ", Z),
	{ string__capitalize_first(X, CapX) },
	write_message("capitalize_first X: ", CapX),
	{ string__uncapitalize_first(CapX, UnCapX) },
	write_message("uncapitalize_first CapX: ", UnCapX),
	{ string__int_to_string(1234, Num) },
	write_message("int_to_string 1234: ", Num),
	{ string__int_to_base_string(1234, 8, Num8) },
	write_message("octal 1234: ", Num8),
	{ string__int_to_base_string(1234, 16, Num16) },
	write_message("hexadecimal 1234: ", Num16),
	{ NumG1 = string__int_to_base_string_group(1234, 10, 3, ",") },
	write_message("Grouped 1234: ", NumG1),
	{ NumG2 = string__int_to_base_string_group(113, 2, 1, "--") },
	write_message("Grouped 113: ", NumG2),
	{ NumG3 = string__int_to_string_thousands(1300000) },
	write_message("Grouped 1300000: ", NumG3),
	{ NumG4 = string__int_to_base_string_group(45999, 10, 0, ",") },
	write_message("Non Grouped 45999: ", NumG4),
	{ string__duplicate_char('f', 5, FiveFs) },
	( { string__to_int("5678", Num5678) } ->
		io__write_string("string_to_int 5678: "),
		io__write_int(Num5678),
		io__write_string("\n")
	;
		{ error("string__to_int(""5678"", _) failed") }
	),
	{ string__to_int("asdf", _) ->
		error("string__to_int(""asdf"", _) succeeded")
	;
		true
	},
	write_message("Five f's: ", FiveFs),
	{ string__pad_right(FiveFs, '.', 10, FsAndDots) },
	write_message("Five f's and five dots: ", FsAndDots),
	{ string__pad_left(FsAndDots, '-', 15, DashesFsAndDots) },
	write_message("Five dashes, five f's and five dots: ", 
		DashesFsAndDots),
	{ Table = string__format_table([left(["aaa", "b", "cc"]), 
		right(["1111111", "", "333"]), right(["1,300,000.00", 
		"9,999.00", "123,456,789.99"])], "|") ++ "\n" },
	write_string(Table),
	[].

:- pred write_message(string, string, io__state, io__state).
:- mode write_message(in, in, di, uo) is det.

write_message(Message, String) -->
	io__write_string(Message),
	io__write_string(String),
	io__write_string("\n").


% This is a test to check the correctness of the string__join_list predicate.

:- module join_list.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module list, string.

main -->
	test_join_list([]),
	test_join_list(["a", "b"]),
	test_join_list(["a", "b", "c"]),
	test_join_list(["a", "", "c"]),
	test_join_list(["abc", "def", "ghi"]),
	test_join_list(["the", "quick", "brown", "fox", "jumped", "over",
		"the", "lazy", "dog"]),
	test_join_list(["this", "is", "a", "test", "to", "check",
		"the correctness", " of the", "join_list ", "predicate\n"]),
	test_join_list([" ", "\t", " \t ", "x"]).

:- pred test_join_list(list(string)::in, io__state::di, io__state::uo) is det.

test_join_list(Pieces) -->
	{ Joined1 = string__join_list(", ", Pieces) },
	io__write_string(Joined1),
	io__write_string("\n"),
	{ Joined2 = string__join_list(" ", Pieces) },
	io__write_string(Joined2),
	io__write_string("\n"),
	{ Joined3 = string__join_list(" x ", Pieces) },
	io__write_string(Joined3),
	io__write_string("\n"),
	{ Joined4 = string__join_list("", Pieces) },
	io__write_string(Joined4),
	io__write_string("\n").

% Test output of special characters such as \r

:- module special_char.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	print("Hello world\r\n"),
	print("\r\n"),
	print("\a\b\v\f\t\n"),
	print("\077"),
	print("\0123"),
	print("\0321"),
	print("\n").

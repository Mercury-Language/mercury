:- module cmd_quote.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.

main -->
	io__write_string("Hello world!\n").


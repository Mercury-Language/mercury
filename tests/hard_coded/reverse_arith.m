:- module reverse_arith.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is cc_multi.
:- implementation.
:- import_module int.
main -->
	( { X = 5, X = 2 * 2 } ->
		io__write_string("yes\n")
	;
		io__write_string("no\n")
	).

:- module trans_intermod_user_equality.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module trans_intermod_user_equality2.

main -->
	{ make_bar(0, 1, Bar) },
	{ use_bar(Bar, N) },
	io__write_int(N),
	io__nl.

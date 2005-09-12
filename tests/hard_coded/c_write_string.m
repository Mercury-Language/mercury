:- module c_write_string.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- pred c_write_string(string, io__state, io__state).
:- mode c_write_string(in, di, uo) is det.

:- pragma foreign_decl("C", "#include <stdio.h>").

:- pragma foreign_proc("C",
	c_write_string(Str::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	fputs(Str, stdout);
	IO = IO0;
").

c_write_string(Str) -->
	io__write_string(Str).

main -->
	c_write_string("Hello, world\n"),
	c_write_string("I am 8 today!\n"),
	c_write_string(X),
	{ X = "fred\n" }.

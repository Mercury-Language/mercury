:- module c_write_string.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- pred c_write_string(string, io__state, io__state).
:- mode c_write_string(in, di, uo) is det.

:- pragma(c_header_code, "#include <stdio.h>").

:- pragma(c_code, c_write_string(Str::in, IO0::di, IO::uo),
	"fputs(Str, stdout); IO = IO0;").
c_write_string(Str) -->
	io__write_string(Str).

main -->
	c_write_string("Hello, world\n"),
	c_write_string("I am 8 today!\n"),
	c_write_string(X),
	{ X = "fred\n" }.

% A test of the pragma(inline, ...) declarations.

:- module pragma_inline.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	c_write_string("Hello, world\n").

:- pred c_write_string(string::in, io__state::di, io__state::uo) is det.

:- pragma(c_code, c_write_string(String::in, IO0::di, IO::uo), "
        printf(""%s"", (char *)String);
        IO = IO0;
").

:- pragma(inline, c_write_string/3).


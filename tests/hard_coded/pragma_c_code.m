% A simple test of the C interface - call printf() to write "Hello, world".

:- module hello.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main --> c_write_string("Hello, world\n").

:- pred c_write_string(string::in, io__state::di, io__state::uo) is det.

:- pragma(c_code, c_write_string(String::in, IO0::di, IO::uo), "
	printf(""%s"", String);
	IO = IO0;
").


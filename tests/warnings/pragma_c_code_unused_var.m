% Test for warning when a variable is not used in pragma(c_code, ...).

:- module pragma_c_code_unused_var.

:- interface.

:- import_module io.

:- pred c_hello_world(string::in, io__state::di, io__state::uo) is det.

:- implementation.

:- pragma(c_code, c_hello_world(Msg::in, IO0::di, IO::uo), "
	printf(""Hello, world"");
	IO = IO0;
").

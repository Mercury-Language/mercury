:- module target_mlobjs.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main --> 
	c_write_string("Hello, world\n").

:- pragma c_header_code("#include ""target_mlobjs_c.h""").

:- pred c_write_string(string::in, io__state::di, io__state::uo) is det.
:- pragma c_code(c_write_string(Message::in, IO0::di, IO::uo), "
	c_write_string(Message);
	IO = IO0;
").


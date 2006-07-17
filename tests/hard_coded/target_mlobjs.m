:- module target_mlobjs.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main --> 
	c_write_string("Hello, world\n").

:- pragma foreign_decl("C", "#include ""target_mlobjs_c.h""").

:- pred c_write_string(string::in, io__state::di, io__state::uo) is det.
:- pragma foreign_proc("C",
	c_write_string(Message::in, IO0::di, IO::uo),
	[promise_pure, will_not_call_mercury],
"
	c_write_string(Message);
	IO = IO0;
").
c_write_string(Str) -->
	io__write_string(Str).

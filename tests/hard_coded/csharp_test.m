% A test of the C# interface.

:- module csharp_test.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	csharp_write_string("Hello, world\n").

:- pragma(foreign_decl, "C#", "
	// some C Sharp declarations
").

:- pragma(foreign_code, "C#", "
	// some C Sharp code
").

:- pred csharp_write_string(string::in, io__state::di, io__state::uo) is det.

:- pragma(foreign_proc, "C#",
	csharp_write_string(Message::in, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure],
"
	// a C sharp procedure
	Console.WriteLine(Message);
").

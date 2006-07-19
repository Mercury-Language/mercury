% XXX Need to add IL and Java implementations.
:- module pragma_foreign_export.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
	call_foreign(!IO).
	
:- pred call_foreign(io::di, io::uo) is det.

:- pred hello_world(io::di, io::uo) is det.
:- pragma foreign_export("C", hello_world(di, uo),
	"exported_hello_world").

hello_world(!IO) :-
	io.write_string("Hello World!\n", !IO).

:- pragma foreign_proc("C",
	call_foreign(IO0::di, IO::uo),
	[promise_pure, may_call_mercury],
"
	exported_hello_world();
	IO = IO0;
").

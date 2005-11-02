:- module io_tab_impure.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
	print_hello(!IO),
	nl(!IO).

:- pred print_hello(io::di, io::uo) is det.

:- pragma promise_pure(print_hello/2).

print_hello(!IO) :-
	impure impure_print_hello.

:- impure pred impure_print_hello is det.

impure_print_hello :-
	impure impure_print("hello").

:- impure pred impure_print(string::in) is det.

impure_print(S) :-
	impure make_io_state(IO0),
	io_tab_impure.print(S, IO0, IO),
	impure consume_io_state(IO).

:- pred print(string::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
	print(S::in, IO0::di, IO::uo),
	[will_not_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
	printf(S);
	IO = IO0;
").

:- impure pred make_io_state(io::uo) is det.

:- pragma foreign_proc("C", 
	make_io_state(_IO::uo),
	[will_not_call_mercury, thread_safe],
"").

:- impure pred consume_io_state(io::di) is det.

:- pragma foreign_proc("C",
	consume_io_state(_IO::di),
	[will_not_call_mercury, thread_safe], 
"").

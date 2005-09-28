:- module foreign_name_mutable.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- mutable(foo, int, 42, ground, [untrailed, foreign_name("C", "FOO")]).

main(!IO) :-
	increment_global(!IO),
	promise_pure (
		semipure get_foo(X)
	),
	io.write_string("X = ", !IO),
	io.write_int(X, !IO),
	io.nl(!IO).

:- pred increment_global(io::di, io::uo) is det.

:- pragma foreign_proc("C",
	increment_global(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	FOO++;
	IO = IO0;
").

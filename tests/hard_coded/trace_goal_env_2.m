:- module trace_goal_env_2.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module string.

:- mutable(x, int, 0, ground, [untrailed, thread_safe]).

main(!IO) :-
	promise_pure (
		p(42, A, !IO),
		io.write_string("A: " ++ A ++ "\n", !IO),
		p(47, B, !IO),
		io.write_string("B: " ++ B ++ "\n", !IO),

		semipure get_x(FinalX),
		io.write_int(FinalX, !IO),
		io.nl(!IO)
	).

:- pred p(int::in, string::out, io::di, io::uo) is det.

p(N, S, !IO) :-
	trace [compiletime(flag("abc")), runtime(env("TRACE_ABC")),
		io(!IO), state(x, !X)]
	(
		io.write_string("<abcABC>", !IO),
		io.write_int(!.X, !IO),
		!:X = !.X + 1,
		io.write_string(" ", !IO),
		io.write_int(N, !IO),
		io.write_string("<abcABC>\n", !IO)
	),
	S = int_to_string(N+1) ++ "xx" ++ int_to_string(N+2).

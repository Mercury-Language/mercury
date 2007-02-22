:- module trace_goal_env_3.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module string.

main(!IO) :-
	promise_pure (
		p(42, A),
		io.write_string("A: " ++ A ++ "\n", !IO)
	).

:- pred p(int::in, string::out) is det.

p(N, S) :-
	trace [runtime(env("TRACE_ABC")
        and not(
            env("TRACE_NEITHER_AB") or env("TRACE_NOR_BC")
        )),
		io(!IO)]
	(
		io.write_string("Seen ABC and not NOT_ABC.\n", !IO)
	),
	S = int_to_string(N+1) ++ "xx" ++ int_to_string(N+2).

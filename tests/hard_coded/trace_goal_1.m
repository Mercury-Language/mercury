:- module trace_goal_1.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module string.

main(!IO) :-
	p(42, X),
	io.write_string(X, !IO),
	io.nl(!IO).

:- pred p(int::in, string::out) is det.

p(N, S) :-
	trace [compiletime(flag("abc") or flag("xyz")), io(!S)] (
		io.write_string("<abc>", !S),
		io.write_int(N, !S),
		io.write_string("<abc>\n", !S)
	),
	S = int_to_string(N+1) ++ "xx" ++ int_to_string(N+2).

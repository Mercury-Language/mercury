:- module unusual_name_mutable.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

:- mutable('123$%^abc 7', int, 42, ground, [untrailed]).

main(!IO) :-
	promise_pure ( semipure 'get_123$%^abc 7'(X)),
	io.write_string("'123$%^abc 7' = ", !IO),
	io.write_int(X, !IO),
	io.nl(!IO).

:- module builtin_call_rep.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
	A = 1,
	B = 2,
	X = A+B,
	write_int(X, !IO),
	nl(!IO).

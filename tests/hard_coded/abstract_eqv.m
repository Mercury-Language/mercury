:- module abstract_eqv.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module abstract_eqv_1.

main(!IO) :-
	test(val1, val2, !IO),
	test(val1, val3, !IO),
	test(val2, val3, !IO).

:- pred test(t_abs::in, t_abs::in, io::di, io::uo) is det.

test(A, B, !IO) :-
	io__write(A, !IO),
	io__write_string(" = ", !IO),
	io__write(B, !IO),
	io__write_string(": ", !IO),
	( A = B ->
		io__write_string("true\n", !IO)
	;
		io__write_string("false\n", !IO)
	).

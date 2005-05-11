:- module poly_io_retry.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array, list.

main(!IO) :-
	polycall(io.write_string("hello"), !IO),
	A = array([0]),
	polycall(array_update, A, B),
	io.write(B, !IO),
	nl(!IO).

:- pred polycall(pred(T, T), T, T).
:- mode polycall(in(pred(di, uo) is det), di, uo) is det.
:- mode polycall(in(pred(array_di, array_uo) is det), array_di, array_uo) 
	is det.

polycall(P, !S) :- P(!S).

:- pred array_update(array(int)::array_di, array(int)::array_uo) is det.

array_update(!A) :- !:A = !.A ^ elem(0) := 1.

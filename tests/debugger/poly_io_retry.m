%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module poly_io_retry.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array.
:- import_module list.

main(!IO) :-
    polycall(poly_io_retry.write_int(1), !IO),
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

:- pred poly_io_retry.write_int(int::in, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    poly_io_retry.write_int(N::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    printf(""%d\\n"", (int) N);
    IO = IO0;
}").

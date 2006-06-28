% unify

:- module dep_par_1.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
main(!IO) :-
    ( X = 1
    & Y = X
    ),
    io.write_int(Y, !IO),
    io.nl(!IO).

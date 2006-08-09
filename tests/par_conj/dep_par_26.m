:- module dep_par_26.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    ( A = one
    & B = A
    & C = B
    ),
    io.print({A,B,C}, !IO),
    io.nl(!IO).

:- func one = int.
:- pragma no_inline(one/0).
one = 1.

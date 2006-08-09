:- module dep_par_28.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    (
        A = one
    &
        B = A,
        C = A
    ),
    io.print({A,B,C}, !IO),
    io.nl(!IO).

:- func one = int.
:- pragma no_inline(one/0).

one = 1.

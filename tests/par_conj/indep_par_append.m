% append

:- module indep_par_append.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.
main(!IO) :-
    As = [1, 2, 3],
    Bs = [4, 5, 6],
    ( append(As, Bs, Cs)
    & append(Bs, As, Ds)
    ),
    append(Cs, Ds, Es),
    io.print(Es, !IO),
    io.nl(!IO).

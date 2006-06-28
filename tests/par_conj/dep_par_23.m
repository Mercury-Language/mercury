% parallel conjunction inside higher order term

:- module dep_par_23.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module list.

main(!IO) :-
    L0 = [1, 2, 3],
    L = list.map((func(X) = Y :- Z=X+1 & Y=Z+1), L0),
    io.print(L, !IO),
    io.nl(!IO).

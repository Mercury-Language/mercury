% scope

:- module dep_par_13.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.

main(!IO) :-
    (
	some [!X] (
	    !:X = [],
	    cons(1, !X),
	    cons(2, !X),
	    X = !.X
	)
    &
	some [!Y] (
	    !:Y = X,
	    cons(10, !Y),
	    cons(20, !Y),
	    Y = !.Y
	)
    ),
    io.print(Y, !IO),
    io.nl(!IO).

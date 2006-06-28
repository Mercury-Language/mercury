% function call

:- module dep_par_4.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    V = 1,
    (
	X = f(V)
    &
	Y = f(X)
    ),
    io.print(Y, !IO),
    io.nl(!IO).

:- func f(int) = int.
f(X) = X+1.

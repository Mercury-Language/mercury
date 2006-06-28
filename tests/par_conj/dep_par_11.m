% conjunction inside parallel conjunction

:- module dep_par_11.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    (
	X = A + B + C,
	A = B + C,
	B = 1,
	C = 2
    &
	U = X + A
    ),
    io.print({X,U,C}, !IO),
    io.nl(!IO).

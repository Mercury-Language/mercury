% call

:- module dep_par_2.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    (
	p(1, X)
    &
	p(X, Y)
    ),
    io.write_int(Y, !IO),
    io.nl(!IO).

:- pred p(int::in, int::out) is det.
p(X, X+1).

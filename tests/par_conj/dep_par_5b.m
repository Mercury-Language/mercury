% IO

:- module dep_par_5b.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    (
	io.write_int(1, !IO)
    &
	io.write_int(2, !IO)
    ),
    io.nl(!IO).

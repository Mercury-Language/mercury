% IO, uniqueness

:- module dep_par_5c.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(IO0, IO) :-
    (
	io.write_int(1, IO0, IO1),
	io.nl(IO1, IO)
    &
	true
    ).

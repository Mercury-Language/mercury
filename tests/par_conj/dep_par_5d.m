% IO, uniqueness

:- module dep_par_5d.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(IO0, IO) :-
    (
	io.write_int(1, IO0, IO1)
    ,
	io.write_int(2, IO1, IO2),
	io.nl(IO2, IO)
    ).

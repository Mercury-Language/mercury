% outer disjunction

:- module dep_par_7.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

main(IO0, IO) :-
    ( 
	( X = 1
	& Y = X
	)
    ;
	Y = 2
    ),
    io.write_int(Y, IO0, IO1),
    io.nl(IO1, IO).

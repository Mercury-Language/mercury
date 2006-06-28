% cc_multi

:- module dep_par_8.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module int.
:- import_module list.

main(IO0, IO) :-
    ( 
	( member(X, [1,2,3])
	; X = 0
	)
    &
	Y = X
    ),
    io.write_int(Y, IO0, IO1),
    io.nl(IO1, IO).

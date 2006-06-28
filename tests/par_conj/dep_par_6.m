% inner disjunction

:- module dep_par_6.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

main(!IO) :-
    (
	( X = 1
	; X = 2
	)
    &
	Y = X
    ),
    io.print(Y, !IO),
    io.nl(!IO).

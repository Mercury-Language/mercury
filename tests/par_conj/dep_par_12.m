% switch

:- module dep_par_12.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

:- type t
    --->    t1
    ;	    t2.

main(!IO) :-
    T = f,
    (
	T = t1,
	( X = 1
	& Y = 2
	)
    ;
	T = t2,
	( Y = 1
	& X = 2
	)
    ),
    io.write_int(Y, !IO),
    io.write_int(X, !IO),
    io.nl(!IO).

:- func f = t.
:- pragma no_inline(f/0).
f = t2.

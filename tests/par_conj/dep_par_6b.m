% disjunction  (wait after a resume point)

:- module dep_par_6b.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

main(!IO) :-
    (
	X = one
    &
	(
	    Y = 0,
	    Y = one
	;
	    % wait(X)
	    X = 2,
	    Y = 2
	;
	    % wait(X)
	    X = 1,
	    Y = 3
	;
	    Y = 0
	)
    ),
    io.write_int(Y, !IO),
    io.nl(!IO).

:- func one = int.
:- pragma no_inline(one/0).
one = 1.

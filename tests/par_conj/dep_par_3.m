% function call

:- module dep_par_3.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    (
	X = 1
    &
	Y = f(X)
    ),
    io.write_int(X*Y, !IO),
    io.nl(!IO).

:- func f(int) = int.
:- pragma no_inline(f/1).
f(X) = (if X = 1 then 2 else 0).

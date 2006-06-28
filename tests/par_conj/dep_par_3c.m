% if-then-else expression

:- module dep_par_3c.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    (
	X = 1
    &
	Y = (if f(X) then 2 else 0)
    ),
    io.write_int(X+Y, !IO),
    io.nl(!IO).

:- pred f(int::in) is semidet.
:- pragma no_inline(f/1).
f(1).

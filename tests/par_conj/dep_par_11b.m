% conjunction with nested parallel conjunction

:- module dep_par_11b.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(IO0, IO) :-
    p(U),
    io.write_int(U, IO0, IO1),
    io.nl(IO1, IO).

:- pred p(int::out) is det.
p(U) :-
    (
	X = A + B + C,
	(
	    A = B + C 
	&
	    B = 1
	),
	C = 2
    &
	U = X + A
    ).

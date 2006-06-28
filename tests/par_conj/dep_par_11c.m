% conjunction with nested parallel conjunction (2)

:- module dep_par_11c.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(IO0, IO) :-
    p(XUC),
    io.print(XUC, IO0, IO1),
    io.nl(IO1, IO).

:- pred p({int, int, int}::out) is det.
p({X,U,C}) :-
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

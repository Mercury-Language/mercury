:- module mismatch_on_call.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(IO0, IO) :-
	p(1,2,X,Y),
	write_int(X, IO0, IO1),
	write_int(Y, IO1, IO2),
	nl(IO2, IO).

:- pred p(int::in, int::in, int::out, int::out) is det.

p(A,B,X,Y) :-
	q(A, G),
	E = A+2,
	q(B, C),
	r(E, C, F, Y),
	X = G+F.

:- pred r(int::in, int::in, int::out, int::out) is det.

r(A, B, C, D) :-
	q(A, C),
	q(B, D).

:- pred q(int::in, int::out) is det.

q(X, X+100).


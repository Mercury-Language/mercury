:- module closure_dependency.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

main(!IO) :-
	a(0, _, P),
	a(1, X, _),
	a(2, Y, _),
	a(3, Z, _),
	write(P, !IO),
	write(X, !IO),
	write(Y, !IO),
	write(Z, !IO),
	nl(!IO).

:- type t(T) --->
	t(pred(T, list(T))).

:- inst t ---> t(pred(in, out) is det).

:- pred a(int::in, list(int)::out, t(int)::out(t)) is det.

a(X, Y, T) :-
	b(X, Z),
	e(X, W),
	d(Z, T),
	c(T, W, Y).

:- pred b(int::in, list(int)::out) is det.

b(X, [X]).

:- pred e(int::in, int::out) is det.

e(_, 100).

:- pred c(t(int), int, list(int)).
:- mode c(in(t), in, out) is det.

c(t(P), X, Y) :-
	P(X, Y).
	
:- pred p(list(int)::in, int::in, list(int)::out) is det.

p(X, Y, [Y | X]).

:- pred d(list(int)::in, t(int)::out(t)) is det.

d(X, t(p(X))).

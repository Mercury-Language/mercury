:- module pl2_3_1.

:- interface.

:- type node	--->	a ; b ; c.

:- pred p(node::in, node::out) is multi.

:- implementation.

p(X, Z) :-
	q(X, Y),
	p(Y, Z).
p(X, X).

:- pred q(node, node).
:- mode q(in, out).

q(a, b).

:- module pl7_2_9.

:- interface.

:- pred mult(nat::in, nat::in, nat::out) is det.

:- type nat	--->	zero ; s(nat).

:- implementation.

mult(zero, _Y, zero).
mult(s(X), Y, Z) :-
	mult(X, Y, Z1),
	add(Z1, Y, Z).

:- pred add(nat, nat, nat).
:- mode add(in, in, out).

add(zero, Y, Y).
add(s(X), Y, s(Z)) :-
	add(X, Y, Z).

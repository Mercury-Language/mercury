:- module sum.

:- interface.

:- pred sum(nat, nat, nat).
% :- mode sum(out, in, out) is nondet. SECOND CLAUSE NOT WELL MODED
:- mode sum(out, out, in) is multi.

:- type nat	--->	zero ; s(nat).

:- implementation.

sum(X, s(Y), s(Z)) :-
	sum(X, Y, Z).
sum(X, zero, X).

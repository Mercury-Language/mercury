:- module bind_in_negated.
:- interface.

:- type foo ---> f(int, int).

:- pred p(foo, foo, foo).
:- mode p(bound(f(free, ground)) >> ground,
	  bound(f(ground, free)) >> ground,
	  out) is det.

:- implementation.

p(A, B, C) :-
	( A = B ->
		C = A
	;
		C = f(1, 1),
		A = f(1, _),
		B = f(_, 1)
	).

:- module alias_det_unification.
:- interface.

:- pred test(f(T) :: in) is det.

:- implementation.

:- type f(T) ---> f(T).

test(X) :-
	X = f(Y),
	X = f(Z),
	Y = Z.


:- module followcode_det_problem.

:- pred p(int::out) is semidet.

p(X) :-
	Z = 4,
	(
		Z = 3
	;
		Z = 4
	),
	q(X).

:- pred q(int::out) is det.

q(42).

:- module implied_mode.

:- pred p(int::out, int::out) is det.

p(5, 6).
	

:- pred q(int::out) is semidet.

q(X) :-
	Y = 2,
	p(X, Y).


:- module semidet_disj.

:- pred p(int::in) is semidet.
:- pred q(int::in) is semidet.
:- external(q/1).
:- pred r(int::in) is semidet.
:- external(r/1).

p(X) :-
	q(X) ; r(X).


:- module mode_merge_insts.

:- type triple ---> triple(int, int, int).

:- inst triple1	= bound(triple(ground, free, free)).
:- inst triple2	= bound(triple(free, ground, free)).
:- inst triple12 = bound(triple(ground, ground, free)).

:- pred p1(triple :: free -> triple1) is det.
:- pred p2(triple :: free -> triple2) is det.
:- pred q(triple :: triple12 -> triple12) is det.

:- external(p1/1).
:- external(p2/1).
:- external(q/1).

:- pred p is det.

p :-
	p1(X),
	p2(X),
	q(X).

:- pred q is det.

q :-
	p1(X),
	p2(Y),
	X = Y,
	q(X).


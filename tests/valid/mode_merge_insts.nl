:- module mode_merge_insts.

:- type triple ---> triple(int, int, int).

:- type list(T) ---> [] ; [T | list(T)].

:- inst triple1	= bound(triple(ground, free, free)).
:- inst triple2	= bound(triple(free, ground, free)).
:- inst triple12 = bound(triple(ground, ground, free)).

:- pred p1(list(triple) :: free -> triple1) is det.
:- pred p2(list(triple) :: free -> triple2) is det.
:- pred q(list(triple) :: triple12 -> triple12) is det.

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


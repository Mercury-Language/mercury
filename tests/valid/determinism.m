:- module determinism.

% This module has a couple of test cases for determinism analysis
% of complicated unifications.

:- interface.
:- import_module list, std_util.

:- inst fg = bound(free - ground).
:- inst gf = bound(ground - free).
:- inst list(Inst) = bound([] ; [Inst | list(Inst)]).

:- pred q(list(pair(int))).
:- mode q(free -> list(fg)) is det.

:- pred r(list(pair(int))).
:- mode r(free -> list(gf)) is det.

:- pred p is det.
:- pred p2 is semidet.
:- pred p3 is semidet.

:- implementation.

p :-
	q(X),
	r(Y),
	X = Y.

p2 :-
	q(X),
	q(Y),
	X = Y.

p3 :-
	r(X),
	r(Y),
	X = Y.

:- external(q/1).
:- external(r/1).

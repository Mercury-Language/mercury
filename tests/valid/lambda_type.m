:- module lambda_type.

:- interface.

:- pred p is det.

:- implementation.

p :-
	_X = lambda([W::out] is det, W = 1),
	_Y = lambda([W::out] is det, W = a).

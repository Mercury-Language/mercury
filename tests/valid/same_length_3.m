:- module same_length_3.
:- interface.

:- pred p(int::out) is det.

:- implementation.

:- pred loop is erroneous.
loop :- loop.

p(X) :-
	loop,
	X = 1
	;
	X = 2.

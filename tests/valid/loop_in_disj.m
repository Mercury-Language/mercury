/*###1 [cc] Warning: incorrect module name in `:- module' declaration.%%%*/
:- module bug.
:- interface.

:- pred p(int::out) is det.

:- implementation.

:- pred loop is erroneous.
loop :- loop.

p(X) :-
	loop
	;
	X = 42.

:- module conflicting_tabling_pragmas.

:- interface.

:- import_module int.

:- func fac(int) = int.

:- implementation.

:- pragma memo(fac/1).
:- pragma loop_check(fac/1).

fac(X) = Y :-
	( X =< 0 ->
		Y = 0
	;	
		Y = X * fac(X - 1)
	).

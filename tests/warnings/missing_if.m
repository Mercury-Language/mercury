:- module missing_if.  

:- interface.  

:- pred bar(int, int).
:- mode bar(in, out) is det.

:- implementation.

:- import_module require.

bar(X, Y) :-
	( 
		X = 4 
	->
		Y = 3
	;
		Y = 9
	;
		error("This is fun.")
	).

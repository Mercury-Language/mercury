:- module error.
:- import_module require.

:- pred t(int::out) is det.
t(_X) :-
	error("").


:- module singleton_test.
:- import_module list, int.

:- interface.

:- pred append(list(int), list(int), list(int)).
:- mode append(in, in, out) is det.

:- implementation.

append([], L, L) :-
	L1 = L2.
append([H | T], L, [H | NT]) :-
	append(T, L, NT).

:- module singleton_test.
:- import_module list, int.

:- interface.

:- pred my_append(list(int), list(int), list(int)).
:- mode my_append(in, in, out) is det.

:- implementation.

my_append([], L, L) :-
	L1 = L2.
my_append([H | T], L, [H | NT]) :-
	my_append(T, L, NT).

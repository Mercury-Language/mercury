:- module pragma_source_file.
:- import_module list, int.

:- interface.

:- pred append(list(int), list(int), list(int)).
:- mode append(in, in, out) is det.

:- implementation.

:- pragma source_file("foo.m").
#1
append([], L, L) :-
#2
	L1 = L2.
#10
append([H | T], L, [H | NT]) :-
	X = Y,
	append(T, L, NT).

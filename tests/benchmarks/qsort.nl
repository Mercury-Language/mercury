%   qsort
%
%   David H. D. Warren
%
%   quicksort a list of 50 integers

:- module qsort.

:- interface.

:- import_module list, int, printlist.

:- pred main1(list(int)).
:- mode main1(out) is det.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- pred main3(list(int), io__state, io__state).
:- mode main3(out, di, uo) is det.

:- implementation.

main --> main3(_).

main1(Out) :-
	data(Data),
	qsort(Data, Out, []).

main3(Out) -->
	{ main1(Out) },
	print_list(Out).

:- pred data(list(int)).
:- mode data(out) is det.

:- pred qsort(list(int), list(int), list(int)).
:- mode qsort(in, out, in) is det.

:- pred partition(list(int), int, list(int), list(int)).
:- mode partition(in, in, out, out) is det.

data([27,74,17,33,94,18,46,83,65,2,32,53,28,85,99,47,28,82,6,11,55,29,39,81,
90,37,10,0,66,51,7,21,85,27,31,63,75,4,95,99,11,28,61,74,18, 92,40,53,59,8]).

qsort([X|L], R, R0) :-
	partition(L, X, L1, L2),
	qsort(L2, R1, R0),
	qsort(L1, R, [X|R1]).
qsort([], R, R).

partition([], _P, [], []).
partition([H|T], P, Lo, Hi) :-
	( H =< P ->
		partition(T, P, Lo1, Hi),
		Lo = [H|Lo1]
	;
		partition(T, P, Lo, Hi1),
		Hi = [H|Hi1]
	).

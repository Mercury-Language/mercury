%   qsort
%
%   David H. D. Warren
%
%   quicksort a list of 50 integers

:- module qsort.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- pred main1(list(int)).
:- mode main1(out) is det.

:- implementation.

:- import_module list, int.

main --> main3(_).

main1(Out) :-
	data(Data),
	qsort(Data, Out, []).

:- pred main3(list(int), io__state, io__state).
:- mode main3(out, di, uo) is det.

main3(Out) -->
	{ main1(Out) },
	print_list(Out).

:- pred data(list(int)).
:- mode data(out) is det.

data([27,74,17,33,94,18,46,83,65,2,32,53,28,85,99,47,28,82,6,11,55,29,39,81,
90,37,10,0,66,51,7,21,85,27,31,63,75,4,95,99,11,28,61,74,18, 92,40,53,59,8]).

:- pred qsort(list(int), list(int), list(int)).
:- mode qsort(in, out, in) is det.

qsort([X|L], R, R0) :-
	partition(L, X, L1, L2),
	qsort(L2, R1, R0),
	qsort(L1, R, [X|R1]).
qsort([], R, R).

:- pred partition(list(int), int, list(int), list(int)).
:- mode partition(in, in, out, out) is det.

partition([], _P, [], []).
partition([H|T], P, Lo, Hi) :-
	( H =< P ->
		partition(T, P, Lo1, Hi),
		Lo = [H|Lo1]
	;
		partition(T, P, Lo, Hi1),
		Hi = [H|Hi1]
	).

:- pred print_list(list(int), io__state, io__state).
:- mode print_list(in, di, uo) is det.

print_list(Xs) -->
	(
		{ Xs = [] }
	->
		io__write_string("[]\n")
	;
		io__write_string("["),
		print_list_2(Xs),
		io__write_string("]\n")
	).

:- pred print_list_2(list(int), io__state, io__state).
:- mode print_list_2(in, di, uo) is det.

print_list_2([]) --> [].
print_list_2([X|Xs]) --> 
	io__write_int(X),
	(
		{ Xs = [] }
	->
		[]
	;
		io__write_string(", "),
		print_list_2(Xs)
	).

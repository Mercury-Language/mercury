%   nreverse
%
%   David H. D. Warren
%
%   "naive"-reverse a list of 30 integers

:- module nrev.

:- interface.

:- import_module list, io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- pred main1(list(int)).
:- mode main1(out) is det.

:- implementation.

main --> main3(_).

main1(Out) :-	
	data(Data),
	nreverse(Data, Out).

:- pred main3(list(int), io__state, io__state).
:- mode main3(out, di, uo) is det.

main3(Out) -->
	{ main1(Out) },
	print_list(Out).

:- pred data(list(int)).
:- mode data(out) is det.

data([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
	16,17,18,19,20,21,22,23, 24,25,26,27,28,29,30]).

:- pred nreverse(list(int), list(int)).
:- mode nreverse(in, out) is det.

nreverse([X|L0], L) :-
	nreverse(L0, L1), concatenate(L1, [X], L).
nreverse([], []).

:- pred concatenate(list(int), list(int), list(int)).
:- mode concatenate(in, in, out) is det.

concatenate([X|L1], L2, [X|L3]) :-	
	concatenate(L1, L2, L3).
concatenate([], L, L).

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

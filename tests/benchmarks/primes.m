:- module primes.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module int, list, prolog.

main --> main3(_).

:- pred main1(list(int)).
:- mode main1(out) is det.

main1(Out) :-	
	data(Data),
	primes(Data, Out).

:- pred main3(list(int), io__state, io__state).
:- mode main3(out, di, uo) is det.

main3(Out) -->
	{ main1(Out) },
	print_list(Out).

:- pred data(int).
:- mode data(out) is det.

data(98).

:- pred primes(int, list(int)).
:- mode primes(in, out) is det.

primes(Limit, Ps) :-
	integers(2, Limit, Is),
	sift(Is, Ps).

:- pred integers(int, int, list(int)).
:- mode integers(in, in, out) is det.

integers(Low, High, Result) :- 
	( Low =< High ->
		M is Low + 1,
		Result = [Low | Rest],
		integers(M, High, Rest)
	;
		Result = []
	).

:- pred sift(list(int), list(int)).
:- mode sift(in, out) is det.

sift([], []).
sift([I | Is], [I | Ps]) :-
	remove(I, Is, New),
	sift(New, Ps).

:- pred remove(int, list(int), list(int)).
:- mode remove(in, in, out) is det.

remove(_P, [], []).
remove(P, [I | Is], Result) :-
	M is I mod P,
	( M = 0 ->
		Result = Nis,
		remove(P, Is, Nis)
	;
		Result = [I | Nis],
		remove(P, Is, Nis)
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

:- module primes.

:- interface.

:- import_module list, int, io.

:- pred main1(list(int)).
:- mode main1(out) is det.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- pred main3(list(int), io__state, io__state).
:- mode main3(out, di, uo) is det.

:- implementation.

:- import_module printlist.

main --> main3(_).

main1(Out) :-	
	data(Data),
	primes(Data, Out).

main3(Out) -->
	{ main1(Out) },
	print_list(Out).

:- pred data(int).
:- mode data(out) is det.

:- pred primes(int, list(int)).
:- mode primes(in, out) is det.

:- pred integers(int, int, list(int)).
:- mode integers(in, in, out) is det.

:- pred sift(list(int), list(int)).
:- mode sift(in, out) is det.

:- pred remove(int, list(int), list(int)).
:- mode remove(in, in, out) is det.

data(98).

primes(Limit, Ps) :-
	integers(2, Limit, Is),
	sift(Is, Ps).

integers(Low, High, Result) :- 
	( Low =< High ->
		M is Low + 1,
		Result = [Low | Rest],
		integers(M, High, Rest)
	;
		Result = []
	).

sift([], []).
sift([I | Is], [I | Ps]) :-
	remove(I, Is, New),
	sift(New, Ps).

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

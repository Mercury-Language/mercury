% 9-queens program

:- module cqueens.

:- interface.

:- import_module list, int, io, printlist.

:- pred main(io__state, io__state).
:- mode main(di, uo) is nondet.

:- pred main1(list(int)).
:- mode main1(out) is nondet.

:- pred main3(list(int), io__state, io__state).
:- mode main3(out, di, uo) is nondet.

:- implementation.

main --> main3(_).

main1(Out) :-	
	data(Data),
	queen(Data, Out).

main3(Out) -->
	{ data(Data), queen(Data, Out) },
	print_list(Out).

:- pred data(list(int)).
:- mode data(out) is det.

:- pred queen(list(int), list(int)).
:- mode queen(in, out) is nondet.

:- pred queen_2(list(int), list(int), list(int)).
:- mode queen_2(in, in, out) is nondet.

:- pred qperm(list(int), list(int)).
:- mode qperm(in, out) is nondet.

:- pred qdelete(int, list(int), list(int)).
:- mode qdelete(out, in, out) is nondet.

:- pred safe(list(int)).
:- mode safe(in) is semidet.

:- pred nodiag(int, int, list(int)).
:- mode nodiag(in, in, in) is semidet.

data([1,2,3,4,5,6,7,8,9]).

queen(Data, Out) :-
	queen_2(Data, [], Out).

queen_2([], _, []).
queen_2(L, History, [Q|M]) :-
	L = [_|_],
	qdelete(Q, L, L1),
	nodiag(Q, 1, History),
	queen_2(L1, [Q|History], M).

qperm([], []).
qperm([X|Y], K) :-
	qdelete(U, [X|Y], Z),
	K = [U|V],
	qperm(Z, V).

qdelete(A, [A|L], L).
qdelete(X, [A|Z], [A|R]) :-
	qdelete(X, Z, R).

safe([]).
safe([N|L]) :-
	nodiag(N, 1, L),
	safe(L).

nodiag(_, _, []).
nodiag(B, D, [N|L]) :-
	NmB is N - B,
	BmN is B - N,
	( D = NmB ->
		fail
	; D = BmN ->
		fail
	;
		true
	),
	D1 is D + 1,
	nodiag(B, D1, L).


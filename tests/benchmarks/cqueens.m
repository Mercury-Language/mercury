% 9-queens program

:- module cqueens.

:- interface.

:- import_module list, int, io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is cc_multi.

:- pred main1(list(int)).
:- mode main1(out) is nondet.

:- implementation.

:- import_module prolog.

main1(Out) :-	
	data(Data),
	queen(Data, Out).

main -->
	( { data(Data), queen(Data, Out) } ->
		print_list(Out)
	;
		io__write_string("No solution\n")
	).

:- pred data(list(int)).
:- mode data(out) is det.

data([1,2,3,4,5,6,7,8,9]).

:- pred queen(list(int), list(int)).
:- mode queen(in, out) is nondet.

queen(Data, Out) :-
	queen_2(Data, [], Out).

:- pred queen_2(list(int), list(int), list(int)).
:- mode queen_2(in, in, out) is nondet.

queen_2([], _, []).
queen_2(L, History, [Q|M]) :-
	L = [_|_],
	qdelete(Q, L, L1),
	nodiag(Q, 1, History),
	queen_2(L1, [Q|History], M).

:- pred qperm(list(int), list(int)).
:- mode qperm(in, out) is nondet.

qperm([], []).
qperm([X|Y], K) :-
	qdelete(U, [X|Y], Z),
	K = [U|V],
	qperm(Z, V).

:- pred qdelete(int, list(int), list(int)).
:- mode qdelete(out, in, out) is nondet.

qdelete(A, [A|L], L).
qdelete(X, [A|Z], [A|R]) :-
	qdelete(X, Z, R).

:- pred safe(list(int)).
:- mode safe(in) is semidet.

safe([]).
safe([N|L]) :-
	nodiag(N, 1, L),
	safe(L).

:- pred nodiag(int, int, list(int)).
:- mode nodiag(in, in, in) is semidet.

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

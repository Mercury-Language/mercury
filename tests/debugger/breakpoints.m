:- module breakpoints.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is cc_multi.
:- func string / string = string.
:- implementation.
:- include_module breakpoints__print_list.
:- import_module list, int, string, breakpoints__print_list.

main -->
	( { queen(data, Out) } ->
		print_list(Out)
	;
		io__write_string("No solution\n")
	).

:- func data = list(int).

:- pred data(list(int)).
:- mode data(out) is det.

:- pred queen(list(int), list(int)).
:- mode queen(in, out) is nondet.

:- pred qperm(list(T), list(T)).
:- mode qperm(in, out) is nondet.

:- pred qdelete(T, list(T), list(T)).
:- mode qdelete(out, in, out) is nondet.

:- pred safe(list(int)).
:- mode safe(in) is semidet.

:- pred nodiag(int, int, list(int)).
:- mode nodiag(in, in, in) is semidet.

data = D :-
	data(D).

data([1,2,3,4,5]).

queen(Data, Out) :-
	qperm(Data, Out),
	safe(Out).

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

X / _ = X.

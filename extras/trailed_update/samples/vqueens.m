% 9-queens program -- using the `var' module and freeze/2.

:- module vqueens.

:- interface.

:- import_module list, int, io.

:- pred main1(list(int)).
:- mode main1(out) is nondet.

:- pred main(io__state, io__state).
:- mode main(di, uo) is cc_multi.

:- implementation.
:- import_module var, unsafe, std_util.

main1(Out) :-	
	data(Data),
	queen(Data, Out).

main -->
	( { data(Data), queen(Data, Out) } ->
		print(Out), nl
	;
		io__write_string("No solution\n")
	).

:- pred data(list(int)).
:- mode data(out) is det.

data([1,2,3,4,5,6,7,8,9,10]).

:- pred queen(list(int), list(int)).
:- mode queen(in, out) is nondet.

queen(Data, Out) :-
	same_len(Data, Posn),
	safe(Posn),
	qperm(Data, Posn, Posn2),
	conv_posn(Posn2, Out).

:- pred same_len(list(int)::in, list(var(int))::out(list_skel(any))) is det.
same_len([], []).
same_len([_|Xs], [N|Ys]) :- init(N), same_len(Xs, Ys).

:- pred conv_posn(list(var(int))::in, list(int)::out) is det.
conv_posn([], []).
conv_posn([var(N)|Xs], [N|Ys]) :- conv_posn(Xs, Ys).

:- pred qperm(list(int), list(var(int)), list(var(int))).
:- mode qperm(in, in(list_skel(any)), out) is nondet.

qperm([], [], []).
qperm([X|Y], [var(U)|V], [var(U)|V2]) :-
	qdelete(U, [X|Y], Z),
	qperm(Z, V, V2).

:- pred qdelete(int, list(int), list(int)).
:- mode qdelete(out, in, out) is nondet.

qdelete(A, [A|L], L).
qdelete(X, [A|Z], [A|R]) :-
	qdelete(X, Z, R).

:- pred safe(list(var(int))).
:- mode safe(in(list_skel(any))) is semidet.

safe([]).
safe([NVar|L]) :-
	freeze(NVar, (pred(N::in) is semidet :- nodiag(N, 1, L))),
	safe(L).

:- pred nodiag(int, int, list(var(int))).
:- mode nodiag(in, in, in(list_skel(any))) is semidet.

nodiag(_, _, []).
nodiag(B, D, [NVar|L]) :-
	freeze(NVar, (pred(N::in) is semidet :- D \= N - B, D \= B - N)),
	nodiag(B, D + 1, L).


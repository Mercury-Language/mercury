% This is a regression test for a couple of bugs relating to
% code generation for higher-order code.

:- module agg.

:- interface.

:- import_module std_util.

:- type agg_func(T, S) ---> agg(S, func(S, T) = S).
:- inst agg_mode == bound(agg(ground, func(in, in) = out is det)).
:- mode agg_in == agg_mode >> agg_mode.
:- mode agg_out == free >> agg_mode.

:- pred aggregate(pred(T), agg_func(T, S), S).
:- mode aggregate(pred(out) is multi, agg_in, out) is det.
:- mode aggregate(pred(out) is nondet, agg_in, out) is det.

:- func agg_pair(agg_func(T, S1), agg_func(T, S2)) = agg_func(T, pair(S1, S2)).
:- mode agg_pair(agg_in, agg_in) = agg_out is det.

:- func count = agg_func(T, int).
:- mode count = agg_out is det.

:- func count(pred(T)) = int.
:- mode count(pred(out) is multi) = out is det.
:- mode count(pred(out) is nondet) = out is det.

:- func sum(func(T) = int) = agg_func(T, int).
:- mode sum(func(in) = out is det) = agg_out is det.

:- func sum_agg(pred(T), func(T) = int) = int.
:- mode sum_agg(pred(out) is multi, func(in) = out is det) = out is det.
:- mode sum_agg(pred(out) is nondet, func(in) = out is det) = out is det.

:- func avg_agg(pred(T), func(T) = int) = maybe(float).
:- mode avg_agg(pred(out) is multi, func(in) = out is det) = out is det.
:- mode avg_agg(pred(out) is nondet, func(in) = out is det) = out is det.

:- func min(func(T) = int) = agg_func(T, maybe(int)).
:- mode min(func(in) = out is det) = agg_out is det.

:- func sum_and_min(func(T) = int) = agg_func(T, pair(int, maybe(int))).
:- mode sum_and_min(func(in) = out is det) = agg_out is det.

:- func sumF(func(T) = float) = agg_func(T, float).
:- mode sumF(func(in) = out is det) = agg_out is det.

:- func wsumF(func(T) = pair(float)) = agg_func(T, float).
:- mode wsumF(func(in) = out is det) = agg_out is det.

:- func minF(func(T) = float) = agg_func(T, maybe(float)).
:- mode minF(func(in) = out is det) = agg_out is det.

:- func sumF_and_minF(func(T) = float) = agg_func(T, pair(float, maybe(float))).
:- mode sumF_and_minF(func(in) = out is det) = agg_out is det.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list.

aggregate(P, agg(S0, F), S) :-
	A = (pred(Val::in, Acc0::in, Acc::out) is det :- Acc = F(Acc0, Val)),
	solutions(P, L), list__foldl(A, L, S0, S).

agg_pair(agg(S1, F1), agg(S2, F2)) = agg(S1-S2, F) :-
	F = (func(Acc1-Acc2, Val) = F1(Acc1, Val)-F2(Acc2, Val)).
	
:- import_module int.

count = agg(0, func(X, _) = X + 1).

count(P) = C :- aggregate(P, count, C).

sum(F) = agg(0, func(X, Y) = X + F(Y)).

sum_agg(P, F) = S :- aggregate(P, sum(F), S).

avg_agg(P, F) = A :-
	aggregate(P, agg_pair(count, sum(F)), C-S),
	( C = 0 ->
		A = no
    	;	C1 = float__float(C),
		S1 = float__float(S),
		A = yes(S1/C1)
	).

min(F) = agg(no, func(Acc, Val) = min(Acc, F(Val))).

:- func min(maybe(int), int) = maybe(int).
min(no, Val) = yes(Val).
min(yes(Acc), Val) = yes(Acc < Val -> Acc ; Val).

sum_and_min(F) = agg_pair(sum(F), min(F)).

:- import_module float.

sumF(F) = agg(0.0, func(X, Y) = X + F(Y)).

wsumF(F) = agg(0.0, (func(X, Y) = X + Y1 * Y2 :- F(Y) = Y1-Y2)).

minF(F) = agg(no, func(Acc0, Val) = minF(Acc0, F(Val))).

:- func minF(maybe(float), float) = maybe(float).
minF(no, Val) = yes(Val).
minF(yes(Acc), Val) = yes(Acc < Val -> Acc ; Val).

sumF_and_minF(F) = agg_pair(sumF(F), minF(F)).

:- import_module io.

:- pred iota(int::in, int::in, int::out) is nondet.
iota(N, M, I) :- N < M, (I = N ; iota(N+1, M, I)).

main -->
	{F = (func(N::in) = (M::out) is det :- M = N*N)},
	{aggregate(iota(1, 10), agg_pair(count, agg_pair(sum(F), min(F))), A)},
	io__write(A), io__nl.


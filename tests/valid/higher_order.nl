:- module higher_order.
:- import_module list, int.

:- pred call(pred(T1, T2), T1, T2).
:- mode call(in, in, out) is det.

:- pred map_list(pred(T1, T2), list(T1), list(T2)).
:- mode map_list(in, in, out) is det.

map_list(_P, [], []).
map_list(P, [X|Xs], [Y|Ys]) :-
	call(P, X, Y),
	map_list(P, Xs, Ys).

:- pred plus2(int, int).
:- mode plus2(in, out) is det.
plus2(X, Y) :- Y is X + 2.

:- pred test(list(int), list(int)).
:- mode test(in, out) is det.
test(L1, L2) :-
	map_list(plus2, L1, L2).

:- pred t is semidet.
t.

:- pred f is semidet.
f :- fail.

:- pred test(pred).
:- mode test(out) is nondet.
test(t).
test(f).

:- module higher_order.
:- import_module list, int.

:- pred map_list(pred(T1, T2), list(T1), list(T2)).
:- mode map_list(pred(in, out) is det, in, out) is det.

map_list(_P, [], []).
map_list(P, [X|Xs], [Y|Ys]) :-
	call(P, X, Y),
	map_list(P, Xs, Ys).

:- pred plus2(int, int).
:- mode plus2(in, out) is det.
plus2(X, Y) :- Y = X + 2.

:- pred test(list(int), list(int)).
:- mode test(in, out) is det.
test(L1, L2) :-
	map_list(plus2, L1, L2).

:- pred t is semidet.
t.

:- pred f is semidet.
f :- fail.

:- pred test(pred).
:- mode test(out((pred) is semidet)) is nondet.
test(t).
test(f).

:- module constrained_poly_insts.
:- interface.

:- pred search(tree(K, V), K, V).
:- mode search(in(tree(K, V =< any)), in(K), out(V =< any)) is semidet.

:- inst k == bound(a ; b ; c).
:- inst v == (pred(in, out) is det).

:- func call_search(tree(K, V), K) = V.
:- mode call_search(in(tree(k, v)), in(k)) = out(v) is semidet.

:- type tree(K, V) ---> empty ; node(tree(K, V), K, V, tree(K, V)).
:- inst tree(K, V) ---> empty ; node(tree(K, V), K, V, tree(K, V)).

:- inst b == bound(42 ; 43 ; 44).

:- pred p(int::in(I), int::out(I)) is semidet <= I =< b.

:- implementation.

call_search(T, K) = V :- search(T, K, V).

search(node(L, K0, V0, R), K, V) :-
	compare(Res, K0, K),
	(
		Res = (<),
		search(R, K, V)
	;
		Res = (=),
		V = V0
	;
		Res = (>),
		search(L, K, V)
	).

p(X, Y) :-
	X = 42,
	Y = X.

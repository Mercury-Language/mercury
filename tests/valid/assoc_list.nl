:- module m.
:- interface.
:- import_module list.

:- type pair(T1, T2).

:- pred assoc_list_member(pair(K,V), list(pair(K,V))).
:- mode assoc_list_member(bound(free - ground) -> ground, in).
:- mode assoc_list_member(bound(free - free) -> ground, in).

assoc_list_member(X, [X|_]).
assoc_list_member(X, [_|Xs]) :-
	assoc_list_member(X, Xs).

:- module assoc_list.
:- interface.
:- import_module list.

:- type pair(T1, T2).

:- pred assoc_list_member(pair(K,V), list(pair(K,V))).
:- mode assoc_list_member(bound(free - ground) -> ground, in) is semidet.
:- mode assoc_list_member(bound(free - free) -> ground, in) is det.

assoc_list_member(X, [X|_]).
assoc_list_member(X, [_|Xs]) :-
	assoc_list_member(X, Xs).

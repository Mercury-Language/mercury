%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module assoc_list.
:- interface.

:- import_module list.
:- import_module std_util.

:- pred assoc_list_member(pair(K, V), list(pair(K, V))).
:- mode assoc_list_member(bound(free - ground) -> ground, in) is semidet.
:- mode assoc_list_member(bound(free - free) -> ground, in) is nondet.

assoc_list_member(X, [X | _]).
assoc_list_member(X, [_ | Xs]) :-
    assoc_list_member(X, Xs).

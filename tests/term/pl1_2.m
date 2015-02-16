%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module pl1_2.

:- interface.

:- type list(T)
    --->    []
    ;       [T | list(T)].

:- pred perm(list(T)::in, list(T)::out) is nondet.

:- implementation.

perm([], []).
perm(L, [H | T]) :-
    append(V, [H | U], L),
    append(V, U, W),
    perm(W, T).

:- pred append(list(T), list(T), list(T)).
:- mode append(out, out, in).
:- mode append(in, in, out).

append([], Y, Y).
append([H | X], Y, [H | Z]) :-
    append(X, Y, Z).

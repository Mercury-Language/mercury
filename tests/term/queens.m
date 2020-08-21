%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module queens.

:- interface.

:- type list(T)
    --->    []
    ;       [T | list(T)].

:- pred queens(list(int)::in, list(int)::out) is nondet.

:- implementation.

:- import_module int.
:- import_module prolog.

queens(X, Y) :-
    perm(X, Y),
    safe(Y).

:- pred perm(list(T), list(T)).
:- mode perm(in, out).

perm([], []).
perm([X | Y], [V | Res]) :-
    delete(V, [X | Y], Rest),
    perm(Rest, Res).

:- pred delete(T, list(T), list(T)).
:- mode delete(out, in, out).

delete(X, [X | Y], Y).
delete(X, [F | T], [F | R]) :-
    delete(X, T, R).

:- pred safe(list(int)).
:- mode safe(in).

safe([]).
safe([X | Y]) :-
    noattack(X, Y, 1),
    safe(Y).

:- pred noattack(int, list(int), int).
:- mode noattack(in, in, di).

noattack(_X, [], _N).
noattack(X, [F | T], N) :-
    X \= F,
    X \= F + N,
    F \= X + N,
    N1 = N + 1,
    noattack(X, T, N1).

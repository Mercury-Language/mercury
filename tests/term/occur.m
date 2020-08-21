%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Benchmark Program - Counting occurrences in lists
%
% Author: B. Ramkumar and L. V. Kale
% Date:
%
% To test: run o/1.
%
% Benchmark is o(31), runs with -A1 -E256 -C128

:- module occur.

:- interface.

:- type list(T)
    --->    []
    ;       [T | list(T)].

:- pred occurall(list(int)::in, list(list(int))::in, list(list(int))::out)
    is nondet.

:- implementation.

:- import_module int.
:- import_module prolog.

occurall([], _X, []).
occurall([X | Y], Z, [[X, W] | V]) :-
    occur(X, Z, W),
    occurall(Y, Z, V).

:- pred occur(T1, list(list(T1)), int).
:- mode occur(in, in, out).

occur(_X, [], 0).
occur(X, [Y | Z], W) :-
    ( if
        count(X, Y, A),
        occur(X, Z, B)
    then
        W = A + B
    else
        fail
    ).

:- pred count(T1, list(T1), int).
:- mode count(in, in, out).

count(_X, [], 0).
count(X, [Y | Z], W) :-
    ( if count(X, Z, W1) then
        addx(X, Y, W1, W)
    else
        fail
    ).

:- pred addx(T1, T1, int, int).
:- mode addx(in, in, in, out).

addx(X, X, W1, W) :-
    W = W1 + 1.
addx(X, Y, W1, W1) :-
    X \= Y.

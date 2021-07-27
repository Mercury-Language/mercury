%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Benchmark Program - Counting occurrences in lists
%
% Author: B. Ramkumar and L. V. Kale
%
% To test: run o/1.
%
% Benchmark is o(31), runs with -A1 -E256 -C128

:- module uniq_mode_inf_bug.

:- interface.
:- import_module list.

:- pred occurall(list(int)::in, list(list(int))::in, list(list(int))::out)
    is nondet.

:- implementation.

:- import_module int.

occurall([], _X, []).
occurall([X | Y], Z, [[X, W] | V]) :-
    occur(X, Z, W),
    occurall(Y, Z, V).

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

count(_X, [], 0).
count(X, [Y | Z], W) :-
    ( if count(X, Z, W1) then
        addx(X, Y, W1, W)
    else
        fail
    ).

addx(X, X, W1, W) :-
    W = W1 + 1.
addx(X, Y, W1, W1) :-
    X \= Y.

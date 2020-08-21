%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Program: Cryptarithmetic puzzle SENDMORY
% Author:  Rong Yang (adapted)
% Date:
% 
% Notes:
% 1. To run:
%     ?- money(S, E, N, D, M, O, R, Y).
% 2. Solution is reached in the domain approach so as to recognize determinism
%     as the ecuations are being resolved.
% 3. After-checks are used and calc/5 ordering is better (L to R).
%     compiled it takes about 50 sec.
% 
:- module money.

:- interface.

:- type list(T)
    --->    []
    ;       [T | list(T)].

:- pred solve(list(int)::out) is nondet.

:- implementation.

:- import_module int.
:- import_module prolog.

solve([S, E, N, D, M, O, R, Y]) :-
    money(S, E, N, D, M, O, R, Y).

:- pred money(int, int, int, int, int, int, int, int).
:- mode money(out, out, out, out, out, out, out, out).

money(S, E, N, D, M, O, R, Y) :-
    carry(C1),
    carry(C2),
    carry(C3),
    carry(C4),
    C4 = M,
    M \= 0,
    domain([S, E, N, D, M, O, R, Y], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
    S \= 0,
    calc(C3, S, M, C4, O),
    calc(C2, E, O, C3, N),
    calc(C1, N, R, C2, E),
    calc( 0, D, E, C1, Y).

:- pred calc(int, int, int, int, int).
:- mode calc(in, in, in, in, in).

calc(C0, D, E, C1, Y) :-
    sum(C0, D, CD),
    sum(CD, E, S),
    carry10(C1, C10),
    sum(C10, Y, S).

:- pred sum(int, int, int).
:- mode sum(in, in, out).

sum(X, Y, Z) :-
    Z = X + Y.

:- pred domain(list(T), list(T)).
:- mode domain(out, in).

domain([], _).
domain([X1 | R], L) :-
    del(X1, L, NL),
    domain(R, NL).

:- pred del(T, list(T), list(T)).
:- mode del(out, in, out).

del(X, [X | T], T).
del(X, [Y | T], [Y | NT]) :-
    del(X, T, NT).

:- pred carry(int).
:- mode carry(out).

carry(1).
carry(0).

:- pred carry10(int, int).
:- mode carry10(in, out).

carry10(0, 0).
carry10(1, 10).

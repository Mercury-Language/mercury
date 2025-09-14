%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% This module tests the code in library/mercury_term_parser.m
% that tries to generate useful diagnostics for mismatched parentheses
% (round, square, and curly).
%---------------------------------------------------------------------------%

:- module unbalanced.

:- interface.

:- pred p(int::in, int::out) is multi.

:- implementation.

:- import_module int.
:- import_module list.

p(A, Z) :-
    ( if
        B = (2 * A,
        C = A + B,
        C > 42
    then
        Z = 1
    else
        Z = 0
    ).

:- pred q(int::in, int::out) is multi.

q(A, Z) :-
    ( if
        B = (
            list.length(
                [1, 2
                    ) *
                        A],
        C = A + B,
        C > 42
    then
        Z = 1
    else
        Z = 0
    ).

:- pred r(int::in, int::out) is multi.

r(A, B) :-
    B = list.length({[1, 2}.

:- pred s(int::in, int::out) is multi.

s(A, B) :-
    B = list.length(]1, 2]).

:- pred t(int::in, int::out) is multi.

t(A, B) :-
    B = list.length([1, 2.
t(A, B) :-
    B = list.length(
        [1, 2

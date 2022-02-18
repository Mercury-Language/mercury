%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This was intended as a test of the code that prints out higher order
% mode mismatches other than arity mismatches (those errors are tested by
% other test cases). However, all these errors are caught by the typechecker
% first.
%
%---------------------------------------------------------------------------%

:- module higher_order_mode_mismatch.

:- interface.

:- pred test_1(pred(int, int, int)::in(pred(in, in, out) is det),
    int::in, int::out) is det.
:- pred test_2((func(int, int) = int)::in,
    int::in, int::out) is det.
:- pred test_3((func(int, int) = int)::in(func(in, in) = out is det),
    int::in, int::out) is det.
:- pred test_4(int::in, int::in, int::out) is det.
:- pred test_5(int::in, int::in, int::out) is det.

:- implementation.

test_1(P, In1, Out) :-
    Out = P(In1, In1).

test_2(F, In1, Out) :-
    F(In1, In1, Out).

test_3(F, In1, Out) :-
    F(In1, In1, Out).

test_4(In1, In2, Out) :-
    Out = In1(In2).

test_5(In1, In2, Out) :-
    In1(In2, Out).

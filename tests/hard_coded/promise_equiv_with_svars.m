%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% promise_equiv_with_svars.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Wed Apr 13 17:38:55 EST 2005
%
% Test that promise_equivalent_solutions does the right thing
% if state variables are included in the variable list.
%
%---------------------------------------------------------------------------%

:- module promise_equiv_with_svars.

:- interface.

:- import_module io.

:- pred main(io :: di, io :: uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.

main(!IO) :-
    p1(A),
    p2(B),
    p3(C, D),
    io.print_line([A, B, C, D], !IO).

:- pred p1(int::out) is det.

p1(!.X) :-
    promise_equivalent_solutions [!.X] (
        !.X = 1
    ).

:- pred p2(int::out) is det.

p2(!:X) :-
    promise_equivalent_solutions [!:X] (
        !:X = 2
    ).

:- pred p3(int::out, int::out) is det.

p3(!X) :-
    promise_equivalent_solutions [!X] (
        !.X = 3,
        !:X = !.X + 1
    ).

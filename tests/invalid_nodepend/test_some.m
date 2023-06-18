%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module test_some.

:- interface.

:- pred p1(int::in) is semidet.
:- some junk pred p2(int::in) is semidet.
:- pred p3(int::in) is semidet.

:- implementation.
:- import_module int.

p1(X) :-
    some junk p3(X).
p3(X) :-
    ( some junk X > 0 -> X = 42 ; X = -42 ).

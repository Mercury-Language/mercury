%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module non_stratification.
:- interface.

:- pred foo(int::in) is semidet.

:- implementation.
:- import_module int.

foo(1).
foo(2).
foo(3).
foo(X) :-
    not foo(X - 1).

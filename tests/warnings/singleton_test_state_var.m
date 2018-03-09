%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module singleton_test_state_var.
:- interface.

:- import_module list.

:- pred foo(int::in, int::in, list(int)::in, int::out) is det.

:- implementation.

foo(A, B, !.State, X) :-
    ( if !.State = [A | !:State] then
        X = B
    else
        X = A
    ).

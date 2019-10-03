%------------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%------------------------------------------------------------------------------%
%
% This test case tests both the occurs check warning and its suppression.
%

:- module occurs.

:- interface.
:- import_module list.

:- pred test(list(int)::in, int::out, int::out) is det.

:- implementation.

test(A, X, Y) :-
    ( if
        A = [_A0, _A1 | A]
    then
        X = 1
    else
        X = 2
    ),
    ( if
        disable_warning [suspected_occurs_check_failure] (
            A = [_A2, _A3, _A4 | A]
        )
    then
        Y = 1
    else
        Y = 2
    ).

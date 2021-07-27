%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for a bug with the state variable transformation.
% It was not correctly handling the case where the condition of an if-then-else
% referred to a state variable, but the "then" part didn't.
% This lead to a determinism error in the following code.

:- module state_var_bug.
:- interface.

:- pred foo(int::in, int::out) is det.

:- implementation.

foo(!X) :-
    ( if copy(!X) then
        true
    else
        true
    ),
    (
        fail
    ;
        copy(!X)
    ).

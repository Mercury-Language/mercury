%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test whether the compiler detects that a variable that occurs only
%
% - once in a scope, and
% - and in the list of variables quantified by that scope
%
% is NOT a singleton.

:- module some_singleton.
:- interface.

:- pred p(int::in, int::out) is det.

:- implementation.

p(A, C) :-
    ( if
        some [B] (
            q(A, B)     % B should NOT be reported as a singleton.
        )
    then
        C = 100
    else
        C = 200
    ).

:- pred q(int::in, int::out) is nondet.

q(1, 11).
q(1, 12).
q(2, 21).

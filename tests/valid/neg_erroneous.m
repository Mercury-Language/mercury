%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The instmap delta of a negated erroneous goal should also be `unreachable'.
% Software Error: instmap.m:
% Unexpected: merge_instmapping_delta_2: error merging var 2

:- module neg_erroneous.
:- interface.

:- pred f(int::out) is semidet.

:- implementation.

f(X) :-
    ( if semidet_true then
        X = 1
    else
        not private_builtin.sorry("sorry"),
        fail
    ).

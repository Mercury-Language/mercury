% The instmap delta of a negated erroneous goal should also be `unreachable'.
% Software Error: instmap.m: Unexpected: merge_instmapping_delta_2: error merging var 2

:- module neg_erroneous.
:- interface.

:- pred f(int::out) is semidet.

:- implementation.

f(X) :-
    ( semidet_true ->
        X = 1
    ;
        not private_builtin.sorry("sorry"),
        fail
    ).

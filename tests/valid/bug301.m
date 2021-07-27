%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The compiler aborted in the float_reg.m pass due to mishandling of
% deconstruction unifications.
%
% Software Error: hlds.instmap:
%   predicate `hlds.instmap.merge_instmapping_delta_2'/9:
% Unexpected: merge_instmapping_delta_2: error merging var 5

:- module bug301.
:- interface.

:- type e
    --->    e(string, int).

:- type f
    --->    f(int, string, int).

:- pred mk(e::in, f::out) is det.

:- implementation.

mk(E, F) :-
    E = e(X, _),
    F = f(1, _, _),
    F = f(_, X, _),
    ( if X = "" then
        F = f(_, _, 3)
    else
        F = f(_, _, -3)
    ).

%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for bug #257: the compiler was not issuing a warning about
% the fact that the variable Gee which was the subject of a
% require_complete_switch did not occur in the scoped goal.

:- module bug257.
:- interface.

:- type xyz
    --->    x
    ;       y
    ;       z.

:- pred oops(xyz::in, int::out) is semidet.

:- implementation.

oops(G, N) :-
    require_complete_switch [Gee]
    (
        G = x,
        N = 1
    ;
        G = y,
        fail
    ).

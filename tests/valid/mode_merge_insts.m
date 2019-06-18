%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module mode_merge_insts.

:- interface.

:- type triple
    --->    triple(int, int, int).

:- inst triple1
    --->    triple(ground, free, free).
:- inst triple2
    --->    triple(free, ground, free).
:- inst triple12
    --->    triple(ground, ground, free).

:- pred p1(triple::out(triple1)) is det.
:- pred p2(triple::out(triple2)) is det.
:- pred q(triple::in(triple12)) is det.

:- implementation.

:- pragma external_pred(p1/1).
:- pragma external_pred(p2/1).
:- pragma external_pred(q/1).

:- pred p is det.

p :-
    p1(X),
    p2(X),
    q(X).

:- pred q is det.

q :-
    p1(X),
    p2(Y),
    X = Y,
    q(X).

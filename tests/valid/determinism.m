%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module has a couple of test cases for determinism analysis
% of complicated unifications.

:- module determinism.

:- interface.

:- import_module list.
:- import_module pair.

:- inst fg == bound(free - ground).
:- inst gf == bound(ground - free).
:- inst list(Inst) == bound([] ; [Inst | determinism.list(Inst)]).

:- pred q(list(pair(int))).
:- mode q(free >> determinism.list(fg)) is det.

:- pred r(list(pair(int))).
:- mode r(free >> determinism.list(gf)) is det.

:- pred p is det.
:- pred p2 is semidet.
:- pred p3 is semidet.

:- implementation.

p :-
    q(X),
    r(Y),
    X = Y.

p2 :-
    q(X),
    q(Y),
    X = Y.

p3 :-
    r(X),
    r(Y),
    X = Y.

:- external(q/1).
:- external(r/1).

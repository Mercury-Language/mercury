%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module constrained_poly_insts3.
:- interface.

:- import_module list.

:- type thing == list(list(int)).
:- inst thing == list_skel(ground).

:- pred p1(thing::in(I), thing::out(I)) is det.

%---------------------------------------------------------------------------%

:- implementation.

p1(X, Y) :-
    q(X, Y).

:- pred q(thing:in(thing), thing::out(thing)) is det.

q(X, X).

%---------------------------------------------------------------------------%

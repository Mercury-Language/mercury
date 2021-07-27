%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module constrained_poly_insts4.
:- interface.

:- import_module list.

:- type thing == list(list(int)).
:- inst thing == list_skel(ground).

:- pred p2(thing, thing).
:- mode p2(in(I =< thing), out(I =< thing)) is det.

%---------------------------------------------------------------------------%

:- implementation.

p2(X, Y) :-
    q(X, Y).

:- pred q(thing::in(thing), thing::out(thing)) is det.

q(X, X).

%---------------------------------------------------------------------------%

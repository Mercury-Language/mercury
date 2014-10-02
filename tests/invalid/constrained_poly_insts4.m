%-----------------------------------------------------------------------------%

:- module constrained_poly_insts4.
:- interface.

:- import_module list.

:- type thing == list(list(int)).
:- inst thing == list_skel(ground).

:- pred p2(thing, thing).
:- mode p2(in(I =< thing), out(I =< thing)) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

p2(X, Y) :-
    q(X, Y).

:- pred q(thing, thing).
:- mode q(in(thing), out(thing)) is det.

q(X, X).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et

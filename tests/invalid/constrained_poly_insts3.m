%-----------------------------------------------------------------------------%

:- module constrained_poly_insts3.
:- interface.

:- import_module list.

:- type thing == list(list(int)).
:- inst thing == list_skel(ground).

:- pred p1(thing, thing).
:- mode p1(in(I), out(I)) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

p1(X, Y) :-
    q(X, Y).

:- pred q(thing, thing).
:- mode q(in(thing), out(thing)) is det.

q(X, X).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et

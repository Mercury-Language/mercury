%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module lambda_type.

:- interface.

:- pred p is det.
:- pred q is det.

:- implementation.

p :-
    _X = (pred(W::out) is det :- W = 1),
    _Y = (pred(W::out) is det :- W = a).

q :-
    _X = (pred(W::out) is det :- W = 1, Z = 1),
    _Y = (pred(W::out) is det :- W = a, Z = a).

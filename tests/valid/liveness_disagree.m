% vim: ts=4 sw=4 et ft=mercury

:- module liveness_disagree.
:- interface.

:- import_module bool.

:- pred p(int::in, bool::in, bool::out) is det.

:- implementation.
:- import_module list.

p(Y, !X) :-
    (
        !.X = no,
        !:X = yes,
        A = [1, 2],
        B = [Y | A],
        _C = B
    ;
        !.X = yes
    ).

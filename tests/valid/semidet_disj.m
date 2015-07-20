%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module semidet_disj.

:- interface.

:- pred p(int::in) is semidet.

:- implementation.

:- pred q(int::in) is semidet.
:- external(q/1).
:- pred r(int::in) is semidet.
:- external(r/1).

p(X) :-
    ( q(X)
    ; r(X)
    ).

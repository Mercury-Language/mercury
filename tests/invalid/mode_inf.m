:- module mode_inf.
:- interface.

:- pred p(int, int).
:- mode p(out, out) is det.

:- implementation.

p(X, Y) :- q(X, Y).

:- pred q(int, int).
q(X, X).


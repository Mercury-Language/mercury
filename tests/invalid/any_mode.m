:- module any_mode.

:- interface.

:- mode p(any -> ground).
p(X) :- q(X).

:- mode q(in).
q(42).

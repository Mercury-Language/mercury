:- module purity_nonsense2.
:- interface.
:- type foo == int.
:- implementation.
:- impure type badtype ---> oops.
:- impure mode badmode :: free -> free.

:- impure pred undefined.
:- pragma promise_pure(undefined/0).
:- pragma promise_pure(undefined2/0).

e12 :- impure (\+ impure imp).
e13 :- semipure (\+ semipure semi).

:- impure pred e14(pred).
:- mode e14(((pred) is semidet)) is semidet.

e14(P) :- impure call(P).

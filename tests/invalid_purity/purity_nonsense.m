:- module purity_nonsense.
:- interface.
:- type foo == int.
:- implementation.







e12 :- impure (\+ impure imp).
e13 :- semipure (\+ semipure semi).

:- impure pred e14(pred).
:- mode e14(((pred) is semidet)) is semidet.

e14(P) :- impure call(P).

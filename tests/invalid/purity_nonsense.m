:- module purity_nonsense.

:- impure func undefined_func = foo.		% no impure functions (yet)

:- impure type badtype ---> oops.
:- impure mode badmode :: free -> free.

:- impure pred undefined.
:- pragma promise_pure(undefined/0).
:- pragma promise_pure(undefined2/0).

e12 :- impure (\+ impure imp).
e13 :- semipure (\+ semipure semi).


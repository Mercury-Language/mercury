:- module complicated_unify.

:- type t ---> f(int).

:- pred p(t::in, t::in) is semidet.

p(X, X).


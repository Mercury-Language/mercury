:- module undef_type.

:- type t1 ---> a ; b ; c ; d(undef1).

:- inst x == bound(a ; b ; c).

:- pred p.
p.

:- pred q(undef2).
q(_).

:- pred r(io__state).
r(_).


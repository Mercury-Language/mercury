:- module abstract_eqv_1.

:- interface.

:- type t_abs.

:- func val1 = t_abs.
:- func val2 = t_abs.
:- func val3 = t_abs.

:- implementation.

:- type t_abs == t_concrete.

:- type t_concrete --->	a ; b ; c.

val1 = a.
val2 = b.
val3 = b.

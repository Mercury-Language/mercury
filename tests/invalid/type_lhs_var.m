:- module type_lhs_var.
:- interface.

:- type foo == int.

:- type A == int.

:- type B ---> foo ; bar.

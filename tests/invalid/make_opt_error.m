:- module make_opt_error.

:- interface.

:- type coord.

:- func x(coord) = int.
:- func y(coord) = int.

:- implementation.

:- type coord ---> coord(x :: int, y :: int).

x(C) = C ^ x.
y(C) = C ^ y.

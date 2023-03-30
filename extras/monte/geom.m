%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module geom.

:- interface.

:- import_module float.

:- type shape == pred(float, float, float).
:- inst shape == (pred(in, in, in) is semidet).

:- pred sphere(float::in, float::in, float::in) is semidet.

:- pred cylinder(float::in, float::in, float::in) is semidet.

:- pred cube(float::in, float::in, float::in) is semidet.

:- pred cone(float::in, float::in, float::in) is semidet.

:- pred torus(float::in, float::in, float::in, float::in) is semidet.

:- pred planeX(float::in, float::in, float::in) is semidet.

:- pred planeY(float::in, float::in, float::in) is semidet.

:- pred planeZ(float::in, float::in, float::in) is semidet.

:- pred union(shape::in(shape), shape::in(shape),
    float::in, float::in, float::in) is semidet.

:- pred intersection(shape::in(shape), shape::in(shape),
    float::in, float::in, float::in) is semidet.

:- pred difference(shape::in(shape), shape::in(shape),
    float::in, float::in, float::in) is semidet.

:- pred translate(shape::in(shape), float::in, float::in, float::in,
    float::in, float::in, float::in) is semidet.

:- pred scale(shape::in(shape), float::in, float::in, float::in,
    float::in, float::in, float::in) is semidet.

:- pred rotateX(shape::in(shape), float::in,
    float::in, float::in, float::in) is semidet.

:- pred rotateY(shape::in(shape), float::in,
    float::in, float::in, float::in) is semidet.

:- pred rotateZ(shape::in(shape), float::in,
    float::in, float::in, float::in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module math.

%-----------------------------------------------------------------------------%

sphere(X, Y, Z) :-
    sqr(X) + sqr(Y) + sqr(Z) =< 1.0.

cylinder(X, Y, Z) :-
    sqr(X) + sqr(Y) =< 1.0,
    0.0 =< Z, Z =< 1.0.

cube(X, Y, Z) :-
    0.0 =< X, X =< 1.0,
    0.0 =< Y, Y =< 1.0,
    0.0 =< Z, Z =< 1.0.

cone(X, Y, Z) :-
    0.0 =< Z, Z =< 1.0,
    sqr(X) + sqr(Y) =< 1.0 - Z.

torus(R, X, Y, Z) :-
    sqr(sqrt(sqr(X) + sqr(Z)) - 1.0) + sqr(Y) =< sqr(R).

planeX(X, _, _) :-
    X >= 0.0.

planeY(_, Y, _) :-
    Y >= 0.0.

planeZ(_, _, Z) :-
    Z >= 0.0.

union(A, B, X, Y, Z) :-
    (
        call(A, X, Y, Z)
    ;
        call(B, X, Y, Z)
    ).

intersection(A, B, X, Y, Z) :-
    call(A, X, Y, Z),
    call(B, X, Y, Z).

difference(A, B, X, Y, Z) :-
    call(A, X, Y, Z),
    not call(B, X, Y, Z).

translate(Shape, Dx, Dy, Dz, X, Y, Z) :-
    call(Shape, X - Dx, Y - Dy, Z - Dz).

scale(Shape, Sx, Sy, Sz, X, Y, Z) :-
    call(Shape, X / Sx, Y / Sy, Z / Sz).

rotateX(Shape, Th, X, Y, Z) :-
    Sth = sin(-Th),
    Cth = cos(-Th),
    call(Shape, X, Cth * Y - Sth * Z, Sth * Y + Cth * Z).

rotateY(Shape, Th, X, Y, Z) :-
    Sth = sin(-Th),
    Cth = cos(-Th),
    call(Shape, Cth * X + Sth * Z, Y, -Sth * X + Cth * Z).

rotateZ(Shape, Th, X, Y, Z) :-
    Sth = sin(-Th),
    Cth = cos(-Th),
    call(Shape, Cth * X - Sth * Y, Sth * X + Sth * Y, Z).

:- func sqr(float) = float.

sqr(X) = X * X.

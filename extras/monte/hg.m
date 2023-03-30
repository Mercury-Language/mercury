%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hg.

:- interface.

:- import_module monte.
:- import_module float.

:- func hgBox = box.

:- pred hg(float::in, float::in, float::in) is semidet.

:- pred head(float::in, float::in, float::in) is semidet.

:- pred body(float::in, float::in, float::in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module geom.

%-----------------------------------------------------------------------------%

hgBox = box(-2.5, -2.0, -2.5, 2.5, 2.0, 4.5).

hg(X, Y, Z) :-
    union(
        translate(head, 0.0, 0.0, 2.0),
    body,
    X, Y, Z).

head(X, Y, Z) :-
    union(
        torus(0.25),
        translate(
            union(
                difference(torus(0.25), planeZ),
                union(
                    translate(
                        scale(sphere, 0.25, 0.25, 0.25),
                        -1.0, 0.0, 0.0
                    ),
                    translate(
                        scale(sphere, 0.25, 0.25, 0.25),
                        1.0, 0.0, 0.0
                    )
                )
            ),
            0.0,
            0.0,
            2.0
        ),
        X,
        Y,
        Z
    ).

body(X, Y, Z) :-
    union(
        translate(
            scale(cylinder, 0.25, 0.25, 3.0),
            0.0,
            0.0,
            -2.0
        ),
        union(
            rotateY(
                translate(
                    scale(cylinder, 0.25, 0.25, 2.0),
                    0.0,
                    0.0,
                    -1.0
                ),
                pi / 2.0
            ),
            union(
                translate(
                    scale(sphere, 0.25, 0.25, 0.25),
                    0.0,
                    0.0,
                    -2.0
                ),
                union(
                    translate(
                        scale(sphere, 0.25, 0.25, 0.25),
                        -1.0,
                        0.0,
                        0.0
                    ),
                    translate(
                        scale(sphere, 0.25, 0.25, 0.25),
                        1.0,
                        0.0,
                        0.0
                    )
                )
            )
        ),
        X,
        Y,
        Z
    ).

:- func pi = float.

pi = 3.14159265359.

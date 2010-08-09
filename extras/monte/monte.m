%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module monte.

:- interface.

:- import_module float, int.

:- type box
    --->    box(
            xmin :: float,
            ymin :: float,
            zmin :: float,
            xmax :: float,
            ymax :: float,
            zmax :: float
        ).

:- type shape == pred(float, float, float).
:- inst shape == (pred(in, in, in) is semidet).

:- type precision == int.

:- type volume == float.

:- pred monte(box::in, shape::in(shape), precision::in, volume::out) is det.

:- implementation.

:- import_module rnd.

monte(Box, Shape, Precision, Volume) :-
    rnd.init(17, Rnd0),
    monte_loop(Box, Shape, Precision, Rnd0, 0, Hits),
    Prop = float(Hits) / float(Precision),
    BoxVolume =
        (Box ^ xmax - Box ^ xmin) *
        (Box ^ ymax - Box ^ ymin) *
        (Box ^ zmax - Box ^ zmin),
    Volume = BoxVolume * Prop.

:- pred monte_loop(box::in, shape::in(shape), int::in, rnd::in,
    int::in, int::out) is det.

monte_loop(Box, Shape, N, !.Rnd, !Hits) :-
    ( N > 0 ->
        frange(Box ^ xmin, Box ^ xmax, X, !Rnd),
        frange(Box ^ ymin, Box ^ ymax, Y, !Rnd),
        frange(Box ^ zmin, Box ^ zmax, Z, !Rnd),
        ( call(Shape, X, Y, Z) ->
            !:Hits = !.Hits + 1
        ;
            true
        ),
        monte_loop(Box, Shape, N - 1, !.Rnd, !Hits)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module monte.

:- interface.

:- import_module float.
:- import_module int.

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

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module rnd.

%-----------------------------------------------------------------------------%

monte(Box, Shape, Precision, Volume) :-
    rnd.init(17, Rnd0),
    monte_outer_loop(Box, Shape, Precision, Rnd0, 0, Hits),
    Prop = float(Hits) / float(Precision),
    BoxVolume =
        (Box ^ xmax - Box ^ xmin) *
        (Box ^ ymax - Box ^ ymin) *
        (Box ^ zmax - Box ^ zmin),
    Volume = BoxVolume * Prop.

:- pred monte_outer_loop(box::in, shape::in(shape), int::in, rnd::in,
    int::in, int::out) is det.

monte_outer_loop(Box, Shape, N, !.Rnd, !Hits) :-
    ( if N > 10000 then
        monte_inner_loop(Box, Shape, 10000, !Rnd, !Hits),
        monte_outer_loop(Box, Shape, N - 10000, !.Rnd, !Hits)
    else
        monte_inner_loop(Box, Shape, N, !.Rnd, _, !Hits)
    ).

:- pred monte_inner_loop(box::in, shape::in(shape), int::in,
    rnd::in, rnd::out, int::in, int::out) is det.

monte_inner_loop(Box, Shape, N, !Rnd, !Hits) :-
    ( if N > 0 then
        frange(Box ^ xmin, Box ^ xmax, X, !Rnd),
        frange(Box ^ ymin, Box ^ ymax, Y, !Rnd),
        frange(Box ^ zmin, Box ^ zmax, Z, !Rnd),
        ( if call(Shape, X, Y, Z) then
            !:Hits = !.Hits + 1
        else
            true
        ),
        monte_inner_loop(Box, Shape, N - 1, !Rnd, !Hits)
    else
        true
    ).

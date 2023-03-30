%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module dots.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module hg.
:- import_module geom.
:- import_module monte.
:- import_module rnd.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    io.format("newgraph\n", [], !IO),
    io.format("xaxis min -2.5 max 2.5 size %2.2f\n", [f(5.0/3.0)], !IO),
    io.format("yaxis min -2.5 max 4.5 size %2.2f\n", [f(7.0/3.0)], !IO),
    io.format("newcurve marksize 0.01 0.01 pts ", [], !IO),
    dots(hgBox, hg, 100000, !IO).

:- pred dots(box::in, monte.shape::in(monte.shape), precision::in,
    io::di, io::uo) is det.

dots(Box, Shape, Precision, !IO) :-
    rnd__init(17, Rnd0),
    dots2(Box, Shape, Precision, Rnd0, !IO).

:- pred dots2(box::in, monte.shape::in(monte.shape), int::in, rnd::in,
    io::di, io::uo) is det.

dots2(Box, Shape, N, !.Rnd, !IO) :-
    ( if N > 0 then
        frange(Box ^ xmin, Box ^ xmax, X, !Rnd),
        frange(Box ^ ymin, Box ^ ymax, Y, !Rnd),
        frange(Box ^ zmin, Box ^ zmax, Z, !Rnd),
        ( if call(Shape, X, Y, Z) then
            io.format("%2.2f %2.2f ", [f(X), f(Z)], !IO)
        else
            true
        ),
        dots2(Box, Shape, N - 1, !.Rnd, !IO)
    else
        true
    ).

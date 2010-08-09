%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module doit.

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
    NumPoints = 1000000,
    monte(hgBox, hg, NumPoints, Volume),
    io.format("The computed volume after testing %d points is %f\n",
        [i(NumPoints), f(Volume)], !IO).

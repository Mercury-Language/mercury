%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module use11.

:- interface.

:- use_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, list.
:- import_module qcheck, nrev.

%---------------------------------------------------------------------------%

main -->
    qcheck(qcheck__f(func(Xs `with_type` list(int), Ys `with_type` list(int))
		     = nrev(Xs ++ Ys) `===` (nrev(Ys) ++ nrev(Xs))),
           "sample testing").


%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module use1.

:- interface.

:- use_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, list.
:- import_module qcheck, nrev2.

%---------------------------------------------------------------------------%

main -->
     	qcheck(qcheck__f(testing2), "testing2").

%------------------------------------------------------------------------------%
% 	Invariant test functions
%------------------------------------------------------------------------------%


:- func testing2(list(int), list(int)) = property.
testing2(Xs, Ys) = (Left `===` Right) :-
	nrev2((Xs ++ Ys), Left),
	nrev2(Ys, Part_a),
	nrev2(Xs, Part_b),
	Right = Part_a ++ Part_b.

%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module use21.

:- interface.

:- use_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, list, bool.
:- import_module qcheck, mymax.

%---------------------------------------------------------------------------%

main -->
     	qcheck(qcheck__f(law), "2 conditions : x<=Y, Y!=0").

:- func law(int, int) = property.
law(X, Y) =  notzero(Y) `===>` ((X =< Y) `===>` (mymax(X, Y) `===` Y)).

:- pred notzero(int).
:- mode notzero(in) is semidet.
notzero(X) :- X \= 0.

%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module use22.

:- interface.

:- use_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, bool.
:- import_module qcheck.

%---------------------------------------------------------------------------%

main -->
     	qcheck(qcheck__f(law1), "passing property"),
     	qcheck(qcheck__f(law2), "passing (func) = property").

:- func law1(int, int) = property.
law1(X, Y) =  notzero(Y) `===>` qcheck__f( (func) = ((X // Y) `===` (X // Y)) ).

:- func law2(int, int) = property.
law2(X, Y) =  notzero(Y) `===>` ( (X // Y) `===` (X // Y) ).

:- pred notzero(int).
:- mode notzero(in) is semidet.
notzero(X) :- X \= 0.

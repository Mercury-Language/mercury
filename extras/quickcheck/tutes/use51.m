%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module use51.

:- interface.

:- type marks == int.

:- use_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, char, float, string, list.
:- import_module qcheck.

%---------------------------------------------------------------------------%

main -->
     	qcheck(qcheck__f(junk), "just to show the inputs", 5, [], []).

:- func junk(marks, char, float, string) = property.
junk(A, B, C, D) = 
	{A,B,C,D} `>>>` [yes].

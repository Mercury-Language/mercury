%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module use20.

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
     	qcheck(qcheck__f(law), "testing mymax using `===>` for bool"),
     	qcheck(qcheck__f(law2), "testing mymax using `===>` for (pred)").

:- func law(int, int) = property.
law(X, Y) = (X =< Y) `===>` (mymax(X, Y) `===` Y).

:- func law2(int, int) = property.
law2(X, Y) = is_less_equal(X, Y) `===>` 
			        (mymax(X, Y) `===` Y).

:- func is_less_equal(int, int) = bool.
is_less_equal(X, Y) = Bool :-
	(if	X =< Y
  	 then
		Bool = yes
	 else
		Bool = no
	).

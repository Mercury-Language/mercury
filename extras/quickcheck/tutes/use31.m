%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module use31.

:- interface.

:- use_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, list, bool.
:- import_module qcheck, nrev.

%---------------------------------------------------------------------------%

main -->
     	qcheck(qcheck__f(testing1), "testing1"),
     	qcheck(qcheck__f(testing2), "testing2"),
     	qcheck(qcheck__f(testing3), "testing3"),
     	qcheck(qcheck__f(testing4), "testing4").

:- func testing1(list(float)) = property.
testing1(Xs) = 
	nrev(nrev(Xs)) `===` Xs.
	
:- func testing2(list(float)) = property.
testing2(Xs) = 
	to_trivial([], Xs, nrev(nrev(Xs)) `===` Xs).

:- func testing3(list(float)) = property.
testing3(Xs) = 
	to_trivial(1, 
		   list_length(Xs), 
                   to_trivial([], Xs, nrev(nrev(Xs)) `===` Xs)
		  ).

:- func testing4(list(float)) = property.
testing4(Xs) = 
	list_length(Xs) `>>>` (nrev(nrev(Xs)) `===` Xs).

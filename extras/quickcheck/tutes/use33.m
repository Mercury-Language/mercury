%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module use33.

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
     	qcheck(qcheck__f(testing5), "testing5").

:- func testing5(list(float)) = property.
testing5(Xs) = 
	odd_even(Xs) `>>>` 
                     (list_length(Xs) `>>>` (nrev(nrev(Xs)) `===` Xs)).

:- func odd_even(list(T)) = string.
:- mode odd_even(in) = out is det.
odd_even(Xs) = Y :-
	(if	list_length(Xs) mod 2 = 1
	 then
		Y = "odd"
	 else
		Y = "even"
	).

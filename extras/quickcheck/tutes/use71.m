%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module use71.

:- interface.

:- use_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, float, list, std_util.
:- import_module qcheck, nrev.

%---------------------------------------------------------------------------%

main -->
	{ freq_list(F) },	
     	qcheck(qcheck__f(testing4), "testing4", 1000, [], [F]).

:- pred freq_list({type_desc, list(frequency)}).
:- mode freq_list(out) is det.
freq_list(F) :-
	F = { type_of([0.0]), [ { 10, [] } , { 90, [] }  ] }.

:- func testing4(list(float)) = property.
testing4(Xs) = 
	list_length(Xs) `>>>` (nrev(nrev(Xs)) `===` Xs).

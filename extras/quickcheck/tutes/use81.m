%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module use81.

:- interface.

:- use_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, char, std_util, list.
:- import_module qcheck, rnd.

:- type strange(T1, T2)
        --->    wierd(T1, T2, T1).

main -->
	qcheck(qcheck__f(prop1), "prop1", 1000, [], []),
	qcheck(qcheck__f(prop2), "prop2", 1000, [], []),
	qcheck(qcheck__f(prop3), "prop3", 1000, [], []),
	qcheck(qcheck__f(prop4), "prop4", 1000, [], []).

:- func prop1(func(strange(int, char), int) = list(int)) = property.
:- mode prop1(in(func(in,in)=out is det)) = out is det.
prop1(FFF) = 
	FFF(wierd(1, '0', 2), 999) `===` FFF(wierd(1, '0', 2), 999).

:- func prop2(func(strange(int, char), int) = list(int)) = property.
:- mode prop2(in(func(in,in)=out is det)) = out is det.
prop2(FFF) = 
	(if	FFF(wierd(1, '0', 2), 999) = FFF(wierd(1, '0', 2), 888)
	 then
		[ info(univ("equal")) | [yes] ] 
	 else
		[ info(univ("not equal")) | [yes] ] 
	).

:- func prop3(func(int) = int) = property.
:- mode prop3(in(func(in)=out is det)) = out is det.
prop3(FFF) = 
     	(if	FFF(1) = FFF(0)
         then
                [ info(univ("equal")) | [yes] ]
         else
                [ info(univ("not equal")) | [yes] ]
        ).
        
:- func prop4(func(int, int) = int) = property.
:- mode prop4(in(func(in, in)=out is det)) = out is det.
prop4(FFF) = 
     	(if	FFF(101, 10) = FFF(11, 15)
         then
                [ info(univ("equal")) | [yes] ]
         else
                [ info(univ("not equal")) | [yes] ]
	).	

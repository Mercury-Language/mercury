%---------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module use91.

:- interface.

:- use_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, list, std_util.
:- import_module qcheck, rnd.

main -->
        qcheck(qcheck__f(prop1), "user function", 100, [], [], 
	       [{type_of(some_function), gen_f}]),
        qcheck(qcheck__f(prop1), "no user function", 100, [], [], []). 

:- func gen_f(type_desc, list(frequency), list({type_desc, list(frequency)}),
                list(user_gen_type), rnd, rnd) = univ.
:- mode gen_f(in, in, in, list_skel_in(user_gen_inst), in, out) = out is det.
gen_f(_, _, _, _, RS, RS) = Univ :-
	Univ = univ(some_function).

:- func some_function(int) = int.
:- mode some_function(in) = out is det.
some_function(X) = Y :-
	Y = 2 * X.
 
:- func prop1(func(int)=int) = property.
:- mode prop1(in(func(in)=out is det)) = out is det.
prop1(FFF) = FFF(8) `===` (8 * 2).  

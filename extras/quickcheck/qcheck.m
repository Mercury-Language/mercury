%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2003, 2005-2007, 2010 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
%	file 		qcheck.m
%	author:		xcsm
%
%	The source code for autotest generator similar to Haskell's quickcheck. 
%	A user guide should be available at ./tutes/ in html format.
%
% NOTE: this code has not compiled since Mercury 0.13 because it uses
% type class instance whose arguments have higher-order types. As such,
% we do not currently include this in the extras distribution or maintain
% it.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module qcheck.
:- interface.

:- import_module rnd.

:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module io.
:- import_module list.
:- import_module string.
:- import_module type_desc.
:- import_module univ.

%---------------------------------------------------------------------------%

	% The invariant function must return a property to 
	% indicate the result of invariant function.
	% yes	  : success
	% no	  : failure 
	% trivial : mark the property being returned by a trivial test
	% info(univ) : store some arbitrary information in type univ,
	% condition  : mark the property being returned by a test which failed
	%	       the pre-conditional 
:- type property == list(flag).
:- type flag 
	--->	yes
	;	no	
	;	trivial
	;	info(univ)	
	;	condition.

	% Each distribution stores the value in univ and its number of 
	% occurences in the first element.
:- type distribution == {int, univ}.

	%	Each result composes of a property, which is the property 
	%	returned by the invariant test funcion. The second argument
	%	stores the generated inputs in a univ.
:- type result 
	--->	result(property, univ).
	
	%	The required format for specific frequency : 
	%	int : relative frequency of this type being generated
	%	list(list(frequency)) : a constructor may have arity 0 to 
	%				infinity. The outer list is a list 
	%				for constructor arguments.
	%				Inner list states the relative 
	%				frequency of all alternatives for 
	%				1 argument
	%	eg: green(coin, color)
	% 	where coin  ---> head ; tail.
	%	      color ---> black ; white.
	% 	Then frequency should be : {99 , [ [{30,[]},{70,[]}], 
 	%					   [{40,[]},{60,[]}]
	%						 ]
	%				   }
	%	For type coin, there is 30% chance of being head
	%	and 70% chance of being tail.
	%	For type color, there is 40% chance of being black
	%	and 60% chance of bing white.
:- type frequency
	--->	{int, list(list(frequency))}.

	%	user_gen_type is the type format for each of user-defined 
	%	generator.
	%	The first element, type_desc, is the type_of the variable 
	%	which this generator is suppose to handle. The second element
	%	should be a user defined generator. The user defined generator
	%	should take a type_desc, which shows what type is required. It's
	%	followed by a Specific Frequency, a list of General frequency,
	%	a list of custom generators, and finally the input and output 
	%	of random number supply.
:- type user_gen_type 
	--->	{ type_desc, 
		  func(type_desc, list(frequency), 
		       list({type_desc, list(frequency)}), 
		       list(user_gen_type), rnd, rnd) = univ
		}.

	%	user_gen_inst is the instance for each user-defined generator	
:- inst user_gen_inst
	==	bound({ ground, 
		        func(in, in, in, 
			     list_skel_in(user_gen_inst), in, out) = out is det
		      }).

%---------------------------------------------------------------------------%

	% 	The implemented instances are designed for invariant 
	%	functions with 0 to 10 arity, and returning a property.
:- typeclass testable(T) where [
	pred test(T, list(list(frequency)), list({type_desc, list(frequency)}), 
	          list(user_gen_type), qcheck__result, rnd, rnd),
	mode test(in, in, in, list_skel_in(user_gen_inst), out, in, out) is det
].

% Mercury doesn't allow instance declarations for function types.
% Hence we have to wrap up the function types in user-defined
% types f1(T), f2(T1, T2), f3(T1, T2, T3), ...
:- type f0                 	
	---> f((func) = property).
:- type f1(T1)             	
	---> f(func(T1) = property).
:- type f2(T1, T2)         	
	---> f(func(T1, T2) = property).
:- type f3(T1, T2, T3)     	
	---> f(func(T1, T2, T3) = property).
:- type f4(T1, T2, T3, T4) 	
	---> f(func(T1, T2, T3, T4) = property).
:- type f5(T1, T2, T3, T4, T5) 	
	---> f(func(T1, T2, T3, T4, T5) = property).
:- type f6(T1, T2, T3, T4, T5, T6) 
	---> f(func(T1, T2, T3, T4, T5, T6) = property).
:- type f7(T1, T2, T3, T4, T5, T6, T7) 
	---> f(func(T1, T2, T3, T4, T5, T6, T7) = property).
:- type f8(T1, T2, T3, T4, T5, T6, T7, T8) 
	---> f(func(T1, T2, T3, T4, T5, T6, T7, T8) = property).
:- type f9(T1, T2, T3, T4, T5, T6, T7, T8, T9) 
	---> f(func(T1, T2, T3, T4, T5, T6, T7, T8, T9) = property).
:- type f10(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) 
	---> f(func(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) = property).

:- instance testable(f0).
:- instance testable(f1(T1)).
:- instance testable(f2(T1, T2)).
:- instance testable(f3(T1, T2, T3)).
:- instance testable(f4(T1, T2, T3, T4)). 
:- instance testable(f5(T1, T2, T3, T4, T5)). 
:- instance testable(f6(T1, T2, T3, T4, T5, T6)). 
:- instance testable(f7(T1, T2, T3, T4, T5, T6, T7)). 
:- instance testable(f8(T1, T2, T3, T4, T5, T6, T7, T8)). 
:- instance testable(f9(T1, T2, T3, T4, T5, T6, T7, T8, T9)). 
:- instance testable(f10(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)). 



%---------------------------------------------------------------------------%

	% qcheck/4(A, B,             G, H) = qcheck/8(A,B,100,[],[],[],G,H)
	% qcheck/7(A, B, C, D, E     G, H) = qcheck/8(A,B,  C, D, E,[],G,H)
	% qcheck/8(A, B, C, D, E, F, G, H)
	% A : invariant test function, satisfing testable(T)
	% B : some test description
	% C : number of tests to run 
	% D : specific frequency
	% E : general frequency
	% F : list of user-defined generator
	% G : io__state in
	% H : io__state out
	%	
	% Quickcheck is able to generate random values for each input 
	% argument of the invariant function at run time, provided 
	% that there is a default/custom generator for that type. 
	% The default generator is able to generate type int, char, 
	% float, string, string, discriminated union and certain functions.
	% Details are explained in the tutorials.
	%
	% If any test fails, quickcheck will report the failed test,
	% and call io__set_exit_status(1).
	% If all tests pass, quickcheck will print a summary of the
	% tests executed.
	%
	% Quickcheck makes the assumption that the distinction between
	% a function and a discriminated union is that num_functors(FuncType) 
	% fails for a function.
	% As a result of this assumption, the function should not be called
	% where qcheck needs to generate a type that is not supported by the
	% default/custom generators.
:- pred qcheck(T, string, io__state, io__state) <= testable(T).
:- mode qcheck(in, in, di, uo) is det.
:- pred qcheck(T,string, int,list(list(frequency)),
	       list({type_desc, list(frequency)}), 
	       io__state, io__state) <= testable(T).
:- mode qcheck(in, in, in, in, in, di, uo) is det.
:- pred qcheck(T, string, int,list(list(frequency)),
	       list({type_desc, list(frequency)}), 
	       list(user_gen_type), io__state, io__state) <= testable(T). 
:- mode qcheck(in, in, in, in, in, list_skel_in(user_gen_inst), di, uo) is det. 

%---------------------------------------------------------------------------%
	% The following are the default generators for int, char, float,
	% and string. Please refer to tutorials for details. 
:- func rand_int(rnd, rnd) = int.
:- mode rand_int(in, out) = out is det.

:- func rand_char(rnd, rnd) = char.
:- mode rand_char(in, out) = out is det.

:- func rand_float(rnd, rnd) = float. 
:- mode rand_float(in, out) = out is det.

:- func rand_string(rnd, rnd) = string.
:- mode rand_string(in, out) = out is det.

%---------------------------------------------------------------------------%

	% rand_union/6 generates a discriminated union via following steps:
	% 1	It determines what frequency to use at this level
	% 2	Then chooses which constructor of discriminated union to generate,
	%  	and generate the argument list for the particular constructor
	% 3 	And finally call construct/3 to build the term. 
:- func rand_union(type_desc, list(frequency), list({type_desc, 
		   list(frequency)}), list(user_gen_type), rnd, rnd) = univ.
:- mode rand_union(in, in, in, list_skel_in(user_gen_inst), in, out) = out is det.

	% rand_function generates a random forward mode function with types 
	% described in type_desc. However rand_function/3 can only generate
	% functions with arity 0 to 10, it will throw an error if unable to
	% generate the required type.
:- func rand_function(type_desc, rnd, rnd) = univ.
:- mode rand_function(in, in, out) = out is det.

	% rand_allint/2 generates an int with equal distribution over all 
	% possible int. 
:- func rand_allint(rnd, rnd) = int.
:- mode rand_allint(in, out) = out is det.

	% oneof/3 randomly selects an element from the list. 
	% An error will be thrown if the list is empty.
:- func oneof(list(T), rnd, rnd) = T.
:- mode oneof(in, in, out) = out is det.

	% The 1st argument is used as random seed, on average there is 5% chance
	% this function will return 0 regardless of 2nd argument, otherwise
	% this function produce an int that is dependent on its argument's 
	% type and value where the argument can be of any type.
:- func any_to_int(int, T) = int.
:- mode any_to_int(in, in) = out is det.
	
	% value_to_int/1 produce an int that is dependent on its argument's 
        % type and value where the argument can be of any type.
:- func value_to_int(T) = int.
:- mode value_to_int(in) = out is det.

	% The new property will store the value in T 
	% as type univ, later calling function will count
	% the number of occurrence of that value
:- func T `>>>` property = property. 
:- mode in `>>>` in = out is det.

	% If the 1st argument is equal to the 2nd argument   
	% then the new property will be marked as trivial
:- func to_trivial(T, T, property) = property.
:- mode to_trivial(in, in, in) = out is det.

	% If the left argument equals the right, then reture property:[yes],
	% otherwise reture property:[no]
:- func T  `===` T  = property.
:- mode in `===` in = out is det.

	% list_length/1 is the same as the function version of list__length.
	% list__length/1 always returns an int.  The problem is that the 
	% compiler doesn't know whether to call the list__length function, or 
	% pass a curried version of the predicate version of list__length/2.
:- func list_length(list(T)) = int.
:- mode list_length(in) = out is det.

	% If the right argument is a property, then 'flag:condition' will
	% be added into the original property provided the left argument is
	% 'bool:yes' or '(pred):succeed'
	% If the right argument is (func) = property, then `===>` will return
	% 'property:[condition]' without evaluating the (func) if the left
	% argument is 'bool:yes' or '(pred):succeed'.
:- typeclass conditional(T1, T2) where [
	( func T1 `===>` T2 = property ),
	( mode in `===>` in = out is det )
].

:- instance conditional(bool, property).
:- instance conditional(bool, f0).
:- instance conditional((pred), property).
:- instance conditional((pred), f0).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module construct.
:- import_module deconstruct.
:- import_module int.
:- import_module integer.
:- import_module pprint.
:- import_module require.
:- import_module time.

%---------------------------------------------------------------------------%

qcheck(TestFunction, Name) -->
	qcheck(TestFunction, Name, 100, [], [], []).

qcheck(TestFunction, Name, Counter, SpecificFrequency, GeneralFrequency) -->
	qcheck(TestFunction, Name, Counter, 
	       SpecificFrequency, GeneralFrequency, []).

	% qcheck/8 first seed the random number on local time, then it  
	% generates the reqiured inputs for the invariant function and runs 
	% it. A table of test results are shown at the end. 
qcheck(TestFunction, Name, TestCount, SpecificFrequency,
       GeneralFrequency, Generators) -->
	time__time(CurrentTime),
	{ init_setup(generate_seed_from_time(CurrentTime), RS0) },
      	testing(TestFunction, Name, SpecificFrequency, GeneralFrequency, 
		Generators, RS0, _, TestCount, YesCount, NoFlag, 
		TrivialCount, Distributions, FailedConditionCount),
	(if 	{ NoFlag = yes }	
	 then
	    	io__set_exit_status(1)
	 else
	    	io__write_string("\nTest Description : "),
	    	io__write_string(Name),
            	io__write_string("\nNumber of test cases that succeeded : "),
            	io__write_int(YesCount),
	    	io__write_string("\nNumber of trivial tests : "),
	    	io__write_int(TrivialCount),
	    	io__write_string("\nNumber of tests cases "),
	    	io__write_string("which failed the pre-condition : "),
	    	io__write_int(FailedConditionCount),
	    	io__write_string("\nDistributions of selected arguments : \n"),
	    	{ list__sort(Distributions, Distributions_Sorted) },
	    	show_dist(Distributions_Sorted),
            	io__nl
	).

%---------------------------------------------------------------------------%

	% generate_seed_from_time/1 converts the input time in calendar time
	% format to the number of seconds from the beginning of current year.
	% The numbers of seconds is reduced to an appropriate range for int to
	% avoid overflow.
:- func generate_seed_from_time(time_t) = int.
:- mode generate_seed_from_time(in) = out.
generate_seed_from_time(CurrentTime) =  Seed :-
        tm(Year, _Month, _MonthDay, Hours, Minutes, Seconds, Yearday, 
		_Weekday, _DST) = time__localtime(CurrentTime),
	TotalSecs = ((( integer(Year) * integer(365) + integer(Yearday)) 
		      * integer(24) + integer(Hours)) 
		      * integer(60) + integer(Minutes)) 
		      * integer(60) + integer(Seconds),
	Seed = int(TotalSecs mod integer(max_int)) + 1.

	% testing/15 recursively runs the test until TestCount drops to 0.
	% After each test it updates the statistics.
:- pred testing(T, string, list(list(frequency)),  
		list({type_desc, list(frequency)}), 
	        list(user_gen_type), rnd, rnd, int, int, bool, int, 
	        list(distribution), int, io__state, io__state) <= testable(T).
:- mode testing(in, in, in, in, list_skel_in(user_gen_inst), 
		in, out, in, out, out, out, out, out, di, uo) is det.
testing(TestFunction, Name, SpecificFrequency, GeneralFrequency, Generators, 
        RS0, RS, TestCount, YesCount, NoFlag, TrivialCount, Distribution, FailedConditionCount, S0, S) :- 
	(if   	TestCount =< 0
	 then 
	 	YesCount = 0,
	      	NoFlag = no,
	      	TrivialCount = 0,
	      	Distribution = [],
	      	FailedConditionCount = 0,
	      	RS = RS0,
	      	S = S0
	 else 
		test(TestFunction, SpecificFrequency, GeneralFrequency, 
	 	     Generators, Result, RS0, RS1),
	      	result(P, Univ) = Result,
	        (if	member(no, P),
			not member(condition, P)
	         then
			io__write_string("\nTest description : ", S0, S1),
			io__write_string(Name, S1, S2),
			io__write_string("\nFalsifiable : \n", S2, S3),
			display_univ(Univ, S3, S),
			RS = RS1,
			YesCount = 0,
			NoFlag = yes,
			TrivialCount = 0,
			Distribution  = [],
			FailedConditionCount = 0
	         else
	      		testing(TestFunction, Name, SpecificFrequency, 
				GeneralFrequency, 
	              		Generators, RS1, RS, TestCount - 1, YesCount0, 
				NoFlag0, TrivialCount0,
	      	      		Distribution0, FailedConditionCount0, S0, S),
			NoFlag = NoFlag0,
	      		update(P, YesCount0, TrivialCount0, 
	             	       Distribution0, FailedConditionCount0, 
	      	               YesCount, TrivialCount, Distribution, 
			       FailedConditionCount) 
	        )
	).

 	% update/8 analyses the current invariant test result and updates
	% the statistic accordingly
:- pred update(property, int, int, list(distribution), int, 
               int, int, list(distribution), int).
:- mode update(in, in, in, in, in, out, out, out, out) is det.
update(P, YesCount0, TrivialCount0, Distribution0, FailedConditionCount0,
          YesCount,  TrivialCount,  Distribution,  FailedConditionCount) :-
	(if	member(condition, P)
	 then
	      	YesCount = YesCount0,
		TrivialCount = TrivialCount0,
		Distribution = Distribution0,
		FailedConditionCount = FailedConditionCount0 + 1
	 else
	 	YesCount = YesCount0 + 1,
		FailedConditionCount = FailedConditionCount0,
		update_dist(P, Distribution0, Distribution),
	 	(if	member(trivial, P)
		 then
		        TrivialCount = TrivialCount0 + 1
	         else
	                TrivialCount = TrivialCount0
		)
	).

	% update_dist/3 recursively exacts the 'flag:info(univ)' from property
	% and adds it to the master list of distribution.
:- pred update_dist(property, list(distribution), list(distribution)).
:- mode update_dist(in, in, out) is det.
update_dist([], Distribution, Distribution).
update_dist([Property | Propertys], Distribution0, Distribution) :-
	(if	Property = info(Univ)
	 then
		update_dist_2(Univ, Distribution0, Distribution1)
	 else
		Distribution1 = Distribution0
	),
	update_dist(Propertys, Distribution1, Distribution).

	% If the first argument is alreay stored in the master list, then 
	% update_dist_2/3 just increments the counter, otherwise adds it to 
	% the list with a new counter being 1.
:- pred update_dist_2(univ, list(distribution), list(distribution)).
:- mode update_dist_2(in, in, out) is det.
update_dist_2(Univ, [], [{1, Univ}]).
update_dist_2(Univ, [Distribution | Distributions], Output) :-
	Distribution = {Counter, Patten},
	(if	Univ = Patten  
	 then
	 	Output = [{Counter + 1, Univ} | Distributions]
	 else
	 	update_dist_2(Univ, Distributions, Output1),
		Output = [Distribution | Output1]
	).

%---------------------------------------------------------------------------%

	% The following are the 11 instances of testable
	% Each instance began by inst cast the invariant test function from
	% ground to func(in, in, ...) = out. 
	% Then the specific frequency is extracted; if the SF list is shorter  
	% than the list of arguments, then the last few argument will have
	% [] as their SF; if the SF list is longer than the list of arguments,
	% then the list few SF is not used. 
	% test/7 then calls gen/6 to generate the arguments, the invariant
	% function is then run with those arguments. 
	% The test result is returned along with the arguments generated.

:- instance testable(f0) where [
 	(test(F, _, _, _, R) --> 
		{ inst_cast_f0(F, NF) },
		{ univ({"no argument"}) = Args },
		{ R = result(apply(NF), Args)  })		
].

:- instance testable(f1(T1)) where [
	(test(F, SF0, GF, Generators, R) -->
		{ inst_cast_f1(F, NF) },
		gen(X, det_headlist(SF0, _), GF, Generators),
		{ univ({X}) = Args },
		{ R = result(NF(X), Args) })
].

:- instance testable(f2(T1, T2)) where [
	(test(F, SF0, GF, Generators, R) -->
		{ inst_cast_f2(F, NF) },
		gen(X, det_headlist(SF0, SF1), GF, Generators),
		gen(Y, det_headlist(SF1,  _), GF, Generators), 
		{ univ({X, Y}) = Args },
		{ R = result(NF(X, Y), Args) })
].

:- instance testable(f3(T1, T2, T3)) where [
	(test(F, SF0, GF, Generators, R) -->
		{ inst_cast_f3(F, NF) },
		gen(X, det_headlist(SF0, SF1), GF, Generators), 
		gen(Y, det_headlist(SF1, SF2), GF, Generators),
		gen(Z, det_headlist(SF2, _), GF, Generators),
		{ univ({X, Y, Z}) = Args },
		{ R = result(apply(NF, X, Y, Z), Args) })
].

:- instance testable(f4(T1, T2, T3, T4)) where [ 
        (test(F, SF0, GF, Generators, R) -->
	        { inst_cast_f4(F, NF) },
                gen(X1, det_headlist(SF0, SF1), GF, Generators), 
		gen(X2, det_headlist(SF1, SF2), GF, Generators), 
		gen(X3, det_headlist(SF2, SF3), GF, Generators), 
		gen(X4, det_headlist(SF3,   _), GF, Generators), 
		{ univ({X1, X2, X3, X4}) = Args },
		{ R = result(NF(X1, X2, X3, X4), Args) })
].

:- instance testable(f5(T1, T2, T3, T4, T5)) where [ 
        (test(F, SF0, GF, Generators, R) -->
	        { inst_cast_f5(F, NF) },
                gen(X1, det_headlist(SF0, SF1), GF, Generators), 
		gen(X2, det_headlist(SF1, SF2), GF, Generators), 
		gen(X3, det_headlist(SF2, SF3), GF, Generators), 
		gen(X4, det_headlist(SF3, SF4), GF, Generators), 
		gen(X5, det_headlist(SF4,   _), GF, Generators), 
		{ univ({X1, X2, X3, X4, X5}) = Args },
		{ R = result(NF(X1, X2, X3, X4, X5), Args) })
].

:- instance testable(f6(T1, T2, T3, T4, T5, T6)) where [ 
        (test(F, SF0, GF, Generators, R) -->
	        { inst_cast_f6(F, NF) },
                gen(X1, det_headlist(SF0, SF1), GF, Generators), 
		gen(X2, det_headlist(SF1, SF2), GF, Generators), 
		gen(X3, det_headlist(SF2, SF3), GF, Generators), 
		gen(X4, det_headlist(SF3, SF4), GF, Generators), 
		gen(X5, det_headlist(SF4, SF5), GF, Generators), 
		gen(X6, det_headlist(SF5,   _), GF, Generators), 
		{ univ({X1, X2, X3, X4, X5, X6}) = Args },
		{ R = result(NF(X1, X2, X3, X4, X5, X6), Args) })
].

:- instance testable(f7(T1, T2, T3, T4, T5, T6, T7)) where [ 
        (test(F, SF0, GF, Generators, R) -->
	        { inst_cast_f7(F, NF) },
                gen(X1, det_headlist(SF0, SF1), GF, Generators), 
		gen(X2, det_headlist(SF1, SF2), GF, Generators), 
		gen(X3, det_headlist(SF2, SF3), GF, Generators), 
		gen(X4, det_headlist(SF3, SF4), GF, Generators), 
		gen(X5, det_headlist(SF4, SF5), GF, Generators), 
		gen(X6, det_headlist(SF5, SF6), GF, Generators), 
		gen(X7, det_headlist(SF6,   _), GF, Generators), 
		{ univ({X1, X2, X3, X4, X5, X6, X7}) = Args },
		{ R = result(NF(X1, X2, X3, X4, X5, X6, X7), Args) })
].

:- instance testable(f8(T1, T2, T3, T4, T5, T6, T7, T8)) where [ 
        (test(F, SF0, GF, Generators, R) -->
	        { inst_cast_f8(F, NF) },
                gen(X1, det_headlist(SF0, SF1), GF, Generators), 
		gen(X2, det_headlist(SF1, SF2), GF, Generators), 
		gen(X3, det_headlist(SF2, SF3), GF, Generators), 
		gen(X4, det_headlist(SF3, SF4), GF, Generators), 
		gen(X5, det_headlist(SF4, SF5), GF, Generators), 
		gen(X6, det_headlist(SF5, SF6), GF, Generators), 
		gen(X7, det_headlist(SF6, SF7), GF, Generators), 
		gen(X8, det_headlist(SF7,   _), GF, Generators), 
		{ univ({X1, X2, X3, X4, X5, X6, X7, X8}) = Args },
		{ R = result(NF(X1, X2, X3, X4, X5, X6, X7, X8), Args) })
].

:- instance testable(f9(T1, T2, T3, T4, T5, T6, T7, T8, T9)) where [ 
        (test(F, SF0, GF, Generators, R) -->
	        { inst_cast_f9(F, NF) },
                gen(X1, det_headlist(SF0, SF1), GF, Generators), 
		gen(X2, det_headlist(SF1, SF2), GF, Generators), 
		gen(X3, det_headlist(SF2, SF3), GF, Generators), 
		gen(X4, det_headlist(SF3, SF4), GF, Generators), 
		gen(X5, det_headlist(SF4, SF5), GF, Generators), 
		gen(X6, det_headlist(SF5, SF6), GF, Generators), 
		gen(X7, det_headlist(SF6, SF7), GF, Generators), 
		gen(X8, det_headlist(SF7, SF8), GF, Generators), 
		gen(X9, det_headlist(SF8,   _), GF, Generators), 
		{ univ({X1, X2, X3, X4, X5, X6, X7, X8, X9}) = Args },
		{ R = result(NF(X1, X2, X3, X4, X5, X6, X7, X8, X9), Args) })
].

:- instance testable(f10(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)) where [ 
        (test(F, SF0, GF, Generators, R) -->
	        { inst_cast_f10(F, NF) },
                gen(X1, det_headlist(SF0, SF1), GF, Generators), 
		gen(X2, det_headlist(SF1, SF2), GF, Generators), 
		gen(X3, det_headlist(SF2, SF3), GF, Generators), 
		gen(X4, det_headlist(SF3, SF4), GF, Generators), 
		gen(X5, det_headlist(SF4, SF5), GF, Generators), 
		gen(X6, det_headlist(SF5, SF6), GF, Generators), 
		gen(X7, det_headlist(SF6, SF7), GF, Generators), 
		gen(X8, det_headlist(SF7, SF8), GF, Generators), 
		gen(X9, det_headlist(SF8, SF9), GF, Generators), 
		gen(X10, det_headlist(SF9,  _), GF, Generators), 
		{ univ({X1, X2, X3, X4, X5, X6, X7, X8, X9, X10}) = Args },
		{ R = result(NF(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10),Args)})
].


%---------------------------------------------------------------------------%

	% The following are the 11 inst casts, each corresponding to the 11 
	% insances of testable.
:- pred inst_cast_f0(f0, (func) = property).
:- mode inst_cast_f0(in, out((func) = out is det)) is det.
:- pragma foreign_proc("C", inst_cast_f0(F0::in, F1::out((func) = out is det)),
	[promise_pure, thread_safe, will_not_call_mercury],
	"F1 = F0;").

:- pred inst_cast_f1(f1(T1), func(T1) = property).
:- mode inst_cast_f1(in, out(func(in) = out is det)) is det.
:- pragma foreign_proc("C", inst_cast_f1(F0::in, F1::out(func(in) = out is det)),
	[promise_pure, thread_safe, will_not_call_mercury],
	"F1 = F0;").

:- pred inst_cast_f2(f2(T1, T2), func(T1, T2) = property).
:- mode inst_cast_f2(in, out(func(in, in) = out is det)) is det.
:- pragma foreign_proc("C", inst_cast_f2(F0::in, F1::out(func(in, in) = out is det)),
	[promise_pure, thread_safe, will_not_call_mercury],
	"F1 = F0;").

:- pred inst_cast_f3(f3(T1, T2, T3), func(T1, T2, T3) = property).
:- mode inst_cast_f3(in, out(func(in, in, in) = out is det)) is det.
:- pragma foreign_proc("C", inst_cast_f3(F0::in, F1::out(func(in, in, in) = out is det)),
	[promise_pure, thread_safe, will_not_call_mercury],
	"F1 = F0;").

:- pred inst_cast_f4(f4(T1, T2, T3, T4), func(T1, T2, T3, T4) = property).
:- mode inst_cast_f4(in, out(func(in, in, in, in) = out is det)) is det.
:- pragma foreign_proc("C", inst_cast_f4(F0::in, 
			      F1::out(func(in, in, in, in) = out is det)),
        [promise_pure, thread_safe, will_not_call_mercury],
	"F1 = F0;").

:- pred inst_cast_f5(f5(T1, T2, T3, T4, T5), 
		     func(T1, T2, T3, T4, T5) = property).
:- mode inst_cast_f5(in, out(func(in, in, in, in, in) = out is det)) is det.
:- pragma foreign_proc("C", inst_cast_f5(F0::in, 
	  	              F1::out(func(in, in, in, in, in)=out is det)),
        [promise_pure, thread_safe, will_not_call_mercury],
	"F1 = F0;").

:- pred inst_cast_f6(f6(T1, T2, T3, T4, T5, T6), 
		     func(T1, T2, T3, T4, T5, T6) = property).
:- mode inst_cast_f6(in, out(func(in, in, in, in, in, in) = out is det)) is det.
:- pragma foreign_proc("C", inst_cast_f6(F0::in, 
	  	              F1::out(func(in, in, in, in, in, in)
			      		   = out is det)),
        [promise_pure, thread_safe, will_not_call_mercury],
	"F1 = F0;").

:- pred inst_cast_f7(f7(T1, T2, T3, T4, T5, T6, T7), 
		     func(T1, T2, T3, T4, T5, T6, T7) = property).
:- mode inst_cast_f7(in, out(func(in, in, in, in, in, in, in) 
		                  = out is det)) is det.
:- pragma foreign_proc("C", inst_cast_f7(F0::in, 
	  	              F1::out(func(in, in, in, in, in, in, in) 
			      	           = out is det)),
        [promise_pure, thread_safe, will_not_call_mercury],
	"F1 = F0;").

:- pred inst_cast_f8(f8(T1, T2, T3, T4, T5, T6, T7, T8), 
		     func(T1, T2, T3, T4, T5, T6, T7, T8) = property).
:- mode inst_cast_f8(in, out(func(in, in, in, in, in, in, in, in) 
			          = out is det)) is det.
:- pragma foreign_proc("C", inst_cast_f8(F0::in, 
	  	     F1::out(func(in, in, in, in, in, in, in, in) 
		     	     = out is det)),
        [promise_pure, thread_safe, will_not_call_mercury],
	"F1 = F0;").

:- pred inst_cast_f9(f9(T1, T2, T3, T4, T5, T6, T7, T8, T9), 
		     func(T1, T2, T3, T4, T5, T6, T7, T8, T9) = property).
:- mode inst_cast_f9(in, 
		     out(func(in, in, in, in, in, in, in, in, in) 
		     	      = out is det)) is det.
:- pragma foreign_proc("C", inst_cast_f9(F0::in, 
	  	              F1::out(func(in, in, in, in, in, in, 
			                   in, in, in) = out is det)),
        [promise_pure, thread_safe, will_not_call_mercury],
	"F1 = F0;").

:- pred inst_cast_f10(f10(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10), 
		      func(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) = property).
:- mode inst_cast_f10(in, 
		      out(func(in, in, in, in, in, in, in, in, in, in) 
		               = out is det)) is det.
:- pragma foreign_proc("C", inst_cast_f10(F0::in, 
	  	               F1::out(func(in, in, in, in, in, in, 
			       		    in, in, in, in) = out is det)),
        [promise_pure, thread_safe, will_not_call_mercury],
	"F1 = F0;").

%---------------------------------------------------------------------------%

	% The following are the 4 instances of conditional, and an inst cast  
	% from ground to (pred)

:- instance conditional(bool, property) where [
	(	Left `===>` Right = Property :-
			(if	Left = yes
	 	 	 then
				Property = Right
	 	 	 else
				Property = [condition|Right]
			)
	)
].		

:- instance conditional(bool, f0) where [
	(	Left `===>` Right = Property :-
			inst_cast_f0(Right, Right_Cast),
			(if	Left = yes
	 	 	 then
				Property = apply(Right_Cast)
	 	 	 else
				Property = [condition]
			)
	)
].		

:- instance conditional((pred), property) where [ 
	(	Left `===>` Right = Property :- 
			inst_cast_p0(Left, Left_Cast),	
			(if 	call(Left_Cast)
	 	 	 then
				Property = Right
	  	 	 else
				Property = [condition|Right]
			)
	)
].

:- instance conditional((pred), f0) where [ 
	(	Left `===>` Right = Property :- 
			inst_cast_p0(Left, Left_Cast),	
			inst_cast_f0(Right, Right_Cast),
			(if 	call(Left_Cast)
	 	 	 then
				Property = apply(Right_Cast)
	  	 	 else
				Property = [condition]
			)
	)
].

:- pred inst_cast_p0((pred), (pred)).
:- mode inst_cast_p0(in, out((pred) is semidet)) is det.
:- pragma foreign_proc("C", inst_cast_p0(F0::in, F1::out((pred) is semidet)),
	[promise_pure, thread_safe, will_not_call_mercury],
	"F1 = F0;").

%---------------------------------------------------------------------------%

	% gen/6 calls gen_arg/7 to generate the first argument of gen/6, 
:- pred gen(T, list(frequency), list({type_desc, list(frequency)}), 
	    list(user_gen_type), rnd, rnd).
:- mode gen(out, in, in, list_skel_in(user_gen_inst), in, out) is det.
gen(Term, Frequencys, GF, Generators, RS0, RS) :-
    	Type = type_of(Term),
    	gen_arg(Type, Frequencys, GF, Generators, Univ, RS0, RS),
	det_univ_to_type(Univ, Term).

	% gen_arg/7 searches for a user-defined generator first, if it is found,
	% then runs the generator. Otherwise, gen_arg/7 will try to determine 
	% which type it is suppose to generate, and calls the appropriate 
	% default generator. rand_function/3 will be called only if the required
	% type is not int, char, float, string, or discriminated union.
:- pred gen_arg(type_desc, list(frequency), list({type_desc, list(frequency)}),
	        list(user_gen_type), univ, rnd, rnd).
:- mode gen_arg(in, in, in, list_skel_in(user_gen_inst), out, in, out) is det.
gen_arg(Datatype, Frequencys, GF, UserGenerators, Univ, RS0, RS) :-
	(if	find_user_gen(Datatype, UserGenerators, UserGenerator)
	 then
		Univ = UserGenerator(Datatype, Frequencys, GF, UserGenerators,
				      RS0, RS)
	 else
		   (	Datatype = type_of(0) -> 
				Temp = rand_int(RS0, RS),
				Univ = univ(Temp)
		   ;	Datatype = type_of('0') ->
				Temp = rand_char(RS0, RS),
				Univ = univ(Temp)
		   ;	Datatype = type_of(0.0) ->
				Temp = rand_float(RS0, RS),
				Univ = univ(Temp)
		   ;	Datatype = type_of("String") ->
				Temp = rand_string(RS0, RS),
				Univ = univ(Temp)
		   ;	\+ num_functors(Datatype) = _ ->
                                Univ = rand_function(Datatype, RS0, RS)
		   ; 
		    		Univ = rand_union(Datatype, Frequencys, 
						  GF, UserGenerators, RS0, RS)
		   )
	).

	% gen_arg_list/7 is similar to gen_arg/7, except it generates a list of
	% univ, instead of just 1 univ as in gen_arg/7.
	% gen_arg_list/7 recursively calls gen_arg/7 until the list is empty.
:- pred gen_arg_list(list(type_desc), list(list(frequency)), 
                     list({type_desc, list(frequency)}), list(user_gen_type), 
		     list(univ), rnd, rnd).
:- mode gen_arg_list(in,in,in,list_skel_in(user_gen_inst),out,in,out) is det.
gen_arg_list([], _, _,  _, [], RS, RS).
gen_arg_list([Type|Types], FrequencyList, GF, UserGenerators, [Univ|Univs], 
	     RS0, RS) :-
		(	FrequencyList = [],
			F = [],
			Fs = []
		;
		 	FrequencyList = [F | Fs]
		),
		gen_arg(Type, F, GF, UserGenerators, Univ, RS0, RS1),
		gen_arg_list(Types, Fs, GF, UserGenerators, Univs, RS1, RS).

%---------------------------------------------------------------------------%
	
	% generate an int of N Bits 
:- pred next(int, int, rnd, rnd).
:- mode next(in, out, in, out) is det.
next(N, Bits, Rnd0, Rnd) :-
    	rnd(F, Rnd0, Rnd),
	Range = pow(2.0, N) - 1.0,
    	Bits = round_to_int(F * Range). 

	% generate int
rand_int(BS0, BS) = Int :-
	Temp = rand_allint(BS0, BS1) rem 2,
	(if	Temp = 0 
	 then
		irange(-100, 100, Int, BS1, BS)
	 else
		Int = rand_allint(BS1, BS)
	).  

	% generate int
rand_allint(BS0, BS) = Int :-
	next(1, Sign, BS0, BS1),
	next(31, TempInt, BS1, BS),
	( Sign > 0 ->
	    Int =  TempInt 
	;
	    Int = -TempInt
	).

	% generate char
rand_char(RS0, RS) = Char :-
       		Int = rand_allint(RS0, RS1) rem 1000,
		(if	char__to_int(Char0, Int) 
		 then
		 	Char = Char0,
			RS = RS1
		 else
		 	Char = rand_char(RS1, RS)
		).

	% generate float
rand_float(BS0, BS) = Flt :-
	next(31, Mant0, BS0, BS1),
        next(1, Sign, BS1, BS2),
        ( Sign > 0 ->
            Mant = Mant0
        ;
            Mant = -Mant0
        ),
        next(7, Exp, BS2, BS3),
        next(1, ExpSign, BS3, BS),
        Flt0 = float(Mant) * pow(2.0, Exp),
        ( ExpSign > 0, Flt0 \= 0.0 ->
            Flt = 1.0/Flt0
        ;
            Flt = Flt0
        ).

	% generate string
rand_string(RS0, RS) = X :-
	     	gen(Charlist, [], [ {type_of(['A']), [{10, []}, {90, []}]} ],
		    [], RS0, RS),
		string__from_char_list(Charlist,X).

	% generate discriminated union
rand_union(Datatype, Frequencys, GF, UserGenerators, RS0, RS) = Univ :-
		NumFunctors = det_num_functors(Datatype),
		TempFreq = get_freq(Datatype, NumFunctors, 0, Frequencys, GF),
        	rnd__irange(0, freq_base(TempFreq) - 1, Selector, RS0, RS1),
		{ Branch, SubBranch } = select_branch(Selector, 0, 0, TempFreq),  
		(if
			get_functor(Datatype, Branch, _, _,
				PseudoArgTypesTemp),
			ArgTypesTemp = list.map(
				ground_pseudo_type_desc_to_type_desc_det,
				PseudoArgTypesTemp)
	 	 then
			ArgTypes = ArgTypesTemp,
			gen_arg_list(ArgTypes, SubBranch, GF,
			 	     UserGenerators, ArgList, RS1, RS), 
			(if	Temp = construct(Datatype, Branch, ArgList)
	 	 	 then
		 		Univ = Temp
		 	 else
				error("rand_union/6 : construct/3 failed")
			)
	 	 else
	 		error("rand_union/6 : get_functor/5 failed")
		).


	% generate random function
rand_function(Type, RS0, RS) = Univ :-
	rnd__irange(1, max_int_argument, X0,  RS0, RS1),
	rnd__irange(1, max_int_argument, X1,  RS1, RS2),
	rnd__irange(1, max_int_argument, X2,  RS2, RS3),
	rnd__irange(1, max_int_argument, X3,  RS3, RS4),
	rnd__irange(1, max_int_argument, X4,  RS4, RS5),
	rnd__irange(1, max_int_argument, X5,  RS5, RS6),
	rnd__irange(1, max_int_argument, X6,  RS6, RS7),
	rnd__irange(1, max_int_argument, X7,  RS7, RS8),
	rnd__irange(1, max_int_argument, X8,  RS8, RS9),
	rnd__irange(1, max_int_argument, X9,  RS9, RS10),
	rnd__irange(1, max_int_argument, X10, RS10, RS),
	type_ctor_and_args(Type, _, Args),
	(	Args = [RetType] ->
	 		has_type(RetVal, RetType),
			Func = dummy(X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10), 
			nailFuncType(RetVal, Func),
			Univ = univ(Func)
	; 	Args = [ArgType1, RetType]  ->
			has_type(Arg1, ArgType1),
			has_type(RetVal, RetType),
			Func = dummy(X0,X1,X2,X3,X4,X5,X6,X7,X8,X9), 
			nailFuncType(Arg1, RetVal, Func),
			Univ = univ(Func)
	; 	Args = [ArgType1, ArgType2, RetType] ->
			has_type(Arg1, ArgType1),
			has_type(Arg2, ArgType2),
			has_type(RetVal, RetType),
			Func = dummy(X0,X1,X2,X3,X4,X5,X6,X7,X8), 
			nailFuncType(Arg1, Arg2, RetVal, Func),
			Univ = univ(Func)
	;	Args = [ArgType1, ArgType2, ArgType3, RetType] ->
			has_type(Arg1, ArgType1),
			has_type(Arg2, ArgType2),
			has_type(Arg3, ArgType3),
			has_type(RetVal, RetType),
			Func = dummy(X0,X1,X2,X3,X4,X5,X6,X7), 
			nailFuncType(Arg1, Arg2, Arg3, RetVal, Func),
			Univ = univ(Func)
	;	Args = [ArgType1, ArgType2, ArgType3, ArgType4, RetType] ->
			has_type(Arg1, ArgType1),
			has_type(Arg2, ArgType2),
			has_type(Arg3, ArgType3),
			has_type(Arg4, ArgType4),
			has_type(RetVal, RetType),
			Func = dummy(X0,X1,X2,X3,X4,X5,X6), 
			nailFuncType(Arg1, Arg2, Arg3, Arg4, RetVal, Func),
			Univ = univ(Func)
	;	Args = [ArgType1, ArgType2, ArgType3, ArgType4, 
	 		ArgType5, RetType] ->
			has_type(Arg1, ArgType1),
			has_type(Arg2, ArgType2),
			has_type(Arg3, ArgType3),
			has_type(Arg4, ArgType4),
			has_type(Arg5, ArgType5),
			has_type(RetVal, RetType),
			Func = dummy(X0,X1,X2,X3,X4,X5), 
			nailFuncType(Arg1, Arg2, Arg3, Arg4, Arg5, 
				     RetVal, Func),
			Univ = univ(Func)
	;	Args = [ArgType1, ArgType2, ArgType3, ArgType4, 
	 		ArgType5, ArgType6, RetType] ->
			has_type(Arg1, ArgType1),
			has_type(Arg2, ArgType2),
			has_type(Arg3, ArgType3),
			has_type(Arg4, ArgType4),
			has_type(Arg5, ArgType5),
			has_type(Arg6, ArgType6),
			has_type(RetVal, RetType),
			Func = dummy(X0,X1,X2,X3,X4), 
			nailFuncType(Arg1, Arg2, Arg3, Arg4, Arg5, 
				     Arg6, RetVal, Func),
			Univ = univ(Func)
	;	Args = [ArgType1, ArgType2, ArgType3, ArgType4, 
	 		ArgType5, ArgType6, ArgType7, RetType] ->
			has_type(Arg1, ArgType1),
			has_type(Arg2, ArgType2),
			has_type(Arg3, ArgType3),
			has_type(Arg4, ArgType4),
			has_type(Arg5, ArgType5),
			has_type(Arg6, ArgType6),
			has_type(Arg7, ArgType7),
			has_type(RetVal, RetType),
			Func = dummy(X0,X1,X2,X3), 
			nailFuncType(Arg1, Arg2, Arg3, Arg4, Arg5, 
				     Arg6, Arg7, RetVal, Func),
			Univ = univ(Func)
	;	Args = [ArgType1, ArgType2, ArgType3, ArgType4, 
	 		ArgType5, ArgType6, ArgType7, ArgType8, 
			RetType] ->
			has_type(Arg1, ArgType1),
			has_type(Arg2, ArgType2),
			has_type(Arg3, ArgType3),
			has_type(Arg4, ArgType4),
			has_type(Arg5, ArgType5),
			has_type(Arg6, ArgType6),
			has_type(Arg7, ArgType7),
			has_type(Arg8, ArgType8),
			has_type(RetVal, RetType),
			Func = dummy(X0,X1,X2), 
			nailFuncType(Arg1, Arg2, Arg3, Arg4, Arg5, 
				     Arg6, Arg7, Arg8, RetVal, Func),
			Univ = univ(Func)
	; 	Args = [ArgType1, ArgType2, ArgType3, ArgType4, 
	 		ArgType5, ArgType6, ArgType7, ArgType8, 
			ArgType9, RetType] ->
			has_type(Arg1, ArgType1),
			has_type(Arg2, ArgType2),
			has_type(Arg3, ArgType3),
			has_type(Arg4, ArgType4),
			has_type(Arg5, ArgType5),
			has_type(Arg6, ArgType6),
			has_type(Arg7, ArgType7),
			has_type(Arg8, ArgType8),
			has_type(Arg9, ArgType9),
			has_type(RetVal, RetType),
			Func = dummy(X0,X1), 
			nailFuncType(Arg1, Arg2, Arg3, Arg4, Arg5, 
				     Arg6, Arg7, Arg8, Arg9, RetVal, Func),
			Univ = univ(Func)
	;	Args = [ArgType1, ArgType2, ArgType3, ArgType4, 
	 		ArgType5, ArgType6, ArgType7, ArgType8, 
			ArgType9, ArgType10, RetType] ->
			has_type(Arg1, ArgType1),
			has_type(Arg2, ArgType2),
			has_type(Arg3, ArgType3),
			has_type(Arg4, ArgType4),
			has_type(Arg5, ArgType5),
			has_type(Arg6, ArgType6),
			has_type(Arg7, ArgType7),
			has_type(Arg8, ArgType8),
			has_type(Arg9, ArgType9),
			has_type(Arg10, ArgType10),
			has_type(RetVal, RetType),
			Func = dummy(X0), 
			nailFuncType(Arg1, Arg2, Arg3, Arg4, Arg5, 
				     Arg6, Arg7, Arg8, Arg9, Arg10,
				     RetVal, Func),
			Univ = univ(Func)
	;	
			error("no default generator for this type") 
	).

%---------------------------------------------------------------------------%

	% There are 11 version of nailFuncType correspond to each arity of random funcion
:- pred nailFuncType(T, (func) = T).
:- mode nailFuncType(unused, unused) is det.

:- pred nailFuncType(T1, T, func(T1) = T).
:- mode nailFuncType(unused, unused, unused) is det.

:- pred nailFuncType(T1, T2, T, func(T1, T2) = T).
:- mode nailFuncType(unused, unused, unused, unused) is det.

:- pred nailFuncType(T1, T2, T3, T, func(T1, T2, T3) = T).
:- mode nailFuncType(unused, unused, unused, unused, unused) is det.

:- pred nailFuncType(T1, T2, T3, T4, T, func(T1, T2, T3, T4) = T).
:- mode nailFuncType(unused, unused, unused, unused, unused, unused) is det.

:- pred nailFuncType(T1, T2, T3, T4, T5, T, func(T1, T2, T3, T4, T5) = T).
:- mode nailFuncType(unused, unused, unused, unused, unused, 
		     unused, unused) is det.

:- pred nailFuncType(T1, T2, T3, T4, T5, T6, T, 
		     func(T1, T2, T3, T4, T5, T6) = T).
:- mode nailFuncType(unused, unused, unused, unused, unused, 
		     unused, unused, unused) is det.

:- pred nailFuncType(T1, T2, T3, T4, T5, T6, T7, T, 
		     func(T1, T2, T3, T4, T5, T6, T7) = T).
:- mode nailFuncType(unused, unused, unused, unused, unused, 
		     unused, unused, unused, unused) is det.

:- pred nailFuncType(T1, T2, T3, T4, T5, T6, T7, T8, T, 
		     func(T1, T2, T3, T4, T5, T6, T7, T8) = T).
:- mode nailFuncType(unused, unused, unused, unused, unused, 
		     unused, unused, unused, unused, unused) is det.

:- pred nailFuncType(T1, T2, T3, T4, T5, T6, T7, T8, T9, T, 
		     func(T1, T2, T3, T4, T5, T6, T7, T8, T9) = T).
:- mode nailFuncType(unused, unused, unused, unused, unused, unused,
		     unused, unused, unused, unused, unused) is det.

:- pred nailFuncType(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T, 
		     func(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) = T).
:- mode nailFuncType(unused, unused, unused, unused, unused, unused, 
		     unused, unused, unused, unused, unused, unused) is det.

nailFuncType(_, _).
nailFuncType(_, _, _).
nailFuncType(_, _, _, _).
nailFuncType(_, _, _, _, _).
nailFuncType(_, _, _, _, _, _).
nailFuncType(_, _, _, _, _, _, _).
nailFuncType(_, _, _, _, _, _, _, _).
nailFuncType(_, _, _, _, _, _, _, _, _).
nailFuncType(_, _, _, _, _, _, _, _, _, _).
nailFuncType(_, _, _, _, _, _, _, _, _, _, _).
nailFuncType(_, _, _, _, _, _, _, _, _, _, _, _).

%---------------------------------------------------------------------------%

	% dummy/11 is the base function for curry.
:- func dummy(int, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) = T11.
:- mode dummy(in,  in, in, in, in, in, in, in, in, in, in) = out is det.
dummy(X, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10) = Y :-
	init_setup(X, RS0),
	rnd__irange(1, max_int_argument, Seed_initial,      RS0, RS1),
	rnd__irange(1, max_int_argument, Seed_any_to_int1,  RS1, RS2),
	rnd__irange(1, max_int_argument, Seed_any_to_int2,  RS2, RS3),
	rnd__irange(1, max_int_argument, Seed_any_to_int3,  RS3, RS4),
	rnd__irange(1, max_int_argument, Seed_any_to_int4,  RS4, RS5),
	rnd__irange(1, max_int_argument, Seed_any_to_int5,  RS5, RS6),
	rnd__irange(1, max_int_argument, Seed_any_to_int6,  RS6, RS7),
	rnd__irange(1, max_int_argument, Seed_any_to_int7,  RS7, RS8),
	rnd__irange(1, max_int_argument, Seed_any_to_int8,  RS8, RS9),
	rnd__irange(1, max_int_argument, Seed_any_to_int9,  RS9, RS10),
	rnd__irange(1, max_int_argument, Seed_any_to_int10, RS10, _),
	Final_Seed  = Seed_initial + any_to_int(Seed_any_to_int1,  X1)
	         		+ any_to_int(Seed_any_to_int2,  X2)
	         		+ any_to_int(Seed_any_to_int3,  X3)
	         		+ any_to_int(Seed_any_to_int4,  X4)
	         		+ any_to_int(Seed_any_to_int5,  X5)
	         		+ any_to_int(Seed_any_to_int6,  X6)
	         		+ any_to_int(Seed_any_to_int7,  X7)
	         		+ any_to_int(Seed_any_to_int8,  X8)
	         		+ any_to_int(Seed_any_to_int9,  X9)
	         		+ any_to_int(Seed_any_to_int10, X10),
	init_setup(Final_Seed, New_RS),
	gen(Y, [], [], [], New_RS, _).

%---------------------------------------------------------------------------%
any_to_int(Seed, X) = Int :-
	Temp = value_to_int(X),
	init_setup(Seed, RS0),
	(if	irange(1, 20, Output, RS0, _),
		Output = 1 
	 then
		Int = 0
	 else
		Int = Temp 
	).

value_to_int(X) = Temp :-
	univ(X) = X1,
	Temp = univ_to_int(X1).

:- func univ_to_int(univ) = int.
:- mode univ_to_int(in) = out is det.
univ_to_int(X) = Y :-
	univ_value(X) = Value,
	deconstruct(Value, canonicalize, Functor, _Arity, ArgList),
	string__to_char_list(Functor, Charlist),
	Temp = charlist_to_int(Charlist),
	Y = Temp + univlist_to_int(ArgList).	
	
		 
:- func charlist_to_int(list(char)) = int.
:- mode charlist_to_int(in) = out is det.
charlist_to_int([]) = 0.
charlist_to_int([X|Xs]) = Y :-
		char__to_int(X, X1),
		Y = X1 * 10 + charlist_to_int(Xs).
 
:- func univlist_to_int(list(univ)) = int.
:- mode univlist_to_int(in) = out is det.
univlist_to_int([]) = 0.
univlist_to_int([X|Xs]) = univ_to_int(X) + univlist_to_int(Xs).

%---------------------------------------------------------------------------%

to_trivial(Input, Pattern, Property0) = Property :-
        (if     Input = Pattern
	 then
	        Property = [trivial|Property0]
	 else
	        Property = Property0
	).

Left `>>>` Right = Property :-
	Property = [ info(univ(Left)) | Right].

Left `===` Right = Property :-
	(if	Left = Right
	 then
	 	Property = [yes]
	 else
	 	Property = [no]
	).

list_length(List) = list__length(List).

oneof(Xs, RS0, RS) = Y :- 
	(if 	list__length(Xs) = 0
	 then
	 	error("The input list should not be empty")
	 else
		rnd__irange(1, list__length(Xs), Nth, RS0, RS), 
		index1_det(Xs, Nth, Y)
	).
	 	
%---------------------------------------------------------------------------%

	% det_headlist/2 returns the first element of a list of lists, and the 
	% list of lists without the first element.
	% If the input list is [], then return [] for both. 
:- func det_headlist(list(list(T)), list(list(T))) = list(T).
:- mode det_headlist(in, out) = out is det.
det_headlist([], []) = [].
det_headlist([X|Xs], Xs) = X.

	% inin_setup/2 converts negative or zero seed to a positive, not-zero seed, 
	% because rnd__init/2 should not be called with negative or zero seed. 
:- pred init_setup(int, rnd).
:- mode init_setup(in, out) is det.
init_setup(Seed, RS0) :-
	(if	Seed =< 0
	 then
		rnd__init(Seed * -1 + 1, RS0)
	 else		
		rnd__init(Seed, RS0)	
	).
 
 	% display_univ/3 should will called with a univ of a tuple, it will then
	% print out the elements in that tuple.
:- pred display_univ(univ, io__state, io__state).
:- mode display_univ(in, di, uo) is det.
display_univ(Univ) -->
	{ Args = univ_value(Univ) },
	{ deconstruct(Args, canonicalize, _, _, Univs) },
	display_univs(Univs).	

	% display_univs prints the values which are stored in a list of univ.
:- pred display_univs(list(univ), io__state, io__state).
:- mode display_univs(in, di, uo) is det.
display_univs([], S, S).
display_univs([Univ|Univs]) --> 
	io__write(univ_value(Univ)),
	io__nl,
	display_univs(Univs).


	% show_dist/3 prints out the distribution.
:- pred	show_dist(list(distribution), io__state, io__state).
:- mode show_dist(in, di, uo) is det.
show_dist([], S, S).
show_dist([Dist|Dists]) -->
	{ Dist = {Freq, Univ} },
	io__write_int(Freq),
	io__write_string("\t"),
	io__write(univ_value(Univ)), 
	io__nl,
	show_dist(Dists).

%---------------------------------------------------------------------------%
	
	% get_freq/5 works out what distribution to use at current level by looking
	% at the SF and GF.  
:- func	get_freq(type_desc, int, int, list(frequency), 
		 list({type_desc, list(frequency)})) = list({int, list(list(frequency))}).
:- mode get_freq(in, in, in, in, in) = out is det.
get_freq(Datatype, NumFunctors, Counter, Frequencys, GF) = List :- 
	(if	NumFunctors = Counter 
	 then
		List = []
	 else
		(if	get_functor_ordinal(Datatype, Counter, Temp_Nth)
		 then
			Real_Nth = Temp_Nth
		 else
			error("get_freq/5 : get_functor_ordinal/3 failed")
		),
		test_freq(Datatype, NumFunctors, Real_Nth, LevelRequired0, 
			  Frequencys, GF, SubBranch),
		List0 = get_freq(Datatype, NumFunctors, Counter + 1, Frequencys, GF),
		(if	LevelRequired0 < 0
		 then
		 	LevelRequired = 0
		 else 
		 	LevelRequired = LevelRequired0
		),
		List = [{LevelRequired, SubBranch} | List0 ]
	).


	% test_freq/7 extract the required_level and the frequency of 
	% sub-constructors by the following steps :
	% 1	If Frequencys is non-empty, then there is specific frequency,
	%	so call freq_info/5 to get the infomation
	% 2	If Frequencys is empty, then search for general frequency
	% 3a	If a match is found in general frequency, then restart test_freq 
	%	with the general frequency for that type as the specific frequency
	% 3b	If no match is found, then apply default frequency	
:- pred	test_freq(type_desc, int, int, int, list(frequency), list({type_desc, 
		  list(frequency)}), list(list(frequency))). 
:- mode test_freq(in, in, in, out, in, in, out) is det.
test_freq(Datatype, NumFunctors, Nth, LevelRequired, Frequencys, GF, NewF) :-
	(if		list__length(Frequencys) = NumFunctors
	 then
			freq_info(0, Nth, Frequencys, LevelRequired, NewF)  	
	 else if 	Frequencys = []
	      then
	      		TempFreq = locate_general(Datatype, GF),	
			(if	TempFreq = []
			 then
				LevelRequired = 1,
				NewF = []
			 else  
			 	test_freq(Datatype, NumFunctors, Nth, 
					  LevelRequired, TempFreq, [], NewF)
			)
	 else
	 		error("test_freq/5 error: freqencys not match args \n")
	).

	% freq_info/5 extracts specifict frequency and its sub-constructor's
	% frequency contained in the Nth element. 
	% The base case may be redundant, to cover [[]] case. 
	% freq_info/5 recursively increases the counter until it equals Nth, 
	% then get the info stored in Frequency.
:- pred freq_info(int, int, list(frequency), int, list(list(frequency))).  	
:- mode freq_info(in, in, in, out, out) is det.  	
freq_info(_, _, [], 100, []).  
freq_info(Counter, Nth, [Frequency | Frequencys], LevelRequired, NewF) :-
	(if	Counter = Nth
	 then
	 	Frequency = {LevelRequired, NewF}
	 else
	 	freq_info(Counter + 1, Nth, Frequencys, LevelRequired, NewF)
	).

	% locate_general/2 passes through general frequency list, looking for 
	% matching type_desc, and return [] to indicate not found. 
:- func locate_general(type_desc, 
		       list({type_desc, list(frequency)})) = list(frequency).
:- mode locate_general(in, in) = out is det.
locate_general(_, []) = [].
locate_general(Datatype, [Info|Infos]) = Freq :-
	Info = {Type, List_freq},
	(if	Datatype = Type 
	 then
	 	Freq = List_freq
	 else
	 	Freq = locate_general(Datatype, Infos)
	).

	% freq_base/1 sums up all the relative frequency. 
:- func freq_base( list({int, list(list(frequency))}) ) = int.
:- mode freq_base(in) = out is det.
freq_base([]) = 0.
freq_base([X | Xs]) = Int + freq_base(Xs) :- 
	X = {Int, _}. 

	% select_branch/4 selects a branch when the Selector is within  
	% that branch's range. 
:- func select_branch(int, int, int, list({int, list(list(frequency))}) ) 
		      = { int, list(list(frequency)) }.
:- mode select_branch(in, in, in, in) = out is det.
select_branch(_, _, _, []) = _ :- 
	error("select_branch/4 : no branch to select").
select_branch(Selector, Counter, Branch0, [TempFreq | TempFreqs]) = { Branch, SubFreq } :-
	TempFreq = { Relative, SubBranch }, 
	(if	Counter + Relative > Selector			
	 then
	 	Branch = Branch0,
		SubFreq = SubBranch
	 else
	 	{ Branch, SubFreq } = select_branch(Selector, Counter + Relative,
						    Branch0 + 1, TempFreqs)
	).

	% find_user_gen/3 pass through the generator list, looking for matching 
	% type_desc. If it's found, then return the generating function, else this
	% predicate will fail.
:- pred find_user_gen(type_desc, list(user_gen_type), 
		      func(type_desc, list(frequency), 
		      list({type_desc, list(frequency)}), 
		      list(user_gen_type), rnd, rnd)=univ).
:- mode find_user_gen(in, list_skel_in(user_gen_inst), 
		      out(func(in, in, in, list_skel_in(user_gen_inst), 
		               in, out) = out is det) ) is semidet. 
find_user_gen(Datatype, [UserGenerator | UserGenerators], Generator) :-
	UserGenerator = {Type, TempGenerator},
	(if	Datatype = Type
	 then
	 	Generator = TempGenerator
	 else
	 	find_user_gen(Datatype, UserGenerators, Generator)
	).

:- func (max_int_argument) = int.
:- mode (max_int_argument) = out is det.
(max_int_argument) = 1000000.


%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- end_module qcheck.


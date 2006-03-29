/*
The "imperative-solve.power" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   The program to be specialised is a solver for a small imperative
   language which uses environments to store values for variables. The
   program contains built-ins and negations. The task is to specialise a
   sub-program calculating the power (X^Y) for a known power and base but
   an unknown environment.
   
  The benchmark program
*/ 

:- module imperative_solve_power.

:- interface.

:- pred imperative_solve_power is semidet.

:- implementation.


:- import_module imperative_solve_impl, run, assoc_list, list, pair.

imperative_solve_power :-
 power_2_5([],Eout1),
 use(Eout1),
 power_2_5(["z" - 1,"y" - 3],Eout2),
 use(Eout2).

/*
  The partial deduction query
  
 :- power(2,5,Ein,Eout).

  The run-time queries
  
 :- power(2,5,[],Eout).
 :- power(2,5,[z/1,y/3],Eout).

  Example solution
  
to be inserted
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
*/

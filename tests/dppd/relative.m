/*
The "relative" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   A Lam & Kusalik benchmark which can be fully unfolded. It uses neither
   negations nor built-ins.
   
  The benchmark program
*/ 

:- module relative.

:- interface.

:- pred relative is semidet.

:- implementation.

:- import_module relative_impl.

relative :-
	relative_john(peter).

/*
  The partial deduction query
  
 :- relative(john,X).

  The run-time queries
  
 :- relative(john,peter).

  Example solution
  
   This benchmark can be fully unfolded. With the ECCE partial deduction
   system one can obtain the following program, which runs more than 300
   times faster than the original:

relative__1(anna).
relative__1(john).
relative__1(carol).
relative__1(jonas).
relative__1(paulina).
relative__1(albertina).
relative__1(peter).
relative__1(maria).
relative__1(mary).
relative__1(jose).
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
*/

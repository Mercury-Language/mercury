/*
The "contains.kmp" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   This is exactly the same benchmark as the Lam & Kusalik contains
   benchmark, except that the run-time query is more challenging (in the
   sense that a Knuth-Morris-Pratt like optimisation will pay off).
   
  The run-time queries
*/

:- module contains_kmp.

:- interface.

:- pred contains_kmp is semidet.

:- implementation.

:- import_module contains, list.
  
contains_kmp :- contains_aab([a,a,a,a,c,a,a,c,a,b,a,a,a,a,a,b]).

/*
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
*/

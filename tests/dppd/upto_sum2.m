/*
The "upto.sum2" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   This is a sophisticated deforestation example coming from the
   functional programming community. It was handed over to me by Jesper
   Jorgensen, who adapted it from Wadler's paper Deforestation:
   Transforming programs to eliminate intermediate trees in TCS,
   73:231-248, 1990. The program calculates the square of integers in
   nodes of a tree and sums these up.
   
  The benchmark program
*/ 

:- module upto_sum2.

:- interface.

:- pred upto_sum2 is semidet.

:- implementation.

:- import_module upto_sum_impl, run.

upto_sum2 :-
 sumtrsquaretr(branch(leaf(3),leaf(2)),X1),
 use(X1),
 sumtrsquaretr(branch(branch(branch(leaf(1),leaf(2)),
                branch(leaf(3),leaf(9))),branch(leaf(3),
                branch(leaf(3),leaf(6)))),X2),
 use(X2),
 sumtrsquaretr(branch(branch(branch(leaf(1),leaf(2)),
                branch(leaf(3),branch(branch(leaf(3),leaf(4)),leaf(6)))),
                branch(branch(leaf(2),branch(leaf(2),leaf(5))),
                branch(branch(leaf(10),leaf(9)),branch(leaf(6),leaf(6))))),X3),
 use(X3).

/*
  The partial deduction query
  
 :- sumtrsquaretr(N,S).

  The run-time queries
  
 :- sumtrsquaretr(branch(leaf(3),leaf(2)),X).
 :- sumtrsquaretr(branch(branch(branch(leaf(1),leaf(2)),
                branch(leaf(3),leaf(9))),branch(leaf(3),
                branch(leaf(3),leaf(6)))),X).
 :- sumtrsquaretr(branch(branch(branch(leaf(1),leaf(2)),
                branch(leaf(3),branch(branch(leaf(3),leaf(4)),leaf(6)))),
                branch(branch(leaf(2),branch(leaf(2),leaf(5))),
                branch(branch(leaf(10),leaf(9)),branch(leaf(6),leaf(6))))),X).

  Example solution
  
to be inserted
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
*/

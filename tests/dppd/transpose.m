/*
The "transpose" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   A Lam & Kusalik benchmark which can be fully unfolded. The program
   transposes matrices (of any dimension) and uses neither negation nor
   built-in's.
   
  The benchmark program
*/

:- module transpose.

:- interface.

:- pred transpose is semidet.

:- implementation.

:- import_module list, run, transpose_impl.

transpose :- 
	transpose([[1,2,3,4,5,6,7,8,9],[2,3,4,5,6,7,8,9,10],
               [3,4,5,6,7,8,9,10,11]],Xtrm),
	use(Xtrm).

/*
  The partial deduction query
  
 :- transpose([[X1,X2,X3,X4,X5,X6,X7,X8,X9],Xr2,Xr3],Xtrm).

  The run-time queries
  
 :- transpose([[1,2,3,4,5,6,7,8,9],[2,3,4,5,6,7,8,9,10],
               [3,4,5,6,7,8,9,10,11]],Xtrm).

  Example solution
  
   This benchmark can be fully unfolded. With the ECCE partial deduction
   system one can obtain the following program, which runs more than 5
   times faster than the original:

transpose__1(X1,X2,X3,X4,X5,X6,X7,X8,X9,
            [X10,X11,X12,X13,X14,X15,X16,X17,X18],
            [X19,X20,X21,X22,X23,X24,X25,X26,X27],
 [[X1,X10,X19],
  [X2,X11,X20],
  [X3,X12,X21],
  [X4,X13,X22],
  [X5,X14,X23],
  [X6,X15,X24],
  [X7,X16,X25],
  [X8,X17,X26],
  [X9,X18,X27]]).
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
*/

/*
The "flip" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   A simple deforestation example from Wadler. The benchmark program
   flips a tree structure twice (thus returning the original tree back).
   
  The benchmark program
*/ 

:- module flip.

:- interface.

:- pred flip is semidet.

:- implementation.

:- import_module flip_impl, run.

flip :-
 	flipflip(tree(leaf(s(zero)),s(s(zero)),leaf(s(s(zero)))) , Res1),
	use(Res1),
	flipflip( tree(leaf(s(zero)),s(s(zero)),tree(leaf(s(s(zero))),zero,
                        leaf(s(s(s(zero)))))) , Res2),
	use(Res2),
	flipflip( tree(tree(leaf(s(zero)),s(s(zero)),leaf(s(s(zero)))),
		s(s(zero)), tree(leaf(s(s(zero))),zero,
				tree(leaf(s(s(s(s(zero))))),s(s(s(s(zero)))),
                        	leaf(s(s(s(s(s(zero))))))))) , Res3 ),
	use(Res3),
	flipflip(tree(tree(leaf(s(zero)),s(s(zero)),tree(leaf(s(zero)),
		s(s(zero)), tree(leaf(s(s(zero))),s(s(s(s(zero)))),
		leaf(s(s(s(zero))))))),s(s(zero)),
                tree(leaf(s(s(zero))),s(s(s(s(zero)))),
                tree(leaf(s(s(s(s(zero))))),s(s(s(s(zero)))),
                tree(leaf(s(s(s(s(zero))))),s(s(s(s(zero)))),
                tree(leaf(s(s(s(s(zero))))),zero,leaf(s(s(s(s(zero)))))))))), 
		Res4 ),
	use(Res4).

/*
  The partial deduction query
  
 :- flipflip(T1,T2).

  The run-time queries
  
 :- flipflip( tree(leaf(s(0)),s(s(0)),leaf(s(s(0)))) , Res ).
 :- flipflip( tree(leaf(s(0)),s(s(0)),tree(leaf(s(s(0))),0,
                        leaf(s(s(s(0)))))) , Res ).
 :- flipflip( tree(tree(leaf(s(0)),s(s(0)),leaf(s(s(0)))),s(s(0)),
                tree(leaf(s(s(0))),0,tree(leaf(s(s(s(s(0))))),s(s(s(s(0)))),
                        leaf(s(s(s(s(s(0))))))))) , Res ).
 :- flipflip( tree(tree(leaf(s(0)),s(s(0)),tree(leaf(s(0)),s(s(0)),
                tree(leaf(s(s(0))),s(s(s(s(0)))),leaf(s(s(s(0))))))),s(s(0)),
                tree(leaf(s(s(0))),s(s(s(s(0)))),
                tree(leaf(s(s(s(s(0))))),s(s(s(s(0)))),
                tree(leaf(s(s(s(s(0))))),s(s(s(s(0)))),
                tree(leaf(s(s(s(s(0))))),0,leaf(s(s(s(s(0)))))))))) , Res ).

  Example solution
  
   The following program can be obtained by the ECCE partial deduction
   system . It runs about 30 % faster than the original.

 flipflip__1(X1,X2) :-
     flip_conj__2(X1,X2).

 flip_conj__2(leaf(X1),leaf(X1)).
 flip_conj__2(tree(X1,X2,X3),tree(X4,X2,X5)) :-
     flip_conj__2(X1,X4),
     flip_conj__2(X3,X5).

   Combined with a bottom-up propagation (for more details see the
   technical report CW 232 ) ECCE can also obtain the following program
   which runs 45 % faster than the original:

flipflip__1(X1,X1) :-
    flip_conj__2(X1).

flip_conj__2(leaf(X1)).
flip_conj__2(tree(X1,X2,X3)) :-
    flip_conj__2(X1),
    flip_conj__2(X3).
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
*/

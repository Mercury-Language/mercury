/*
The "rotateprune" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   A quite sophisticated deforestation example (originally by
   Proietti/Pettorossi ?). This benchmark contains no built-in's nor
   negations. This particular benchmark program is treated in more detail
   e.g. in the technical report CW 226.
   
  The benchmark program
*/

:- module rotateprune.

:- interface.

:- pred rotateprune is semidet.

:- implementation.

:- import_module rotateprune_impl, run.

rotateprune :-
 rp( tree(leaf(s(zero)),s(s(zero)),leaf(s(s(zero)))), Res1),
 use(Res1),
 rp( tree(leaf(s(zero)),s(s(zero)),tree(leaf(s(s(zero))),zero,leaf(s(s(s(zero)))))), Res2),
 use(Res2),
 rp( tree(tree(leaf(s(zero)),s(s(zero)),leaf(s(s(zero)))),s(s(zero)),
        tree(leaf(s(s(zero))),zero,tree(leaf(s(s(s(s(zero))))),s(s(s(s(zero)))),
        leaf(s(s(s(s(s(zero))))))))), Res3),
 use(Res3),
 rp( tree(tree(leaf(s(zero)),s(s(zero)),tree(leaf(s(zero)),s(s(zero)),
        tree(leaf(s(s(zero))),s(s(s(s(zero)))),leaf(s(s(s(zero))))))),s(s(zero)),
        tree(leaf(s(s(zero))),s(s(s(s(zero)))),
        tree(leaf(s(s(s(s(zero))))),s(s(s(s(zero)))),
        tree(leaf(s(s(s(s(zero))))),s(s(s(s(zero)))),
        tree(leaf(s(s(s(s(zero))))),zero,leaf(s(s(s(s(zero)))))))))), Res4),
 use(Res4).

/*

  The partial deduction query
  
 :- rp(T1,T2).

  The run-time queries
  
 :- rp( tree(leaf(s(zero)),s(s(zero)),leaf(s(s(zero)))), Res).
 :- rp( tree(leaf(s(zero)),s(s(zero)),tree(leaf(s(s(zero))),zero,leaf(s(s(s(zero)))))), Res).
 :- rp( tree(tree(leaf(s(zero)),s(s(zero)),leaf(s(s(zero)))),s(s(zero)),
        tree(leaf(s(s(zero))),zero,tree(leaf(s(s(s(s(zero))))),s(s(s(s(zero)))),
        leaf(s(s(s(s(s(zero))))))))), Res).
 :- rp( tree(tree(leaf(s(zero)),s(s(zero)),tree(leaf(s(zero)),s(s(zero)),
        tree(leaf(s(s(zero))),s(s(s(s(zero)))),leaf(s(s(s(zero))))))),s(s(zero)),
        tree(leaf(s(s(zero))),s(s(s(s(zero)))),
        tree(leaf(s(s(s(s(zero))))),s(s(s(s(zero)))),
        tree(leaf(s(s(s(s(zero))))),s(s(s(s(zero)))),
        tree(leaf(s(s(s(s(zero))))),zero,leaf(s(s(s(s(zero)))))))))), Res).

  Example solution
  
   The following can be obtained by the ECCE partial deduction system .
   It runs considerably faster than the original program (more than 5
   times actually).

 rp__1(X1,X2) :- rotate_conj__2(X1,X2).

 rotate_conj__2(leaf(X1),leaf(X1)).
 rotate_conj__2(tree(X1,zero,X2),leaf(zero)) :-
    rotate__4(X1),
    rotate__4(X2).
 rotate_conj__2(tree(X1,s(X2),X3),tree(X4,s(X2),X5)) :-
    rotate_conj__2(X1,X4),
    rotate_conj__2(X3,X5).
 rotate_conj__2(tree(X1,s(X2),X3),tree(X4,s(X2),X5)) :-
    rotate_conj__2(X1,X5),
    rotate_conj__2(X3,X4).

 rotate__4(leaf(X1)).
 rotate__4(tree(X1,X2,X3)) :- rotate__4(X1), rotate__4(X3).
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
*/

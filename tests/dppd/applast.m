/*

The "applast" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   This is a benchmark by Michael Leuschel which contains no built-in's
   nor negations. Solving this benchmarks requires conjunctive partial
   deductions as well as a bottom-up success propagation. It is a simple
   example which illustrates the difficulties that can arise when
   specialising the ground representation. More details can be found in
   the technical reports CW 210 and CW 232.
   
  The benchmark program
*/

:- module applast.

:- interface.

:- pred applast is semidet.

:- implementation.

:- import_module list, run, applast_impl.

applast :-
 % not well moded.
 %applast([a,b,c,d],L1,e),
 %use(L1),
 applast([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,a,a,b,w,x,y],z,L2),
 use(L2),
 applast([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,a,a,b,w,x,y],z,q).


/*

  The partial deduction query
  
 :- applast(L,X,Last).

  The run-time queries
  
 :- applast([a,b,c,d],L,e).
 :- applast([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,a,a,b,w,x,y],z,L).
 :- applast([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,a,a,b,w,x,y],z,q).

  Example solution
  
   Combined with a bottom-up propagation (for more details see the
   technical report CW 232 ) the ECCE partial deduction system can obtain
   the following program:

 applast__1(L,a) :- al(L).

 al([]).
 al([H|T]) :- al(T).
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be

    */

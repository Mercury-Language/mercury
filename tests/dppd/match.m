/*
The "match" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   This is the semi-naive string matcher from the Lam & Kusalik
   benchmarks. The goal is to obtain a KMP matcher for the pattern aab.
   This benchmark program uses the \== builtin. The run-time queries are
   less sophisticated than the ones for match.kmp.
   
  The benchmark program
*/ 

:- module match.

:- interface.

:- pred match is semidet.

:- implementation.

:- import_module list, match_impl.

match :- 
	match_aab([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,a,a,b,w,x,y,z]).

/*
  The partial deduction query
  
 :- match([a,a,b],String).

  The run-time queries
  
 :- match([a,a,b],[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,a,a,b,w,x,y,z]).

  Example solution
  
   The following KMP-matcher can be obtained by the ECCE partial
   deduction system (to my knowledge this has first been achieved by John
   Gallagher with his SP system).

match__1([X1|X2]) :- match1__2(X1,X2).

match1__2(X1,[X2|X3]) :- a \= X1, match1__2(X2,X3).
match1__2(a,[X1|X2]) :- match1__3(X1,X2).

match1__3(X1,X2) :- a \= X1, match1__2(X1,X2).
match1__3(a,[X1|X2]) :- match1__4(X1,X2).

match1__4(X1,X2) :- b \= X1, match1__3(X1,X2).
match1__4(b,X1).
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
*/

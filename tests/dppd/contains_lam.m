/*
The "contains" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   This is the contains example from the Lam & Kusalik benchmarks. It is
   a string matcher which tries to be somewhat clever, but is highly
   non-deterministic and in the end not very efficient at all. This
   benchmark program uses the \== builtin.
   
  The benchmark program
*/ 

:- module contains_lam.

:- interface.

:- pred contains_lam is semidet.

:- implementation.

:- import_module contains.
:- import_module list.

contains_lam :- 
		contains_aab([a,b,c,d,e,f,g,h,a,a,b,
              i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).

/*
  The partial deduction query
  
 :- contains([a,a,b],X).

  The run-time queries
  
 :- contains([a,a,b],[a,b,c,d,e,f,g,h,a,a,b,
              i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).

  Example solution
  
   The following specialised program can be obtained by the ECCE partial
   deduction system . It runs more than 10 times faster than the
   original.

contains__1([X1|X2]) :-  con__2(X1,X2).

con__2(a,[X1|X2]) :- con__3(X1,X2).
con__2(X1,[X2|X3]) :- X1 \= a,con__2(X2,X3).

con__3(a,[X1|X2]) :- con__4(X1,X2).
con__3(X1,[X2|X3]) :- X1 \= a,con__2(X2,X3).

con__4(b,X1).
con__4(X1,[X2|X3]) :- X1 \= b,con__2(X2,X3).
con__4(a,[X1|X2]) :- con__3(X1,X2).
con__4(a,[X1|X2]) :- con__4(X1,X2).
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be

*/

/*
The "match.kmp" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   This is exactly like the Lam & Kusalik benchmark match, except that
   the run-time queries are more sophisticated than the ones for match.
*/

:- module match_kmp.

:- interface.

:- pred match_kmp is semidet.

:- implementation.

:- import_module match_impl, list.

match_kmp :-
 match_aab([a,a,a,a,c,d,a,a,a,e,f,g,h,a,a,b,d,f]),
 match_aab([a,b,a,b,a,a,a,a,c,a,a,a,a,a,a,a,a,b]),
 match_aab([a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,b]),
 match_aab([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,a,a,b,w,x,y,z]).

/* 
  The run-time queries
  
 :- match([a,a,b], [a,a,a,a,c,d,a,a,a,e,f,g,h,a,a,b,d,f]).
 :- match([a,a,b], [a,b,a,b,a,a,a,a,c,a,a,a,a,a,a,a,a,b]).
 :- match([a,a,b], [a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,b]).
 :- match([a,a,b], [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,a,a,b,w,x,y,z])
.
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
*/

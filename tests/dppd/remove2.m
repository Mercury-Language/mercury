/*
The "remove2" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   This is an even more sophisticated deforestation example, handed over
   to me by Jesper Jorgensen and adapted from Turchin's paper The concept
   of a supercompiler in TOPLAS, 8(3):292-325, 1986.
   
  The benchmark program
*/ 

:- module remove2.

:- interface.

:- pred remove2 is semidet.

:- implementation.

:- import_module list, run, remove2_impl.

remove2 :-
	rr([a,a,b,b,a,a,a,a,c,d,a,b,a,a,d,d],Y1),
	use(Y1),
	rr([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],Y2),
	use(Y2),
	rr([a,b,b,b,c,d,e,f,g,g,h,i,j,k,l,m,m,m,m,m,m,m,m,
                n,o,p,q,r,s,t,u,v,v,v,v,w,x,y,z,z,z],Y3),
	use(Y3).

/*
  The partial deduction query
  
 :- rr(X,Y).

  The run-time queries
  
 :- rr([a,a,b,b,a,a,a,a,c,d,a,b,a,a,d,d],Y).
 :- rr([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],Y).
 :- rr([a,b,b,b,c,d,e,f,g,g,h,i,j,k,l,m,m,m,m,m,m,m,m,
                n,o,p,q,r,s,t,u,v,v,v,v,w,x,y,z,z,z],Y).

  Example solution
  
to be inserted
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
*/

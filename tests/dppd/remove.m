/*
The "remove" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   This is a rather sophisticated deforestation example, handed over to
   me by Jesper Jorgensen.
   
  The benchmark program
  
*/
:- module remove.

:- interface.

:- pred remove is semidet.

:- implementation.

:- import_module list, remove_impl, run.

remove :-
 	rr([a,a,b,b,a,a,a,a,c,d,a,b,a,a,d,d],Y),
	use(Y),
 	rr([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z], Z),
	use(Z),
	rr([a,b,b,b,c,d,e,f,g,g,h,i,j,k,l,m,m,m,m,m,m,m,m,
                n,o,p,q,r,s,t,u,v,v,v,v,w,x,y,z,z,z],ZZ),
	use(ZZ).


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

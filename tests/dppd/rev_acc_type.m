/*
The "rev_acc_type" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   This benchmark is difficult in the sense that it causes the generation
   of an infinite number of characteristic trees in a quite natural
   manner. Indeed, the program is simply the well-known reverse with
   accumulating parameter program to which a type check on the
   accumulator has been added. In that way the growth of the accumulator
   causes a growth of the type checking computation, and thus a growth of
   the characteristic tree describing that computation. Further details
   can be found in the paper: Global Control for Partial Deduction
   through Characteristic Atoms and Global Trees.
   
  The benchmark program
  
rev([],_A,_A).
rev([_H|_T],_Acc,_Res) :-
        is_list(_Acc),
        rev(_T,[_H|_Acc],_Res).


is_list([]).
is_list([_H|_T]) :- is_list(_T).

  The partial deduction query
  
 :- rev(L,[],R).

  The run-time queries
  
 :- rev([],[],R).
 :- rev([a],[],R).
 :- rev([a,b,c,d,e,f,g],[],R).
 :- rev([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r],[],R).
 :- rev([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
                1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                21,22,23,24,25,26,27,28,29,30],[],R).
 :- rev([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
                1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                21,22,23,24,25,26,27,28,29,30,
              a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
                1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                21,22,23,24,25,26,27,28,29,30,
              a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
                1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                21,22,23,24,25,26,27,28,29,30],[],R).

  Example solution
  
to be inserted
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be

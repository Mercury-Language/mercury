
The "rev_acc_type.inffail" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   This benchmark has an infinite determinate failure at run-time, and
   thus might cause difficulties to some partial deduction systems.
   
  The benchmark program
  
rev([],_A,_A).
rev([_H|_T],_Acc,_Res) :-
        is_list(_Acc),
        rev(_T,[_H|_Acc],_Res).


is_list([]).
is_list([_H|_T]) :- is_list(_T).

  The partial deduction query
  
 :- rev(L,[],a).

  The run-time queries
  
 :- rev([],[],a).
 :- rev([a],[],a).
 :- rev([a,b,c,d,e,f,g],[],a).
 :- rev([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r],[],a).
 :- rev([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
                1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                21,22,23,24,25,26,27,28,29,30],[],a).
 :- rev([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
                1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                21,22,23,24,25,26,27,28,29,30,
              a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
                1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                21,22,23,24,25,26,27,28,29,30,
              a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
                1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                21,22,23,24,25,26,27,28,29,30],[],a).

  Example solution
  
to be inserted
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be

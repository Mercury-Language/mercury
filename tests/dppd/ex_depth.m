
The "ex_depth" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   A (more difficult) variation of the Lam & Kusalik depth benchmark
   using a different meta-interpreter. It consists of a simple non-ground
   meta-interpreter which has to be specialised for a (not
   fully-unfoldable) object program. It uses neither negations nor
   built-ins.
   
  The benchmark program
  
solve([],Depth,Depth).
solve([Head|Tail],DepthSoFar,Res) :-
        claus(Head,Body),
        solve(Body,s(DepthSoFar),IntDepth),
        solve(Tail,IntDepth,Res).

claus(member(X,[X|T]),[]).
claus(member(X,[Y|T]),[member(X,T)]).

claus(inboth(X,L1,L2),[member(X,L1),member(X,L2)]).

claus(app([],L,L),[]).
claus(app([H|X],Y,[H|Z]),[app(X,Y,Z)]).

claus(delete(X,[X|T],T),[]).
claus(delete(X,[Y|T],[Y|D]),[delete(X,T,D)]).

claus(test(A,L1,L2,Res),
        [inboth(A,L1,L2),delete(A,L1,D1),app(D1,L2,Res)]).

  The partial deduction query
  
 :- solve([inboth(X,Y,Z)],0,Depth).

  The run-time queries
  
 :- solve([inboth(d,[a,b,c,d,e,f,d],[f,e,d,c,b,a])],0,Depth).
 :- solve([inboth(a,[a,b,c,d,e,f,d],[f,e,d,c,b,a])],0,Depth).
 :- solve([inboth(d,[],[f,e,d,c,b,a])],0,Depth).
 :- solve([inboth(d,[a,b,c,d,e,f,d],[])],0,Depth).
 :- solve([inboth(g,[a,b,c,d,e,f,d],[f,e,d,c,b,a])],0,Depth).
 :- solve([inboth(a,[a],[b])],0,Depth).
 :- solve([inboth(e,[a,b,c,d,e,f,d,e,g,h,i,l,m,n],
                        [f,e,d,c,b,a])],0,Depth).
 :- solve([inboth(d,[a,b,c,d,e,f,d,e,g,h,i,l,m,n],
                        [a,b,c,d,e,f,d,e,g,h,i,l,m,n])],0,Depth).
 :- solve([inboth(X,[a,b,c,d,e,f,d,e,g,h,i,l,m,n],
                        [a,b,c,d,e,f,d,e,g,h,i,l,m,n])],0,Depth).

  Example solution
  
   With the ECCE partial deduction system one can obtain the following
   (which runs 3 times faster than the original, slightly faster versions
   can also be obtained):

 solve__1(X1,[X1|X2],X3,X4) :-
      solve__2(X1,X3,s(s(0)),X4).
 solve__1(X1,[X2|X3],X4,X5) :-
      solve__2(X1,X3,s(s(0)),X6),
      solve__2(X1,X4,X6,X5).

 solve__2(X1,[X1|X2],X3,s(X3)).
 solve__2(X1,[X2|X3],X4,X5) :-
      solve__2(X1,X3,s(X4),X5).
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be

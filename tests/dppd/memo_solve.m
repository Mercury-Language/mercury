
The "memo-solve" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   A variation of ex_depth, with a simple loop prevention mechanism,
   based on keeping a call stack. The program uses negation.
   
  The benchmark program
  
memo_solve([],MemoList).
memo_solve([Head|Tail],MemoList) :-
        \+(member(Head,MemoList)),
        claus(Head,Body),
        memo_solve(Body,[Head|MemoList]),
        memo_solve(Tail,MemoList).

member(X,[X|T]).
member(X,[Y|T]) :- member(X,T).

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
  
 :- memo_solve([inboth(X,Y,Z)],ML).

  The run-time queries
  
 :- memo_solve([inboth(d,[a,b,c,d,e,f,d],[f,e,d,c,b,a])],[]).
 :- memo_solve([inboth(a,[a,b,c,d,e,f,d],[f,e,d,c,b,a])],[]).
 :- memo_solve([inboth(d,[],[f,e,d,c,b,a])],[]).
 :- memo_solve([inboth(d,[a,b,c,d,e,f,d],[])],[]).
 :- memo_solve([inboth(g,[a,b,c,d,e,f,d],[f,e,d,c,b,a])],[]).
 :- memo_solve([inboth(a,[a],[b])],[]).
 :- memo_solve([inboth(e,[a,b,c,d,e,f,d,e,g,h,i,l,m,n],
                        [f,e,d,c,b,a])],[]).
 :- memo_solve([inboth(d,[a,b,c,d,e,f,d,e,g,h,i,l,m,n],
                        [a,b,c,d,e,f,d,e,g,h,i,l,m,n])],[]).
 :- memo_solve([inboth(X,[a,b,c,d,e,f,d,e,g,h,i,l,m,n],
                        [a,b,c,d,e,f,d,e,g,h,i,l,m,n])],[]).

  Example solution
  
   The following specialised program can be obtained by the ECCE partial
   deduction system . It about 20 % faster than the original. One can
   certainly improve upon the program below.

memo_solve__1(X1,[X2|X3],[X4|X5],X6) :-
    not(member__2(inboth(X1,[X2|X3],[X4|X5]),X6)),
    not(member__3(X1,X2,X3,X6)),
    claus_conj__4(X1,X2,X3,inboth(X1,[X2|X3],[X4|X5]),X6),
    not(member__3(X1,X4,X5,X6)),
    claus_conj__4(X1,X4,X5,inboth(X1,[X2|X3],[X4|X5]),X6).

member__2(X1,[X1|X2]).
member__2(X1,[X2,X3|X4]) :-
    member__2(X1,[X3|X4]).

member__3(X1,X2,X3,[member(X1,[X2|X3])|X4]).
member__3(X1,X2,X3,[X4,X5|X6]) :-
    member__2(member(X1,[X2|X3]),[X5|X6]).

claus_conj__4(X1,X1,X2,X3,X4).
claus_conj__4(X1,X2,[X3|X4],X5,X6) :-
    not(member__5(X1,X3,X4,X5,X6)),
    claus_conj__4(X1,X3,X4,member(X1,[X2,X3|X4]),[X5|X6]).

member__5(X1,X2,X3,X4,X5) :-
    member__2(member(X1,[X2|X3]),[X4|X5]).
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be

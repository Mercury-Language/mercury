
The "liftsolve.lmkng" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   Specialise part of the lifting meta-interpreter for the ground
   representation (adapted from a "non-executable" one by John Gallagher,
   similar to the InstanceDemo by Hill and Gallagher). Some details about
   this meta-interpreter can also be found in a PEPM'95 paper by Michael
   Leuschel and Danny De Schreye. This benchmark poses some difficult
   control problems (for some systems at least) and might generate an
   infinite number of characteristic trees.
   
  The benchmark program
  
/* --------------------- */
/* solve(GrRules,NgGoal) */
/* --------------------- */
solve(GrRules,[]).
solve(GrRules,[NgH|NgT]) :-
        non_ground_member(term(clause,[NgH|NgBody]),GrRules),
        solve(GrRules,NgBody),
        solve(GrRules,NgT).

/* -------------------------------------- */
/* non_ground_member(NgExpr,GrListOfExpr) */
/* -------------------------------------- */
non_ground_member(NgX,[GrH|GrT]) :-
        make_non_ground(GrH,NgX).
non_ground_member(NgX,[GrH|GrT]) :-
        non_ground_member(NgX,GrT).


/* --------------------------------------------------- */
/* make_non_ground(GroundRepOfExpr,NonGroundRepOfExpr) */
/* --------------------------------------------------- */
make_non_ground(G,NG) :-
        mkng(G,NG,[],Sub).

mkng(var(N),X,[],[sub(N,X)]).
mkng(var(N),X,[sub(N,X)|T],[sub(N,X)|T]).
mkng(var(N),X,[sub(M,Y)|T],[sub(M,Y)|T1]) :-
        N \== M,
        mkng(var(N),X,T,T1).
mkng(term(F,Args),term(F,IArgs),InSub,OutSub) :-
        l_mkng(Args,IArgs,InSub,OutSub).

l_mkng([],[],Sub,Sub).
l_mkng([H|T],[IH|IT],InSub,OutSub) :-
        mkng(H,IH,InSub,IntSub),
        l_mkng(T,IT,IntSub,OutSub).


member(X,[X|T]).
member(X,[Y|T]) :-
        member(X,T).

append([],L,L).
append([H|T],M,[H|T2]) :-
        append(T,M,T2).

  The partial deduction query
  
 :- l_mkng(Lg,Ln,[sub(N,X)],S).

  The run-time queries
  
 :- l_mkng([var(1)],Ln,[sub(1,X)],S).
 :- l_mkng([var(1),var(2)],Ln,[sub(2,X)],S).
 :- l_mkng([term(a,[]),var(1),var(3)],Ln,[sub(1,X)],S).
 :- l_mkng([term(f,[var(1),var(2),var(1)]),
                 term(g,[var(2),var(1),var(2)]),
                 term(h,[term(a,[]),term(b,[]),var(1)])],Ln,[sub(1,X)],S).

  Example solution
  
 To be inserted.
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be

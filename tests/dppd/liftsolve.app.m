
The "liftsolve.db1" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   Specialise the lifting meta-interpreter for the ground representation
   (adapted from a "non-executable" but specialisable one by John
   Gallagher, similar to the InstanceDemo by Hill and Gallagher) with the
   append program as object program. Some details about this
   meta-interpreter can also be found in a PEPM'95 paper by Michael
   Leuschel and Danny De Schreye.
   
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
  
 :- solve([
         term(clause,[term(app,[term(null,[]),var(l),var(l)]) ]),
         term(clause,[term(app,[term(cons,[var(h),var(x)]),var(y),
                                        term(cons,[var(h),var(z)])]),
                term(app,[var(x),var(y),var(z)]) ])
                ],
        [term(app,[X,Y,Z])]).

  The run-time queries
  
 :- solve([
         term(clause,[term(app,[term(null,[]),var(l),var(l)]) ]),
         term(clause,[term(app,[term(cons,[var(h),var(x)]),var(y),
                                        term(cons,[var(h),var(z)])]),
                term(app,[var(x),var(y),var(z)]) ])
                ],
        [term(app,[term(null,[]),Y,Z])]).

 :- solve([
         term(clause,[term(app,[term(null,[]),var(l),var(l)]) ]),
         term(clause,[term(app,[term(cons,[var(h),var(x)]),var(y),
                                        term(cons,[var(h),var(z)])]),
                term(app,[var(x),var(y),var(z)]) ])
                ],
        [term(app,[term(cons,[term(a,[]),term(null,[])]),
                        term(cons,[term(b,[]),term(null,[])]),Z])]).

 :- solve([
         term(clause,[term(app,[term(null,[]),var(l),var(l)]) ]),
         term(clause,[term(app,[term(cons,[var(h),var(x)]),var(y),
                                        term(cons,[var(h),var(z)])]),
                term(app,[var(x),var(y),var(z)]) ])
                ],
        [term(app,[X,Y,term(cons,[term(a,[]),term(cons,
                                        [term(b,[]),term(null,[])])])])]).

  Example solution
  
   With the ECCE partial deduction system one can obtain the following
   program, which runs 20 times faster than the original and in which the
   overhead of the ground representation has been almost completely
   removed:

solve__1(term(null,[]),X1,X1).
solve__1(term(cons,[X1,X2]),X3,term(cons,[X1,X4])) :-
    solve__1(X2,X3,X4).
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be

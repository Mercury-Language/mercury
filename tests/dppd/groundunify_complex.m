
The "groundunify.complex" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   A ground unification algorithm calculating explicit substitutions
   which uses built-ins and negation. The program is taken from a
   Lopstr'92 article by John Gallagher and Andre de Waal. More details
   about this program can also be found in the technical report CW 210.
   
  The benchmark program
  
unify(X,Y,S) :-
        unify(X,Y,[],S).

unify(var(N),T,S,S1) :-
        bound(var(N),S,B,V),
        unify(var(N),T,S,S1,B,V).
unify(struct(F,Args),var(N),S,S1) :-
        unify(var(N),struct(F,Args),S,S1).
unify(struct(F,Args1),struct(F,Args2),S,S2) :-
        unifyargs(Args1,Args2,S,S2).

unify(var(_),T,S,S1,B,true) :-
        unify(B,T,S,S1).
unify(var(N),T,S,S1,_,false) :-
        unify1(T,var(N),S,S1).

unifyargs([],[],S,S).
unifyargs([T|Ts],[R|Rs],S,S2) :-
        unify(T,R,S,S1),
        unifyargs(Ts,Rs,S1,S2).

unify1(struct(F,Args),var(N),S,[var(N)/struct(F,Args)|S]) :-
        \+(occur_args(var(N),Args,S)).
unify1(var(N),var(N),S,S).
unify1(var(M),var(N),S,S1) :-
        M \== N,
        bound(var(M),S,B,V),
        unify1(var(M),var(N),S,S1,B,V).
unify1(var(_),var(N),S,S1,B,true) :-
        unify1(B,var(N),S,S1).
unify1(var(M),var(N),S,[var(N)/var(M)|S],_,false).

bound(var(N),[var(N)/T|_],T,true) :-
        T \== var(N).
bound(var(N),[B/_|S],T,F) :-
        B \== var(N),
        bound(var(N),S,T,F).
bound(var(_),[],_,false).

dereference(var(N),[var(N)/T|_],T) :-
        T \== var(N).
dereference(var(N),[B/_|S],T) :-
        B \== var(N),
        dereference(var(N),S,T).

occur(var(N),var(M),S) :-
        dereference(var(M),S,T),
        occur(var(N),T,S).
occur(var(N),var(N),_).
occur(var(N),struct(_,Args),S) :-
        occur_args(var(N),Args,S).

occur_args(var(N),[A|_],S) :-
        occur(var(N),A,S).
occur_args(var(N),[_|As],S) :-
        occur_args(var(N),As,S).

  The partial deduction query
  
 :- unify(struct(p,[X,X]),struct(p,[struct(f,[Y,struct(a,[])]),Z]),Sub).

  The run-time queries
  
 :- unify(struct(p,[var(3),var(3)]),
                struct(p,[struct(f,[var(2),struct(a,[])]),var(1)]),Sub).
 :- unify(struct(p,[struct(a,[]),struct(a,[])]),
                struct(p,[struct(f,[var(2),struct(a,[])]),struct(a,[])]),Sub).
 :- unify(struct(p,[var(1),var(1)]),
                struct(p,[struct(f,[var(1),struct(a,[])]),var(2)]),Sub).
 :- unify(struct(p,[struct(f,[var(1),struct(a,[])]),
                         struct(f,[var(1),struct(a,[])])]),
                struct(p,[struct(f,[struct(b,[]),struct(a,[])]),var(2)]),Sub).

  Example solution
  
   The best solution so far, using the ECCE partial deduction system runs
   2.5 times faster than the original.

unify__1(var(X1),X2,X3,X4) :-
    not(occur_args__2(X1,X2)),
    unify__5(X2,struct(a,[]),X3,['/'(var(X1),struct(f,[X2,struct(a,[])]))],X4).
unify__1(struct(f,[X1,X2]),X3,X4,X5) :-
    unify__3(X1,X3,X6),
    unify_conj__4(X2,X6,X1,X2,X4,X5).
occur_args__2(X1,X2) :-
    occur__11(X1,X2).
unify__3(var(X1),X2,X3) :-
    unify1__24(X2,X1,X3).
unify__3(struct(X1,X2),var(X3),['/'(var(X3),struct(X1,X2))]) :-
    not(occur_args__10(X3,X2)).
unify__3(struct(X1,X2),struct(X1,X3),X4) :-
    unifyargs__9(X2,X3,[],X4).
unify_conj__4(var(X1),X2,X3,X4,X5,X6) :-
    bound_conj__23(X1,X2,X2,X3,X4,X5,X6).
unify_conj__4(struct(a,[]),X1,X2,X3,X4,X5) :-
    unify__5(X2,X3,X4,X1,X5).
unify__5(X1,X2,var(X3),X4,X5) :-
    bound_conj__6(X3,X4,X1,X2,X5).
unify__5(X1,X2,struct(f,[X3,X4]),X5,X6) :-
    unify__7(X1,X3,X5,X7),
    unify__7(X2,X4,X7,X6).
bound_conj__6(X1,['/'(var(X1),X2)|X3],X4,X5,X6) :-
    X2 \= var(X1),
    unify__21(X2,X4,X5,X1,X3,X6).
bound_conj__6(X1,['/'(X2,X3)|X4],X5,X6,X7) :-
    X2 \= var(X1),
    bound_conj__12(X1,X4,f,[X5,X6],X2,X3,X4,X7).
bound_conj__6(X1,[],X2,X3,['/'(var(X1),struct(f,[X2,X3]))]) :-
    not(occur_args__22(X1,X2,X3)).
unify__7(var(X1),X2,X3,X4) :-
    bound_conj__18(X1,X3,X2,X3,X4).
unify__7(struct(X1,X2),var(X3),X4,X5) :-
    unify__8(X3,X1,X2,X4,X5).
unify__7(struct(X1,X2),struct(X1,X3),X4,X5) :-
    unifyargs__9(X2,X3,X4,X5).
unify__8(X1,X2,X3,['/'(var(X1),X4)|X5],X6) :-
    X4 \= var(X1),
    unify__13(X4,X2,X3,var(X1),X4,X5,X6).
unify__8(X1,X2,X3,['/'(X4,X5)|X6],X7) :-
    X4 \= var(X1),
    bound_conj__12(X1,X6,X2,X3,X4,X5,X6,X7).
unify__8(X1,X2,X3,[],['/'(var(X1),struct(X2,X3))]) :-
    not(occur_args__10(X1,X3)).
unifyargs__9([],[],X1,X1).
unifyargs__9([X1|X2],[X3|X4],X5,X6) :-
    unify__7(X1,X3,X5,X7),
    unifyargs__9(X2,X4,X7,X6).
occur_args__10(X1,[X2|X3]) :-
    occur__11(X1,X2).
occur_args__10(X1,[X2,X3|X4]) :-
    occur_args__10(X1,[X3|X4]).
occur__11(X1,var(X1)).
occur__11(X1,struct(X2,[X3|X4])) :-
    occur_args__10(X1,[X3|X4]).
bound_conj__12(X1,['/'(var(X1),X2)|X3],X4,X5,X6,X7,X8,X9) :-
    X2 \= var(X1),
    unify__13(X2,X4,X5,X6,X7,X8,X9).
bound_conj__12(X1,['/'(X2,X3)|X4],X5,X6,X7,X8,X9,X10) :-
    X2 \= var(X1),
    bound_conj__12(X1,X4,X5,X6,X7,X8,X9,X10).
bound_conj__12(X1,[],X2,X3,X4,X5,X6,['/'(var(X1),struct(X2,X3)),'/'(X4,X5)|X6])
 :-
    not(occur_args__14(X1,X3,['/'(X4,X5)|X6])).
unify__13(var(X1),X2,X3,X4,X5,X6,X7) :-
    bound_conj__17(X1,X4,X5,X6,X2,X3,X7).
unify__13(struct(X1,X2),X1,X3,X4,X5,X6,X7) :-
    unifyargs__9(X2,X3,['/'(X4,X5)|X6],X7).
occur_args__14(X1,[X2|X3],X4) :-
    occur__15(X1,X2,X4).
occur_args__14(X1,[X2,X3|X4],X5) :-
    occur_args__14(X1,[X3|X4],X5).
occur__15(X1,var(X2),['/'(X3,X4)|X5]) :-
    dereference_conj__16(X2,X3,X4,X5,X1,X3,X4,X5).
occur__15(X1,var(X1),X2).
occur__15(X1,struct(X2,[X3|X4]),X5) :-
    occur_args__14(X1,[X3|X4],X5).
dereference_conj__16(X1,var(X1),X2,X3,X4,X5,X6,X7) :-
    X2 \= var(X1),
    occur__15(X4,X2,['/'(X5,X6)|X7]).
dereference_conj__16(X1,X2,X3,['/'(X4,X5)|X6],X7,X8,X9,X10) :-
    X2 \= var(X1),
    dereference_conj__16(X1,X4,X5,X6,X7,X8,X9,X10).
bound_conj__17(X1,var(X1),X2,X3,X4,X5,X6) :-
    X2 \= var(X1),
    unify__13(X2,X4,X5,var(X1),X2,X3,X6).
bound_conj__17(X1,X2,X3,X4,X5,X6,X7) :-
    X2 \= var(X1),
    bound_conj__12(X1,X4,X5,X6,X2,X3,X4,X7).
bound_conj__18(X1,['/'(var(X1),X2)|X3],X4,X5,X6) :-
    X2 \= var(X1),
    unify__7(X2,X4,X5,X6).
bound_conj__18(X1,['/'(X2,X3)|X4],X5,X6,X7) :-
    X2 \= var(X1),
    bound_conj__18(X1,X4,X5,X6,X7).
bound_conj__18(X1,[],X2,X3,X4) :-
    unify1__19(X2,X1,X3,X4).
unify1__19(struct(X1,X2),X3,X4,['/'(var(X3),struct(X1,X2))|X4]) :-
    not(occur_args__14(X3,X2,X4)).
unify1__19(var(X1),X1,X2,X2).
unify1__19(var(X1),X2,X3,X4) :-
    X1 \= X2,
    bound_conj__20(X1,X3,X2,X3,X4).
bound_conj__20(X1,['/'(var(X1),X2)|X3],X4,X5,X6) :-
    X2 \= var(X1),
    unify1__19(X2,X4,X5,X6).
bound_conj__20(X1,['/'(X2,X3)|X4],X5,X6,X7) :-
    X2 \= var(X1),
    bound_conj__20(X1,X4,X5,X6,X7).
bound_conj__20(X1,[],X2,X3,['/'(var(X2),var(X1))|X3]).
unify__21(var(X1),X2,X3,X4,X5,X6) :-
    var(X4) \= var(X1),
    bound_conj__12(X1,X5,f,[X2,X3],var(X4),var(X1),X5,X6).
unify__21(struct(f,[X1,X2]),X3,X4,X5,X6,X7) :-
    unify__7(X1,X3,['/'(var(X5),struct(f,[X1,X2]))|X6],X8),
    unify__7(X2,X4,X8,X7).
occur_args__22(X1,X2,X3) :- occur__11(X1,X2).
occur_args__22(X1,X2,X3) :- occur__11(X1,X3).
bound_conj__23(X1,['/'(var(X1),X2)|X3],X4,X5,X6,X7,X8) :-
    X2 \= var(X1),
    unify_conj__4(X2,X4,X5,X6,X7,X8).
bound_conj__23(X1,['/'(X2,X3)|X4],X5,X6,X7,X8,X9) :-
    X2 \= var(X1),
    bound_conj__23(X1,X4,X5,X6,X7,X8,X9).
bound_conj__23(X1,[],X2,X3,X4,X5,X6) :-
    unify__5(X3,X4,X5,['/'(var(X1),struct(a,[]))|X2],X6).
unify1__24(struct(X1,X2),X3,['/'(var(X3),struct(X1,X2))]) :-
    not(occur_args__10(X3,X2)).
unify1__24(var(X1),X1,[]).
unify1__24(var(X1),X2,['/'(var(X2),var(X1))]) :-
    X1 \= X2.
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be

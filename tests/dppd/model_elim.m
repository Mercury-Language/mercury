
The "model_elim.app" Benchmark

   Part of the DPPD Library.
   
  General Description
  
   This benchmark uses the Poole-Goebel model elimination theorem prover
   used by Andre de Waal and John Gallager. The FOL object theory for
   this particular benchmark represents the append program. The program
   contains no negations nor built-in's.
   
  The benchmark program
  
solve((G1,G2),[]) :- solve(G1,[]),solve(G2,[]).

solve(G,A) :-
        prove(G,A).

prove(G,A) :-
        member(G,A).
prove(G,A) :- neg(G,GN),
        contrapositive((GN:-B)),
        proveall(B,[GN|A]).

proveall([],_).
proveall([G|R],A) :- prove(G,A),
        proveall(R,A).

contrapositive((G:-B)) :- input_clause(_,_,[G|B]).
contrapositive((G:-[B|Bs1])) :- input_clause(_,_,[B|Bs]),
        contrapositive1(G,Bs,Bs1).

contrapositive1(G,[G|Xs],Xs).
contrapositive1(G,[X|Xs],[X|Xs1]) :- contrapositive1(G,Xs,Xs1).

member(X,[X|_]).
member(X,[_|Xs]) :- member(X,Xs).

neg(neg(F),pos(F)).
neg(pos(F),neg(F)).

input_clause(app1,axiom,
    [pos(app([],L,L))]).

input_clause(app2,axiom,
    [pos(app([H|X],Y,[H|Z])),
     neg(app(X,Y,Z))]).

input_clause(testp1,axiom,
    [pos(q(X)),
     neg(p(X))]).
input_clause(testp2,axiom,
    [pos(p(X)),
     pos(q(a)),
     pos(q(b))]).

  The partial deduction query
  
 :- solve(neg(app(X,Y,Z)),[]).

  The run-time queries
  
 :- solve(neg(app([a,b],[c,d],L)),[]).
 :- solve(neg(app([a,b,c,d,e,f,g,h,i,k,l,m,n,o,p],
                        [q,r,s,t,u,v,w,x,y,z],L)),[]).
 :- solve(neg(app(X,Y,[a,b,c,d])),[]).

  Example solution
  
to be inserted
     _________________________________________________________________
   
   
    Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be

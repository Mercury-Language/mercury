
:- module rotateprune_impl.

:- interface.

:- pred rp(tree(nat)::in, tree(nat)::out) is multi.

:- type tree(T) 
	--->	tree(tree(T), T, tree(T))
	;	leaf(T).	

:- type nat
	--->	s(nat)
	;	zero.

:- implementation.
  
rp(T1,T2) :- rotate(T1,U), prune(U,T2).

:- pred rotate(tree(T)::in, tree(T)::out) is multi.

rotate(leaf(N),leaf(N)).
rotate(tree(L,N,R),tree(RL,N,RR)) :- rotate(L,RL), rotate(R,RR).
rotate(tree(L,N,R),tree(RR,N,RL)) :- rotate(L,RL), rotate(R,RR).

:- pred prune(tree(nat)::in, tree(nat)::out) is det.

prune(leaf(N),leaf(N)).
prune(tree(_,zero,_),leaf(zero)).
prune(tree(L,s(N),R),tree(PL,s(N),PR)) :- prune(L,PL), prune(R,PR).



father(john, mary).
father(john, andrew).

mother(jane, mary).
mother(jane, andrew).
mother(jane, george).

sibling(X, Y) :-
	( 
		father(F, X), father(F, Y)
	;
		mother(M, X), mother(M, Y)
	). 



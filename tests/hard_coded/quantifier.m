:- module quantifier.

:- interface.

:- import_module io.

:- pred main(io__state::di,io__state::uo) is det.

:- implementation.

:- import_module list, int.

:- pred sum(list(int),int).
:- mode sum(in,out) is det.

sum([],0).
sum([X|L],X + N1) :- sum(L,N1). 


:- pred foo(pred(int)).
:- mode foo(free >> (pred(out) is det)) is det.

foo(sum([1,2,3])).


main -->
	( {P = (pred(X :: out) is det :- X = 6),
		foo(Q), 
		all [X] (call(P,X) <=> call(Q,X))}
	->
		print("equivalent")
	;
		print("not equivalent")
	), nl.


	
:- module flip_impl.

:- interface.

:- pred flipflip(tree(T), tree(T)).
:- mode flipflip(in, out) is det.

:- type tree(T)
	--->	leaf(T)
	;	tree(tree(T), T, tree(T)).

:- type nat
	--->	zero
	;	s(nat).

:- implementation.

flipflip(XT,YT) :- flip(XT,TT), flip(TT,YT).

:- pred flip(tree(T), tree(T)).
:- mode flip(in, out) is det.

flip(leaf(X),leaf(X)).
flip(tree(XT,Info,YT),tree(FYT,Info,FXT)) :-
        flip(XT,FXT),
        flip(YT,FYT).


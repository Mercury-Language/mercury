
:- module remove2_impl.

:- interface.

:- import_module list.

:- pred rr(list(T)::in, list(T)::out) is det.

:- implementation.

rr(X,Y) :- f(X,T), f(T,Y).

:- pred f(list(T)::in, list(T)::out) is det.

f([],[]).
f([A|T],Y) :- h(A,T,Y).

:- pred h(T::in, list(T)::in, list(T)::out) is det.

h(A,[],[A]).
h(A,[B|S],Y) :- g(A,B,[B|S],S,Y).

:- pred g(T::in, T::in, list(T)::in, list(T)::in, list(T)::out) is det.

g(A,B,T,S,[A|Y]) :- 
	( A = B ->
		f(S,Y)
	;
		f(T,Y)
	).



:- module remove_impl.

:- interface.

:- import_module list.

:- pred rr(list(T)::in, list(T)::out) is det.

:- implementation.

rr(X,Y) :- r(X,T), r(T,Y).

:- pred r(list(T)::in, list(T)::out) is det.

r([],[]).
r([X],[X]).
r([X,Y|T],[X|T1]) :-
	( X = Y ->
		r(T,T1)
	;
		r([Y | T], T1)
	).


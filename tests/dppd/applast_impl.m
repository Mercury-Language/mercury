
:- module applast_impl.

:- interface.

:- import_module list.

:- pred applast(list(T), T, T) is semidet.
:- mode applast(in, in, in) is semidet.
:- mode applast(in, in, out) is semidet.
%:- mode applast(in, out, in) is semidet.

:- implementation.

applast(L,X,Last) :- app(L,[X],LX),list_last(Last,LX).

:- pred list_last(T, list(T)).
:- mode list_last(out, in) is semidet.

list_last(X,[H|T]) :- 
	(
		T = [],
		X = H
	;
		T = [_|_],
		list_last(X,T)
	).

:- pred app(list(T)::in, list(T)::in, list(T)::out) is det.

app([],L,L).
app([H|L1],L2,[H|L3]) :- app(L1,L2,L3).


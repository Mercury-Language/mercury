:- module select.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred select(T::out, list(T)::in, list(T)::out) is nondet.

:- implementation.

select(X, [X | Xs], Xs).
select(X, [Y | Xs], [Y | Zs]) :-
	select(X, Xs, Zs).

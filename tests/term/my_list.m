:- module my_list.

:- interface.

:- type list(T)	--->	[] ; [T | list(T)].

:- pred list(list(T)::in) is semidet.

:- implementation.

list([_H | Ts]) :-
	list(Ts).
list([]).

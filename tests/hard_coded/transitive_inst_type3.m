:- module transitive_inst_type3.
:- interface.

:- type my_list(T) ---> [] ; [T | my_list(T)].
:- inst my_list(I) ---> [] ; [I | my_list(I)].

:- pred length(my_list(T), int).
:- mode length(in(my_list(ground)), out) is det.

:- implementation.

:- import_module int.

length([], 0).
length([_ | Xs], N + 1) :-
	length(Xs, N).

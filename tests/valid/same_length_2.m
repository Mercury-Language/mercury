:- module same_length_2.
:- import_module list.

:- mode my_input_list_skel :: list_skel -> list_skel.
:- mode my_output_list_skel :: free -> list_skel.
:- mode my_list_skel_output :: list_skel -> ground.

:- pred q(list(T)::my_input_list_skel).
:- pred r(list(T)::my_output_list_skel).

:- external(q/1).
:- external(r/1).

:- pred p.

p :-
	r(X),
	q(X).

:- pred p2(list(T)::my_output_list_skel).

p2(X) :-
	r(X),
	q(X)
	;
	r(X),
	q(X).


:- module same_length_2.
:- interface.
:- import_module list.

:- mode my_input_list_skel == list_skel >> list_skel.
:- mode my_output_list_skel == free >> list_skel.
:- mode my_list_skel_output == list_skel >> ground.

:- pred p is semidet.

:- pred p2(list(T)::my_output_list_skel) is nondet.

:- implementation.

:- pred q(list(T)::my_input_list_skel).
:- pred r(list(T)::my_output_list_skel).

q(_X) :- q([]).
r(X) :- r(X).

p :-
	r(X),
	q(X).

p2(X) :-
	r(X),
	q(X)
	;
	r(X),
	q(X).


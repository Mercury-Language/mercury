:- module same_length_3.

:- type list(T) ---> [] ; [T | list(T)].

:- inst list_skel = bound([] ; [free | list_skel]).

:- mode input_list_skel :: list_skel -> list_skel.
:- mode output_list_skel :: free -> list_skel.
:- mode list_skel_output :: list_skel -> ground.

:- pred q(list(T)::input_list_skel) is det.
:- pred r(list(T)::output_list_skel) is det.

:- pred p is det.

p :-
	r(X),
	q(X).

:- pred p2(list(T)::output_list_skel) is nondet.

p2(X) :-
	r(X),
	q(X)
	;
	r(X),
	q(X).


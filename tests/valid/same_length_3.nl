:- module l2.

:- type list(T) ---> [] ; [T | list(T)].

:- inst list_skel = bound([] ; [free | list_skel]).

:- mode input_list_skel :: list_skel -> list_skel.
:- mode output_list_skel :: free -> list_skel.
:- mode list_skel_output :: list_skel -> ground.

:- pred q(list(T)::input_list_skel).
:- pred r(list(T)::output_list_skel).

:- pred p.

p :-
	r(X),
	q(X).

:- pred p2(list(T)::output_list_skel).

p2(X) :-
	r(X),
	q(X)
	;
	r(X),
	q(X).


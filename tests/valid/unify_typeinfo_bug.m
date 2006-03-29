:- module unify_typeinfo_bug.

:- interface.

:- import_module list, map.

:- type t
	---> t(map(int, string), list(int)).

:- pred unify_t(t, t).
:- mode unify_t(in, in) is semidet.

:- implementation.

:- import_module univ.

unify_t(t(A1, B1), t(A2, B2)) :-
	unify_map(A1, A2),
	B1 = B2.

:- pred unify_map(map(int, string), map(int, string)).
:- mode unify_map(in, in) is semidet.

unify_map(A, B) :-
	univ(A) = univ(B).



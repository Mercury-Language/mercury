:- module inadmissible.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int, list.

main(!IO) :-
	(
		gtmax(2, [2,3,1])
	->
		write_string("max", !IO)
	;
		write_string("not max", !IO)
	),
	nl(!IO).

:- pred gtmax(int::in, list(int)::in) is semidet.

gtmax(A, As) :-
	list_to_set(As, SA),
	oset_max(SA, Max),
	A > Max.

:- pred ltmax(int::in, list(int)::in) is semidet.

ltmax(A, As) :-
	list_to_set(As, SA),
	oset_max(SA, Max),
	A < Max.

:- pred list_to_set(list(int)::in, list(int)::out) is det.

list_to_set(As, S) :- S = As.

:- pred list_to_oset(list(int)::in, list(int)::out) is det.

list_to_oset(As, S) :- sort(int_comp, As, S).

:- pred oset_max(list(int)::in, int::out) is nondet.

oset_max(S, M) :- append(_, [M], S).

:- pred int_comp(int::in, int::in, comparison_result::out) is det.

int_comp(A, B, R) :- compare(R, A, B).

:- module checked_nondet_tailcall_noinline.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, solutions.

main --> 
	{ join(1, Ones) },
	write_ints(Ones),
	{ join(2, Twos) },
	write_ints(Twos),
	{ join(3, Threes) },
	write_ints(Threes),
	{ join(4, Fours) },
	write_ints(Fours),
	{ join(5, Fives) },
	write_ints(Fives).

:- pred edge1(int::in, int::out) is nondet.

edge1(1, 11).
edge1(1, 12).

:- pred edge2(int::in, int::out) is nondet.

edge2(11, 18).
edge2(12, 19).
edge2(2, 21).
edge2(2, 22).
edge2(3, 31).
edge2(4, 41).

:- pred edge12(int::in, int::out) is nondet.

edge12(A, B) :-
	( edge1(A, C) ->
		% When we come here after the last success of edge1,
		% the call to edge2 will be a checked nondet tailcall.
		% When we come here after a non-last success of edge1,
		% the check will fail, and we will have to do a
		% non-tail call.
		D = C
	;
		% When this arm of the switch is taken, the call to edge2
		% will be a checked nondet tailcall.
		D = A
	),
	edge2(D, B).

:- pred join(int::in, list(int)::out) is det.   

join(A, Bs) :- 
	solutions((pred(B::out) is nondet :- edge12(A, B)), Bs).

:- pred write_ints(list(int)::in, io__state::di, io__state::uo) is det.

write_ints([]) -->
	io__write_string("\n").
write_ints([X | Xs]) -->
	io__write_int(X),
	io__write_string(" "),
	write_ints(Xs).

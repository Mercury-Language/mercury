:- module dependency2.

:- interface.

:- import_module io.
:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.
:- import_module bool, int, list, require, std_util.

main -->
	{ turn_on_origin_debug },
	{ test(L) },
	io__write(L),
	io__write_string(".\n").

:- pred test(list(int)::out) is cc_multi.

test(L) :-
	p(U),
	( U = 1 ->
		A = 1
	;
		A = U
	),
	q(V),
	(
		V = no,
		r(A, [3, 4], BX),
		BX = B - _
	;
		V = yes,
		B = 4
	),
	AB = {A, B},
	(
		A = 2,
		C = 5,
		D = []
	;
		C = 6,
		AB = {Aprime, Bprime},
		D = [Aprime, Bprime]
	),
	L = [A, B, C | D].

:- pred p(int::out) is det.

p(1).

:- pred q(bool::out) is det.

q(no).

:- pred r(int::in, list(T)::in, pair(T)::out) is det.

r(A, L, BX) :-
	(
		A = 1,
		L = [E1, E2 | _]
	->
		BX = E1 - E2
	;
		error("r: bad input")
	).

:- pred turn_on_origin_debug is det.

:- pragma foreign_proc("C",
	turn_on_origin_debug,
	[will_not_call_mercury, promise_pure],
"
	extern	int	MR_DD_debug_origin;

	MR_DD_debug_origin = 1;
").

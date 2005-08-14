:- module print_table.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module std_util, int, float, string, list.

main -->
	{ p(5, 5, P55) },
	{ p(4, 3, P43) },
	{ p(2, 2, P22) },
	{ p(1, 0, P10) },
	{ q(3, 2, Q32) ->
		MaybeQ32 = yes(Q32)
	;
		MaybeQ32 = no
	},
	{ q(4, 2, Q42) ->
		MaybeQ42 = yes(Q42)
	;
		MaybeQ42 = no
	},
	{ r(3, R3) },
	{ s(3.5, 1, "abc", 1, SA, TA) },
	{ s(3.5, 2, "abc", 2, SB, TB) },
	{ s(3.5, 2, "xyz", 3, SC, TC) },
	{ s(3.5, 2, "xyz", 4, SD, TD) },
	{ s(9.2, 2, "def", 5, SE, TE) },
	{ solutions(t(1, 2), T12) },
	{ solutions(t(2, 2), T22) },
	{ tdone },
	{ solutions(u(1, 2, 2), U12) },
	{ solutions(u(2, 2, 2), U22) },
	{ udone },
	io__write_int(P55),
	io__nl,
	io__write_int(P43),
	io__nl,
	io__write_int(P22),
	io__nl,
	io__write_int(P10),
	io__nl,
	io__write(MaybeQ32),
	io__nl,
	io__write(MaybeQ42),
	io__nl,
	io__write_int(R3),
	io__nl,
	io__write_string(SA),
	io__write_string(" "),
	io__write_float(TA),
	io__nl,
	io__write_string(SB),
	io__write_string(" "),
	io__write_float(TB),
	io__nl,
	io__write_string(SC),
	io__write_string(" "),
	io__write_float(TC),
	io__nl,
	io__write_string(SD),
	io__write_string(" "),
	io__write_float(TD),
	io__nl,
	io__write_string(SE),
	io__write_string(" "),
	io__write_float(TE),
	io__nl,
	io__write(T12),
	io__nl,
	io__write(T22),
	io__nl,
	io__write(U12),
	io__nl,
	io__write(U22),
	io__nl.

:- pred p(int::in, int::in, int::out) is det.
:- pragma memo(p/3).

p(A, B, S) :-
	( B = 0 ->
		S = 0
	;
		p(A, B - 1, S0),
		S = A * B + S0
	).

:- pred q(int::in, int::in, int::out) is semidet.
:- pragma memo(q/3).

q(A, B, S) :-
	( B = 0 ->
		S = 0
	; A = 4 * B ->
		fail
	;
		q(A, B - 1, S0),
		S = A * B + S0
	).

:- pred r(int::in, int::out) is det.
:- pragma memo(r/2).

r(A, S) :-
	( A = 0 ->
		S = 0
	;
		r(A - 1, S0),
		S = A + S0
	).

:- pred s(float::in, int::in, string::in, int::in, string::out, float::out)
	is det.
:- pragma memo(s/6).

s(A, B, C, D, S, T) :-
	string__format("%3.1f", [f(A)], AS),
	string__int_to_string(B, BS),
	string__append_list(["[", AS, C, BS, "]"], S0),
	S = from_char_list(list__condense(
		list__duplicate(D, to_char_list(S0)))),
	T = A + float(D).

:- pred t(int::in, int::in, int::out) is nondet.
:- pragma memo(t/3).

t(A, B, C) :-
	( A = 1 ->
		(
			C = (A * 100) + (B * 10)
		;
			C = (B * 100) + (A * 10)
		)
	;
		fail
	).

:- pred tdone is det.

tdone.

:- pred u(int::in, int::in, int::in, int::out) is nondet.
:- pragma memo(u/4, [value, value, promise_implied, output]).

u(A, B, Bcopy, C) :-
	( A = 1 ->
		(
			C = (A * 100) + (B * 10)
		;
			C = (Bcopy * 100) + (A * 10)
		)
	;
		fail
	).

:- pred udone is det.

udone.

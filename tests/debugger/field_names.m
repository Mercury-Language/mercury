:- module field_names.

:- interface.
:- import_module io.

:- pred main(io__state::di, state::uo) is det.

:- implementation.

:- type t1
	--->	t1f1(
			t1a	:: int,
			t1b	:: int,
				   int,
			t1d	:: int
		)
	;	t1f2(
			t1e	:: int,
				   int,
			t1g	:: int
		).

:- type t2(U)
	--->	t2f(
			t2a	:: U,
				   int,
			t2b	:: t1,
			t2c	:: t1
		).

:- type t3(V)
	--->	some [W] t3f(
			t3a	:: V,
			t3b	:: int,
			t3c	:: W,
			t3d	:: t1
		).

:- type t4	== t2(float).

:- type t5
	--->	t5f(
			t5a	:: t1
		).

:- type t6
	--->	t6f(
				   float
		).

:- type t7
	--->	t7f(
				   float,
				   int
		).

:- type t8
	--->	t8a ; t8b.

main -->
	{ make_t1f1(41, 42, 43, 44, T1F1) },
	{ make_t1f2(51, 52, 53, T1F2) },
	{ make_t2(0.6, 61, T1F1, T1F2, T2) },
	{ make_t3(T1F2, 72, T1F1, T3) },
	{ make_t4(T2, T4) },
	{ make_t5(T1F1, T5) },
	{ make_t6(0.9, T6) },
	{ make_t7(0.9, 77, T7) },
	{ make_t8(T8) },
	io__write(T1F1), nl,
	io__write(T1F2), nl,
	io__write(T2), nl,
	io__write(T3), nl,
	io__write(T4), nl,
	io__write(T5), nl,
	io__write(T6), nl,
	io__write(T7), nl,
	io__write(T8), nl.

:- pred make_t1f1(int::in, int::in, int::in, int::in, t1::out) is det.
make_t1f1(A, B, C, D, t1f1(A, B, C, D)).

:- pred make_t1f2(int::in, int::in, int::in, t1::out) is det.
make_t1f2(A, B, C, t1f2(A, B, C)).

:- pred make_t2(T::in, int::in, t1::in, t1::in, t2(T)::out) is det.
make_t2(A, B, C, D, t2f(A, B, C, D)).

:- pred make_t3(T::in, int::in, t1::in, t3(T)::out) is det.
make_t3(A, B, C, 'new t3f'(A, B, "xyzzy", C)).

:- pred make_t4(t2(float)::in, t4::out) is det.
make_t4(A, A).

:- pred make_t5(t1::in, t5::out) is det.
make_t5(A, t5f(A)).

:- pred make_t6(float::in, t6::out) is det.
make_t6(A, t6f(A)).

:- pred make_t7(float::in, int::in, t7::out) is det.
make_t7(A, B, t7f(A, B)).

:- pred make_t8(t8::out) is det.
make_t8(t8a).

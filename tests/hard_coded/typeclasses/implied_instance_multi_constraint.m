
:- module implied_instance_multi_constraint.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- typeclass a(T) where [
	pred a(T::in, io__state::di, io__state::uo) is det
].
:- typeclass b(T) where [
	pred b(T::in, io__state::di, io__state::uo) is det
].
:- typeclass c(T) where [
	pred c(T::in, io__state::di, io__state::uo) is det
].
:- typeclass d(T) where [
	pred d(T::in, io__state::di, io__state::uo) is det
].

:- type foo(A, B, C) ---> foo(A, B, C).

:- instance a(int) where [
	pred(a/3) is int_a
].

:- instance b(int) where [
	pred(b/3) is int_b
].

:- instance c(int) where [
	pred(c/3) is int_c
].

:- instance d(foo(A, B, C)) <= (a(A), b(B), c(C)) where [
	pred(d/3) is my_d
].

:- pred my_d(foo(A, B, C), io__state, io__state) <= (a(A), b(B), c(C)).
:- mode my_d(in, di, uo) is det.

my_d(foo(A, B, C)) -->
	a(A),
	b(B),
	c(C).

:- pred int_a(int::in, io__state::di, io__state::uo) is det.
int_a(_) --> io__write_string("A\n").
:- pred int_b(int::in, io__state::di, io__state::uo) is det.
int_b(_) --> io__write_string("B\n").
:- pred int_c(int::in, io__state::di, io__state::uo) is det.
int_c(_) --> io__write_string("C\n").

main -->
	d(foo(7, 42, 69)).

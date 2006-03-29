% This module tests the use of existential types,
% including type inference,
% but not including type class constraints.
% This test is designed to test boxing/unboxing
% of types with non-word size, i.e. chars and floats.

:- module existential_float.
:- interface.
:- import_module univ.

:- type foo ---> left ; right.

:- some [T] func call_univ_value(univ) = T.

:- some [T] func my_exist_c = T.
:- some [T] func my_exist_f = T.
:- some [T] func my_exist_fn = T.

:- some [T] pred my_exist_p_multi(T::out) is multi.
:- some [T] pred my_exist_p_semi(foo::in, T::out) is semidet.

:- import_module io.

:- pred main(io__state::di, state::uo) is cc_multi.

:- implementation.
:- import_module int.
:- import_module solutions.

main -->
	foo(univ(my_exist_c)),
	foo(univ(my_exist_f)),
	foo(univ(my_exist_fn)),
	foo(univ(call_my_exist_c)),
	foo(univ(call_my_exist_f)),
	foo(univ(call_my_exist_fn)),
	write(my_exist_c), nl,
	write(my_exist_f), nl,
	write(my_exist_fn), nl,
	write(call_my_exist_c), nl,
	write(call_my_exist_f), nl,
	write(call_my_exist_fn), nl,
	( { call_my_exist_p_semi(left, X1) } ->
		write(X1), nl
	;
		print("no."), nl
	),
	( { call_my_exist_p_semi(right, X2) } ->
		write(X2), nl
	;
		print("no."), nl
	),
	( { my_exist_p_semi(left, X3) } ->
		write(X3), nl
	;
		print("no."), nl
	),
	( { my_exist_p_semi(right, X4) } ->
		write(X4), nl
	;
		print("no."), nl
	),
	{ unsorted_solutions(my_univ_p_multi, List) },
	write(List), nl.

my_exist_c = 'c'.

my_exist_f = 42.0.

my_exist_fn = (func(X) = 2 * X).

my_exist_p_multi(1.0).
my_exist_p_multi(2.0).

my_exist_p_semi(left, 33.3).

call_my_exist_c = my_exist_c.

call_my_exist_f = my_exist_f.

call_my_exist_fn = my_exist_fn.

call_my_exist_p_multi(X) :- my_exist_p_multi(X).

call_my_exist_p_semi(A, B) :- my_exist_p_semi(A, B).

:- mode my_univ_p_multi(out) is multi.
my_univ_p_multi(univ(X)) :- call_my_exist_p_multi(X).

:- pred foo(univ::in, io__state::di, state::uo) is det.
foo(X) -->
	write(univ_value(X)), nl,
	write(call_univ_value(X)), nl.

call_univ_value(Univ) = univ_value(Univ).

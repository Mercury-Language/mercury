% This module tests the use of existential types,
% including type inference,
% but not including type class constraints.
% This test is designed to test boxing/unboxing
% of types with non-word size, i.e. chars and floats.

:- module existential_float.
:- interface.
:- import_module std_util.

:- some [T] func call_univ_value(univ) = T.

:- some [T] func my_exist_c = T.
:- some [T] func my_exist_f = T.
:- some [T] func my_exist_fn = T.

:- import_module io.

:- pred main(io__state::di, state::uo) is det.

:- implementation.
:- import_module int.

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
	write(call_my_exist_fn), nl.

my_exist_c = 'c'.

my_exist_f = 42.0.

my_exist_fn = (func(X) = 2 * X).

call_my_exist_c = my_exist_c.

call_my_exist_f = my_exist_f.

call_my_exist_fn = my_exist_fn.

:- pred foo(univ::in, io__state::di, state::uo) is det.
foo(X) -->
	write(univ_value(X)), nl,
	write(call_univ_value(X)), nl.

call_univ_value(Univ) = univ_value(Univ).

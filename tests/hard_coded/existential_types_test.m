% This module tests the use of existential types,
% including type inference,
% but not including type class constraints.

:- module existential_types_test.
:- interface.
:- import_module std_util.

	% my_univ_value(Univ):
	%	returns the value of the object stored in Univ.
:- some [T] func my_univ_value(univ) = T.

:- some [T] func call_my_univ_value(univ) = T.

:- some [T] func my_exist_t = T.

:- import_module io.

:- pred main(io__state::di, state::uo) is det.
:- implementation.
main -->
	foo(univ(42)),
	foo(univ("blah")),
	foo(univ(my_exist_t)),
	foo(univ(call_my_exist_t)),
	write(my_exist_t), nl,
	write(call_my_exist_t), nl.

my_exist_t = 43.

call_my_exist_t = my_exist_t.

:- pred foo(univ::in, io__state::di, state::uo) is det.
foo(X) -->
	write(my_univ_value(X)), nl,
	write(call_my_univ_value(X)), nl.

call_my_univ_value(Univ) = my_univ_value(Univ).

my_univ_value(Univ) = univ_value(Univ).

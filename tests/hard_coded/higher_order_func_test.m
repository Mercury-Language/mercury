:- module higher_order_func_test.
:- import_module int.

:- interface.
:- import_module std_util, list, io.

:- pred main(io__state::di, io__state::uo) is det.

:- func my_map(func(X) = Y, list(X)) = list(Y).
:- mode my_map(func(in) = out is det, in) = out is det.
:- mode my_map(func(in) = out is semidet, in) = out is semidet.

:- implementation.

my_map(_, []) = [].
my_map(F, [H0|T0]) = [apply(F, H0) | my_map(F, T0)].

main -->
	{ L1 = [1,2,3] },
	{ L2 = my_map((func(X::in) = (Y::out) is det :- Y = 2*X), L1) },
	{ L3 = my_map((func(X2) = Y2 :- Y2 = 5*X2), L2) },
	{ L = my_map(func(X3) = 10*X3, L3) },
	list__foldl(io__write_int, L),
	io__write_string("\n").


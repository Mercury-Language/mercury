
:- module implied_instance_poly.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list.

:- typeclass f(T) where [
	pred p(T::in, io__state::di, io__state::uo) is det
].

:- instance f(int) where [
	pred(p/3) is io__write_int
].

:- instance f(list(T)) <= f(T) where [
	pred(p/3) is my_write_list
].

main --> foo(1), io__nl.

:- pred my_write_list(list(T), io__state, io__state) <= f(T).
:- mode my_write_list(in, di, uo) is det.


my_write_list(X) --> io__write_list(X, ", ", p).


:- pred foo(T, io__state, io__state) <= f(T).
:- mode foo(in, di, uo) is det.

foo(X) -->
	p([X, X, X]).

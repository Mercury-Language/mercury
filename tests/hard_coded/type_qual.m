% Test the use of explicit type qualification using the `with_type` operator.

:- module type_qual.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module list, std_util.

main -->
	test1,
	test2([] `with_type` list(io__state)),
	test3.

:- pred test1(io__state::di, io__state::uo) is det.

test1 -->
	io__read(X `with_type` io__read_result(int)),
	io__write(X),
	nl.

:- pred test2(T::in, io__state::di, io__state::uo) is det.

test2(X) -->
	io__write(type_of(X `with_type` T)),
	nl,
	io__write(type_of(_ `with_type` list(T))),
	nl.

:- pred test3(io__state::di, io__state::uo) is det.

test3 -->
	io__write(empty_list), nl,
	io__write(type_of(empty_list)), nl,
	{ empty(X) },
	io__write(X), nl,
	io__write(type_of(X)), nl.

empty_list = [] `with_type` list(int).

empty([] `with_type` list(int)).


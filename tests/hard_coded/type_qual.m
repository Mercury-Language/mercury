% Test the use of explicit type qualification using the `with_type` operator.

:- module type_qual.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.


:- implementation.
:- import_module bool, list, map, std_util.

main -->
	test1,
	test2([] `with_type` list(io__state)),
	test3,
	test4,
	test5(yes),
	test5(no),
	test6.

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

:- pred test4(io__state::di, io__state::uo) is det.

test4 -->
	{ List = build_list `with_type` TypeOfList },
	io__write(type_of(List)), nl,
	io__write(List), nl,
	{ EmptyList = [] `with_type` TypeOfList },
	io__write(type_of(EmptyList)), nl,
	io__write(EmptyList), nl.

:- pred test5(bool::in, io__state::di, io__state::uo) is det.

	% Test use of same type variable in different clauses.
test5(yes) -->
	{ _ = [1, 2, 3] `with_type` T },
	{ Y = [] `with_type` T },
	io__write(type_of(Y)), nl,
	io__write(Y), nl.
test5(no) -->	
	{ _ = ["1", "2", "3"] `with_type` T },
	{ Y = [] `with_type` T },
	io__write(type_of(Y)), nl,
	io__write(Y), nl.

:- pred test6(io__state::di, io__state::uo) is det.

test6 -->
	(
		{
			X = type_of([] `with_type` list(int))
		<=>
			X = type_of([1, 2, 3])
		}
	->
		io__write_string("bi-implication succeeded\n")
	;
		io__write_string("bi-implication failed\n")
	).

empty_list = [] `with_type` list(int).

empty([] `with_type` list(int)).

:- some [T] func build_list = list(T).

build_list = ["a", "b", "c"].

:- type my_map(_K, V) == map(int, V).

:- pred map_search(my_map(K, V)::in, int::in, V::out) is semidet.

map_search(Map `with_type` map(int, V), Key `with_type` int,
		Value `with_type` V) :- 
	map__search(Map, Key, Value).

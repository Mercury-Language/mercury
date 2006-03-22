% Test the use of explicit type qualification using the : operator.

:- module type_qual.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module type_desc.

main -->
	test1,
	test2([] : list(io__state)),
	test3,
	test4,
	test5(yes),
	test5(no),
	test6.

:- pred test1(io__state::di, io__state::uo) is det.

test1 -->
	io__read(X : io__read_result(int)),
	io__write(X),
	nl.

:- pred test2(T::in, io__state::di, io__state::uo) is det.

test2(X) -->
	io__write(type_of(X : T)),
	nl,
	io__write(type_of(_ : list(T))),
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
	{ List = build_list : TypeOfList },
	io__write(type_of(List)), nl,
	io__write(List), nl,
	{ EmptyList = [] : TypeOfList },
	io__write(type_of(EmptyList)), nl,
	io__write(EmptyList), nl.

:- pred test5(bool::in, io__state::di, io__state::uo) is det.

	% Test use of same type variable in different clauses.
test5(yes) -->
	{ _ = [1, 2, 3] : T },
	{ Y = [] : T },
	io__write(type_of(Y)), nl,
	io__write(Y), nl.
test5(no) -->
	{ _ = ["1", "2", "3"] : T },
	{ Y = [] : T },
	io__write(type_of(Y)), nl,
	io__write(Y), nl.

:- pred test6(io__state::di, io__state::uo) is det.

test6 -->
	(
		{
			X = type_of([] : list(int))
		<=>
			X = type_of([1, 2, 3])
		}
	->
		io__write_string("bi-implication succeeded\n")
	;
		io__write_string("bi-implication failed\n")
	).

% inferred
empty_list = [] : list(int).

% inferred
empty([] : list(int)).

:- some [T] func build_list = list(T).

build_list = ["a", "b", "c"].

:- type my_map(_K, V) == map(int, V).

:- pred map_search(my_map(K, V)::in, int::in, V::out) is semidet.

map_search(Map : map(int, V), Key : int, Value : V) :-
	map__search(Map, Key, Value).

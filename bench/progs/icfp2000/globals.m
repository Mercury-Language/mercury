:- module globals.

:- interface.

:- import_module io.

:- pred init(io__state, io__state).
:- mode init(di, uo) is det.

:- pred get(T1, T2, io__state, io__state).
:- mode get(in, out, di, uo) is det.

:- pred set(T1, T2, io__state, io__state).
:- mode set(in, in, di, uo) is det.

:- implementation.

:- import_module map, require, string, std_util.

init-->
	{ my_map_init(Map) },
	{ type_to_univ(Map, UMap1) },
	{ unsafe_promise_unique(UMap1, UMap) },
	io__set_globals(UMap).

get(Name, Value) -->
	io__get_globals(UMap0),
	(
		{ univ_to_type(UMap0, Map0) }
	->
		(
			{ map__search(Map0, univ(Name), UValue) }
		->
			(
				{ univ_to_type(UValue, Value0) }
			->
				{ Value = Value0 }
			;
				{ error("globals: value has bad type") }
			)
		;
			{ error("get: global not found") }
		)
	;
		{ error("globals: global store stuffed up") }
	).

set(Name, Value) -->
	io__get_globals(UMap0),
	(
		{ univ_to_type(UMap0, Map0) }
	->
		{ type_to_univ(Value, UValue) },
		{ map__set(Map0, univ(Name), UValue, Map) },
		{ type_to_univ(Map, UMap1) },
		{ unsafe_promise_unique(UMap1, UMap) },
		io__set_globals(UMap)
	;
		{ error("globals: global store stuffed up") }
	).

:- pred my_map_init(map(univ, univ)::out) is det.

my_map_init(Map) :-
	map__init(Map).


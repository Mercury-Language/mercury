:- module globals.

:- interface.

:- import_module io.

:- pred init_globals(io__state, io__state).
:- mode init_globals(di, uo) is det.

:- pred get_global(string, T, io__state, io__state).
:- mode get_global(in, out, di, uo) is det.

:- pred set_global(string, T, io__state, io__state).
:- mode set_global(in, in, di, uo) is det.

:- implementation.

:- import_module list, map, require, string, std_util.

init_globals -->
	{ my_map_init(Map) },
	{ type_to_univ(Map, UMap1) },
	{ unsafe_promise_unique(UMap1, UMap) },
	io__set_globals(UMap).

get_global(Name, Value) -->
	io__get_globals(UMap0),
	(
		{ univ_to_type(UMap0, Map0) }
	->
		(
			{ map__search(Map0, Name, UValue) }
		->
			(
				{ univ_to_type(UValue, Value0) }
			->
				{ Value = Value0 }
			;
				{ string__format(
					"globals: value for `%s' has bad type",
					[s(Name)], Str) },
				{ error(Str) }
			)
		;
			{ string__format("globals: %s not found",
				[s(Name)], Str) },
			{ error(Str) }
		)
	;
		{ error("globals: global store stuffed up") }
	).

set_global(Name, Value) -->
	io__get_globals(UMap0),
	(
		{ univ_to_type(UMap0, Map0) }
	->
		{ type_to_univ(Value, UValue) },
		{ map__set(Map0, Name, UValue, Map) },
		{ type_to_univ(Map, UMap1) },
		{ unsafe_promise_unique(UMap1, UMap) },
		io__set_globals(UMap)
	;
		{ error("globals: global store stuffed up") }
	).

:- pred my_map_init(map(string, univ)::out) is det.

my_map_init(Map) :-
	map__init(Map).

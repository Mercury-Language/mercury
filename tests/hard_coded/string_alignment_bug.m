% mercury 0.8 failed this test on some architectures,
% because string literals were not aligned but deep_copy()
% was assuming that they were.

:- module string_alignment_bug.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module bool, int, list, map, require, set_ordlist, std_util, string.

main -->
	init_globals,
	{ gen_tiles(10, 10, Tiles) },
	set_global("Tiles", Tiles),
	{ init_selection(Selection) },
	set_global("Selection", Selection),
	{ init_file(MFN) },
	set_global("CurrentFile", MFN).
	%main(bedit__setup, ["robot"]).

:- pred init_file(maybe(string)::out) is det.
init_file(no).

%------------------------------------------------------------------------------%

:- type pos      ==      pair(int).

:- type selection	==	set_ordlist(pos).

:- pred init_selection(selection::out) is det.

init_selection(Sel) :-
	set_ordlist__init(Sel).

%------------------------------------------------------------------------------%

:- type tiles == map(pos, tile).
:- type tile ---> tile(kind, list(attr)).
:- type kind ---> plain ; pit ; gear(chirality) ; conv(dir).
:- type chirality ---> clock ; anti.
:- type attr ---> wall(dir) ; start ; flag(int).
:- type dir ---> north ; south ; east ; west.

:- pred gen_tiles(int, int, map(pos, tile)).
:- mode gen_tiles(in, in, out) is det.

gen_tiles(Xmax, Ymax, Tiles) :-
	map__init(Tiles0),
	AllPos = (pred(Pos::out) is nondet :-
		between(0, Xmax-1, X),
		between(0, Ymax-1, Y),
		Pos = X - Y
	),
	AddTile = (pred(Pos::in, T0::in, T::out) is det :-
		map__set(T0, Pos, tile(plain, []), T)
	),
	aggregate(AllPos, AddTile, Tiles0, Tiles).

%------------------------------------------------------------------------------%

:- pred between(int, int, int).
:- mode between(in, in, out) is nondet.

between(Min, Max, I) :-
	Min =< Max,
	(
		I = Min
	;
		Min1 = Min + 1,
		between(Min1, Max, I)
	).

%------------------------------------------------------------------------------%

:- pred init_globals(io__state, io__state).
:- mode init_globals(di, uo) is det.

:- pred get_global(string, T, io__state, io__state).
:- mode get_global(in, out, di, uo) is det.

:- pred set_global(string, T, io__state, io__state).
:- mode set_global(in, in, di, uo) is det.

:- import_module list, map, require, string, std_util.

init_globals -->
	{ my_map_init(Map) },
	{ type_to_univ(Map, UMap1) },
	{ copy(UMap1, UMap) },
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
		io__write_string("Current global store:\n"),
		io__write(Map0),
		nl,
		io__write_string("Adding `"),
		io__write_string(Name),
		io__write_string("': "),
		io__write(Value),
		nl,
		{ map__set(Map0, Name, UValue, Map) },
		io__write_string("New global store:\n"),
		io__write(Map),
		nl,
		{ type_to_univ(Map, UMap1) },
		{ copy(UMap1, UMap) },
		io__set_globals(UMap)
	;
		{ error("globals: global store stuffed up") }
	).

:- pred my_map_init(map(string, univ)::out) is det.

my_map_init(Map) :-
	map__init(Map).

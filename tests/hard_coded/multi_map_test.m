:- module multi_map_test.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module multi_map, list, std_util.

main -->
	{ multi_map__from_corresponding_lists([1,  2,  1], [11, 22, 13], M) },
	{ multi_map__lookup(M, 1, Vs0) },
	{ list__sort_and_remove_dups(Vs0, Vs) },
	io__write(Vs),
	io__nl.


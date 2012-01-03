% Test type specialization of higher-order terms (the map.lookup).
:- module type_spec_ho_term.

:- interface.

:- import_module io.

:- pred main(io.state::di, io.state::uo) is det.

:- implementation.

:- import_module list, map, term, varset.

main -->
	{ varset.init(VarSet0 `with_type` varset) },
	{ varset.new_vars(4, Vars, VarSet0, _VarSet) },
	{ map.from_corresponding_lists(Vars, ["a", "b", "c", "d"], Map) },
	{ lookup_list(Map,
		[list.det_index1(Vars, 1), list.det_index1(Vars, 3)],
		List) },
	io.write_list(List, ", ", io.write_string),
	io.nl.

:- pred lookup_list(map(T, U)::in, list(T)::in, list(U)::out) is det.
:- pragma type_spec(lookup_list/3, T = var).

lookup_list(Map, List0, List) :-
	list.map(map.lookup(Map), List0, List).


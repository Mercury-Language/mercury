% Test type specialization of higher-order terms (the map__lookup).
:- module type_spec_ho_term.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, map, term, varset.

main -->
	{ varset__init(VarSet0 `with_type` varset) },
	{ varset__new_vars(VarSet0, 4, Vars, _VarSet) },
	{ map__from_corresponding_lists(Vars, ["a", "b", "c", "d"], Map) },
	{ lookup_list(Map,
		[list__index1_det(Vars, 1), list__index1_det(Vars, 3)],
		List) },
	io__write_list(List, ", ", io__write_string),
	io__nl.

:- pred lookup_list(map(T, U)::in, list(T)::in, list(U)::out) is det.
:- pragma type_spec(lookup_list/3, T = var).

lookup_list(Map, List0, List) :-
	list__map(map__lookup(Map), List0, List).


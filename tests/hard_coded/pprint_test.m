% Test to_doc's special handling of some types.

:- module pprint_test.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module int, varset, term, list, sparse_bitset, map, array, pprint.

:- func line_length = int.

line_length = 72.

:- pred get_new_vars(var(int)::out, var(int)::out) is det.

get_new_vars(Var1, Var2) :-
	varset__init(VarSet0),
	varset__new_var(VarSet0, Var1, VarSet1),
	varset__new_var(VarSet1, Var2, _VarSet2).

main -->
	{ get_new_vars(Var1, Var2) },
 	pprint__write(line_length, to_doc(Var1)),
	io__nl,
 	pprint__write(line_length, to_doc(Var2)),
	io__nl,

 	pprint__write(line_length, to_doc([5, 6])),
	io__nl,
 	pprint__write(line_length, to_doc(["five", "six", "seven"])),
	io__nl,
 	pprint__write(line_length, to_doc([Var1, Var2])),
	io__nl,

	{ Set0 = sparse_bitset__init },
	{ sparse_bitset__insert(Set0, 42, Set1) },
	{ sparse_bitset__insert(Set1, 84, Set2) },
 	pprint__write(line_length, to_doc(Set1)),
	io__nl,
 	pprint__write(line_length, to_doc(Set2)),
	io__nl,

	{ Map0 = map__init },
	{ map__det_insert(Map0, 41, 42, Map1) },
	{ map__det_insert(Map1, 82, 83, Map2) },
 	pprint__write(line_length, to_doc(Map1)),
	io__nl,
 	pprint__write(line_length, to_doc(Map2)),
	io__nl,

	{ array__from_list([1, 2, 3], Array) },
 	pprint__write(line_length, to_doc(Array)),
	io__nl,

 	pprint__write(line_length, to_doc({7, 8, "abc"})),
	io__nl.

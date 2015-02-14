%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test to_doc's special handling of some types.

:- module pprint_test.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module array.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pprint.
:- import_module sparse_bitset.
:- import_module term.
:- import_module varset.

:- func line_length = int.

line_length = 72.

:- pred get_new_vars(var(int)::out, var(int)::out) is det.

get_new_vars(Var1, Var2) :-
    varset__init(VarSet0),
    varset__new_var(Var1, VarSet0, VarSet1),
    varset__new_var(Var2, VarSet1, _VarSet2).

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
    { sparse_bitset__insert(42, Set0, Set1) },
    { sparse_bitset__insert(84, Set1, Set2) },
    pprint__write(line_length, to_doc(Set1)),
    io__nl,
    pprint__write(line_length, to_doc(Set2)),
    io__nl,

    { Map0 = map__init },
    { map__det_insert(41, 42, Map0, Map1) },
    { map__det_insert(82, 83, Map1, Map2) },
    pprint__write(line_length, to_doc(Map1)),
    io__nl,
    pprint__write(line_length, to_doc(Map2)),
    io__nl,

    { array__from_list([1, 2, 3], Array) },
    pprint__write(line_length, to_doc(Array)),
    io__nl,

    pprint__write(line_length, to_doc({7, 8, "abc"})),
    io__nl.

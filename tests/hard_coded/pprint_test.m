%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test to_doc's special handling of some types.

:- module pprint_test.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

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
    varset.init(VarSet0),
    varset.new_var(Var1, VarSet0, VarSet1),
    varset.new_var(Var2, VarSet1, _VarSet2).

main(!IO) :-
    get_new_vars(Var1, Var2),
    pprint.write(line_length, to_doc(Var1), !IO),
    io.nl(!IO),
    pprint.write(line_length, to_doc(Var2), !IO),
    io.nl(!IO),

    pprint.write(line_length, to_doc([5, 6]), !IO),
    io.nl(!IO),
    pprint.write(line_length, to_doc(["five", "six", "seven"]), !IO),
    io.nl(!IO),
    pprint.write(line_length, to_doc([Var1, Var2]), !IO),
    io.nl(!IO),

    Set0 = sparse_bitset.init,
    sparse_bitset.insert(42, Set0, Set1),
    sparse_bitset.insert(84, Set1, Set2),
    pprint.write(line_length, to_doc(Set1), !IO),
    io.nl(!IO),
    pprint.write(line_length, to_doc(Set2),!IO),
    io.nl(!IO),

    Map0 = map.init,
    map.det_insert(41, 42, Map0, Map1),
    map.det_insert(82, 83, Map1, Map2),
    pprint.write(line_length, to_doc(Map1), !IO),
    io.nl(!IO),
    pprint.write(line_length, to_doc(Map2), !IO),
    io.nl(!IO),

    array.from_list([1, 2, 3], Array),
    pprint.write(line_length, to_doc(Array),!IO),
    io.nl(!IO),

    pprint.write(line_length, to_doc({7, 8, "abc"}), !IO),
    io.nl(!IO).

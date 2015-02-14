%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module term_size_cells.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module univ.

:- type tree(K, V)
    --->    leaf
    ;       node(tree(K, V), K, V, tree(K, V)).

main(!IO) :-
    static(IntList, StringList, Tree),
    dynamic(IntList, DoubleIntList, FloatList, PairList, UnivList, Univ),
    io.write(IntList, !IO),
    io.nl(!IO),
    io.write(StringList, !IO),
    io.nl(!IO),
    io.write(Tree, !IO),
    io.nl(!IO),
    io.write(DoubleIntList, !IO),
    io.nl(!IO),
    io.write(FloatList, !IO),
    io.nl(!IO),
    io.write(PairList, !IO),
    io.nl(!IO),
    io.write(UnivList, !IO),
    io.nl(!IO),
    io.write(Univ, !IO),
    io.nl(!IO).

% Return some static terms.

:- pred static(list(int)::out, list(string)::out, tree(string, int)::out)
    is det.

static(IntList, StringList, Tree) :-
    IntList = [1, 2, 3],
    StringList = ["a", "bb", "ccc"],
    Tree = node(leaf, "one", 1, node(leaf, "two", 2, leaf)).

% Return some dynamic terms.

:- pred dynamic(list(int)::in, list(int)::out, list(float)::out,
    list(pair(int, float))::out, list(univ)::out, univ::out) is det.

dynamic(IntList, DoubleIntList, FloatList, PairList, UnivList, Univ) :-
    list.append(IntList, IntList, DoubleIntList),
    FloatList = list.map(float, IntList),
    PairList = list.map(pair_float, IntList),
    UnivList = list.map(convert_type_to_univ, IntList),
    Univ = convert_type_to_univ(
        node(node(leaf, 2, 2.0 - "two", leaf), 1, 1.0 - "one", leaf)).

:- func pair_float(int) = pair(int, float).

pair_float(Int) = Int - float(Int).

:- func convert_type_to_univ(T) = univ.

convert_type_to_univ(T) = Univ :-
    type_to_univ(T, Univ).

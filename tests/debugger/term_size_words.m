:- module term_size_words.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module std_util, list, int, float.

:- type tree(K, V)
	--->	leaf
	;	node(tree(K, V), K, V, tree(K, V)).

main(!IO) :-
	static(IntList, StringList, Tree),
	dynamic(IntList, DoubleIntList, FloatList, PairList, UnivList, Univ),
	io__write(IntList, !IO),
	io__nl(!IO),
	io__write(StringList, !IO),
	io__nl(!IO),
	io__write(Tree, !IO),
	io__nl(!IO),
	io__write(DoubleIntList, !IO),
	io__nl(!IO),
	io__write(FloatList, !IO),
	io__nl(!IO),
	io__write(PairList, !IO),
	io__nl(!IO),
	io__write(UnivList, !IO),
	io__nl(!IO),
	io__write(Univ, !IO),
	io__nl(!IO).

% Return some static terms.

:- pred static(list(int)::out, list(string)::out, tree(string, int)::out)
	is det.

static(IntList, StringList, Tree) :-
	IntList = [1,2,3],
	StringList = ["a", "bb", "ccc"],
	Tree = node(leaf, "one", 1, node(leaf, "two", 2, leaf)).

% Return some dynamic terms.

:- pred dynamic(list(int)::in, list(int)::out, list(float)::out,
	list(pair(int, float))::out, list(univ)::out, univ::out) is det.

dynamic(IntList, DoubleIntList, FloatList, PairList, UnivList, Univ) :-
	list__append(IntList, IntList, DoubleIntList),
	FloatList = list__map(float, IntList),
	PairList = list__map(pair_float, IntList),
	UnivList = list__map(convert_type_to_univ, IntList),
	Univ = convert_type_to_univ(
		node(node(leaf, 2, 2.0 - "two", leaf), 1, 1.0 - "one", leaf)).

:- func pair_float(int) = pair(int, float).

pair_float(Int) = Int - float(Int).

:- func convert_type_to_univ(T) = univ.

convert_type_to_univ(T) = Univ :-
	type_to_univ(T, Univ).

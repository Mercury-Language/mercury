% This is a regression test. Before 15/3/2002, we got incorrect answers
% due to simplistic treatment of type_ctor_descs.

:- module type_ctor_desc_manip.

:- interface.
:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.
:- import_module int, float, list, map, type_desc.

main -->
	{ map__init(MapII0) },
	{ map__det_insert(MapII0, 1, 1, MapII) },
	{ map__init(MapIF0) },
	{ map__det_insert(MapIF0, 1, 1.0, MapIF) },
	{ map__init(MapFI0) },
	{ map__det_insert(MapFI0, 1.0, 1, MapFI) },

	{ TypeF1a = type_of(f1a) },
	{ TypeF1b = type_of(f1b) },
	{ TypeF2  = type_of(f2) },
	{ TypeP1  = type_of(p1) },
	{ TypeP2  = type_of(p2) },
	{ TypeT1  = type_of({"one"}) },
	{ TypeT3  = type_of({"one", 2, 3.0}) },
	{ TypeLI  = type_of([1]) },
	{ TypeLF  = type_of([1.0]) },
	{ TypeMFI = type_of(MapFI) },
	{ TypeMIF = type_of(MapIF) },
	{ TypeMII = type_of(MapII) },

	{ TypeDescs = [TypeF1a, TypeF1b, TypeF2, TypeP1, TypeP2,
		TypeT1, TypeT3, TypeLF, TypeLI, TypeMFI, TypeMIF, TypeMII] },
	{ TypeCtorDescs0 = list__map(type_ctor, TypeDescs) },
	{ TypeCtorDescs = list__remove_adjacent_dups(TypeCtorDescs0) },

	list__foldl(test_deconstruct, TypeDescs),
	test_comparisons_among(TypeDescs, TypeDescs),
	test_comparisons_among(TypeCtorDescs, TypeCtorDescs).

:- pred test_deconstruct(type_desc::in, io__state::di, io__state::uo) is det.

test_deconstruct(Type) -->
	{ type_ctor_and_args(Type, TypeCtor, TypeArgs) },
	io__write(TypeCtor),
	io__write_string(" "),
	io__write(TypeArgs),
	io__nl.

:- pred test_comparisons_among(list(T)::in, list(T)::in,
	io__state::di, io__state::uo) is det.

test_comparisons_among(L, All) -->
	(
		{ L = [] }
	;
		{ L = [H | T] },
		test_comparisons_with(H, All),
		test_comparisons_among(T, All)
	).

:- pred test_comparisons_with(T::in, list(T)::in,
	io__state::di, io__state::uo) is det.

test_comparisons_with(X, L) -->
	(
		{ L = [] }
	;
		{ L = [H | T] },
		test_comparison(X, H),
		test_comparisons_with(X, T)
	).

:- pred test_comparison(T::in, T::in, io__state::di, io__state::uo) is det.

test_comparison(X, Y) -->
	{ compare(R, X, Y) },
	(
		{ R = (<) },
		io__write(X),
		io__write_string(" < "),
		io__write(Y),
		io__nl
	;
		{ R = (=) },
		io__write(X),
		io__write_string(" = "),
		io__write(Y),
		io__nl
	;
		{ R = (>) },
		io__write(X),
		io__write_string(" > "),
		io__write(Y),
		io__nl
	).

:- func f1a(float) = float.

f1a(X) = Y :-
	Y = X + 1.0.

:- func f1b(int) = int.

f1b(X) = Y :-
	Y = X + 1.

:- func f2(int, int) = int.

f2(X, Y) = Z :-
	Z = X + Y.

:- pred p1(int::in) is semidet.

p1(X) :-
	X < 42.

:- pred p2(int::in, int::in) is semidet.

p2(X, Y) :-
	X < Y.

% A test of types with floating point fields.

:- module float_field.
:- interface.
:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- type foo.
:- type bar.
:- type baz.

:- type foo2 ---> foo2(float). % no_tag type
:- type bar2 ---> bar2(int, float, int). % ordinary d.u. type
:- type baz2 == float. % equivalence type

:- func foo_val(foo) = float.
:- func bar_val(bar) = float.
:- func baz_val(baz) = float.

:- func foo2_val(foo2) = float.
:- func bar2_val(bar2) = float.
:- func baz2_val(baz2) = float.

:- implementation.
:- import_module float, math, string, list.

:- type foo ---> foo(float). % no_tag type
:- type bar ---> bar(int, float, int). % ordinary d.u. type
:- type baz == float. % equivalence type

foo_val(foo(X)) = X.
bar_val(bar(_, X, _)) = X.
baz_val(X) = X.

foo2_val(foo2(X)) = X.
bar2_val(bar2(_, X, _)) = X.
baz2_val(X) = X.

main -->
	{ Foo = foo(1.0) },
	print(Foo), nl,
	print(foo_val(Foo)), nl,
	{ Bar = bar(2, 3.0, 4) },
	print(Bar), nl,
	print(bar_val(Bar)), nl,
	{ Baz = 5.0 },
	print(Baz), nl,
	print(baz_val(Baz)), nl,

	{ Foo2 = foo2(1.0) },
	print(Foo2), nl,
	print(foo2_val(Foo2)), nl,
	{ Bar2 = bar2(2, 3.0, 4) },
	print(Bar2), nl,
	print(bar2_val(Bar2)), nl,
	{ Baz2 = 5.0 },
	print(Baz2), nl,
	print(baz2_val(Baz2)), nl.


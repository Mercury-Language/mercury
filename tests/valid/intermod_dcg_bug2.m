:- module intermod_dcg_bug2.

:- interface.

:- import_module io.

:- pred foo(io__state, io__state).
:- mode foo(di, uo) is det.

:- implementation.

:- import_module int, list.

:- pragma inline(foo/2).

foo -->
	{ list__foldl(
		(pred(_::in, di, uo) is det -->
			=(Var0),
			:=(Var0 + 1)
		), [1, 2, 3], 0, Count) },	
	io__write_int(Count),
	
	list__foldl(
		(pred(X::in, di, uo) is det -->
			io__write(X)
		), [1, 2, 3]).

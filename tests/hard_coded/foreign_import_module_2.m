:- module foreign_import_module_2.

:- interface.

:- pred foo(int::in, int::out) is det.

:- implementation.

:- import_module int.

:- pragma export(foo(in, out), "foo").

foo(X, X+1).

:- pragma foreign_code("MC++", "
	static void foo2(MR_Integer X, MR_Ref(MR_Integer) Y)
	{
		*Y = X + 1;
	}
").

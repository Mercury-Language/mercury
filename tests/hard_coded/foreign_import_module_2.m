:- module foreign_import_module_2.

:- interface.

:- pred foo(int::in, int::out) is det.

:- implementation.

:- import_module int.

:- pragma export(foo(in, out), "foo").

foo(X, X+1).

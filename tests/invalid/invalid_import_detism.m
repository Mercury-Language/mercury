:- module invalid_import_detism.

:- interface.

:- pred foo(int::in, int::out) is nondet.
:- pred bar(int::in, int::out) is multi.

:- implementation.

:- pragma import(foo(in, out), "IMPORTED_FOO").
:- pragma import(bar(in, out), "IMPORTED_BAR").

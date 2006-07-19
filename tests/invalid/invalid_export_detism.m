:- module invalid_export_detism.

:- interface.

:- pred foo(int::in, int::out) is nondet.

:- implementation.

:- pragma foreign_export("C", foo(in, out), "EXPORTED_FOO").

foo(1, 2).
foo(2, 3).
foo(3, 4).
foo(3, 5).
foo(3, 6).


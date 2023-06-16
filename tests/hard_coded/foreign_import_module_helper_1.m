%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module foreign_import_module_helper_1.

:- interface.

:- pred foo(int::in, int::out) is det.

:- implementation.

:- import_module int.

:- pragma foreign_export("C", foo(in, out), "foo").
:- pragma foreign_export("C#", foo(in, out), "foo").
:- pragma foreign_export("Java", foo(in, out), "foo").

foo(X, X+1).

:- pragma foreign_code("C#",
"
    static void foo2(int X, ref int Y)
    {
        Y = X + 1;
    }
").

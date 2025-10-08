%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module foreign_import_module.

:- interface.

:- import_module int.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- pred bar(int::in, int::out) is det.

:- implementation.

main(!IO) :-
    bar(41, X),
    io.write_line(X, !IO),

    bar2(41, Y),
    io.write_line(Y, !IO).

:- pragma foreign_import_module(c,      foreign_import_module_helper_1).
:- pragma foreign_import_module(csharp, foreign_import_module_helper_1).
:- pragma foreign_import_module(java,   foreign_import_module_helper_1).

:- pragma foreign_proc("C",
    bar(X::in, Y::out),
    [may_call_mercury, promise_pure],
"
    foo(X, &Y);
").
:- pragma foreign_proc("C#",
    bar(X::in, Y::out),
    [may_call_mercury, promise_pure],
"
    int Y1 = 0, Y2 = 0;

    Y1 = foreign_import_module_helper_1.foo(X);
    foreign_import_module_helper_1.foo2(X, ref Y2);

    if (Y1 == Y2) {
        Y = Y1;
    } else {
        throw new System.Exception(""Y1 != Y2"");
    }
").
:- pragma foreign_proc("Java",
    bar(X::in, Y::out),
    [may_call_mercury, promise_pure],
"
    Y = foreign_import_module_helper_1.foo(X);
").

% XXX The definition of "bar2" is identical to just plain "bar";
% the only difference between them is that only "bar" is exported.
% (There used to be more differences, but they seemed to deal with
% peculiarities of the IL backend, which is not an issue anymore.)
% XXX *If* this test has any purpose beyond testing the foreign_import_module
% pragma (which is NOT clear), and *if* the exported/not exported distinction
% has any bearing on this extra purpose, then both should be documented.
% Otherwise, I see no reason not to delete "bar2".

:- pred bar2(int::in, int::out) is det.
:- pragma foreign_proc("C",
    bar2(X::in, Y::out),
    [may_call_mercury, promise_pure],
"
    foo(X, &Y);
").
:- pragma foreign_proc("C#",
    bar2(X::in, Y::out),
    [may_call_mercury, promise_pure],
"
    int Y1 = 0, Y2 = 0;

    Y1 = foreign_import_module_helper_1.foo(X);
    foreign_import_module_helper_1.foo2(X, ref Y2);

    if (Y1 == Y2) {
        Y = Y1;
    } else {
        throw new System.Exception(""Y1 != Y2"");
    }
").
:- pragma foreign_proc("Java",
    bar2(X::in, Y::out),
    [may_call_mercury, promise_pure],
"
    Y = foreign_import_module_helper_1.foo(X);
").

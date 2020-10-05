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
    io.write(X, !IO),
    io.nl(!IO),

    bar2(41, Y),
    io.write(Y, !IO),
    io.nl(!IO).

:- pragma foreign_import_module(c, foreign_import_module_2).
:- pragma foreign_import_module(java, foreign_import_module_2).
% not actually necessary in Erlang
% :- pragma foreign_import_module(erlang, foreign_import_module_2).

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
    int Y1, Y2;

    foreign_import_module_2.mercury_code.foo(X, ref Y1);
    foreign_import_module_2__csharp_code.mercury_code.foo2(X, ref Y2);

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
    Y = foreign_import_module_2.foo(X);
").
:- pragma foreign_proc("Erlang",
    bar(X::in, Y::out),
    [may_call_mercury, promise_pure],
"
    Y = foreign_import_module_2:foo(X)
").

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

    foreign_import_module_2.mercury_code.foo(X, ref Y1);
    foreign_import_module_2__cpp_code.mercury_code.foo2(X, ref Y2);

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
    Y = foreign_import_module_2.foo(X);
").
:- pragma foreign_proc("Erlang",
    bar2(X::in, Y::out),
    [may_call_mercury, promise_pure],
"
    Y = foreign_import_module_2:foo(X)
").

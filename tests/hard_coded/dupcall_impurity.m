%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a test case to test that duplicate call elimination
% takes purity into account.

:- module dupcall_impurity.
:- interface.
:- import_module io.

:- impure pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string.

:- impure pred test1(io::di, io::uo) is det.
:- impure pred test2(io::di, io::uo) is det.

main -->
    impure test1,
    impure test2.

test1 -->
    { impure next_x(X0) },
    { impure next_x(X1) },
    print(X0), nl,
    print(X1), nl.

test2 -->
    { semipure get_x(X0) },
    { impure incr_x },
    { semipure get_x(X1) },
    print(X0), nl,
    print(X1), nl.

:- semipure pred get_x(int::out) is det.
:- impure pred next_x(int::out) is det.
:- impure pred incr_x is det.

:- pragma foreign_decl("C", "extern int my_global;").
:- pragma foreign_code("C", "int my_global;").

:- pragma foreign_proc("C",
    get_x(X::out),
    [will_not_call_mercury, promise_semipure],
"
    X = my_global;
").

:- pragma foreign_proc("C",
    next_x(X::out),
    [will_not_call_mercury],
"
    X = my_global++;"
).

:- pragma foreign_proc("C",
    incr_x,
    [will_not_call_mercury],
"
    my_global++;
").

:- pragma foreign_code("C#", "static int my_global;").

:- pragma foreign_proc("C#", get_x(X::out),
        [promise_semipure], "X = my_global;").
:- pragma foreign_proc("C#", next_x(X::out), [], "X = my_global++;").
:- pragma foreign_proc("C#", incr_x, [], "my_global++;").

:- pragma foreign_code("Java", "static int my_global;").

:- pragma foreign_proc("Java",
    get_x(X::out),
    [will_not_call_mercury, promise_semipure],
"
        X = my_global;
").

:- pragma foreign_proc("Java",
    next_x(X::out),
    [will_not_call_mercury],
"
        X = my_global++;
").

:- pragma foreign_proc("Java",
    incr_x,
    [will_not_call_mercury],
"
        my_global++;
").

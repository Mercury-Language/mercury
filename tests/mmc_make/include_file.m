:- module include_file.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- pragma foreign_decl("C", include_file("inc/decl.h")).
:- pragma foreign_code("C", include_file("inc/code.c")).

:- pragma foreign_decl("Java", include_file("inc/decl.java")).
:- pragma foreign_code("Java", include_file("inc/code.java")).

:- pragma foreign_decl("C#", include_file("inc/decl.cs")).
:- pragma foreign_code("C#", include_file("inc/code.cs")).

:- pragma foreign_decl("Erlang", include_file("inc/decl.erl")).
:- pragma foreign_code("Erlang", include_file("inc/code.erl")).

:- pred test(int::out, int::out) is det.

:- pragma foreign_proc("C",
    test(X::out, Y::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = ones();
    Y = twos();
").

:- pragma foreign_proc("Java",
    test(X::out, Y::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = Ones.ones();
    Y = twos();
").

:- pragma foreign_proc("C#",
    test(X::out, Y::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = Ones.ones();
    Y = twos();
").

:- pragma foreign_proc("Erlang",
    test(X::out, Y::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = ones(),
    Y = twos()
").

main(!IO) :-
    test(X, Y),
    io.write_int(X, !IO),
    io.nl(!IO),
    io.write_int(Y, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et

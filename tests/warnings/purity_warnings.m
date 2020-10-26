%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Various checks that impurity declarations are treated properly.
% XXX We miss some things that we should warn about: see the XXXs below.
%
%---------------------------------------------------------------------------%

:- module purity_warnings.
:- interface.

:- import_module io.
:- impure pred main(io::di, io::uo) is det.

:- implementation.

:- import_module string.

main(!IO) :-
    impure impure_pred1(!IO),
    impure impure_pred2(!IO),
    semipure semipure_pred(!IO),
    impure impure_method1a(!IO),
    impure impure_method2a(!IO),
    semipure semipure_method_a(!IO),
    impure impure_method1b(!IO),
    impure impure_method2b(!IO),
    semipure semipure_method_b(!IO),
    semipure io.write_string("main 1\n", !IO),              % warn
    impure io.print("main 2\n", !IO).                       % warn

:- impure pred impure_pred1(io::di, io::uo) is det.         % warn
impure_pred1(!IO) :-
    io.write_string("impure_pred1\n", !IO).

:- impure pred impure_pred2(io::di, io::uo) is det.         % warn
impure_pred2(!IO) :-
    io.write_string("impure_pred2\n", !IO),
    semipure get_x(X),
    io.print("X = ", !IO),
    io.print(X, !IO),
    io.nl(!IO).

:- semipure pred semipure_pred(io::di, io::uo) is det.
semipure_pred(!IO) :-
    semipure io.write_string("semipure_pred1\n", !IO).      % warn

:- typeclass foo(IO) where [
    (impure pred impure_method1a(IO::di, IO::uo) is det),
    (impure pred impure_method1b(IO::di, IO::uo) is det),
    (impure pred impure_method2a(IO::di, IO::uo) is det),
    (impure pred impure_method2b(IO::di, IO::uo) is det),
    (semipure pred semipure_method_a(IO::di, IO::uo) is det),
    (semipure pred semipure_method_b(IO::di, IO::uo) is det)
].

:- instance foo(io) where [
    pred(impure_method1a/2) is impure_method1a_impl,
    pred(impure_method2a/2 )is impure_method2a_impl,
    pred(semipure_method_a/2 )is semipure_method_a_impl,
    ( impure_method1b(!IO) :-
        impure io.print("impure_method1b\n", !IO)           % XXX should warn
    ),
    ( impure_method2b(!IO) :-
        io.write_string("impure_method2b\n", !IO),
        semipure get_x(X),
        io.print("X = ", !IO),
        io.print(X, !IO),
        io.nl(!IO)
    ),
    ( semipure_method_b(!IO) :-
        semipure io.print("semipure_method_b\n", !IO)       % XXX should warn
    )
].

:- impure pred impure_method1a_impl(io::di, io::uo) is det.
impure_method1a_impl(!IO) :-
    impure io.print("impure_method1a_impl\n", !IO).         % warn

:- semipure pred impure_method2a_impl(io::di, io::uo) is det.
impure_method2a_impl(!IO) :-
    io.write_string("impure_method2a_impl\n", !IO),
    semipure get_x(X),
    io.print("X = ", !IO),
    io.print(X, !IO),
    io.nl(!IO).

:- semipure pred semipure_method_a_impl(io::di, io::uo) is det.
semipure_method_a_impl(!IO) :-
    semipure print("semipure_method_a_impl\n", !IO).        % warn

:- pragma foreign_decl("C", "extern int x;").
:- pragma foreign_code("C", "int x = 0;").
:- pragma foreign_code("C#", "
static int x = 0;
").
:- pragma foreign_code("Java", "
static int x = 0;
").

:- impure pred set_x(int::in) is det.
:- pragma foreign_proc("C", set_x(X::in), [will_not_call_mercury], "x=X;" ).
:- pragma foreign_proc("C#", set_x(X::in), [will_not_call_mercury], "x=X;" ).
:- pragma foreign_proc("Java", set_x(X::in), [will_not_call_mercury], "x=X;" ).

:- impure pred incr_x is det.
:- pragma foreign_proc("C", incr_x, [will_not_call_mercury], "++x;" ).
:- pragma foreign_proc("C#", incr_x, [will_not_call_mercury], "++x;" ).
:- pragma foreign_proc("Java", incr_x, [will_not_call_mercury], "++x;" ).

:- semipure pred get_x(int::out) is det.
:- pragma promise_semipure(get_x/1).
:- pragma foreign_proc("C", get_x(X::out), [will_not_call_mercury], "X=x;").
:- pragma foreign_proc("C#", get_x(X::out), [will_not_call_mercury], "X=x;").
:- pragma foreign_proc("Java", get_x(X::out), [will_not_call_mercury], "X=x;").

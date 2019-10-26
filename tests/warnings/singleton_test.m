%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% The .exp files are for C, C#, Java and Erlang respectively.

:- module singleton_test.
:- interface.
:- import_module io, list.

:- pred my_append(list(int), list(int), list(int)).
:- mode my_append(in, in, out) is det.

:- func my_append_func(list(int), list(int)) = list(int).
:- mode my_append_func(in, in) = out is det.

:- func my_c_func(int, int) = int.
:- mode my_c_func(in, in) = out is det.

:- pred my_c_pred(int, int, int).
:- mode my_c_pred(in, in, out) is det.

:- pred c_hello_world(string::in, io__state::di, io__state::uo) is det.

:- pred test_head(int::in, int::in, int::in, int::in, int::out, int::out)
    is det.

:- implementation.

my_append([], L, L) :-
    disable_warning [singleton_vars] L = L1,
    L = L2.
my_append([H | T], L, [H | NT]) :-
    my_append(T, L, NT).

my_append_func([], L) = L :- L1 = L2.
my_append_func([H | T], L) = [H | my_append_func(L, L)].

:- pragma foreign_proc("C", my_c_pred(X::in, Y::in, Z::out),
    [promise_pure, will_not_call_mercury], "
    Z = 2 * X;
").
:- pragma foreign_proc("C#", my_c_pred(X::in, Y::in, Z::out),
    [promise_pure, will_not_call_mercury], "
    Z = 2 * X;
").
:- pragma foreign_proc("Java", my_c_pred(X::in, Y::in, Z::out),
    [promise_pure, will_not_call_mercury], "
    Z = 2 * X;
").
:- pragma foreign_proc("Erlang", my_c_pred(X::in, Y::in, Z::out),
    [promise_pure, will_not_call_mercury], "
    Z = 2 * X
").

:- pragma foreign_proc("C", my_c_func(X::in, Y::in) = (Z::out),
    [promise_pure, will_not_call_mercury], "
    Z = 2 * Y;
").
:- pragma foreign_proc("C#", my_c_func(X::in, Y::in) = (Z::out),
    [promise_pure, will_not_call_mercury], "
    Z = 2 * Y;
").
:- pragma foreign_proc("Java", my_c_func(X::in, Y::in) = (Z::out),
    [promise_pure, will_not_call_mercury], "
    Z = 2 * Y;
").
:- pragma foreign_proc("Erlang", my_c_func(X::in, Y::in) = (Z::out),
    [promise_pure, will_not_call_mercury], "
    Z = 2 * Y
").

:- pragma foreign_decl("C", "#include <stdio.h>").

:- pragma foreign_proc("C", c_hello_world(Msg::in, IO0::di, IO::uo),
        [promise_pure, will_not_call_mercury], "
    printf(""Hello, world"");
    IO = IO0;
").
:- pragma foreign_proc("C#", c_hello_world(Msg::in, IO0::di, IO::uo),
        [promise_pure, will_not_call_mercury], "
    System.Console.WriteLine(""Hello, world"");
    IO = IO0;
").
:- pragma foreign_proc("Java", c_hello_world(Msg::in, IO0::di, IO::uo),
        [promise_pure, will_not_call_mercury], "
    System.out.println(""Hello, world"");
    IO = IO0;
").
:- pragma foreign_proc("Erlang", c_hello_world(Msg::in, IO0::di, IO::uo),
        [promise_pure, will_not_call_mercury], "
    io:format(""Hello, world""),
    IO = IO0
").

test_head(A, B, C, _D, C, _D).

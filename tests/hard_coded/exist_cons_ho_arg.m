%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case checks whether the mode checker can get higher-order inst
% info out of types, such as the type of the second argument of the command
% function symbol below, when they are not specified as parts of modes.
%

:- module exist_cons_ho_arg.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

main(!IO) :-
    test_foo(!IO),
    test_bar(!IO).

%---------------------------------------------------------------------------%

:- type command
    --->    some [T] command(T, pred(T::in, T::out) is det).

:- pred test_foo(io::di, io::uo) is det.

test_foo(!IO) :-
    Command = foo_command,
    Command = command(X, Pred),
    Pred(X, Y),
    io.write_line(Y, !IO).

:- pred test_bar(io::di, io::uo) is det.

test_bar(!IO) :-
    Command = bar_command,
    Command = command(X, Pred),
    Pred(X, Y),
    io.write_line(Y, !IO).

%---------------------------------------------------------------------------%

:- pred foo_pred(int::in, int::out) is det.

foo_pred(N, N + 1).

:- func foo_command = command.

foo_command = 'new command'(41, foo_pred).

%---------------------------------------------------------------------------%

:- pred bar_pred(string::in, string::out) is det.

bar_pred(S, S ++ S ++ S).

:- func bar_command = command.

bar_command = 'new command'("abc", bar_pred).

%---------------------------------------------------------------------------%

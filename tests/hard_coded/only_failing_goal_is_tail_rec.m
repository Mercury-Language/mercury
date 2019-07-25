%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% This is a regression test testing for a bug in the MLDS code generator.
% For the predicate test_foo, it used to generate C code which returned
% the value of the "succeeded" variable, but did *not* declare that variable,
% because the only subgoals in the body of test_foo that could fail are
% all recursive calls optimized into gotos.
%

:- module only_failing_goal_is_tail_rec.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

main(!IO) :-
    Foo1 = foo_foo(foo_bar(bar_foo(foo_bar(bar_int(41))))),
    runtest(Foo1, !IO),
    Foo2 = foo_foo(foo_bar(bar_foo(foo_bar(bar_int(43))))),
    runtest(Foo2, !IO).

:- pred runtest(foo::in, io::di, io::uo) is det.

runtest(Foo, !IO) :-
    io.write(Foo, !IO),
    io.write_string(" -> ", !IO),
    ( if test_foo(Foo) then
        io.write_string("success\n", !IO)
    else
        io.write_string("failure\n", !IO)
    ).

:- type foo
    --->    foo_foo(foo)
    ;       foo_bar(bar).

:- type bar
    --->    bar_foo(foo)
    ;       bar_int(int).

:- pred test_foo(foo::in) is semidet.

test_foo(Foo) :-
    (
        Foo = foo_foo(Foo1),
        test_foo(Foo1)
    ;
        Foo = foo_bar(Bar),
        test_bar(Bar)
    ).

:- pred test_bar(bar::in) is semidet.
:- pragma no_inline(test_bar/1).

test_bar(Bar) :-
    (
        Bar = bar_foo(Foo),
        test_foo(Foo)
    ;
        Bar = bar_int(N),
        N < 42
    ).

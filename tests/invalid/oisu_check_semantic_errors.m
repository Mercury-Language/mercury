%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module oisu_check_semantic_errors.
:- interface.

:- import_module bool.

:- type foo.
:- type bar.

:- pragma oisu(foo/0,
        creators([pred(create_foo1/2), pred(create_foo2/1),
            pred(create_foo3/2), pred(destroy_foo4/3)]),
        mutators([pred(mutate_foo1/3), pred(mutate_foo2/3),
            pred(create_bar1/2), pred(mutate_foobar1/5)]),
        destructors([pred(destroy_foo1/3), pred(destroy_foo2/3),
            pred(destroy_foo3/4), pred(destroy_bar1/3)])
    ).

:- pragma oisu(bar/0,
        creators([pred(create_bar1/2), pred(create_foobar1/3)]),
        mutators([pred(mutate_bar1/3), pred(mutate_foobar1/5)]),
        destructors([pred(destroy_bar1/3), pred(destroy_foo1/3)])
    ).

:- pred create_foo1(int::in, foo::out) is semidet.
:- pred create_foo2(foo::in) is det.
:- pred create_foo3(foo::out, foo::in) is det.

:- pred mutate_foo1(foo::in, bool::in, foo::out) is det.
:- pred mutate_foo2(int::in, foo::out, foo::in) is det.

:- pred destroy_foo1(foo::in, bool::out, int::out) is semidet.
:- pred destroy_foo2(foo::out, bool::in, int::in) is det.
:- pred destroy_foo3(foo::in, foo::out, bool::out, int::out) is det.
:- pred destroy_foo4(foo, bool, int).
:- mode destroy_foo4(in, out, out) is det.
:- mode destroy_foo4(out, in, in) is det.

:- pred create_bar1(int::in, bar::out) is semidet.

:- pred mutate_bar1(bar::in, bar::out, int::in) is det.

:- pred destroy_bar1(bar::in, int::out, int::out) is det.

:- pred create_foobar1(int::in, foo::out, bar::out) is det.

:- pred mutate_foobar1(int::in, foo::in, foo::out, bar::in, bar::out) is det.

:- implementation.

:- import_module int.

:- type foo
    --->    foo(bool, int).

:- type bar
    --->    bar(int, int).

create_foo1(N, Foo) :-
    N > 10,
    Foo = foo(no, N).

create_foo2(_Foo).

create_foo3(Foo, Foo).

mutate_foo1(Foo0, B, Foo) :-
    Foo0 = foo(_, N),
    Foo = foo(B, N).

mutate_foo2(N, Foo, Foo0) :-
    Foo0 = foo(B, _),
    Foo = foo(B, N).

destroy_foo1(Foo, B, N) :-
    Foo = foo(B, N),
    N > 10.

destroy_foo2(Foo, B, N) :-
    Foo = foo(B, N).

destroy_foo3(Foo0, Foo, B, N) :-
    Foo0 = foo(B, N),
    Foo = Foo0.

destroy_foo4(Foo, B, N) :-
    Foo = foo(B, N).

create_bar1(N, Bar) :-
    N > 10,
    Bar = bar(N, N).

mutate_bar1(Bar0, Bar, N) :-
    Bar0 = bar(_, M),
    Bar = bar(N, M).

destroy_bar1(Bar, N, M) :-
    Bar = bar(N, M).

create_foobar1(N, Foo, Bar) :-
    Foo = foo(no, N),
    Bar = bar(N, N).

mutate_foobar1(N, Foo0, Foo, Bar0, Bar) :-
    Foo0 = foo(B, _),
    Foo = foo(B, N),
    Bar0 = bar(_, M),
    Bar = bar(N, M).

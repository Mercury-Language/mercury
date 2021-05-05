%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module oisu_check_add_pragma_errors.
:- interface.

:- type foo.
:- type baz.

:- pragma oisu(foo/0,
        creators([pred(create_foo/1)]),
        mutators([pred(mutate_foo/3)])
    ).

:- pragma oisu(foo/0,
        creators([pred(create_foo/1)]),
        mutators([pred(mutate_foo/3)]),
        destructors([pred(destroy_foo/3)])
    ).

:- pragma oisu(quux/0,
        creators([pred(create_quux/1)]),
        mutators([pred(mutate_quux/3)]),
        destructors([pred(destroy_quux/2)])
    ).

:- implementation.

:- pragma oisu(bar/0,
        creators([pred(create_bar/1)]),
        mutators([pred(mutate_bar/3)]),
        destructors([pred(destroy_bar/3)])
    ).

:- pragma oisu(baz/0,
        creators([pred(create_baz/1)]),
        mutators([pred(mutate_baz/3)])
    ).

:- type foo
    --->    foo(int, int).

:- type bar
    --->    bar(int).

:- type baz
    --->    baz(float).

:- pred create_foo(foo::out) is det.
:- pred mutate_foo(int::in, foo::in, foo::out) is det.
:- pred destroy_foo(foo::in, int::out, int::out) is det.

create_foo(Foo) :-
    Foo = foo(0, 0).

mutate_foo(N, Foo0, Foo) :-
    Foo0 = foo(_, _),
    Foo = foo(N, N).

destroy_foo(Foo, A, B) :-
    Foo = foo(A, B).

:- pred create_bar(bar::out) is det.
:- pred mutate_bar(int::in, bar::in, bar::out) is det.
:- pred destroy_bar(bar::in, int::out) is det.

create_bar(Bar) :-
    Bar = bar(0).

mutate_bar(N, Bar0, Bar) :-
    Bar0 = bar(_),
    Bar = bar(N).

destroy_bar(Bar, A) :-
    Bar = bar(A).

:- pred create_baz(baz::out) is det.
:- pred mutate_baz(float::in, baz::in, baz::out) is det.
:- pred destroy_baz(baz::in, float::out) is det.

create_baz(Baz) :-
    Baz = baz(0).

mutate_baz(N, Baz0, Baz) :-
    Baz0 = baz(_),
    Baz = baz(N).

destroy_baz(Baz, A) :-
    Baz = baz(A).

:- pred create_quux(baz::out) is det.
:- pred mutate_quux(float::in, baz::in, baz::out) is det.
:- pred destroy_quux(baz::in, float::out) is det.

create_quux(Baz) :-
    Baz = baz(0).

mutate_quux(N, Baz0, Baz) :-
    Baz0 = baz(_),
    Baz = baz(N).

destroy_quux(Baz, A) :-
    Baz = baz(A).

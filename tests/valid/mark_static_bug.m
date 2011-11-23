% A term with an argument produced in a from_ground_term_construct scope was
% not itself being marked to be constructed statically.  A side-effect was an
% exception being thrown while generating a lookup switch in high-level C
% grades.

:- module mark_static_bug.
:- interface.

:- type foo
    --->    foo(int, bar).

:- type bar
    --->    none
    ;       bar(int, int, int, baz).

:- type baz
    --->    baz(int).

:- pred mkfoo(int::in, foo::out) is semidet.

:- implementation.

mkfoo(X, Foo) :-
    (
        X = 1,
        Bar = bar(100, 200, 300, baz(400)),
        Foo = foo(1, Bar)
    ;
        X = 2,
        Foo = foo(2, none)
    ;
        X = 3,
        Foo = foo(3, none)
    ;
        X = 4,
        Foo = foo(4, none)
    ).

% vim: ft=mercury ts=4 sts=4 sw=4 et

%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module field_syntax_error.
:- interface.

:- type foo
    --->    foo(f1 :: int, f2 :: int).

:- func baz1(foo) = int.
:- func baz2(foo) = int.
:- func baz3(foo) = int.
:- func baz4(foo) = int.
:- func baz5(foo) = foo.

:- implementation.

% A "get" with a variable field name.
baz1(Foo) = Foo^Bar.

% A "get" with a constant name.
baz2(Foo) = Foo^42.

% A "set" with a variable field name.
baz3(Foo) = Foo^Bar:=43.

% A "set" with a constant name.
baz4(Foo) = Foo^44:=45.

% A "set" with a "=" instead of a ":=".
baz5(!.Foo) = !:Foo :-
    !Foo ^ f1 := 42,
    !Foo ^ f2 = 43.

:- type bar
    --->    bar(f(N) :: int).

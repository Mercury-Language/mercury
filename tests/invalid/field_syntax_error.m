:- module field_syntax_error.
:- interface.

:- type foo ---> f(bar :: int).

:- func baz(foo) = int.
:- func baz2(foo) = int.
:- func baz3(foo) = int.
:- func baz4(foo) = int.

:- implementation.

baz(Foo) = Foo^Bar.
baz2(Foo) = Foo^42.
baz3(Foo) = Foo^Bar:=43.
baz4(Foo) = Foo^44:=45.


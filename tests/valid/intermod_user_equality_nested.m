:- module intermod_user_equality_nested.

:- interface.

:- import_module intermod_user_equality_nested2.
:- pred check_foo(foo::in, foo::in) is semidet.

:- implementation.

check_foo(Foo, Bar) :- Foo = Bar.


:- module intermod_user_equality.

:- interface.

:- import_module intermod_user_equality2.
:- pred check_foo(foo::in, foo::in) is semidet.

:- implementation.

check_foo(Foo, Bar) :- Foo = Bar.


:- module exported_unify2.

:- interface.

:- type foo ---> foo where equality is unify_foo.

:- implementation.

:- import_module std_util.

:- pred unify_foo(foo::in, foo::in) is semidet.

unify_foo(_, _) :- semidet_fail.


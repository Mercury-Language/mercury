:- module exported_unify.

:- interface.

:- pred unify_foo(T::in, T::in) is semidet.

:- implementation.

:- import_module exported_unify2.

unify_foo(A, A).



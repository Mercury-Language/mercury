:- module solver_type_bug_2.
:- interface.

:- solver type foo where
	representation is int,
	initialisation is foo_init.

:- pred foo_init(foo::oa) is det.

:- solver type bar where
	representation is int,
	initialisation is bar_init.

:- pred bar_init(bar::oa) is det.

:- pred p(foo::oa) is det.

:- implementation.

foo_init(F) :-
	promise_pure impure F = 'representation to any foo/0'(1).

bar_init(F) :-
	promise_pure impure F = 'representation to any bar/0'(2).

p(_).


:- module intermod_user_equality2.

:- interface.

:- type foo.

:- pred foo_field1(foo::in, int::out) is cc_nondet.

:- implementation.

:- type foo
	--->	ctor1(int, int)
	;	ctor2(int, int)
	where equality is foo_unify.

:- pred foo_unify(foo::in, foo::in) is semidet.
foo_unify(X, X).

foo_field1(ctor1(X, _), X).
foo_field1(ctor2(X, _), X).

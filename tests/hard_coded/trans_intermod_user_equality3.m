:- module trans_intermod_user_equality3.

:- interface.

:- type foo
	--->	ctor1(int, int)
	;	ctor2(int, int)
	where equality is foo_unify.

:- pred foo_unify(foo::in, foo::in) is semidet.

:- implementation.

foo_unify(X, X).

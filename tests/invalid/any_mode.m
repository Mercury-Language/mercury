:- module any_mode.

:- implementation.

:- pred p(foo::(any >> ground)) is semidet.
p(X) :- q(X).

:- pred q(foo::in) is semidet.
q(foo(123)).

:- solver type foo
	where representation is int,
	      initialisation is init_foo,
	      ground         is ground,
	      any            is ground,
	      equality       is eq_foo.

:- pred init_foo(foo::out(any)) is det.
init_foo(foo(42)).

:- func foo(int::in) = (foo::out(any)) is det.
:- pragma promise_pure(foo/1).
foo(N) = X :-
	impure X = 'representation to any foo/0'(N).

:- pred eq_foo(foo::in(any), foo::in(any)) is semidet.
:- pragma promise_pure(eq_foo/2).
eq_foo(X, Y) :-
	impure RX = 'representation of any foo/0'(X),
	impure RY = 'representation of any foo/0'(Y),
	RX = RY.


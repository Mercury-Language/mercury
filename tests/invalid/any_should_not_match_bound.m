:- module any_should_not_match_bound.

:- interface.

:- pred p(T::in, T::out(any)) is det.

:- pred test_any_poly(T::in(any), T::out) is det.
:- pred test_any_tuple({int, foo}::in(any), {int, foo}::out) is det.
:- pred test_any_du(du(T)::in(any), du(T)::out) is det.
:- pred test_any_solver(foo::in(any), foo::out) is det.

:- solver type foo.

:- type du(T) ---> nil ; cons(T, du(T)).

:- implementation.

p(X, X).

test_any_poly(X, Y) :-
	p(X, Y).

test_any_tuple(X, Y) :-
	p(X, Y).

test_any_du(X, Y) :-
	p(X, Y).

test_any_solver(X, Y) :-
	p(X, Y).



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


:- module any_should_not_match_bound.

:- interface.

:- pred p(T::in, T::out(any)) is det.

:- pred test_any_poly(T::in(any), T::out) is det.
:- pred test_any_tuple({int, foo}::in(any), {int, foo}::out) is det.
:- pred test_any_du(du(T)::in(any), du(T)::out) is det.
:- pred test_any_solver(foo::in(any), foo::out) is det.

:- solver type foo ---> bar ; baz.

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

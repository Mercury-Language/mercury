:- module any_matches_bound.

:- interface.

:- pred p(T::in, T::out(any)) is det.

:- pred test_any_string(string::in(any), string::out) is det.
:- pred test_any_int(int::in(any), int::out) is det.
:- pred test_any_float(float::in(any), float::out) is det.
:- pred test_any_char(character::in(any), character::out) is det.
:- pred test_any_enum(enum::in(any), enum::out) is det.
:- pred test_any_tuple({enum, int}::in(any), {enum, int}::out) is det.
:- pred test_any_du(du(enum)::in(any), du(enum)::out) is det.

:- type enum ---> foo ; bar ; baz.

:- type du(T) ---> nil ; cons(T, du(T)).

:- implementation.

p(X, X).

test_any_string(X, Y) :-
	p(X, Y).

test_any_int(X, Y) :-
	p(X, Y).

test_any_float(X, Y) :-
	p(X, Y).

test_any_char(X, Y) :-
	p(X, Y).

test_any_enum(X, Y) :-
	p(X, Y).

test_any_tuple(X, Y) :-
	p(X, Y).

test_any_du(X, Y) :-
	p(X, Y).

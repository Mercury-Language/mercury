%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_type_error.
:- interface.

:- import_module list.

:- type non_empty_list(T) =< list(T)
    --->    [T | list(T)].

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- type citrus =< fruit
    --->    orange
    ;       lemon.

:- type orange_fruit =< fruit
    --->    orange.

:- type orange_non_fruit
    --->    orange.

%---------------------------------------------------------------------------%

:- implementation.

:- pred citrus_to_fruit(citrus::in, fruit::out) is det.

citrus_to_fruit(X, Y) :-
    Y = coerce(X).

:- pred orange_fruit_to_citrus(orange_fruit::in, citrus::out) is det.

orange_fruit_to_citrus(X, Y) :-
    Y = coerce(X).

:- pred bad_unrelated(orange_non_fruit::in, citrus::out) is det.

bad_unrelated(X, Y) :-
    Y = coerce(X).

:- pred list_to_list(list(citrus)::in, list(fruit)::out) is det.

list_to_list(X, Y) :-
    Y = coerce(X).

%---------------------------------------------------------------------------%

:- type fruit2 == fruit.
:- type citrus2 == citrus.

:- pred citrus_to_fruit2(citrus2::in, fruit2::out) is det.

citrus_to_fruit2(X, Y) :-
    Y = coerce(X).

%---------------------------------------------------------------------------%

:- type phantom(T)
    --->    phantom.

:- pred bad_phantom(phantom(int)::in, phantom(float)::out) is det.

bad_phantom(X, Y) :-
    Y = coerce(X).

%---------------------------------------------------------------------------%

:- type wrap(T)
    --->    nil
    ;       wrap(T, {T, wrap(T)}).

:- pred wrap_to_wrap(wrap(citrus)::in, wrap(fruit)::out) is det.

wrap_to_wrap(X, Y) :-
    Y = coerce(X).

%---------------------------------------------------------------------------%

:- type wrap_ho(T)
    --->    wrap_ho(pred(T)).

:- pred bad_higher_order(wrap_ho(citrus)::in, wrap_ho(fruit)::out) is det.

bad_higher_order(X, Y) :-
    Y = coerce(X).

%---------------------------------------------------------------------------%

:- type wrap_ft(T)
    --->    wrap_ft(ft(T)).

:- type ft(T).
:- pragma foreign_type("C", ft(T), "int").
:- pragma foreign_type("C#", ft(T), "int").
:- pragma foreign_type("Java", ft(T), "int").

:- pred bad_foreign_type(wrap_ft(citrus)::in, wrap_ft(fruit)::out) is det.

bad_foreign_type(X, Y) :-
    Y = coerce(X).

%---------------------------------------------------------------------------%

:- import_module set.

:- type wrap_abs(T)
    --->    wrap_abs(set(T)).

:- pred bad_abs_type(wrap_abs(citrus)::in, wrap_abs(fruit)::out) is det.

bad_abs_type(X, Y) :-
    Y = coerce(X).

%---------------------------------------------------------------------------%

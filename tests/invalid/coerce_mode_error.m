%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_mode_error.
:- interface.

:- type list(T)
    --->    []
    ;       [T | list(T)].

:- type non_empty_list(T) =< list(T)
    --->    [T | list(T)].

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- type citrus =< fruit
    --->    orange
    ;       lemon.

:- inst citrus_fruit for fruit/0
    --->    orange
    ;       lemon.

%---------------------------------------------------------------------------%

:- implementation.

:- pred bad_coerce_free_input(fruit::free>>free, fruit::out) is det.

bad_coerce_free_input(X, Y) :-
    Y = coerce(X).

:- pred fruit_to_citrus(fruit, citrus).
:- mode fruit_to_citrus(in(citrus_fruit), out) is det.

fruit_to_citrus(X, Y) :-
    Y = coerce(X).

:- pred bad_fruit_to_citrus(fruit::in, citrus::out) is det.

bad_fruit_to_citrus(X, Y) :-
    Y = coerce(X).

:- pred bad_nelist_f_to_list_c(non_empty_list(fruit), list(citrus)).
:- mode bad_nelist_f_to_list_c(in, out) is det.

bad_nelist_f_to_list_c(X, Y) :-
    Y = coerce(X).

%---------------------------------------------------------------------------%

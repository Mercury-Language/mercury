%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_mode_error_1.
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

:- pred fruit_to_citrus(fruit::in(citrus_fruit), citrus::out) is det.

fruit_to_citrus(X, Y) :-
    Y = coerce(X).

:- pred bad_fruit_to_citrus(fruit::in, citrus::out) is det.

bad_fruit_to_citrus(X, Y) :-
    Y = coerce(X).

:- pred bad_nelist_f_to_list_c(non_empty_list(fruit)::in, list(citrus)::out)
    is det.

bad_nelist_f_to_list_c(X, Y) :-
    Y = coerce(X).

%---------------------------------------------------------------------------%

:- type least2(T) =< list(T)
    --->    [T | non_empty_list(T)].

:- pred bad_from_list_to_least2(list(T), least2(T)).
:- mode bad_from_list_to_least2(in, out) is semidet.

bad_from_list_to_least2(List, Xs) :-
    List = [_ | _],
    Xs = coerce(List).

:- pred from_list_to_least2(list(T), least2(T)).
:- mode from_list_to_least2(in, out) is semidet.

from_list_to_least2(List, Xs) :-
    % Unfortunately the mode checker does not assign a precise enough inst
    % to List for the coerce expression to be accepted.
    List = [_, _ | _],
    Xs = coerce(List).

%---------------------------------------------------------------------------%

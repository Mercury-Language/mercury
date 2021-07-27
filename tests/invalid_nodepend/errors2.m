%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module errors2.

%---------------------------------------------------------------------------%

:- pred bind_type_param(TypeParam::input, TypeParam2::output).

bind_type_param(Argument) :-
    Argument = 0.

%---------------------------------------------------------------------------%

% currently the compiler just gives a warning for this test case

:- pred unresolved_polymorphism.

unresolved_polymorphism :-
    bind_type_param(Arg, Arg).

%---------------------------------------------------------------------------%

:- pred produce_string(string).

:- pred expect_int(int).

:- pred type_error.

type_error :-
    produce_string(X),
    expect_int(X).

:- pred type_error_2.

type_error_2 :-
    produce_string(X),
    expect_int(Y),
    X = Y.

:- pred type_error_3.

type_error_3 :-
    X = Y,
    produce_string(X),
    expect_int(Y).

:- type foo_type
    --->    foo_functor(int, character, string).

:- type bar_1_type
    --->    bar_functor(int, character, string).

:- type bar_2_type
    --->    bar_functor(character, int, string).

:- pred type_error_4.

type_error_4 :-
    Y = 0,
    X = foo_functor(Y, 'x', 1.0).

:- pred type_error_5.
:- type foo
    --->    a
    ;       b
    ;       c.

type_error_5 :-
    Y = 'a',
    X = foo_functor(0, Y, 1.0).

:- pred type_error_6.

type_error_6 :-
    Y = 'a',
    X = bar_functor(0, Y, 1.0).

:- pred type_error_7.

type_error_7 :-
    Y = 'a',
    Z = bar_functor(A, B, C),
    expect_int(C).

:- use_module list, string.

:- pred type_error_8.

type_error_8 :-
    from_char_list([], Str),
    string__from_char_list(list.[], Str).

:- pred type_error_9.

type_error_9 :-
    X = {1, "2", '3'},
    Y = {"1", '2', 3},
    X = Y.

%---------------------------------------------------------------------------%

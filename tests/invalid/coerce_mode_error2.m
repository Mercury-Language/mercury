%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_mode_error2.
:- interface.

:- type fruit
    --->    apple
    ;       orange
    ;       lemon
    ;       banana.

:- type citrus =< fruit
    --->    orange
    ;       lemon.

:- type foo(T)
    --->    nil
    ;       foo(int, T).

:- type foo_citrus =< foo(citrus)
    --->    foo(int, citrus).

%---------------------------------------------------------------------------%

:- implementation.

:- pred test1(foo_citrus::out) is multi.

test1(Y) :-
    (
        X = foo(1, apple)       % apple cannot be converted
    ;
        X = foo(2, banana)      % banana cannot be converted
    ;
        X = foo(3, orange)
    ),
    Y = coerce(X).

:- pred test2(foo_citrus::out) is multi.

test2(Y) :-
    (
        X = foo(1, apple)       % apple cannot be converted
    ;
        X = nil                 % nil cannot be converted either
                                % and will be reported preferentially
    ),
    Y = coerce(X).

%---------------------------------------------------------------------------%

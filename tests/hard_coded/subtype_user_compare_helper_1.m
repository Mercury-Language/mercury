%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_user_compare_helper_1.
:- interface.

:- type abs_fruit.

:- type abs_citrus.

:- func abs_fruit_lemon = abs_fruit.
:- func abs_fruit_pomelo = abs_fruit.

:- func abs_citrus_lemon = abs_citrus.
:- func abs_citrus_pomelo = abs_citrus.

%---------------------------------------------------------------------------%

:- implementation.

:- type abs_fruit == fruit.
:- type abs_citrus == citrus.

:- type fruit
    --->    apple0
    ;       banana1
    ;       lemon2
    ;       orange3
    ;       peach4
    ;       pomelo5
    ;       tomato6
    where comparison is fruit_compare.

:- type citrus =< fruit
    --->    lemon2
    ;       orange3
    ;       pomelo5.

:- pred fruit_compare(comparison_result::uo, fruit::in, fruit::in) is det.

fruit_compare(Res, A, B) :-
    IntA = fruit_int(A),
    IntB = fruit_int(B),
    compare(Res, IntA, IntB).

:- func fruit_int(fruit) = int.

fruit_int(Fruit) = Int :-
    promise_equivalent_solutions [Int]
    ( Fruit = apple0,  Int = 0
    ; Fruit = banana1, Int = -1
    ; Fruit = lemon2,  Int = -2
    ; Fruit = orange3, Int = -3
    ; Fruit = peach4,  Int = -4
    ; Fruit = pomelo5, Int = -5
    ; Fruit = tomato6, Int = -6
    ).

abs_fruit_lemon = lemon2.
abs_fruit_pomelo = pomelo5.

abs_citrus_lemon = lemon2.
abs_citrus_pomelo = pomelo5.

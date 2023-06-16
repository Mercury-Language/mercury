%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module subtype_user_compare.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module subtype_user_compare_helper_1.

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

main(!IO) :-
    io.write_string("compare fruit (local):  ", !IO),
    test_compare(lemon2 : fruit, pomelo5 : fruit, !IO),

    io.write_string("compare citrus (local): ", !IO),
    test_compare(lemon2 : citrus, pomelo5 : citrus, !IO),

    io.write_string("compare fruit (abstract): ", !IO),
    test_compare(abs_fruit_lemon, abs_fruit_pomelo, !IO),

    io.write_string("compare citrus (abstract): ", !IO),
    test_compare(abs_citrus_lemon, abs_citrus_pomelo, !IO).

:- pred test_compare(T::in, T::in, io::di, io::uo) is det.

test_compare(A, B, !IO) :-
    compare(Res, A, B),
    io.print_line(Res, !IO).

%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module inst_with_no_type.

:- interface.

:- inst chars
    --->    a
    ;       b
    ;       c.

:- inst i1
    --->    t1_f1(ground, ground)
    ;       t1_f2(ground).

:- inst i1_no_match
    --->    t1_f1(ground)
    ;       t1_f2(ground).

:- type t1
    --->    t1_f1(int, int)
    ;       t1_f2(string).

:- inst i2(I)
    --->    i2_f1(ground, I)
    ;       i2_f2(ground, ground, ground)
    ;       i2_f3(ground)
    ;       i2_f4.

:- type t2(T)
    --->    i2_f1(character, T)
    ;       i2_f2(T, T, int)
    ;       i2_f3(float)
    ;       i2_f4
    ;       i2_f5.

:- inst i2_no_match(I)
    --->    i2_f1(ground, I)
    ;       i2_f2(ground, ground, ground, ground)
    ;       i2_f3(ground)
    ;       i2_f4.

:- inst i3
    --->    "a"
    ;       "b"
    ;       "c".

:- inst i4_no_match
    --->    1
    ;       2
    ;       "3".

:- inst i5
    --->    1
    ;       2.

:- inst i6
    --->    1.1
    ;       1.2.

:- inst i7_no_match
    --->    1.0
    ;       1.

:- inst tuple
    --->    {ground, ground}.

:- inst ho == (pred(in, out) is det).

:- inst any_inst == any.

:- inst mostly_unique_inst == mostly_unique(apple ; lemon).

:- inst mostly_unique_no_match == mostly_unique(apple ; banana).

:- inst unique_inst == unique(apple ; lemon).

:- inst unique_no_match == unique(apple ; banana).

:- type fruit.
:- inst citrus
    --->    lemon
    ;       orange.

:- implementation.

:- import_module inst_with_no_type_helper_1.

:- import_module io.

    % Even though t is defined in the implementation of inst_with_no_type_2,
    % we should get a warning for the following insts, since the function
    % symbol ft is not user visible from this scope.
    %
:- inst t_no_match
    --->    ft.

:- type fruit
    --->    apple
    ;       orange
    ;       lemon.

:- pred q(t::in, io::di, io::uo) is det.

q(T, !IO) :-
    p(T, !IO).

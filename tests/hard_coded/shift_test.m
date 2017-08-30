%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% test the handling of `>>', `<<', unchecked_left_shift
% and unchecked_right_shift.

:- module shift_test.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module int.
:- import_module list.
:- import_module string.

main -->
    shift_test(legacy_left_shift, "<<", 64, 0, 64),
    shift_test(legacy_left_shift, "<<", 64, 2, 256),
    shift_test(legacy_left_shift, "<<", -64, 2, -256),
    shift_test(legacy_left_shift, "<<", 64, -2, 16),
    shift_test(legacy_left_shift, "<<", -64, -2, -16),
    shift_test(legacy_left_shift, "<<", 64, -256, 0),
    shift_test(legacy_left_shift, "<<", -64, -256, -1),
    shift_test(legacy_left_shift, "<<", 25, 3, 200),
    shift_test(legacy_left_shift, "<<", -25, 3, -200),
    shift_test(legacy_left_shift, "<<", 25, -3, 3),
    shift_test(legacy_left_shift, "<<", -25, -3, -4),

    shift_test(legacy_right_shift, ">>", 64, 0, 64),
    shift_test(legacy_right_shift, ">>", 64, 2, 16),
    shift_test(legacy_right_shift, ">>", -64, 2, -16),
    shift_test(legacy_right_shift, ">>", 64, -2, 256),
    shift_test(legacy_right_shift, ">>", -64, -2, -256),
    shift_test(legacy_right_shift, ">>", 64, 256, 0),
    shift_test(legacy_right_shift, ">>", -64, 256, -1),
    shift_test(legacy_right_shift, ">>", 25, 3, 3),
    shift_test(legacy_right_shift, ">>", -25, 3, -4),
    shift_test(legacy_right_shift, ">>", 25, -3, 200),
    shift_test(legacy_right_shift, ">>", -25, -3, -200),

    shift_test(unchecked_left_shift, "unchecked_left_shift",
        64, 2, 256),
    shift_test(unchecked_right_shift, "unchecked_right_shift",
        -64, 2, -16),

    io__write_string("The following cases test undefined behaviour\n"),
    io__write_string("(they cause overflow):\n"),
    shift_test(legacy_left_shift, "<<", 64, 256, 0),
    shift_test(legacy_left_shift, "<<", -64, 256, 0),
    shift_test(legacy_right_shift, ">>", 64, -256, 0),
    shift_test(legacy_right_shift, ">>", -64, -256, 0).

:- pred shift_test((func(int, int) = int)::(func(in, in) = out is det),
    string::in, int::in, int::in, int::in,
    io__state::di, io__state::uo) is det.

shift_test(Func, FuncName, Left, Right, Result) -->
    io__format("%d %s %d = %d (%d)\n",
        [i(Left), s(FuncName), i(Right),
        i(Func(Left, Right)), i(Result)]).


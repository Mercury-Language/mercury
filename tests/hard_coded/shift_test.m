%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% test the handling of `>>', `<<', unchecked_left_shift
% and unchecked_right_shift.

:- module shift_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    shift_test(int.(<<), "<<",  64,    0,     expected(  64), !IO),
    shift_test(int.(<<), "<<",  64,    2,     expected( 256), !IO),
    shift_test(int.(<<), "<<", -64,    2,     expected(-256), !IO),
    shift_test(int.(<<), "<<",  64,   -2, not_expected(  16), !IO),
    shift_test(int.(<<), "<<", -64,   -2, not_expected( -16), !IO),
    shift_test(int.(<<), "<<",  64, -256, not_expected(   0), !IO),
    shift_test(int.(<<), "<<", -64, -256, not_expected(  -1), !IO),
    shift_test(int.(<<), "<<",  25,    3,     expected( 200), !IO),
    shift_test(int.(<<), "<<", -25,    3,     expected(-200), !IO),
    shift_test(int.(<<), "<<",  25,   -3, not_expected(   3), !IO),
    shift_test(int.(<<), "<<", -25,   -3, not_expected(  -4), !IO),

    shift_test(int.(>>), ">>",  64,    0,     expected(  64), !IO),
    shift_test(int.(>>), ">>",  64,    2,     expected(  16), !IO),
    shift_test(int.(>>), ">>", -64,    2,     expected( -16), !IO),
    shift_test(int.(>>), ">>",  64,   -2, not_expected( 256), !IO),
    shift_test(int.(>>), ">>", -64,   -2, not_expected(-256), !IO),
    shift_test(int.(>>), ">>",  64,  256, not_expected(   0), !IO),
    shift_test(int.(>>), ">>", -64,  256, not_expected(  -1), !IO),
    shift_test(int.(>>), ">>",  25,    3,     expected(   3), !IO),
    shift_test(int.(>>), ">>", -25,    3,     expected(  -4), !IO),
    shift_test(int.(>>), ">>",  25,   -3, not_expected( 200), !IO),
    shift_test(int.(>>), ">>", -25,   -3, not_expected(-200), !IO),

    shift_test(unchecked_left_shift,  "uhch_<<",  64, 2, expected(256), !IO),
    shift_test(unchecked_right_shift, "unch_>>", -64, 2, expected(-16), !IO),

    io.format("%s %s\n",
        [s("The following cases test undefined behaviour"),
        s("(they cause overflow):")], !IO),
    shift_test(unchecked_left_shift,  "unch_<<",  64,  256, expected(0), !IO),
    shift_test(unchecked_left_shift,  "unch_<<", -64,  256, expected(0), !IO),
    shift_test(unchecked_right_shift, "unch_>>",  64, -256, expected(0), !IO),
    shift_test(unchecked_right_shift, "unch_>>", -64, -256, expected(0), !IO).

:- type maybe_expected
    --->    expected(int)
            % We expect success with the given result.
    ;       not_expected(int).
            % We expect failure. With the now-deleted legacy version
            % of the shift operator, we *used* to expect the given result.
            % (We don't do anything with this expected result from a
            % now-obsolete predicate.)

:- pred shift_test((func(int, int) = int)::(func(in, in) = out is det),
    string::in, int::in, int::in, maybe_expected::in, io::di, io::uo)
    is cc_multi.

shift_test(Func, FuncName, Left, Right, MaybeExpected, !IO) :-
    ( try []
        (
            Actual = Func(Left, Right),
            ActualStr0 = string.format("%d", [i(Actual)])
        )
    then
        ActualStr = ActualStr0,
        ActualResult = yes
    catch domain_error(ErrorStr) ->
        ActualStr = ErrorStr,
        ActualResult = no
    ),
    (
        ActualResult = yes,
        (
            MaybeExpected = expected(Expected),
            ExpectedStr = string.format("%d", [i(Expected)])
        ;
            MaybeExpected = not_expected(_LegacyExpected),
            ExpectedStr = "unexpected success"
        )
    ;
        ActualResult = no,
        (
            MaybeExpected = expected(_Expected),
            ExpectedStr = "unexpected failure"
        ;
            MaybeExpected = not_expected(_LegacyExpected),
            ExpectedStr = "expected failure"
        )
    ),
    io.format("%d %s %d = %s (%s)\n",
        [i(Left), s(FuncName), i(Right), s(ActualStr), s(ExpectedStr)], !IO).

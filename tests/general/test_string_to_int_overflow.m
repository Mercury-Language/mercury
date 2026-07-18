%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The .exp file is for when int is 32-bit.
% The .exp2 file is for when int is 64-bit.
%
%---------------------------------------------------------------------------%

:- module test_string_to_int_overflow.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    test(string.to_int("999"), !IO),
    test(base_string_to_int(10, "999"), !IO),
    test(string.to_int("99999999999999999999"), !IO),
    test(string.to_int("-99999999999999999999"), !IO),

    line(!IO),

    % Boundary values for 32-bit integers.
    test(base_string_to_int(10, "2147483647"), !IO),
    test(base_string_to_int(10, "2147483648"), !IO),
    test(base_string_to_int(10, "-2147483648"), !IO),
    test(base_string_to_int(10, "-2147483649"), !IO),

    line(!IO),

    % Boundary values for 64-bit integers.
    test(base_string_to_int(10, "9223372036854775807"), !IO),
    test(base_string_to_int(10, "9223372036854775808"), !IO),
    test(base_string_to_int(10, "-9223372036854775808"), !IO),
    test(base_string_to_int(10, "-9223372036854775809"), !IO),

    line(!IO),

    test(base_string_to_int(16, "7fffffff"), !IO),
    test(base_string_to_int(16, "80000000"), !IO),
    test(base_string_to_int(16, "-80000000"), !IO),
    test(base_string_to_int(16, "-80000001"), !IO),

    line(!IO),

    test(base_string_to_int(16, "7fffffffffffffff"), !IO),
    test(base_string_to_int(16, "8000000000000000"), !IO),
    test(base_string_to_int(16, "-8000000000000000"), !IO),
    test(base_string_to_int(16, "-8000000000000001"), !IO),

    line(!IO),

    test(base_string_to_int(36, "ZIK0ZJ"), !IO),
    test(base_string_to_int(36, "ZIK0ZK"), !IO),
    test(base_string_to_int(36, "-ZIK0ZK"), !IO),
    test(base_string_to_int(36, "-ZIK0ZL"), !IO),

    line(!IO),

    test(base_string_to_int(36, "1Y2P0IJ32E8E7"), !IO),
    test(base_string_to_int(36, "1Y2P0IJ32E8E8"), !IO),
    test(base_string_to_int(36, "-1Y2P0IJ32E8E8"), !IO),
    test(base_string_to_int(36, "-1Y2P0IJ32E8E9"), !IO),

    line(!IO),

    % Regression tests for incorrect overflow check. For each of these
    % values, processing the final digit overflows by 2^63 or more
    % (2^31 on 32-bit platforms), so the wrapped-around result has the
    % same sign as the accumulator and lies beyond it. The old overflow
    % check (N0 =< N) therefore failed to detect the overflow. (Smaller
    % overflows wrap to a value of the opposite sign, which the old
    % check did detect.)
    %
    % There are no cases for base 2, because none exist. With base 2 the
    % magnitude of the result of processing one digit is less than twice
    % the magnitude of the accumulator, so an overflowing result always
    % lands in the opposite-sign range that the old check detected.

    % Overflow check regression tests for 64-bit ints.
    test(base_string_to_int(10, "23058430092136939520"), !IO),
    test(base_string_to_int(10, "-23058430092136939520"), !IO),
    test(base_string_to_int(16, "14000000000000000"), !IO),
    test(base_string_to_int(16, "-14000000000000000"), !IO),
    test(base_string_to_int(8, "2400000000000000000000"), !IO),
    test(base_string_to_int(8, "-2400000000000000000000"), !IO),
    test(base_string_to_int(36, "5000000000000"), !IO),
    test(base_string_to_int(36, "-5000000000000"), !IO),

    line(!IO),

    % Overflow check regression tests for 32-bit ints.
    test(base_string_to_int(10, "5368709120"), !IO),
    test(base_string_to_int(10, "-5368709120"), !IO),
    test(base_string_to_int(16, "140000000"), !IO),
    test(base_string_to_int(16, "-140000000"), !IO),
    test(base_string_to_int(8, "50000000000"), !IO),
    test(base_string_to_int(8, "-50000000000"), !IO),
    test(base_string_to_int(36, "2200000"), !IO),
    test(base_string_to_int(36, "-2200000"), !IO),

    line(!IO),

    % Check truncating division is used in overflow checks.
    test(base_string_to_int(10, "-9223372036854775810"), !IO),
    test(base_string_to_int(10, "-2147483650"), !IO).

:- pred test(pred(T)::in(pred(out) is semidet), io::di, io::uo) is det.

test(P, !IO) :-
    ( if P(X) then
        io.write_line(X, !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- pred line(io::di, io::uo) is det.

line(!IO) :-
    io.write_string("--------\n", !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

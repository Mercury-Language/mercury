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

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    line("Basic tests", !IO),

    test(string.to_int("0"), !IO),
    test(string.to_int("-0"), !IO),
    test(string.to_int("+0"), !IO),
    test(string.to_int("1"), !IO),
    test(string.to_int("+1"), !IO),
    test(string.to_int("-1"), !IO),
    test(string.to_int("999"), !IO),
    test(base_string_to_int(10, "999"), !IO),
    test(string.to_int("99999999999999999999"), !IO),
    test(string.to_int("-99999999999999999999"), !IO),

    line("Decimal boundary values for 32-bit ints", !IO),

    test(base_string_to_int(10, "2147483647"), !IO),
    test(base_string_to_int(10, "2147483648"), !IO),
    test(base_string_to_int(10, "-2147483648"), !IO),
    test(base_string_to_int(10, "-2147483649"), !IO),

    line("Decimal boundary values for 64-bit ints", !IO),

    test(base_string_to_int(10, "9223372036854775807"), !IO),
    test(base_string_to_int(10, "9223372036854775808"), !IO),
    test(base_string_to_int(10, "-9223372036854775808"), !IO),
    test(base_string_to_int(10, "-9223372036854775809"), !IO),

    line("Hexadecimal boundary values for 32-bit ints", !IO),

    test(base_string_to_int(16, "7fffffff"), !IO),
    test(base_string_to_int(16, "80000000"), !IO),
    test(base_string_to_int(16, "-80000000"), !IO),
    test(base_string_to_int(16, "-80000001"), !IO),

    line("Hexadecimal boundary values for 64-bit ints", !IO),

    test(base_string_to_int(16, "7fffffffffffffff"), !IO),
    test(base_string_to_int(16, "8000000000000000"), !IO),
    test(base_string_to_int(16, "-8000000000000000"), !IO),
    test(base_string_to_int(16, "-8000000000000001"), !IO),

    line("Octal boundary values for 32-bit ints", !IO),

    test(base_string_to_int(8, "17777777777"), !IO),
    test(base_string_to_int(8, "20000000000"), !IO),
    test(base_string_to_int(8, "-20000000000"), !IO),
    test(base_string_to_int(8, "-20000000001"), !IO),

    line("Octal boundary values for 64-bit ints", !IO),

    test(base_string_to_int(8, "777777777777777777777"), !IO),
    test(base_string_to_int(8, "1000000000000000000000"), !IO),
    test(base_string_to_int(8, "-1000000000000000000000"), !IO),
    test(base_string_to_int(8, "-1000000000000000000001"), !IO),

    line("Binary boundary values for 32-bit ints", !IO),

    test(base_string_to_int(2, "1111111111111111111111111111111"), !IO),
    test(base_string_to_int(2, "10000000000000000000000000000000"), !IO),
    test(base_string_to_int(2, "-10000000000000000000000000000000"), !IO),
    test(base_string_to_int(2, "-10000000000000000000000000000001"), !IO),

    line("Binary boundary values for 64-bit ints", !IO),

    test(base_string_to_int(2, "111111111111111111111111111111" ++
        "111111111111111111111111111111111"), !IO),
    test(base_string_to_int(2, "100000000000000000000000000000" ++
        "0000000000000000000000000000000000"), !IO),
    test(base_string_to_int(2, "-10000000000000000000000000000" ++
        "00000000000000000000000000000000000"), !IO),
    test(base_string_to_int(2, "-10000000000000000000000000000" ++
        "00000000000000000000000000000000001"), !IO),

    line("Base-36 boundary values for 32-bit ints", !IO),

    test(base_string_to_int(36, "ZIK0ZJ"), !IO),
    test(base_string_to_int(36, "ZIK0ZK"), !IO),
    test(base_string_to_int(36, "-ZIK0ZK"), !IO),
    test(base_string_to_int(36, "-ZIK0ZL"), !IO),

    line("Base-36 boundary values for 64-bit ints", !IO),

    test(base_string_to_int(36, "1Y2P0IJ32E8E7"), !IO),
    test(base_string_to_int(36, "1Y2P0IJ32E8E8"), !IO),
    test(base_string_to_int(36, "-1Y2P0IJ32E8E8"), !IO),
    test(base_string_to_int(36, "-1Y2P0IJ32E8E9"), !IO),

    line("Explicit plus sign", !IO),

    test(base_string_to_int(10, "+2147483647"), !IO),
    test(base_string_to_int(10, "+2147483648"), !IO),
    test(base_string_to_int(10, "+9223372036854775807"), !IO),
    test(base_string_to_int(10, "+9223372036854775808"), !IO),

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

    line("Overflow regression tests for 32-bit ints", !IO),

    test(base_string_to_int(10, "5368709120"), !IO),
    test(base_string_to_int(10, "-5368709120"), !IO),
    test(base_string_to_int(16, "140000000"), !IO),
    test(base_string_to_int(16, "-140000000"), !IO),
    test(base_string_to_int(8, "50000000000"), !IO),
    test(base_string_to_int(8, "-50000000000"), !IO),
    test(base_string_to_int(36, "2200000"), !IO),
    test(base_string_to_int(36, "-2200000"), !IO),

    line("Overflow regression tests for 64-bit ints", !IO),

    test(base_string_to_int(10, "23058430092136939520"), !IO),
    test(base_string_to_int(10, "-23058430092136939520"), !IO),
    test(base_string_to_int(16, "14000000000000000"), !IO),
    test(base_string_to_int(16, "-14000000000000000"), !IO),
    test(base_string_to_int(8, "2400000000000000000000"), !IO),
    test(base_string_to_int(8, "-2400000000000000000000"), !IO),
    test(base_string_to_int(36, "5000000000000"), !IO),
    test(base_string_to_int(36, "-5000000000000"), !IO),

    line("Test truncating division is used in overflow checks", !IO),
    test(base_string_to_int(10, "-9223372036854775810"), !IO),
    test(base_string_to_int(10, "-2147483650"), !IO),

    % Leading zeros do not count towards overflow: the number of digits
    % in the string is not bounded, only the value.

    line("Leading zero tests", !IO),

    test(base_string_to_int(10, "0000000000000000000000000000001"), !IO),
    test(base_string_to_int(10, "0000000000002147483647"), !IO),
    test(base_string_to_int(10, "0000000000009223372036854775807"), !IO),
    test(base_string_to_int(10, "0000000000009223372036854775808"), !IO),
    test(base_string_to_int(10, "-0000000002147483648"), !IO),
    test(base_string_to_int(10, "-0000009223372036854775808"), !IO),
    test(base_string_to_int(10, "-0000009223372036854775809"), !IO).

:- pred test(pred(T)::in(pred(out) is semidet), io::di, io::uo) is det.

test(P, !IO) :-
    ( if P(X) then
        io.write_line(X, !IO)
    else
        io.write_string("no\n", !IO)
    ).

:- pred line(string::in, io::di, io::uo) is det.

line(Title, !IO) :-
    io.format("### %s ###\n", [s(Title)], !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

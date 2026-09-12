%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the overflow behaviour of string.to_uint/2.
%
% The .exp file is for when uint is 32-bit.
% The .exp2 file is for when uint is 64-bit.
%
%---------------------------------------------------------------------------%

:- module string_to_uint_overflow.
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

    test(string.to_uint("0"), !IO),
    test(string.to_uint("1"), !IO),
    test(string.to_uint("999"), !IO),
    test(string.to_uint("99999999999999999999"), !IO),

    line("Decimal boundary values for 32-bit uints", !IO),

    test(base_string_to_uint(10, "4294967294"), !IO), % One under.
    test(base_string_to_uint(10, "4294967295"), !IO), % Boundary.
    test(base_string_to_uint(10, "4294967296"), !IO), % One over.

    line("Decimal boundary values for 64-bit uints", !IO),

    test(base_string_to_uint(10, "18446744073709551614"), !IO), % One under.
    test(base_string_to_uint(10, "18446744073709551615"), !IO), % Boundary.
    test(base_string_to_uint(10, "18446744073709551616"), !IO), % One over.

    line("Hexadecimal boundary values for 32-bit uints", !IO),

    test(base_string_to_uint(16, "fffffffe"), !IO),
    test(base_string_to_uint(16, "ffffffff"), !IO),
    test(base_string_to_uint(16, "100000000"), !IO),

    line("Hexadecimal boundary values for 64-bit uints", !IO),

    test(base_string_to_uint(16, "fffffffffffffffe"), !IO),
    test(base_string_to_uint(16, "ffffffffffffffff"), !IO),
    test(base_string_to_uint(16, "10000000000000000"), !IO),

    line("Octal boundary values for 32-bit uints", !IO),

    test(base_string_to_uint(8, "37777777776"), !IO),
    test(base_string_to_uint(8, "37777777777"), !IO),
    test(base_string_to_uint(8, "40000000000"), !IO),

    line("Octal boundary values for 64-bit uints", !IO),

    test(base_string_to_uint(8, "1777777777777777777776"), !IO),
    test(base_string_to_uint(8, "1777777777777777777777"), !IO),
    test(base_string_to_uint(8, "2000000000000000000000"), !IO),

    line("Binary boundary values for 32-bit uints", !IO),

    test(base_string_to_uint(2, "11111111111111111111111111111110"), !IO),
    test(base_string_to_uint(2, "11111111111111111111111111111111"), !IO),
    test(base_string_to_uint(2, "100000000000000000000000000000000"), !IO),

    line("Binary boundary values for 64-bit uints", !IO),

    test(base_string_to_uint(2, "11111111111111111111111111111111" ++
        "11111111111111111111111111111110"), !IO),
    test(base_string_to_uint(2, "11111111111111111111111111111111" ++
        "11111111111111111111111111111111"), !IO),
    test(base_string_to_uint(2, "1" ++ "00000000000000000000000000000000" ++
        "00000000000000000000000000000000"), !IO),

    line("Base-36 boundary values for 32-bit uints", !IO),

    test(base_string_to_uint(36, "1Z141Z2"), !IO),
    test(base_string_to_uint(36, "1Z141Z3"), !IO),
    test(base_string_to_uint(36, "1Z141Z4"), !IO),

    line("Base-36 boundary values for 64-bit uints", !IO),

    test(base_string_to_uint(36, "3W5E11264SGSE"), !IO),
    test(base_string_to_uint(36, "3W5E11264SGSF"), !IO),
    test(base_string_to_uint(36, "3W5E11264SGSG"), !IO),

    % The base_string_to_uint/3 fast path skips the overflow check entirely
    % for strings that are short enough that no string of that many digits
    % can overflow. The number of digits for which this holds depends on
    % the base and the word size; see safe_uint_digits_for_word{32,64}_and_base
    % in string.m.
    %
    % Note that the number of safe digits for a uint is not always the same as
    % it is for an int of the same word size: in base 10 with 64-bit words, for
    % example, 19 digits are safe for a uint but only 18 are safe for an int.
    %
    % The following paired tests check that the overflow behaviour is correct
    % at the boundary between the fast and the checked paths. The first test
    % in each pair is the largest value with the largest number of digits
    % that the fast path accepts: as many copies of the digit Base - 1 as the
    % table allows. If the table said one more digit is safe, then this value
    % would overflow on the unchecked loop and be converted to a wrong answer.
    % The second test in each pair adds that one more digit, which must take
    % the checked path and be rejected.
    %
    % Note that in base 2 and base 16, max_uint consists entirely of the
    % digit Base - 1, so for those bases the first test of each pair is
    % max_uint itself, as tested in the boundary sections above.

    line("Fast path boundary values for 32-bit uints", !IO),

    test(base_string_to_uint(2, "11111111111111111111111111111111"), !IO),
    test(base_string_to_uint(2, "111111111111111111111111111111111"), !IO),

    test(base_string_to_uint(8, "7777777777"), !IO),
    test(base_string_to_uint(8, "77777777777"), !IO),

    test(base_string_to_uint(10, "999999999"), !IO),
    test(base_string_to_uint(10, "9999999999"), !IO),

    test(base_string_to_uint(16, "FFFFFFFF"), !IO),
    test(base_string_to_uint(16, "FFFFFFFFF"), !IO),

    test(base_string_to_uint(36, "ZZZZZZ"), !IO),
    test(base_string_to_uint(36, "ZZZZZZZ"), !IO),

    line("Fast path boundary values for 64-bit uints", !IO),

    test(base_string_to_uint(2, "11111111111111111111111111111111" ++
        "11111111111111111111111111111111"), !IO),
    test(base_string_to_uint(2, "11111111111111111111111111111111" ++
        "111111111111111111111111111111111"), !IO),

    test(base_string_to_uint(8, "777777777777777777777"), !IO),
    test(base_string_to_uint(8, "7777777777777777777777"), !IO),

    test(base_string_to_uint(10, "9999999999999999999"), !IO),
    test(base_string_to_uint(10, "99999999999999999999"), !IO),

    test(base_string_to_uint(16, "FFFFFFFFFFFFFFFF"), !IO),
    test(base_string_to_uint(16, "FFFFFFFFFFFFFFFFF"), !IO),

    test(base_string_to_uint(36, "ZZZZZZZZZZZZ"), !IO),
    test(base_string_to_uint(36, "ZZZZZZZZZZZZZ"), !IO),

    % Regression tests for incorrect overflow check. Bases 10, 16, 8
    % and 2 each exercise one of the specialised accumulator closures;
    % base 36 exercises the generic accumulator that is shared by all
    % other bases.
    %
    % For each of these values, processing the final digit wraps past
    % 2^64 (2^32 on 32-bit platforms) and back to a value that is not
    % below the accumulator, so the old overflow check (N0 =< N) failed
    % to detect the overflow.
    %
    % The base 2 tests are max_uint followed by one more 1 digit:
    % the true value 2 * max_uint + 1 wraps to exactly max_uint, passing
    % the old check. This is the only case in base 2 that the old check
    % fails to detect.

    line("Overflow regression tests for 32-bit uints", !IO),

    test(base_string_to_uint(10, "5368709120"), !IO),
    test(base_string_to_uint(16, "140000000"), !IO),
    test(base_string_to_uint(8, "50000000000"), !IO),
    test(base_string_to_uint(2, "111111111111111111111111111111111"), !IO),
    test(base_string_to_uint(36, "11000000"), !IO),

    line("Overflow regression tests for 64-bit uints", !IO),

    test(base_string_to_uint(10, "23058430092136939520"), !IO),
    test(base_string_to_uint(16, "14000000000000000"), !IO),
    test(base_string_to_uint(8, "2400000000000000000000"), !IO),
    test(base_string_to_uint(2,
        "11111111111111111111111111111111" ++
        "111111111111111111111111111111111"), !IO),
    test(base_string_to_uint(36, "E00000000000Z"), !IO).

%---------------------------------------------------------------------------%

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
:- end_module string_to_uint_overflow.
%---------------------------------------------------------------------------%

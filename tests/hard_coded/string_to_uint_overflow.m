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

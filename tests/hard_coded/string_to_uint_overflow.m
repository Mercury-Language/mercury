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

:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    test(string.to_uint("0"), !IO),
    test(string.to_uint("1"), !IO),
    test(string.to_uint("999"), !IO),
    test(string.to_uint("99999999999999999999"), !IO),

    line(!IO),

    % Boundary values for 32-bit unsigned integers.
    test(base_string_to_uint(10, "4294967294"), !IO), % One under.
    test(base_string_to_uint(10, "4294967295"), !IO), % Boundary.
    test(base_string_to_uint(10, "4294967296"), !IO), % One over.

    line(!IO),

    % Boundary values for 64-bit unsigned integers.
    test(base_string_to_uint(10, "18446744073709551614"), !IO), % One under.
    test(base_string_to_uint(10, "18446744073709551615"), !IO), % Boundary.
    test(base_string_to_uint(10, "18446744073709551616"), !IO), % One over.

    line(!IO),

    % Regression tests for incorrect overflow check. For each of these
    % values, processing the final digit wraps past 2^64 (2^32 on 32-bit
    % platforms) and back to a value that is not below the accumulator,
    % so the old overflow check (N0 =< N) failed to detect the overflow.
    %
    % The base 2 tests are max_uint followed by one more 1 digit:
    % the true value 2 * max_uint + 1 wraps to exactly max_uint, passing
    % the old check. This is the only case in base 2 that the old check
    % fails to detect.

    % Overflow check regression tests for 64-bit uints.
    test(base_string_to_uint(10, "23058430092136939520"), !IO),
    test(base_string_to_uint(16, "14000000000000000"), !IO),
    test(base_string_to_uint(8, "2400000000000000000000"), !IO),
    test(base_string_to_uint(2,
        "11111111111111111111111111111111" ++
        "111111111111111111111111111111111"), !IO),

    line(!IO),

    % Overflow check regression tests for 32-bit uints.
    test(base_string_to_uint(10, "5368709120"), !IO),
    test(base_string_to_uint(16, "140000000"), !IO),
    test(base_string_to_uint(8, "50000000000"), !IO),
    test(base_string_to_uint(2, "111111111111111111111111111111111"), !IO).

%---------------------------------------------------------------------------%

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
:- end_module string_to_uint_overflow.
%---------------------------------------------------------------------------%

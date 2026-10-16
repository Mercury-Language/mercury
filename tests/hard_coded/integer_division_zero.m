%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A zero partial remainder followed by zero digits must stay canonical during
% long division. Otherwise the digit count can misrepresent its magnitude,
% both in the returned remainder and before later significant input digits.
%
% Build large values using integer shifts, so this also runs on 32-bit hosts.
%

:- module integer_division_zero.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module integer.
:- import_module list.
:- import_module string.

main(!IO) :-
    check("canonical exact remainder",
        ((pred) is semidet :-
            build_check_exponent_offset_shift_rem(27, 0, 28, integer.zero)
        ), !IO),
    check("canonical nonzero remainder",
        ((pred) is semidet :-
            build_check_exponent_offset_shift_rem(27, 0, 28, integer.one)
        ), !IO),
    check("significant digits after zero partial remainder",
        ((pred) is semidet :-
            B = integer.one << 27,
            build_check_signed_variants((B << 70) + B + integer.one, B,
                (integer.one << 70) + integer.one, integer.one)
        ), !IO),
    check("scaled and unscaled divisors",
        ((pred) is semidet :-
            list.all_true(build_check_exponent_values,
                [13, 14, 15, 26, 27, 28, 41, 55, 83])
        ), !IO),
    check("zero and small dividends",
        ((pred) is semidet :-
            B = integer.one << 55,
            build_check_signed_variants(integer.zero, B, integer.zero,
                integer.zero),
            build_check_signed_variants(integer.one, B, integer.zero,
                integer.one),
            build_check_signed_variants(B - integer.one, B, integer.zero,
                B - integer.one)
        ), !IO).

:- pred build_check_exponent_values(int::in) is semidet.

build_check_exponent_values(Exponent) :-
    list.all_true(
        (pred(Offset::in) is semidet :-
            B = (integer.one << Exponent) + integer(Offset),
            list.all_true(
                (pred(Shift::in) is semidet :-
                    list.all_true(
                        (pred(R::in) is semidet :-
                            build_check_exponent_offset_shift_rem(Exponent,
                                Offset, Shift, R)),
                        [integer.zero, integer.one, B - integer.one])
                ),
                [1, 14, 28, 56])
        ),
        [-1, 0, 1]).

:- pred build_check_exponent_offset_shift_rem(int::in, int::in, int::in,
    integer::in) is semidet.

build_check_exponent_offset_shift_rem(Exponent, Offset, Shift, R) :-
    B = (integer.one << Exponent) + integer(Offset),
    Q = integer.one << Shift,
    A = B * Q + R,
    build_check_signed_variants(A, B, Q, R).

:- pred build_check_signed_variants(integer::in, integer::in,
    integer::in, integer::in) is semidet.

build_check_signed_variants(A, B, Q, R) :-
    check_division(A, B, Q, R),
    check_division(-A, B, -Q, -R),
    check_division(A, -B, -Q, R),
    check_division(-A, -B, Q, -R).

:- pred check_division(integer::in, integer::in, integer::in, integer::in)
    is semidet.

check_division(A, B, ExpectedQ, ExpectedR) :-
    integer.divide_with_rem(A, B, Q, R),
    Q = ExpectedQ,
    R = ExpectedR,
    A // B = ExpectedQ,
    A rem B = ExpectedR,
    A = Q * B + R,
    integer.abs(R) < integer.abs(B),
    ( if ExpectedR = integer.zero then
        integer.is_zero(R),
        not (R < integer.zero),
        not (R > integer.zero)
    else
        not integer.is_zero(R)
    ),
    integer.to_string(Q) = integer.to_string(ExpectedQ),
    integer.to_string(R) = integer.to_string(ExpectedR),
    ( if
        ExpectedR \= integer.zero,
        ( (A < integer.zero, B > integer.zero)
        ; (A > integer.zero, B < integer.zero)
        )
    then
        A div B = ExpectedQ - integer.one,
        A mod B = ExpectedR + B
    else
        A div B = ExpectedQ,
        A mod B = ExpectedR
    ).

:- pred check(string::in, (pred)::in((pred) is semidet),
    io::di, io::uo) is det.

check(Label, Test, !IO) :-
    ( if Test then
        io.write_string("PASS " ++ Label ++ "\n", !IO)
    else
        io.write_string("FAIL " ++ Label ++ "\n", !IO)
    ).

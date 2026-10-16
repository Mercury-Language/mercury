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
    Zero = integer.zero,
    One  = integer.one,

    build_check_exponent_offset_shift_rem(27, 0, 28, Zero,
        [], ExactFails),
    report_results("canonical exact remainder", ExactFails, !IO),

    build_check_exponent_offset_shift_rem(27, 0, 28, One,
        [], NonZeroFails),
    report_results("canonical nonzero remainder", NonZeroFails, !IO),

    Shift27 = integer.one << 27,
    build_check_signed_variants((Shift27 << 70) + Shift27 + integer.one,
        Shift27, (integer.one << 70) + integer.one, integer.one,
        [], DigitFails),
    report_results("significant digits after zero partial remainder",
        DigitFails, !IO),

    list.foldl(build_check_exponent_values,
        [13, 14, 15, 26, 27, 28, 41, 55, 83],
        [], ScaledFails),
    report_results("scaled and unscaled divisors", ScaledFails, !IO),

    Shift55 = integer.one << 55,
    build_check_signed_variants(Zero, Shift55, Zero, Zero,
        [], ZeroSmallFails1),
    build_check_signed_variants(One, Shift55, Zero, One,
        ZeroSmallFails1, ZeroSmallFails2),
    build_check_signed_variants(Shift55 - One, Shift55, Zero, Shift55 - One,
        ZeroSmallFails2, ZeroSmallFails),
    report_results("zero and small dividends", ZeroSmallFails, !IO).

%---------------------------------------------------------------------------%

:- pred build_check_exponent_values(int::in,
    list(a_b_q_r)::in, list(a_b_q_r)::out) is det.

build_check_exponent_values(Exponent, !Fails) :-
    list.foldl(build_check_exponent_offset_values(Exponent),
        [-1, 0, 1], !Fails).

:- pred build_check_exponent_offset_values(int::in, int::in,
    list(a_b_q_r)::in, list(a_b_q_r)::out) is det.

build_check_exponent_offset_values(Exponent, Offset, !Fails) :-
    list.foldl(build_check_exponent_offset_shift_values(Exponent, Offset),
        [1, 14, 28, 56], !Fails).

:- pred build_check_exponent_offset_shift_values(int::in, int::in, int::in,
    list(a_b_q_r)::in, list(a_b_q_r)::out) is det.

build_check_exponent_offset_shift_values(Exponent, Offset, Shift, !Fails) :-
    B = (integer.one << Exponent) + integer(Offset),
    list.foldl(build_check_exponent_offset_shift_rem(Exponent, Offset, Shift),
        [integer.zero, integer.one, B - integer.one], !Fails).

:- pred build_check_exponent_offset_shift_rem(int::in, int::in, int::in,
    integer::in, list(a_b_q_r)::in, list(a_b_q_r)::out) is det.

build_check_exponent_offset_shift_rem(Exponent, Offset, Shift, R, !Fails) :-
    B = (integer.one << Exponent) + integer(Offset),
    Q = integer.one << Shift,
    A = B * Q + R,
    build_check_signed_variants(A, B, Q, R, !Fails).

%---------------------------------------------------------------------------%

:- pred build_check_signed_variants(integer::in, integer::in,
    integer::in, integer::in, list(a_b_q_r)::in, list(a_b_q_r)::out) is det.

build_check_signed_variants(A, B, Q, R, !Fails) :-
    check_division(A, B, Q, R, !Fails),
    check_division(-A, B, -Q, -R, !Fails),
    check_division(A, -B, -Q, R, !Fails),
    check_division(-A, -B, Q, -R, !Fails).

%---------------------------------------------------------------------------%

:- pred check_division(integer::in, integer::in, integer::in, integer::in,
    list(a_b_q_r)::in, list(a_b_q_r)::out) is det.

check_division(A, B, ExpectedQ, ExpectedR, !Fails) :-
    integer.divide_with_rem(A, B, Q, R),
    ( if
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
        )
    then
        true
    else
        !:Fails = [a_b_q_r(A, B, Q, R) | !.Fails]
    ).

%---------------------------------------------------------------------------%

:- type a_b_q_r
    --->    a_b_q_r(integer, integer, integer, integer).

:- pred report_results(string::in, list(a_b_q_r)::in, io::di, io::uo) is det.

report_results(Label, Fails, !IO) :-
    (
        Fails = [],
        io.write_string("PASS " ++ Label ++ "\n", !IO)
    ;
        Fails = [_ | _],
        io.write_string("FAIL " ++ Label ++ "\n", !IO),
        list.foldl(write_a_b_q_r, Fails, !IO)
    ).

:- pred write_a_b_q_r(a_b_q_r::in, io::di, io::uo) is det.

write_a_b_q_r(a_b_q_r(A, B, Q, R), !IO) :-
    AStr = integer.to_string(A),
    BStr = integer.to_string(B),
    QStr = integer.to_string(Q),
    RStr = integer.to_string(R),
    io.format("a_b_q_r(%s, %s, %s, %s)\n",
        [s(AStr), s(BStr), s(QStr), s(RStr)], !IO).

%---------------------------------------------------------------------------%

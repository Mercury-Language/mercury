%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regressions for the rational zero handling fixed in fa271a0ec (#141/#142).
% The GCD in this subtraction used to encounter an arithmetic zero that was
% not structurally equal to integer.zero and throw a division-by-zero error.
% This test needs neither private constructors nor the later integer_append
% fix. Also check normalization and rejection of an arithmetic-zero divisor.
%

:- module rational_zero_arithmetic.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module exception.
:- import_module integer.
:- import_module list.
:- import_module rational.
:- import_module string.

main(!IO) :-
    A = integer.one << 27,
    B = integer.one << 13,
    X = rational.from_integers(integer.one, A),
    Y = rational.from_integers(integer.one, B),
    Expected = rational.from_integers(integer(-16383), A),
    check("subtraction through GCD", ((pred) is semidet :-
        X - Y = Expected,
        (X - Y) + Y = X,
        X - X = rational.zero,
        X < Y), !IO),
    Zero = A rem B,
    check("normalizing arithmetic-zero numerators", ((pred) is semidet :-
        list.all_true(
            (pred(Z::in) is semidet :-
                R = rational.from_integers(Z, integer(13)),
                R = rational.zero,
                rational.numer(R) = integer.zero,
                rational.denom(R) = integer.one,
                R + X = X,
                R * X = rational.zero
            ),
            [integer.zero, Zero, (-A) rem B])
        ), !IO),
    try((pred(R::out) is det :-
        R = rational.from_integers(Zero, Zero)), Result),
    (
        Result = exception(_),
        io.write_string("PASS rejecting arithmetic-zero denominator\n", !IO)
    ;
        Result = succeeded(_),
        io.write_string("FAIL rejecting arithmetic-zero denominator\n", !IO)
    ).

:- pred check(string::in, (pred)::in((pred) is semidet),
    io::di, io::uo) is det.

check(Label, Test, !IO) :-
    ( if Test then
        io.write_string("PASS " ++ Label ++ "\n", !IO)
    else
        io.write_string("FAIL " ++ Label ++ "\n", !IO)
    ).

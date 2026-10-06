%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regressions for the zero handling fixed in commit fa271a0ec (#141/#142).
% Exact division provides arithmetic zeros through the public API. Before
% that commit these could have zero digits but a nonzero digit count, so
% is_zero failed and multiplication by a larger multiword integer was not
% canonical. The larger operand makes the zero supply the multiplier digit,
% exercising the mul_by_digit(0, _) guard added by that commit.
%
% Do not require the remainder itself to be canonical here: canonicalizing
% partial remainders in long division is a separate fix. This test passes on
% fa271a0ec without that later fix. No private representation is constructed.
%

:- module integer_zero_arithmetic.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module integer.
:- import_module list.
:- import_module string.

main(!IO) :-
    A = integer.one << 27,
    B = integer.one << 13,
    Zeros = [integer.zero, A rem B, (-A) rem B],
    check("recognizing arithmetic zeros", ((pred) is semidet :-
        list.all_true(integer.is_zero, Zeros)), !IO),
    check("multiword zero products", ((pred) is semidet :-
        list.all_true(multiword_zero_products, Zeros)), !IO),
    check("nonzero controls", ((pred) is semidet :-
        not integer.is_zero(B),
        not integer.is_zero(-B),
        A * integer.one = A,
        A * (-integer.one) = -A), !IO).

:- pred multiword_zero_products(integer::in) is semidet.

multiword_zero_products(Zero) :-
    Large = integer.one << 28,
    list.all_true(
        (pred(Factor::in) is semidet :-
            Left = Zero * Factor,
            Right = Factor * Zero,
            Left = integer.zero,
            Right = integer.zero,
            integer.to_string(Left) = "0",
            integer.to_string(Right) = "0"
        ),
        [Large, -Large, Large + integer.one, -Large - integer.one]),
    Zero * Zero = integer.zero.

:- pred check(string::in, (pred)::in((pred) is semidet),
    io::di, io::uo) is det.

check(Label, Test, !IO) :-
    ( if Test then
        io.write_string("PASS " ++ Label ++ "\n", !IO)
    else
        io.write_string("FAIL " ++ Label ++ "\n", !IO)
    ).

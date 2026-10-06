%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A noncanonical nonzero remainder from integer division can also break the
% GCD used by rational arithmetic. Recognizing noncanonical zeros in is_zero/1
% is not sufficient: ((2^55 << 42) + 1) rem 2^55 used to contain leading zeros.
%

:- module integer_division_rational.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module integer.
:- import_module rational.

main(!IO) :-
    B = integer.one << 55,
    A = (B << 42) + integer.one,
    X = rational.from_integers(integer.one, A),
    Y = rational.from_integers(integer.one, B),
    Difference = X - Y,
    io.write_string(integer.to_string(rational.numer(Difference)), !IO),
    io.write_string(" / ", !IO),
    io.write_string(integer.to_string(rational.denom(Difference)), !IO),
    io.nl(!IO).

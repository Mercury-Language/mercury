%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test case for arithmetic on complex, imag, and float.

:- module complex_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module complex_numbers.
:- import_module complex_numbers.complex.
:- import_module complex_numbers.complex_imag.
:- import_module complex_numbers.float_imag.
:- import_module complex_numbers.imag.
:- import_module complex_numbers.imag_complex.
:- import_module complex_numbers.imag_float.
:- import_module float.

main(!IO) :-
    io.print_line("tests of (complex op complex)", !IO),
    X = 3.0 + 4.0 * i,
    io.print("X = ", !IO),     io.print_line(X, !IO),
    io.print("X + X = ", !IO), io.print_line(X + X, !IO),
    io.print("X - X = ", !IO), io.print_line(X - X, !IO),
    io.print("X * X = ", !IO), io.print_line(X * X, !IO),
    io.print("X / X = ", !IO), io.print_line(X / X, !IO),
    Y = - 5.0 + 6.0 * i,
    io.print("Y = ", !IO),     io.print_line(Y, !IO),
    io.print("Y + Y = ", !IO), io.print_line(Y + Y, !IO),
    io.print("Y - Y = ", !IO), io.print_line(Y - Y, !IO),
    io.print("Y * Y = ", !IO), io.print_line(Y * Y, !IO),
    io.print("Y / Y = ", !IO), io.print_line(Y / Y, !IO),
    io.print("X + Y = ", !IO), io.print_line(X + Y, !IO),
    io.print("X - Y = ", !IO), io.print_line(X - Y, !IO),
    io.print("X * Y = ", !IO), io.print_line(X * Y, !IO),
    io.print("X / Y = ", !IO), io.print_line(X / Y, !IO),
    io.nl(!IO),

    io.print_line("tests of (imag op imag)", !IO),
    Z = 4.0 * i,
    io.print("Z = ", !IO),     io.print_line(Z, !IO),
    io.print("Z + Z = ", !IO), io.print_line(Z + Z, !IO),
    io.print("Z - Z = ", !IO), io.print_line(Z - Z, !IO),
    io.print("Z * Z = ", !IO), io.print_line(Z * Z, !IO),
    io.print("Z / Z = ", !IO), io.print_line(Z / Z, !IO),
    io.nl(!IO),

    io.print_line("tests of (float op imag)", !IO),
    io.print("5.0 + Z = ", !IO), io.print_line(5.0 + Z, !IO),
    io.print("5.0 - Z = ", !IO), io.print_line(5.0 - Z, !IO),
    io.print("5.0 * Z = ", !IO), io.print_line(5.0 * Z, !IO),
    io.print("5.0 / Z = ", !IO), io.print_line(5.0 / Z, !IO),
    io.nl(!IO),

    io.print_line("tests of (imag op float)", !IO),
    io.print("Z + 5.0 = ", !IO), io.print_line(Z + 5.0, !IO),
    io.print("Z - 5.0 = ", !IO), io.print_line(Z - 5.0, !IO),
    io.print("Z * 5.0 = ", !IO), io.print_line(Z * 5.0, !IO),
    io.print("Z / 5.0 = ", !IO), io.print_line(Z / 5.0, !IO),
    io.nl(!IO),

    io.print_line("tests of (complex op imag)", !IO),
    io.print("X + Z = ", !IO), io.print_line(X + Z, !IO),
    io.print("X - Z = ", !IO), io.print_line(X - Z, !IO),
    io.print("X * Z = ", !IO), io.print_line(X * Z, !IO),
    io.print("X / Z = ", !IO), io.print_line(X / Z, !IO),
    io.print("Y + Z = ", !IO), io.print_line(Y + Z, !IO),
    io.print("Y - Z = ", !IO), io.print_line(Y - Z, !IO),
    io.print("Y * Z = ", !IO), io.print_line(Y * Z, !IO),
    io.print("Y / Z = ", !IO), io.print_line(Y / Z, !IO),
    io.nl(!IO),

    io.print_line("tests of (imag op complex)", !IO),
    io.print("Z + X = ", !IO), io.print_line(Z + X, !IO),
    io.print("Z - X = ", !IO), io.print_line(Z - X, !IO),
    io.print("Z * X = ", !IO), io.print_line(Z * X, !IO),
    io.print("Z / X = ", !IO), io.print_line(Z / X, !IO),
    io.print("Z + Y = ", !IO), io.print_line(Z + Y, !IO),
    io.print("Z - Y = ", !IO), io.print_line(Z - Y, !IO),
    io.print("Z * Y = ", !IO), io.print_line(Z * Y, !IO),
    io.print("Z / Y = ", !IO), io.print_line(Z / Y, !IO).

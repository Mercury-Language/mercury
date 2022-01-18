%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%

% A test case for arithmetic on complex, imag, and float.

:- module complex_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module complex_numbers.
:- import_module complex_numbers.complex.
:- import_module complex_numbers.complex_imag.
:- import_module complex_numbers.float_imag.
:- import_module complex_numbers.imag.
:- import_module complex_numbers.imag_complex.
:- import_module complex_numbers.imag_float.

:- import_module float.

%-----------------------------------------------------------------------------%

main(!IO) :-
    print_line("tests of (complex op complex)", !IO),
    X = 3.0 + 4.0 * i,
    print("X = ", !IO), print_line(X, !IO),
    print("X + X = ", !IO), print_line(X + X, !IO),
    print("X - X = ", !IO), print_line(X - X, !IO),
    print("X * X = ", !IO), print_line(X * X, !IO),
    print("X / X = ", !IO), print_line(X / X, !IO),
    Y = - 5.0 + 6.0 * i,
    print("Y = ", !IO), print_line(Y, !IO),
    print("Y + Y = ", !IO), print_line(Y + Y, !IO),
    print("Y - Y = ", !IO), print_line(Y - Y, !IO),
    print("Y * Y = ", !IO), print_line(Y * Y, !IO),
    print("Y / Y = ", !IO), print_line(Y / Y, !IO),
    print("X + Y = ", !IO), print_line(X + Y, !IO),
    print("X - Y = ", !IO), print_line(X - Y, !IO),
    print("X * Y = ", !IO), print_line(X * Y, !IO),
    print("X / Y = ", !IO), print_line(X / Y, !IO),
    nl(!IO),

    print_line("tests of (imag op imag)", !IO),
    Z = 4.0 * i,
    print("Z = ", !IO), print_line(Z, !IO),
    print("Z + Z = ", !IO), print_line(Z + Z, !IO),
    print("Z - Z = ", !IO), print_line(Z - Z, !IO),
    print("Z * Z = ", !IO), print_line(Z * Z, !IO),
    print("Z / Z = ", !IO), print_line(Z / Z, !IO),
    nl(!IO),

    print_line("tests of (float op imag)", !IO),
    print("5.0 + Z = ", !IO), print_line(5.0 + Z, !IO),
    print("5.0 - Z = ", !IO), print_line(5.0 - Z, !IO),
    print("5.0 * Z = ", !IO), print_line(5.0 * Z, !IO),
    print("5.0 / Z = ", !IO), print_line(5.0 / Z, !IO),
    nl(!IO),

    print_line("tests of (imag op float)", !IO),
    print("Z + 5.0 = ", !IO), print_line(Z + 5.0, !IO),
    print("Z - 5.0 = ", !IO), print_line(Z - 5.0, !IO),
    print("Z * 5.0 = ", !IO), print_line(Z * 5.0, !IO),
    print("Z / 5.0 = ", !IO), print_line(Z / 5.0, !IO),
    nl(!IO),

    print_line("tests of (complex op imag)", !IO),
    print("X + Z = ", !IO), print_line(X + Z, !IO),
    print("X - Z = ", !IO), print_line(X - Z, !IO),
    print("X * Z = ", !IO), print_line(X * Z, !IO),
    print("X / Z = ", !IO), print_line(X / Z, !IO),
    print("Y + Z = ", !IO), print_line(Y + Z, !IO),
    print("Y - Z = ", !IO), print_line(Y - Z, !IO),
    print("Y * Z = ", !IO), print_line(Y * Z, !IO),
    print("Y / Z = ", !IO), print_line(Y / Z, !IO),
    nl(!IO),

    print_line("tests of (imag op complex)", !IO),
    print("Z + X = ", !IO), print_line(Z + X, !IO),
    print("Z - X = ", !IO), print_line(Z - X, !IO),
    print("Z * X = ", !IO), print_line(Z * X, !IO),
    print("Z / X = ", !IO), print_line(Z / X, !IO),
    print("Z + Y = ", !IO), print_line(Z + Y, !IO),
    print("Z - Y = ", !IO), print_line(Z - Y, !IO),
    print("Z * Y = ", !IO), print_line(Z * Y, !IO),
    print("Z / Y = ", !IO), print_line(Z / Y, !IO).

%-----------------------------------------------------------------------------%
:- end_module complex_test.
%-----------------------------------------------------------------------------%

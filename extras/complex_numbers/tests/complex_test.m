% A test case for arithmetic on complex, imag, and float.

:- module complex_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module float.
:- import_module complex_numbers.
:- import_module complex_numbers__complex, complex_numbers__imag.
:- import_module complex_numbers__complex_imag, complex_numbers__imag_complex.
:- import_module complex_numbers__float_imag, complex_numbers__imag_float.

main -->
	print("tests of (complex op complex)"), nl,
	{ X = 3.0 + 4.0 * i},
	print("X = "), print(X), nl,
	print("X + X = "), print(X + X), nl,
	print("X - X = "), print(X - X), nl,
	print("X * X = "), print(X * X), nl,
	print("X / X = "), print(X / X), nl,
	{ Y = - 5.0 + 6.0 * i},
	print("Y = "), print(Y), nl,
	print("Y + Y = "), print(Y + Y), nl,
	print("Y - Y = "), print(Y - Y), nl,
	print("Y * Y = "), print(Y * Y), nl,
	print("Y / Y = "), print(Y / Y), nl,
	print("X + Y = "), print(X + Y), nl,
	print("X - Y = "), print(X - Y), nl,
	print("X * Y = "), print(X * Y), nl,
	print("X / Y = "), print(X / Y), nl,
	nl,

	print("tests of (imag op imag)"), nl,
	{ Z = 4.0 * i},
	print("Z = "), print(Z), nl,
	print("Z + Z = "), print(Z + Z), nl,
	print("Z - Z = "), print(Z - Z), nl,
	print("Z * Z = "), print(Z * Z), nl,
	print("Z / Z = "), print(Z / Z), nl,
	nl,

	print("tests of (float op imag)"), nl,
	print("5.0 + Z = "), print(5.0 + Z), nl,
	print("5.0 - Z = "), print(5.0 - Z), nl,
	print("5.0 * Z = "), print(5.0 * Z), nl,
	print("5.0 / Z = "), print(5.0 / Z), nl,
	nl,

	print("tests of (imag op float)"), nl,
	print("Z + 5.0 = "), print(Z + 5.0), nl,
	print("Z - 5.0 = "), print(Z - 5.0), nl,
	print("Z * 5.0 = "), print(Z * 5.0), nl,
	print("Z / 5.0 = "), print(Z / 5.0), nl,
	nl,

	print("tests of (complex op imag)"), nl,
	print("X + Z = "), print(X + Z), nl,
	print("X - Z = "), print(X - Z), nl,
	print("X * Z = "), print(X * Z), nl,
	print("X / Z = "), print(X / Z), nl,
	print("Y + Z = "), print(Y + Z), nl,
	print("Y - Z = "), print(Y - Z), nl,
	print("Y * Z = "), print(Y * Z), nl,
	print("Y / Z = "), print(Y / Z), nl,
	nl,

	print("tests of (imag op complex)"), nl,
	print("Z + X = "), print(Z + X), nl,
	print("Z - X = "), print(Z - X), nl,
	print("Z * X = "), print(Z * X), nl,
	print("Z / X = "), print(Z / X), nl,
	print("Z + Y = "), print(Z + Y), nl,
	print("Z - Y = "), print(Z - Y), nl,
	print("Z * Y = "), print(Z * Y), nl,
	print("Z / Y = "), print(Z / Y), nl.

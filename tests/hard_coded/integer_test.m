%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A very basic check of arithmetic on big integers.
%

:- module integer_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module integer.
:- import_module list.
:- import_module require.
:- import_module string.

main(!IO) :-
    X = integer.det_from_string("+1234567890987654321"),
    Y = integer.det_from_string("98765432101234567890123400000009999111"),
    Z = integer(200),
    test(X, Y, Z, !IO).

:- pred test(integer::in, integer::in, integer::in, io::di, io::uo) is det.

test(X, Y, Z, !IO) :-
    Plus = X + Y,
    Times = X * Y,
    Minus = X - Y,
    Div = Y // X,
    Rem = Y rem X,
    Pow = integer.pow(X, Z),
    fac(Z, Fac),

    write_message("X: ", X, !IO),
    write_message("Y: ", Y, !IO),
    write_message("Z: ", Z, !IO),
    write_message("X + Y: ", Plus, !IO),
    write_message("X * Y: ", Times, !IO),
    write_message("X - Y: ", Minus, !IO),
    write_message("Y // X: ", Div, !IO),
    write_message("Y rem X: ", Rem, !IO),
    write_message("fac(Z): ", Fac, !IO),
    write_message("pow(X,Z): ", Pow, !IO),
    X0 = integer(100000), X1 = integer(3),
    write_integer(X0, !IO), io.write_string(" div mod ", !IO),
        write_integer(X1, !IO), io.write_string(" = ", !IO),
        write_integer(X0 div X1, !IO), io.write_string(" ", !IO),
        write_integer(X0 mod X1, !IO), io.nl(!IO),
    write_integer(-X0, !IO), io.write_string(" div mod ", !IO),
        write_integer(X1, !IO), io.write_string(" = ", !IO),
        write_integer(X0 div -X1, !IO), io.write_string(" ", !IO),
        write_integer(X0 mod -X1, !IO), io.nl(!IO),
    write_integer(X0, !IO), io.write_string(" div mod ", !IO),
        write_integer(-X1, !IO), io.write_string(" = ", !IO),
        write_integer(-X0 div X1, !IO), io.write_string(" ", !IO),
        write_integer(-X0 mod X1, !IO), io.nl(!IO),
    write_integer(-X0, !IO), io.write_string(" div mod ", !IO),
        write_integer(-X1, !IO), io.write_string(" = ", !IO),
        write_integer(-X0 div -X1, !IO), io.write_string(" ", !IO),
        write_integer(-X0 mod -X1, !IO), io.nl(!IO),
    write_integer(X0, !IO), io.write_string(" // rem ", !IO),
        write_integer(X1, !IO), io.write_string(" = ", !IO),
        write_integer(X0 // X1, !IO), io.write_string(" ", !IO),
        write_integer(X0 rem X1, !IO), io.nl(!IO),
    write_integer(-X0, !IO), io.write_string(" // rem ", !IO),
        write_integer(X1, !IO), io.write_string(" = ", !IO),
        write_integer(X0 // -X1, !IO), io.write_string(" ", !IO),
        write_integer(X0 rem -X1, !IO), io.nl(!IO),
    write_integer(X0, !IO), io.write_string(" // rem ", !IO),
        write_integer(-X1, !IO), io.write_string(" = ", !IO),
        write_integer(-X0 // X1, !IO), io.write_string(" ", !IO),
        write_integer(-X0 rem X1, !IO), io.nl(!IO),
    write_integer(-X0, !IO), io.write_string(" // rem ", !IO),
        write_integer(-X1, !IO), io.write_string(" = ", !IO),
        write_integer(-X0 // -X1, !IO), io.write_string(" ", !IO),
        write_integer(-X0 rem -X1, !IO), io.nl(!IO),
    int.min_int(Minint),
    ( if integer(Minint) < integer(0) then
        io.write_string("integer(min_int) ok\n", !IO)
    else
        io.write_string("integer(min_int) failed\n", !IO)
    ).

:- pred fac(integer::in, integer::out) is det.

fac(X, F) :-
    ( if X =< integer.zero then
        F = integer.one
    else
        fac(X - integer.one, F1),
        F = F1 * X
    ).

:- pred write_message(string::in, integer::in, io::di, io::uo) is det.

write_message(String, Int, !IO) :-
    io__write_string(String, !IO),
    Str = integer.to_string(Int),
    io.write_string(Str, !IO),
    io.nl(!IO).

:- pred write_integer(integer::in, io::di, io::uo) is det.

write_integer(X, !IO) :-
    S = integer.to_string(X),
    io.write_string(S, !IO).

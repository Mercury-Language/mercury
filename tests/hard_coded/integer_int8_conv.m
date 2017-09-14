%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion of int8s to integers and integers to int8s.

:- module integer_int8_conv.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module integer.
:- import_module int8.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("*** Testing int8 -> integer conversion ***\n\n", !IO),
    list.foldl(do_to_integer_test, test_int8s, !IO),
    io.nl(!IO),
    io.write_string("*** Testing integer -> int8 conversion ***\n\n", !IO),
    list.foldl(do_from_integer_test, test_integers, !IO).

:- pred do_to_integer_test(int8::in, io::di, io::uo) is det.

do_to_integer_test(U, !IO) :-
    Integer = integer.from_int8(U),
    io.write_string("int8 = ", !IO),
    io.write_int8(U, !IO),
    io.write_string(", integer = ", !IO),
    io.print(Integer, !IO),
    io.nl(!IO).

:- pred do_from_integer_test(integer::in, io::di, io::uo) is det.

do_from_integer_test(Integer, !IO) :-
    io.write_string("integer = ", !IO),
    io.print(Integer, !IO),
    io.write_string(", int8 = ", !IO),
    ( if integer.to_int8(Integer, U) then
        io.write_int8(U, !IO)
    else
        io.write_string("<<OUT-OF-RANGE>>", !IO)
    ),
    io.nl(!IO).

:- func test_int8s = list(int8).

test_int8s = [
    -128i8,
    -64i8,
    -32i8,
    -1i8,
    0i8,
    1i8,
    2i8,
    4i8,
    8i8,
    10i8,
    16i8,
    32i8,
    64i8,
    127i8
].

:- func test_integers = list(integer).

test_integers = [
    det_from_string("-9223372036854775808"),
    det_from_string("-4294967296"),
    det_from_string("-2147483648"),
    det_from_string("-2"),
    det_from_string("-1"),
    det_from_string("0"),
    det_from_string("1"),
    det_from_string("2"),
    det_from_string("16383"),
    det_from_string("16384"),
    det_from_string("16385"),
    det_from_string("1073741824"),
    det_from_string("2147483648"),
    det_from_string("4294967295"),
    det_from_string("4294967296"),
    det_from_string("4294967297"),
    det_from_string("9223372036854775808"),
    det_from_string("18446744073709551615"),
    det_from_string("18446744073709551616")
].

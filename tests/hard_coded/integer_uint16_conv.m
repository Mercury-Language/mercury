%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion of uint16s to integers and integers to uint16s.

:- module integer_uint16_conv.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module integer.
:- import_module uint16.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("*** Testing uint16 -> integer conversion ***\n\n", !IO),
    list.foldl(do_to_integer_test, test_uint16s, !IO),
    io.nl(!IO),
    io.write_string("*** Testing integer -> uint16 conversion ***\n\n", !IO),
    list.foldl(do_from_integer_test, test_integers, !IO).

:- pred do_to_integer_test(uint16::in, io::di, io::uo) is det.

do_to_integer_test(U, !IO) :-
    Integer = integer.from_uint16(U),
    io.write_string("uint16 = ", !IO),
    io.write_uint16(U, !IO),
    io.write_string(", integer = ", !IO),
    io.print_line(Integer, !IO).

:- pred do_from_integer_test(integer::in, io::di, io::uo) is det.

do_from_integer_test(Integer, !IO) :-
    io.write_string("integer = ", !IO),
    io.print(Integer, !IO),
    io.write_string(", uint16 = ", !IO),
    ( if integer.to_uint16(Integer, U) then
        io.write_uint16(U, !IO)
    else
        io.write_string("<<OUT-OF-RANGE>>", !IO)
    ),
    io.nl(!IO).

:- func test_uint16s = list(uint16).

test_uint16s = [
    0u16,
    1u16,
    2u16,
    4u16,
    8u16,
    10u16,
    16u16,
    32u16,
    64u16,
    16383u16,  % i.e. integer.base - 1
    16384u16,  % i.e. integer.base
    16385u16,  % i.e. integer.base + 1,
    32768u16,
    65535u16
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
    det_from_string("65535"),
    det_from_string("65536"),
    det_from_string("1073741824"),
    det_from_string("2147483648"),
    det_from_string("4294967295"),
    det_from_string("4294967296"),
    det_from_string("4294967297"),
    det_from_string("9223372036854775808"),
    det_from_string("18446744073709551615"),
    det_from_string("18446744073709551616")
].

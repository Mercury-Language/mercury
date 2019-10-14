%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion of uint32s to integers and integers to uint32s.

:- module integer_uint32_conv.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module integer.
:- import_module uint32.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("*** Testing uint32 -> integer conversion ***\n\n", !IO),
    list.foldl(do_to_integer_test, test_uint32s, !IO),
    io.nl(!IO),
    io.write_string("*** Testing integer -> uint32 conversion ***\n\n", !IO),
    list.foldl(do_from_integer_test, test_integers, !IO).

:- pred do_to_integer_test(uint32::in, io::di, io::uo) is det.

do_to_integer_test(U, !IO) :-
    Integer = integer.from_uint32(U),
    io.write_string("uint32 = ", !IO),
    io.write_uint32(U, !IO),
    io.write_string(", integer = ", !IO),
    io.print(Integer, !IO),
    io.nl(!IO).

:- pred do_from_integer_test(integer::in, io::di, io::uo) is det.

do_from_integer_test(Integer, !IO) :-
    io.write_string("integer = ", !IO),
    io.print(Integer, !IO),
    io.write_string(", uint32 = ", !IO),
    ( if integer.to_uint32(Integer, U) then
        io.write_uint32(U, !IO)
    else
        io.write_string("<<OUT-OF-RANGE>>", !IO)
    ),
    io.nl(!IO).

:- func test_uint32s = list(uint32).

test_uint32s = [
    0u32,
    1u32,
    2u32,
    4u32,
    8u32,
    10u32,
    16u32,
    32u32,
    64u32,
    16383u32,
    16384u32,
    16385u32,
    32768u32,
    65536u32,
    1073741823u32, % i.e. integer.base - 1
    1073741824u32, % i.e. integer.base
    1073741825u32, % i.e. integer.base + 1
    2147483647u32,
    4294967295u32
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
    det_from_string("1073741823"),
    det_from_string("1073741824"),
    det_from_string("1073741825"),
    det_from_string("2147483648"),
    det_from_string("4294967295"),
    det_from_string("4294967296"),
    det_from_string("4294967297"),
    det_from_string("9223372036854775808"),
    det_from_string("18446744073709551615"),
    det_from_string("18446744073709551616")
].

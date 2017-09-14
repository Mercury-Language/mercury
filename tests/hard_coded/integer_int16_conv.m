%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion of int16s to integers and integers to int16s.

:- module integer_int16_conv.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module integer.
:- import_module int16.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("*** Testing int16 -> integer conversion ***\n\n", !IO),
    list.foldl(do_to_integer_test, test_int16s, !IO),
    io.nl(!IO),
    io.write_string("*** Testing integer -> int16 conversion ***\n\n", !IO),
    list.foldl(do_from_integer_test, test_integers, !IO).

:- pred do_to_integer_test(int16::in, io::di, io::uo) is det.

do_to_integer_test(U, !IO) :-
    Integer = integer.from_int16(U),
    io.write_string("int16 = ", !IO),
    io.write_int16(U, !IO),
    io.write_string(", integer = ", !IO),
    io.print(Integer, !IO),
    io.nl(!IO).

:- pred do_from_integer_test(integer::in, io::di, io::uo) is det.

do_from_integer_test(Integer, !IO) :-
    io.write_string("integer = ", !IO),
    io.print(Integer, !IO),
    io.write_string(", int16 = ", !IO),
    ( if integer.to_int16(Integer, U) then
        io.write_int16(U, !IO)
    else
        io.write_string("<<OUT-OF-RANGE>>", !IO)
    ),
    io.nl(!IO).

:- func test_int16s = list(int16).

test_int16s = [
    -32768i16,
    -128i16,
    -64i16,
    -32i16,
    -1i16,
    0i16,
    1i16,
    2i16,
    4i16,
    8i16,
    10i16,
    16i16,
    32i16,
    64i16,
    127i16,
    16383i16,  % i.e. integer.base - 1
    16384i16,  % i.e. integer.base
    16385i16,  % i.e. integer.base + 1,
    32767i16
].

:- func test_integers = list(integer).

test_integers = [
    det_from_string("-9223372036854775808"),
    det_from_string("-4294967296"),
    det_from_string("-2147483648"),
    det_from_string("-32768"),
    det_from_string("-2"),
    det_from_string("-1"),
    det_from_string("0"),
    det_from_string("1"),
    det_from_string("2"),
    det_from_string("16383"),
    det_from_string("16384"),
    det_from_string("16385"),
    det_from_string("32767"),
    det_from_string("1073741824"),
    det_from_string("2147483648"),
    det_from_string("4294967295"),
    det_from_string("4294967296"),
    det_from_string("4294967297"),
    det_from_string("9223372036854775808"),
    det_from_string("18446744073709551615"),
    det_from_string("18446744073709551616")
].

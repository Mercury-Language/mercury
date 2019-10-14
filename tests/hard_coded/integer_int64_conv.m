%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion of int64s to integers and integers to int64s.

:- module integer_int64_conv.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module integer.
:- import_module int64.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("*** Testing int64 -> integer conversion ***\n\n", !IO),
    list.foldl(do_to_integer_test, test_int64s, !IO),
    io.nl(!IO),
    io.write_string("*** Testing integer -> int64 conversion ***\n\n", !IO),
    list.foldl(do_from_integer_test, test_integers, !IO).

:- pred do_to_integer_test(int64::in, io::di, io::uo) is det.

do_to_integer_test(U, !IO) :-
    Integer = integer.from_int64(U),
    io.write_string("int64 = ", !IO),
    io.write_int64(U, !IO),
    io.write_string(", integer = ", !IO),
    io.print(Integer, !IO),
    io.nl(!IO).

:- pred do_from_integer_test(integer::in, io::di, io::uo) is det.

do_from_integer_test(Integer, !IO) :-
    io.write_string("integer = ", !IO),
    io.print(Integer, !IO),
    io.write_string(", int64 = ", !IO),
    ( if integer.to_int64(Integer, U) then
        io.write_int64(U, !IO)
    else
        io.write_string("<<OUT-OF-RANGE>>", !IO)
    ),
    io.nl(!IO).

:- func test_int64s = list(int64).

test_int64s = [
    -9223372036854775808i64,
    -2147483648i64,
    -32768i64,
    -128i64,
    -64i64,
    -32i64,
    -1i64,
    0i64,
    1i64,
    2i64,
    4i64,
    8i64,
    10i64,
    16i64,
    32i64,
    64i64,
    127i64,
    16383i64,
    16384i64,
    16385i64,
    32767i64,
    1073741823i64, % i.e. integer.base - 1
    1073741824i64, % i.e. integer.base
    1073741825i64, % i.e. integer.base + 1
    2147483647i64,
    9223372036854775807i64
].

:- func test_integers = list(integer).

test_integers = [
    det_from_string("-9223372036854775808"),
    det_from_string("-4294967296"),
    det_from_string("-2147483649"),
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
    det_from_string("1073741823"),
    det_from_string("1073741824"),
    det_from_string("1073741825"),
    det_from_string("2147483647"),
    det_from_string("2147483648"),
    det_from_string("4294967295"),
    det_from_string("4294967296"),
    det_from_string("4294967297"),
    det_from_string("9223372036854775807"),
    det_from_string("9223372036854775808"),
    det_from_string("18446744073709551615"),
    det_from_string("18446744073709551616")
].

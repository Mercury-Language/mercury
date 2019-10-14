%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion of uint64s to integers and integers to uint64s.

:- module integer_uint64_conv.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module integer.
:- import_module uint64.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("*** Testing uint64 -> integer conversion ***\n\n", !IO),
    list.foldl(do_to_integer_test, test_uint64s, !IO),
    io.nl(!IO),
    io.write_string("*** Testing integer -> uint64 conversion ***\n\n", !IO),
    list.foldl(do_from_integer_test, test_integers, !IO).

:- pred do_to_integer_test(uint64::in, io::di, io::uo) is det.

do_to_integer_test(U, !IO) :-
    Integer = integer.from_uint64(U),
    io.write_string("uint64 = ", !IO),
    io.write_uint64(U, !IO),
    io.write_string(", integer = ", !IO),
    io.print(Integer, !IO),
    io.nl(!IO).

:- pred do_from_integer_test(integer::in, io::di, io::uo) is det.

do_from_integer_test(Integer, !IO) :-
    io.write_string("integer = ", !IO),
    io.print(Integer, !IO),
    io.write_string(", uint64 = ", !IO),
    ( if integer.to_uint64(Integer, U) then
        io.write_uint64(U, !IO)
    else
        io.write_string("<<OUT-OF-RANGE>>", !IO)
    ),
    io.nl(!IO).

:- func test_uint64s = list(uint64).

test_uint64s = [
    0u64,
    1u64,
    2u64,
    4u64,
    8u64,
    10u64,
    16u64,
    32u64,
    64u64,
    16383u64,
    16384u64,
    16385u64,
    32768u64,
    65536u64,
    1073741823u64, % i.e. integer.base - 1
    1073741824u64, % i.e. integer.base
    1073741825u64, % i.e. integer.base + 1
    2147483647u64,
    4294967295u64,
    9223372036854775807u64,
    18446744073709551615u64
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

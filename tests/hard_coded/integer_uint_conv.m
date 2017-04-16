%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion of uints to integers and integers to uints.

:- module integer_uint_conv.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module integer.
:- import_module uint.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("*** Testing uint -> integer conversion ***\n\n", !IO),
    list.foldl(do_to_integer_test, test_uints, !IO),
    io.nl(!IO),
    io.write_string("*** Testing integer -> uint conversion ***\n\n", !IO),
    list.foldl(do_from_integer_test, test_integers, !IO).

:- pred do_to_integer_test(uint::in, io::di, io::uo) is det.

do_to_integer_test(U, !IO) :-
    Integer = integer.from_uint(U),
    io.write_string("uint = ", !IO),
    io.write_uint(U, !IO),
    io.write_string(", integer = ", !IO),
    io.print(Integer, !IO),
    io.nl(!IO).

:- pred do_from_integer_test(integer::in, io::di, io::uo) is det.

do_from_integer_test(Integer, !IO) :-
    io.write_string("integer = ", !IO),
    io.print(Integer, !IO),
    io.write_string(", uint = ", !IO),
    ( if integer.to_uint(Integer, U) then
        io.write_uint(U, !IO)
    else
        io.write_string("<<OUT-OF-RANGE>>", !IO)
    ),
    io.nl(!IO).

:- func test_uints = list(uint).

test_uints = [
    cast_from_int(0),
    cast_from_int(1),
    cast_from_int(2),
    cast_from_int(4),
    cast_from_int(8),
    cast_from_int(10),
    cast_from_int(16),
    cast_from_int(32),
    cast_from_int(64),
    cast_from_int(16383),  % i.e. integer.base - 1
    cast_from_int(16384),  % i.e. integer.base
    cast_from_int(16385),  % i.e. integer.base + 1,
    cast_from_int(32768),
    cast_from_int(65536),
    cast_from_int(int.max_int),
    cast_from_int(int.max_int) + cast_from_int(1),
    uint.max_uint
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

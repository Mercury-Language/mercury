%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion of uint8s to integers and integers to uint8s.

:- module integer_uint8_conv.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module integer.
:- import_module uint8.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("*** Testing uint8 -> integer conversion ***\n\n", !IO),
    list.foldl(do_to_integer_test, test_uint8s, !IO),
    io.nl(!IO),
    io.write_string("*** Testing integer -> uint8 conversion ***\n\n", !IO),
    list.foldl(do_from_integer_test, test_integers, !IO).

:- pred do_to_integer_test(uint8::in, io::di, io::uo) is det.

do_to_integer_test(U, !IO) :-
    Integer = integer.from_uint8(U),
    io.write_string("uint8 = ", !IO),
    io.write_uint8(U, !IO),
    io.write_string(", integer = ", !IO),
    io.print(Integer, !IO),
    io.nl(!IO).

:- pred do_from_integer_test(integer::in, io::di, io::uo) is det.

do_from_integer_test(Integer, !IO) :-
    io.write_string("integer = ", !IO),
    io.print(Integer, !IO),
    io.write_string(", uint8 = ", !IO),
    ( if integer.to_uint8(Integer, U) then
        io.write_uint8(U, !IO)
    else
        io.write_string("<<OUT-OF-RANGE>>", !IO)
    ),
    io.nl(!IO).

:- func test_uint8s = list(uint8).

test_uint8s = [
    0u8,
    1u8,
    2u8,
    4u8,
    8u8,
    10u8,
    16u8,
    32u8,
    64u8,
    255u8
].

:- func test_integers = list(integer).

test_integers = [
    det_from_string("-255"),
    det_from_string("-128"),
    det_from_string("-2"),
    det_from_string("-1"),
    det_from_string("0"),
    det_from_string("1"),
    det_from_string("2"),
    det_from_string("127"),
    det_from_string("254"),
    det_from_string("255"),
    det_from_string("256"),
    det_from_string("16383"),
    det_from_string("16384"),
    det_from_string("16385")
].

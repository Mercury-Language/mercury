%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

% Test converting strings to arbitrary precision integers.

:- module base_string_to_integer.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module integer.
:- import_module list.
:- import_module require.
:- import_module string.

main(!IO) :-
    Base2 = [
        "0",
        "1",
        "01",
        "-000001",
        "11",
        "111",
        "11111",
        "101010",
        "10000000000000000000000000000000",
        "1000000000000000000000000000000000000000000000000000000000000000",
        "10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    ],
    check_base_valid(2, Base2, !IO),
    Base8 = [
        "0",
        "1",
        "-1",
        "10",
        "-10",
        "76543210",
        "-76543210",
        "7777777777777777777777777"
    ],
    check_base_valid(8, Base8, !IO),
    Base10 = [
        "0",
        "1",
        "10",
        "11",
        "1234567890",
        "-1",
        "-10",
        "-1234567890",
        "1234567891234567891234567890",
        "-1234567891234567891234567890"
    ],
    check_base_valid(10, Base10, !IO),
    Base16 = [
        "0",
        "1",
        "-1",
        "10",
        "A",
        "-A",
        "a",
        "-a",
        "F",
        "-F",
        "fedcba0987654321",
        "-fedcba0987654321",
        "fffffffffffffffffffffffffffffffffff",
        "-fffffffffffffffffffffffffffffffffff"
    ],
    check_base_valid(16, Base16, !IO),
    Base36 = [
        "0",
        "1",
        "-1",
        "10",
        "A",
        "-A",
        "a",
        "-a",
        "Z",
        "-Z",
        "zyxwvutsrqponmlkjihgfedcba0987654321",
        "-zyxwvutsrqponmlkjihgfedcba0987654321",
        "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz",
        "-zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"
    ],
    check_base_valid(36, Base36, !IO),
    InvalidBase2 = ["3", "4", "5", "6", "a", "A", "z", "13"],
    check_base_invalid(2, InvalidBase2, !IO),
    InvalidBase10 = ["abc", "-123a", "ZZZ"],
    check_base_invalid(10, InvalidBase10, !IO).

:- pred check_base_valid(int::in, list(string)::in, io::di, io::uo) is det.

check_base_valid(_, [], !IO).
check_base_valid(Base, [String | Strings], !IO) :-
    Num = integer.det_from_base_string(Base, String),
    NumStr = integer.to_string(Num),
    io.format("%s (base %d) = %s\n", [s(String), i(Base), s(NumStr)], !IO),

    StringB = integer.to_base_string(Num, Base),
    io.format("to_base_string produces \"%s\"\n", [s(StringB)], !IO),
    io.nl(!IO),

    check_base_valid(Base, Strings, !IO).

:- pred check_base_invalid(int::in, list(string)::in, io::di, io::uo) is det.

check_base_invalid(_, [], !IO).
check_base_invalid(Base, [String | Strings], !IO) :-
    ( if integer.from_base_string(Base, String, Num) then
        NumStr = integer.to_string(Num),
        string.format("ERROR: %s (base %d) = %s\n",
            [s(String), i(Base), s(NumStr)], ErrorMsg),
        error(ErrorMsg)
    else
        check_base_invalid(Base, Strings, !IO)
    ).

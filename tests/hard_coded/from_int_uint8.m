%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion of ints to unsigned 8-bit integers.

:- module from_int_uint8.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint8.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(do_test, numbers, !IO).

:- pred do_test(int::in, io::di, io::uo) is det.

do_test(Int, !IO) :-
    io.format("from_int(%d) = ", [i(Int)], !IO),
    ( if
        uint8.from_int(Int, UInt8)
    then
        io.format("%s\n", [s(uint8_to_string(UInt8))], !IO)
    else
        io.write_string("<<out-of-range>>\n", !IO)
    ).

:- func numbers = list(int).

numbers = [
    -2147483648,
    -128,
    -1,
    0,
    1,
    2,
    8,
    10,
    16,
    127,
    255,
    256,
    2147483647
].

%---------------------------------------------------------------------------%
:- end_module from_int_uint8.
%---------------------------------------------------------------------------%

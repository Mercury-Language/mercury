%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion of Mercury ints to unsigned 16-bit integers.

:- module from_int_uint16.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module uint16.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(do_test, numbers, !IO).

:- pred do_test(int::in, io::di, io::uo) is det.

do_test(Int, !IO) :-
    io.format("from_int(%d) = ", [i(Int)], !IO),
    ( if
        uint16.from_int(Int, UInt16)
    then
        io.format("%s\n", [s(uint16_to_string(UInt16))], !IO)
    else
        io.write_string("<<out-of-range>>\n", !IO)
    ).

:- func numbers = list(int).

numbers = [
    -2147483648,
    -32768,
    -128,
    0,
    1,
    2,
    8,
    10,
    16,
    127,
    32767,
    32768,
    65534,
    65535,
    65536,
    2147483647
].

%---------------------------------------------------------------------------%
:- end_module from_int_uint16.
%---------------------------------------------------------------------------%

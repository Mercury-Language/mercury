%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion of Mercury ints to unsigned 8-bit integers.

:- module from_uint_uint8.
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

:- pred do_test(uint::in, io::di, io::uo) is det.

do_test(UInt, !IO) :-
    io.format("from_uint(%u) = ", [u(UInt)], !IO),
    ( if uint8.from_uint(UInt, UInt8) then
        io.format("%u\n", [u8(UInt8)], !IO)
    else
        io.write_string("<<out-of-range>>\n", !IO)
    ).

:- func numbers = list(uint).

numbers = [
    0u,
    1u,
    2u,
    8u,
    10u,
    16u,
    127u,
    255u,
    256u,
    2_147_483_647u
].

%---------------------------------------------------------------------------%
:- end_module from_uint_uint8.
%---------------------------------------------------------------------------%

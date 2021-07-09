%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%

% Test conversion of uints to unsigned 16-bit integers.

:- module from_uint_uint16.
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

:- pred do_test(uint::in, io::di, io::uo) is det.

do_test(UInt, !IO) :-
    io.format("from_uint(%u) = ", [u(UInt)], !IO),
    ( if uint16.from_uint(UInt, UInt16) then
        io.format("%u\n", [u16(UInt16)], !IO)
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
    32_767u,
    32_768u,
    65_534u,
    65_535u,
    65_536u,
    2_147_483_645u,
    2_147_483_647u,
    4_294_967_295u
].

%---------------------------------------------------------------------------%
:- end_module from_uint_uint16.
%---------------------------------------------------------------------------%

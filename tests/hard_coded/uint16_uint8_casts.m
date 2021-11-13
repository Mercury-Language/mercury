%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Testing casting uint16s to/from uint8s.
%
%---------------------------------------------------------------------------%

:- module uint16_uint8_casts.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.
:- import_module uint16.
:- import_module uint8.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(do_cast_from_uint8_test, uint8s, !IO),
    io.nl(!IO),
    list.foldl(do_cast_to_uint8_test, uint16s, !IO).

:- pred do_cast_from_uint8_test(uint8::in, io::di, io::uo) is det.

do_cast_from_uint8_test(U8, !IO) :-
    io.format("cast_from_uint8(%uu8) = %uu16\n",
        [u8(U8), u16(cast_from_uint8(U8))], !IO).

:- pred do_cast_to_uint8_test(uint16::in, io::di, io::uo) is det.

do_cast_to_uint8_test(U16, !IO) :-
    io.format("cast_to_uint8(%uu16) = %uu8\n",
        [u16(U16), u8(cast_to_uint8(U16))], !IO).

:- func uint8s = list(uint8).

uint8s = [
    0u8,
    7u8,
    8u8,
    15u8,
    16u8,
    31u8,
    32u8,
    63u8,
    64u8,
    127u8,
    128u8,
    254u8,
    255u8
].

:- func uint16s = list(uint16).

uint16s = [
    0u16,
    7u16,
    8u16,
    15u16,
    16u16,
    31u16,
    32u16,
    63u16,
    64u16,
    127u16,
    128u16,
    254u16,
    255u16
].

%---------------------------------------------------------------------------%
:- end_module uint16_uint8_casts.
%---------------------------------------------------------------------------%

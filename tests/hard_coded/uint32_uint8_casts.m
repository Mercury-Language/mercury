%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test casting uint32s to/from uint8s.
%
%---------------------------------------------------------------------------%

:- module uint32_uint8_casts.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.
:- import_module uint32.
:- import_module uint8.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(do_cast_from_uint8_test, uint8s, !IO),
    io.nl(!IO),
    list.foldl(do_cast_to_uint8_test, uint32s, !IO).

:- pred do_cast_from_uint8_test(uint8::in, io::di, io::uo) is det.

do_cast_from_uint8_test(U8, !IO) :-
    io.format("cast_from_uint8(%uu8) = %uu32\n",
        [u8(U8), u32(cast_from_uint8(U8))], !IO).

:- pred do_cast_to_uint8_test(uint32::in, io::di, io::uo) is det.

do_cast_to_uint8_test(U32, !IO) :-
    io.format("cast_to_uint8(%uu32) = %uu8\n",
        [u32(U32), u8(cast_to_uint8(U32))], !IO).

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

:- func uint32s = list(uint32).

uint32s = [
    0u32,
    7u32,
    8u32,
    15u32,
    16u32,
    31u32,
    32u32,
    63u32,
    64u32,
    127u32,
    128u32,
    254u32,
    255u32
].

%---------------------------------------------------------------------------%
:- end_module uint32_uint8_casts.
%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test casting uint32s to/from uint16s.
%
%---------------------------------------------------------------------------%

:- module uint32_uint16_casts.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.
:- import_module uint32.
:- import_module uint16.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(do_cast_from_uint16_test, uint16s, !IO),
    io.nl(!IO),
    list.foldl(do_cast_to_uint16_test, uint32s, !IO).

:- pred do_cast_from_uint16_test(uint16::in, io::di, io::uo) is det.

do_cast_from_uint16_test(U8, !IO) :-
    io.format("cast_from_uint16(%uu16) = %uu32\n",
        [u16(U8), u32(cast_from_uint16(U8))], !IO).

:- pred do_cast_to_uint16_test(uint32::in, io::di, io::uo) is det.

do_cast_to_uint16_test(U32, !IO) :-
    io.format("cast_to_uint16(%uu32) = %uu16\n",
        [u32(U32), u16(cast_to_uint16(U32))], !IO).

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
    255u16,
    32766u16,
    32767u16,
    32768u16,
    65534u16,
    65535u16
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
    255u32,
    32766u32,
    32767u32,
    32768u32,
    65534u32,
    65535u32
].

%---------------------------------------------------------------------------%
:- end_module uint32_uint16_casts.
%---------------------------------------------------------------------------%

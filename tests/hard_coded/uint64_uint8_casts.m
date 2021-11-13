%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test casting uint64s to/from uint8s.
%
%---------------------------------------------------------------------------%

:- module uint64_uint8_casts.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.
:- import_module uint64.
:- import_module uint8.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(do_cast_from_uint8_test, uint8s, !IO),
    io.nl(!IO),
    list.foldl(do_cast_to_uint8_test, uint64s, !IO).

:- pred do_cast_from_uint8_test(uint8::in, io::di, io::uo) is det.

do_cast_from_uint8_test(U8, !IO) :-
    io.format("cast_from_uint8(%uu8) = %uu64\n",
        [u8(U8), u64(cast_from_uint8(U8))], !IO).

:- pred do_cast_to_uint8_test(uint64::in, io::di, io::uo) is det.

do_cast_to_uint8_test(U32, !IO) :-
    io.format("cast_to_uint8(%uu64) = %uu8\n",
        [u64(U32), u8(cast_to_uint8(U32))], !IO).

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

:- func uint64s = list(uint64).

uint64s = [
    0u64,
    7u64,
    8u64,
    15u64,
    16u64,
    31u64,
    32u64,
    63u64,
    64u64,
    127u64,
    128u64,
    254u64,
    255u64
].

%---------------------------------------------------------------------------%
:- end_module uint64_uint8_casts.
%---------------------------------------------------------------------------%

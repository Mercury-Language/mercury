%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test casting int32s to/from int16s.
%
%---------------------------------------------------------------------------%

:- module int32_int16_casts.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.
:- import_module int32.
:- import_module int16.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(do_cast_from_int16_test, int16s, !IO),
    io.nl(!IO),
    list.foldl(do_cast_to_int16_test, int32s, !IO).

:- pred do_cast_from_int16_test(int16::in, io::di, io::uo) is det.

do_cast_from_int16_test(I16, !IO) :-
    io.format("cast_from_int16(%di16) = %di16\n",
        [i16(I16), i32(cast_from_int16(I16))], !IO).

:- pred do_cast_to_int16_test(int32::in, io::di, io::uo) is det.

do_cast_to_int16_test(I32, !IO) :-
    io.format("cast_to_int16(%di32) = %di16\n",
        [i32(I32), i16(cast_to_int16(I32))], !IO).

:- func int16s = list(int16).

int16s = [
    -32768i16,
    -128i16,
    -64i16,
    -32i16
    -16i16
    -8i16,
    -1i16,
    0i16,
    1i16,
    7i16,
    8i16,
    15i16,
    16i16,
    31i16,
    32i16,
    63i16,
    64i16,
    127i16,
    128i16,
    32767i16
].

:- func int32s = list(int32).

int32s = [
    -32768i32
    -128i32,
    -64i32,
    -32i32,
    -16i32,
    -8i32,
    -1i32,
    0i32,
    1i32,
    7i32,
    8i32,
    15i32,
    16i32,
    31i32,
    32i32,
    63i32,
    64i32,
    127i32,
    128i32,
    32767i32
].

%---------------------------------------------------------------------------%
:- end_module int32_int16_casts.
%---------------------------------------------------------------------------%

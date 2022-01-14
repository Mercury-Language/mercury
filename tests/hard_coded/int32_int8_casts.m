%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test casting int32s to/from int8s.
%
%---------------------------------------------------------------------------%

:- module int32_int8_casts.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.
:- import_module int32.
:- import_module int8.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(do_cast_from_int8_test, int8s, !IO),
    io.nl(!IO),
    list.foldl(do_cast_to_int8_test, int32s, !IO).

:- pred do_cast_from_int8_test(int8::in, io::di, io::uo) is det.

do_cast_from_int8_test(I8, !IO) :-
    io.format("cast_from_int8(%di8) = %di8\n",
        [i8(I8), i32(cast_from_int8(I8))], !IO).

:- pred do_cast_to_int8_test(int32::in, io::di, io::uo) is det.

do_cast_to_int8_test(I32, !IO) :-
    io.format("cast_to_int8(%di32) = %di8\n",
        [i32(I32), i8(cast_to_int8(I32))], !IO).

:- func int8s = list(int8).

int8s = [
    -128i8,
    -64i8,
    -32i8
    -16i8
    -8i8,
    -1i8,
    0i8,
    1i8,
    7i8,
    8i8,
    15i8,
    16i8,
    31i8,
    32i8,
    63i8,
    64i8,
    127i8
].

:- func int32s = list(int32).

int32s = [
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
    127i32
].

%---------------------------------------------------------------------------%
:- end_module int32_int8_casts.
%---------------------------------------------------------------------------%

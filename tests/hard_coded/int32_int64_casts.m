%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test casting int32s to/from int64s.
%
%---------------------------------------------------------------------------%

:- module int32_int64_casts.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.
:- import_module int32.
:- import_module int64.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(do_cast_from_int64_test, int64s, !IO),
    io.nl(!IO),
    list.foldl(do_cast_to_int64_test, int32s, !IO).

:- pred do_cast_from_int64_test(int64::in, io::di, io::uo) is det.

do_cast_from_int64_test(I64, !IO) :-
    io.format("cast_from_int64(%di64) = %di64\n",
        [i64(I64), i32(cast_from_int64(I64))], !IO).

:- pred do_cast_to_int64_test(int32::in, io::di, io::uo) is det.

do_cast_to_int64_test(I32, !IO) :-
    io.format("cast_to_int64(%di32) = %di64\n",
        [i32(I32), i64(cast_to_int64(I32))], !IO).

:- func int64s = list(int64).

int64s = [
    -2147483648i64,
    -2147483647i64,
    -32768i64,
    -128i64,
    -64i64,
    -32i64
    -16i64
    -8i64,
    -1i64,
    0i64,
    1i64,
    7i64,
    8i64,
    15i64,
    16i64,
    31i64,
    32i64,
    63i64,
    64i64,
    127i64,
    128i64,
    32767i64,
    2147483647i64
].

:- func int32s = list(int32).

int32s = [
    -2147483648i32,
    -2147483647i32,
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
    32767i32,
    2147483647i32
].

%---------------------------------------------------------------------------%
:- end_module int32_int64_casts.
%---------------------------------------------------------------------------%
